% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Universal ICC Jurisdiction (Universalist Reading)
 *   domain: international_law/treaty_interpretation
 *
 * SUMMARY:
 *   The Rome Statute, adopted in 1998 and entering force in 2002, is subject
 *   to competing constitutional readings. The universalist reading claims the
 *   statute establishes inherent ICC jurisdiction over core crimes based on
 *   territorial presence and victim status, independent of the accused's
 *   state's consent. Non-party states assert this violates foundational
 *   international law principles of state consent and sovereignty. This story
 *   captures the universalist instantiation: the reading that claims the
 *   statute codifies a trans-sovereign mandate for accountability. The kernel
 *   itself—the text of the statute and its foundational authority claim—is
 *   fixed; the readings diverge on what jurisdictional authority the statute
 *   instantiates.
 *
 * KEY AGENTS:
 *   - International Criminal Court: institutional agenda-setter, interprets Rome Statute as granting universal jurisdiction
 *   - Non-party states (US, Russia, China, India): institutional victims, exposed to jurisdiction without consent
 *   - Victims of core crimes globally: powerless beneficiaries, gain standing in ICC regardless of state affiliation
 *   - Military personnel of non-parties: moderate-power payers, identity-locked into exposure
 *   - UNSC permanent members: asymmetrically positioned, can invoke ICC jurisdiction while retaining veto
 *   - Human rights advocates: organized beneficiaries, mobilize to support expansive interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.68).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.72).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universal ICC Jurisdiction (Universalist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/treaty_interpretation").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, '5eddf5b2-33b7-4a70-8c88-5f0ec4807361').
narrative_ontology:cs_kernel_codification('5eddf5b2-33b7-4a70-8c88-5f0ec4807361', fixed_text).
narrative_ontology:cs_authority_grounding('5eddf5b2-33b7-4a70-8c88-5f0ec4807361', extraction).
narrative_ontology:cs_interpretation_layer_present('5eddf5b2-33b7-4a70-8c88-5f0ec4807361').
narrative_ontology:cs_reading_relation('5eddf5b2-33b7-4a70-8c88-5f0ec4807361', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('5eddf5b2-33b7-4a70-8c88-5f0ec4807361', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('5eddf5b2-33b7-4a70-8c88-5f0ec4807361', foundational, core_crimes_are_universal_wrongs).
narrative_ontology:cs_axiom_status(core_crimes_are_universal_wrongs, holdable).
narrative_ontology:cs_axiom_grounding('5eddf5b2-33b7-4a70-8c88-5f0ec4807361', core_crimes_are_universal_wrongs, deontological).
narrative_ontology:cs_axiom('5eddf5b2-33b7-4a70-8c88-5f0ec4807361', foundational, jurisdiction_transcends_state_consent).
narrative_ontology:cs_axiom_status(jurisdiction_transcends_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('5eddf5b2-33b7-4a70-8c88-5f0ec4807361', jurisdiction_transcends_state_consent, deontological).
narrative_ontology:cs_reference_frame('5eddf5b2-33b7-4a70-8c88-5f0ec4807361', universal_jurisdictional_mandate).
narrative_ontology:cs_drift_state('5eddf5b2-33b7-4a70-8c88-5f0ec4807361', contemporary_post_2020, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5eddf5b2-33b7-4a70-8c88-5f0ec4807361', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_criminal_court).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes_globally).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, human_rights_advocates).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, military_personnel_of_non_parties).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, national_judiciaries_losing_primacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, unsc_permanent_members).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, party_states_providing_cooperation).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, contested_state_of_palestine).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, party_states_providing_cooperation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ICC interprets the Rome Statute as granting it authority to prosecute nationals of non-party states when crimes occur on the territory of parties, or when the UNSC refers situations. It administers this jurisdiction through prosecutorial discretion and pre-trial chambers, issuing arrest warrants that assert enforcement reach beyond any single state's borders. The universalist reading frames this as an assertion that core crimes (genocide, crimes against humanity, war crimes) are inherently international wrongs, not subject to sovereign exemption.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_criminal_court, agenda_setter,
    institutional, generational, analytical, global).

% States that have not ratified the Rome Statute (including major powers like the US, Russia, China, and India) are exposed to ICC jurisdiction when their nationals commit crimes on party-state territory or when the UNSC acts. They bear the cost of assertion of external authority over their nationals without having consented to the statute. Their exit options are formal accession (a political act with domestic implications) or diplomatic non-cooperation (constrained by reputational and institutional pressure).
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_states, payer,
    institutional, generational, constrained, global).

% Persons harmed by genocide, crimes against humanity, and war crimes derive a potential path to justice through the ICC regardless of whether their home state ratified the Rome Statute or cooperates with prosecution. The universalist reading asserts that their status as crime victims creates standing that transcends state borders and consent regimes. Their access to ICC processes depends on situational referral, prosecutorial discretion, and the cooperation of party states where investigations occur.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes_globally, beneficiary,
    powerless, immediate, trapped, global).

% Soldiers and commanders of non-party states who engage in armed conflict in party-state territory face exposure to ICC prosecution without their nation having consented to the statute. Their national identity and military obligation are fused with the potential legal risk—they cannot exit the national armed forces without exiting a core identity commitment. The universalist interpretation subjects them to international law they had no formal voice in adopting.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, military_personnel_of_non_parties, payer,
    moderate, biographical, identity_locked, global).

% Courts of non-party states see their exclusive jurisdiction over their nationals' conduct eroded when the ICC asserts authority. Under the universalist reading, the ICC's reach is not subordinate to national trial capacity or willingness—it operates as a superior forum for crimes deemed 'of concern to the international community.' National judiciaries retain formal authority but face pressure to subordinate to the ICC's mandate and cooperate with investigations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, national_judiciaries_losing_primacy, payer,
    organized, generational, constrained, national).

% The UNSC's power to refer situations to the ICC grants permanent members (especially those not party to the statute) a mechanism to weaponize ICC jurisdiction against adversaries while potentially exempting themselves or allies. The universalist reading treats UNSC referral as activating latent ICC authority; permanent members can selectively invoke this without being bound by it themselves, generating asymmetric exposure for non-cooperating states.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, unsc_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, unsc_permanent_members, beneficiary).

% NGOs and civil society organizations advocating for accountability for international crimes benefit from the universalist reading because it expands the potential reach of justice mechanisms. They fund investigations, submit briefs, and lobby states to cooperate with the ICC. They are not constrained by the statute but mobilize to support its expansive interpretation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% States that ratified the Rome Statute incur obligations to cooperate with ICC investigations and arrest warrants. Under the universalist reading, they become enforcement arms for a jurisdiction that extends beyond their territory. They benefit from the deterrent effect and the legitimacy of participation in an international institution, but bear the diplomatic and operational costs of enforcing the ICC's universal mandate against non-party nationals.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, party_states_providing_cooperation, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, party_states_providing_cooperation, beneficiary).

% Palestine's accession to the Rome Statute and subsequent ICC investigations of Israeli conduct exemplifies the universalist reading: a non-sovereign entity gains standing to invoke ICC jurisdiction, asserting its status as a victim of international crimes regardless of formal state status. The universalist interpretation treats Palestine's ICC membership as granting it parity with traditional states in access to international justice.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, contested_state_of_palestine, beneficiary,
    powerless, immediate, identity_locked, regional).

% International law scholars and state representatives who hold that jurisdiction requires express consent are excluded from the ICC's institutional forums but present in diplomatic and academic discourse. They argue that the universalist reading violates the foundational principle of consent-based obligation in international law and that it creates accountability without representation—the core charge against arbitrary power.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, sovereignty_traditionalists, excluded,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__universalist_reading, international_criminal_court).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single institutional venue for prosecution of core crimes (genocide, crimes against humanity, war crimes) that transcends state boundaries and creates a unified standard of accountability for conduct deemed to harm the international community itself, rather than individual states.
% TRANSFER_FUNCTION: Transfers enforcement authority from national judiciaries and the UNSC's case-by-case discretion to a permanent international institution with standing to prosecute individuals based on the location of conduct and the status of the injured parties, regardless of the accused person's state citizenship or the state's Rome Statute membership.
% ABSENT_VOICES: States that have not ratified the Rome Statute and who reject the universalist interpretation (the US, Russia, China, India) are not seated in the ICC's governance but are subject to its jurisdiction assertions. They would argue that universal criminal jurisdiction without consent violates fundamental principles of state consent and the Westphalian system; their voices are structurally excluded from the ICC's institutional decision-making.
% DISAPPEARANCE_RATIONALE: If the universalist reading of the Rome Statute vanished—if the ICC were forced to recognize only consensual jurisdiction—the institutional landscape would reorganize: non-party states would gain de facto immunity from ICC prosecution absent their ratification, justice mechanisms would revert to UNSC referral and national prosecution, and the reach of international criminal law would contract substantially. Victims in non-party-state territories would lose a forum for accountability.
% FOUNDING_PROBLEM: After the Cold War, atrocities in Rwanda, the former Yugoslavia, and elsewhere revealed that national judicial systems were either unwilling or unable to prosecute their own officials for mass crimes, and the UNSC was politically deadlocked. The Rome Statute was intended to create a permanent institution capable of addressing this accountability gap—to ensure that core crimes did not go unpunished due to state failure or sovereignty shield.
% FOUNDING_PROBLEM_CORROBORATION: The ICC's preamble and prosecutor's office assert the founding problem remains live: state failures in prosecuting international crimes persist, and the ICC fills a necessary gap. However, major non-party states and scholars attesting from outside the ICC assert that the founding problem (unwilling/unable states) has been substantially addressed in some regions through hybrid courts, domestic reforms, and regional mechanisms; the ICC's universalist jurisdiction persists now as an assertion of institutional authority rather than a response to systematic failure. Empirically, the ICC's conviction rate is low and its reach is geographically uneven—facts contested by the institutions' beneficiaries and acknowledged by critics.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).

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
 *   Extractiveness is substantial (0.68) because the universalist reading asserts jurisdiction over non-consenting states' nationals, extracting enforcement obligation without prior agreement. The constraint imposes a transfer of authority from national judiciaries to an international forum. Suppression is high (0.72) because the ICC must actively prevent non-party states from withdrawing from cooperation and must defend its jurisdiction against sustained diplomatic challenge. Theater is moderate (0.41): the ICC performs genuine accountability (real trials, real victims represented), but growing portions of its operating budget and legitimacy discourse go to defending the jurisdictional assertion itself rather than executing trials. The measurement series show extraction and theater rising over 24 years as the ICC faces resistance and must increasingly justify its reach. Suppression rises as non-party states develop coordination strategies and the ICC's enforcement machinery hardened to overcome non-cooperation.
 *
 * PERSPECTIVAL GAP:
 *   From the ICC's institutional seat, the universalist reading is a coherent interpretation of a statute designed to fill an accountability gap—authority follows from the text and the mandate. From non-party-state seats, the same reading appears as assertion of authority without consent. From victim seats in non-party states, the reading opens a forum that would otherwise be closed. The engine computes these divergences from the stakeholder power and exit configurations: the ICC is institutional + analytical exit (it defines the constraint's meaning), while non-parties are institutional + constrained exit (diplomacy or accession are costly). The victim seats are powerless + trapped (no exit from harm or status as victims). This structural asymmetry explains why the same constraint registers differently: the agenda-setter controls its interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC operates at d ≈ 0.0–0.2 (beneficiary framing: it sets rules, faces no binding external authority, exits via sovereignty claim). Non-party states sit at d ≈ 0.8–0.95 (target framing: they bear enforcement exposure, constrained exit via diplomacy or accession, no seat in ICC governance). Victims globally sit at d ≈ 0.3–0.5 (mixed: they gain a forum, but trapped in immediate circumstances and dependent on prosecutorial discretion). UNSC permanent members sit at d ≈ 0.1–0.4 (they control referral as a weapon but are insulated from the statute itself). The directionality derivation shows why the constraint asymmetrically extracts from non-parties while benefiting the institution and selected victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state failure in prosecuting mass atrocities) is contested: some evidence suggests it has been substantially addressed through hybrid courts and domestic reform, yet the ICC persists with a universalist interpretation that expands beyond addressing that gap. The theater ratio rising from 0.28 to 0.41 over 24 years indicates that an increasing share of the ICC's activity is defensive—justifying its reach, negotiating cooperation, responding to non-party resistance—rather than prosecuting cases. This is the classic piton drift: a coordination mechanism (addressing the accountability gap) whose primary function atrophies while the extraction (assertion of universal jurisdiction) persists because the institution's survival depends on it. However, the tangled_rope classification holds because the constraint still contains genuine coordination (victims DO gain access, crimes ARE prosecuted) alongside extraction (authority is asserted without consent). The constraint has not yet fully degraded into piton; it remains a hybrid structure where coordination and extraction co-exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_foreclosure,
    'Does the universalist reading logically foreclose the sovereigntist reading within the same interpretive framework, or do they represent live competing positions held by different institutional actors?',
    'Analysis of the Rome Statute''s text on jurisdiction (Article 12-13): does the language permit ONLY the universalist interpretation, or are both readings linguistically defensible? Examine whether major signatory states can rationally maintain the sovereigntist reading without internal contradiction.',
    'If the universalist reading strictly forecloses the sovereigntist reading, the constraint is a resolved legal question with institutional enforcement authority. If both readings are defensible, the constraint represents a contested kernel where institutional power determines which reading prevails, not textual clarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the Rome Statute''s language permits multiple defensible readings of jurisdiction or mandates only the universalist interpretation.').

omega_variable(
    victim_standing_vs_state_consent,
    'On what grounds does the universalist reading assert that victims'' status as harmed persons creates standing that supersedes state consent requirements? Is this rooted in the statute''s text, in customary international law, in natural law, or in institutional necessity?',
    'Track how the ICC''s preamble, statutes, and case law ground this claim. Compare with academic commentary from non-institutional sources (states, scholars outside ICC constituencies). Determine whether the grounding is textual, doctrinal, or pragmatic.',
    'If grounding is purely institutional (the ICC asserts it), the reading is a product of institutional power, not independent authority. If grounding is textual or widely endorsed, it has stronger legitimacy. This affects whether the extraction is justified coordination cost or unjustified institutional overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_standing_vs_state_consent, conceptual, 'The epistemic ground of the victim-standing claim in the universalist reading.').

omega_variable(
    unsc_referral_asymmetry,
    'Does the UNSC''s ability to refer situations to the ICC while retaining veto power over the institution itself constitute a structural corruption of the universalist mandate? Does universal jurisdiction that can be selectively weaponized by permanent members remain universal?',
    'Examine patterns of UNSC referrals and vetoes (who refers whom, which situations are blocked). Determine whether the ICC acts as an independent institution or as an agent of permanent-member strategic interests when UNSC referral occurs.',
    'If the UNSC referral mechanism is asymmetrically used to target non-allies of permanent members, the universalist reading''s claim to transcend power politics is falsified—it becomes a forum for managed extraction via institutional proxy. If referrals follow principled patterns indifferent to permanent-member interests, the universalist claim holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unsc_referral_asymmetry, empirical, 'Whether the UNSC''s gatekeeping role corrupts the ICC''s universalist mandate.').

omega_variable(
    identity_lock_in_military_roles,
    'For personnel of non-party states, how much of their suppression is structural (legal exposure) and how much is internalized (belief that national duty overrides ICC exposure, fusion of military identity with national sovereignty)?',
    'Post-defection or post-decommissioning trajectory: if personnel who leave military service retain belief in the ICC''s illegitimacy over them, suppression is internalized. If they relocate their ethical reasoning to international law frameworks upon exit, suppression was structural. Interview soldiers and veterans of non-party states about their subjective experience of the ICC jurisdiction.',
    'If suppression is primarily structural, removing legal exposure (via accession or ICC reform) would free choice. If suppression is internalized, the identity-lock persists even after structural exit—the constraint is more extractive and more difficult to remedy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_military_roles, empirical, 'The balance between structural and internalized suppression for military personnel of non-parties.').

omega_variable(
    complementarity_mechanism_reality,
    'The Rome Statute''s complementarity principle (Article 17) claims the ICC is subordinate to national prosecution. Does the universalist reading genuinely honor this, or has institutional practice made the ICC''s role superior regardless of national court capacity?',
    'Examine ICC case law on admissibility decisions: how often does the ICC defer to national proceedings? How much capacity do national courts need to demonstrate before the ICC steps back? Determine whether complementarity is operational or ceremonial.',
    'If complementarity is real, the universalist reading retains coordination legitimacy (it respects national primary authority). If complementarity is theater, the extraction is less justified and the reading is more authoritarian—it claims universalism while implementing institutional supremacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_mechanism_reality, empirical, 'Whether the complementarity mechanism is operationally real or ceremonial in ICC practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 2002, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2002, 0.28).
narrative_ontology:measurement_basis(rome_tr_t2002, projected).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2008, 0.32).
narrative_ontology:measurement_basis(rome_tr_t2008, observed).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2014, 0.37).
narrative_ontology:measurement_basis(rome_tr_t2014, observed).
narrative_ontology:measurement(rome_tr_t2020, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(rome_tr_t2020, observed).
narrative_ontology:measurement(rome_tr_t2026, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2026, 0.41).
narrative_ontology:measurement_basis(rome_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2002, 0.45).
narrative_ontology:measurement_basis(rome_be_t2002, projected).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2008, 0.52).
narrative_ontology:measurement_basis(rome_be_t2008, observed).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2014, 0.61).
narrative_ontology:measurement_basis(rome_be_t2014, observed).
narrative_ontology:measurement(rome_be_t2020, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(rome_be_t2020, observed).
narrative_ontology:measurement(rome_be_t2026, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(rome_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2002, 0.58).
narrative_ontology:measurement_basis(rome_su_t2002, projected).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2008, 0.64).
narrative_ontology:measurement_basis(rome_su_t2008, observed).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2014, 0.69).
narrative_ontology:measurement_basis(rome_su_t2014, observed).
narrative_ontology:measurement(rome_su_t2020, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(rome_su_t2020, observed).
narrative_ontology:measurement(rome_su_t2026, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(rome_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__universalist_reading, 0.18).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% The Rome Statute's jurisdictional authority is contested across three structural readings forming a kernel family. The universalist_reading (this story) claims inherent ICC jurisdiction transcending consent; the sovereigntist_reading claims jurisdiction requires express state consent; the hybrid_complementarity_reading claims jurisdiction balances universal aspiration with national primacy. Each reading instantiates a different constraint with different ε values and stakeholder structures. The universalist reading is the ICC's institutional interpretation and has become procedurally dominant; the sovereigntist and hybrid readings persist as live positions held by non-party states and some signatory states. This constraint's high extractiveness (0.68) reflects the institutional assertion of authority without consent; sibling readings would show different structural positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__universalist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
