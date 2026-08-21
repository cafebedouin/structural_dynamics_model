% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Universal Jurisdiction Mandate (Universalist Reading)
 *   domain: international_law/justice/sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'universalist reading' of the Rome
 *   Statute's jurisdiction, which posits a broad mandate for international
 *   criminal justice that can transcend state consent, particularly through
 *   territorial jurisdiction or UN Security Council referrals. This reading
 *   emphasizes the gravity of core international crimes as a universal
 *   concern, justifying the ICC's authority even over non-party states. The
 *   constraint is claimed as a 'rope' by its proponents, framing it as a
 *   necessary coordination mechanism for global justice, but its operational
 *   metrics reflect significant extraction and suppression from targeted
 *   non-party states.
 *
 * KEY AGENTS:
 *   - international_criminal_court: Primary agenda_setter (institutional/constrained)
 *   - states_parties: Beneficiaries (organized/constrained)
 *   - non_party_states_targeted: Primary payers (institutional/trapped)
 *   - victims_of_atrocities: Primary beneficiaries (powerless/trapped)
 *   - international_justice_advocates: Beneficiaries (organized/mobile)
 *   - accused_individuals: Payers (powerless/trapped)
 *   - un_security_council: Agenda_setter (institutional/arbitrage)
 *   - sovereigntist_scholars: Excluded voices (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.75).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.65).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universal Jurisdiction Mandate (Universalist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/justice/sovereignty").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, '11e7fcde-9b4e-4cd0-88e3-c7094440b3db').
narrative_ontology:cs_kernel_codification('11e7fcde-9b4e-4cd0-88e3-c7094440b3db', fixed_text).
narrative_ontology:cs_authority_grounding('11e7fcde-9b4e-4cd0-88e3-c7094440b3db', lineage).
narrative_ontology:cs_interpretation_layer_present('11e7fcde-9b4e-4cd0-88e3-c7094440b3db').
narrative_ontology:cs_reading_relation('11e7fcde-9b4e-4cd0-88e3-c7094440b3db', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_reading_relation('11e7fcde-9b4e-4cd0-88e3-c7094440b3db', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_axiom('11e7fcde-9b4e-4cd0-88e3-c7094440b3db', foundational, atrocity_crimes_universal_concern).
narrative_ontology:cs_axiom_status(atrocity_crimes_universal_concern, holdable).
narrative_ontology:cs_axiom_grounding('11e7fcde-9b4e-4cd0-88e3-c7094440b3db', atrocity_crimes_universal_concern, deontological).
narrative_ontology:cs_axiom('11e7fcde-9b4e-4cd0-88e3-c7094440b3db', foundational, state_sovereignty_not_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('11e7fcde-9b4e-4cd0-88e3-c7094440b3db', state_sovereignty_not_absolute, deontological).
narrative_ontology:cs_reference_frame('11e7fcde-9b4e-4cd0-88e3-c7094440b3db', post_nuremberg_era_justice).
narrative_ontology:cs_drift_state('11e7fcde-9b4e-4cd0-88e3-c7094440b3db', contemporary_geopolitical_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('11e7fcde-9b4e-4cd0-88e3-c7094440b3db', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_atrocities).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_justice_advocates).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states_targeted).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, accused_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, states_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institution asserting and exercising jurisdiction over core international crimes, including over nationals of non-party states under specific conditions (territoriality, UNSC referral). It interprets the Rome Statute to maximize its universal reach.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% States that have ratified the Rome Statute and generally support the ICC's mandate. They benefit from a system of international accountability but are also subject to its jurisdiction, even if they may occasionally disagree with its application.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, states_parties, beneficiary,
    organized, biographical, constrained, global).

% States that have not ratified the Rome Statute and actively resist the ICC's assertion of jurisdiction over their nationals or territory. They bear the cost of this asserted authority through diplomatic pressure, potential sanctions, and the risk of their citizens being prosecuted.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_states_targeted, payer,
    institutional, generational, trapped, global).

% Individuals who have suffered genocide, war crimes, crimes against humanity, or aggression. They are the direct beneficiaries of the universalist reading, which seeks to provide them justice regardless of their state's consent or capacity.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_atrocities, beneficiary,
    powerless, immediate, trapped, local).

% NGOs, legal scholars, and activists who champion the cause of international criminal justice and support the broadest possible interpretation of the ICC's jurisdiction. They benefit from the institutionalization and expansion of this mandate.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_justice_advocates, beneficiary,
    organized, generational, mobile, global).

% Individuals accused of core international crimes, particularly those from non-party states over whom jurisdiction is asserted. They are the direct targets of the universalist mandate, facing potential arrest, trial, and imprisonment by the ICC.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, accused_individuals, payer,
    powerless, biographical, trapped, global).

% Can refer situations to the ICC, thereby granting jurisdiction even over non-party states. Its actions are a key enabler of the universalist reading's practical application, though its political nature can also limit it.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% Legal and political theorists who argue for the absolute primacy of state sovereignty and strict consent-based jurisdiction in international law. Their arguments are often marginalized or dismissed by proponents of the universalist reading.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, sovereigntist_scholars, excluded,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__universalist_reading, international_criminal_court).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a permanent international court to prosecute individuals for the most serious international crimes, ensuring accountability where national systems are unwilling or unable to act, and to deter future atrocities.
% TRANSFER_FUNCTION: Transfers the authority to prosecute core international crimes from national states to the ICC, particularly when states are unwilling or unable to do so, and transfers individuals to ICC custody for trial.
% ABSENT_VOICES: Non-party states (especially those targeted by ICC investigations) and legal scholars who prioritize absolute state sovereignty would object, arguing that the universalist interpretation undermines fundamental principles of international law and state consent.
% DISAPPEARANCE_RATIONALE: If the universalist interpretation of Rome Statute jurisdiction vanished, the international legal framework for atrocity crimes would significantly weaken. Accountability for crimes in non-party states would largely revert to ad hoc political solutions or be left to national systems, leading to greater impunity and a fragmentation of international justice efforts.
% FOUNDING_PROBLEM: The historical problem of widespread impunity for perpetrators of genocide, war crimes, and crimes against humanity, often due to national governments' unwillingness or inability to prosecute, leading to cycles of violence and injustice.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN bodies, and independent legal experts consistently document ongoing atrocities and the persistent challenge of impunity, corroborating that the founding problem remains live and urgent. This corroboration comes from sources outside the direct beneficiaries of the ICC's expanded jurisdiction.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) reflects the imposition of jurisdiction on non-consenting states, extracting their sovereign prerogative. Suppression (0.65) is substantial due to the diplomatic and legal pressure exerted to enforce this jurisdiction, limiting alternatives for targeted states. The theater ratio (0.40) indicates that while the ICC performs genuine justice functions, a significant portion of its activity involves asserting and defending its universal mandate against political resistance, sometimes without immediate practical effect. Resistance (0.80) is high, stemming from non-party states and those who view the ICC's claims as an infringement on sovereignty. Accessibility collapse (0.70) is high for targeted states, as the ICC's jurisdiction, once triggered, leaves few legal alternatives for avoiding prosecution.
 *
 * PERSPECTIVAL GAP:
 *   The international_criminal_court and international_justice_advocates perceive this constraint as a vital 'rope' for global coordination against impunity, where the costs are justified by the universal good of justice. Conversely, non_party_states_targeted and accused_individuals experience it as a 'snare' or 'tangled_rope', an extractive imposition on their sovereignty or liberty, enforced by a body to which they have not consented. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The international_criminal_court and international_justice_advocates are clear beneficiaries, gaining authority and legitimacy from the universalist mandate. Victims_of_atrocities are also direct beneficiaries, as the constraint aims to deliver justice to them. Non_party_states_targeted and accused_individuals are the primary targets/payers, bearing the costs of asserted jurisdiction and potential prosecution. States_parties are beneficiaries of the system's existence but also subject to its rules, placing them closer to symmetric. The UN_security_council acts as an enabler, amplifying the constraint's reach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_jurisdiction_legitimacy,
    'Is the claim of universal jurisdiction over non-party nationals, particularly via territoriality or UNSC referral, truly legitimate under established international law, or does it represent an evolving, contested interpretation?',
    'Consensus among international legal scholars and state practice over time, or a definitive ruling by an international court with universal jurisdiction over states (e.g., ICJ on a related matter).',
    'If deemed fully legitimate, the constraint''s ''rope'' claim gains stronger footing. If contested, its extractive and suppressive aspects are amplified, pushing it towards ''tangled_rope'' or ''snare'' for non-consenting parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_jurisdiction_legitimacy, conceptual, 'Ambiguity regarding the legal basis for universal jurisdiction over non-parties.').

omega_variable(
    icc_enforcement_capacity_gap,
    'Does the ICC possess sufficient independent enforcement capacity (e.g., arrest, evidence collection) to realize its universal mandate, or is its effectiveness fundamentally limited by reliance on state cooperation and political will?',
    'Empirical analysis of arrest warrant execution rates, cooperation levels from states, and the impact of political pressure on investigations and prosecutions.',
    'If enforcement capacity is severely limited, the ''theater_ratio'' would increase, and the constraint might drift towards a ''piton'' for its universal claims, as the performance of justice outstrips its actual reach. If capacity is robust, the ''rope'' function is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(icc_enforcement_capacity_gap, empirical, 'Gap between claimed universal mandate and actual enforcement power.').

omega_variable(
    sovereignty_vs_justice_priority,
    'To what extent should the principle of state sovereignty be considered transcended by international criminal justice for core crimes, versus remaining a fundamental limit on international institutional authority?',
    'Ongoing international legal and political discourse, evolving customary international law, and the outcomes of future treaty negotiations or state practice regarding international criminal jurisdiction.',
    'A stronger prioritization of international justice would reinforce the universalist reading''s legitimacy. A stronger emphasis on sovereignty would undermine the constraint''s claims, highlighting its extractive nature from the perspective of non-consenting states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_justice_priority, preference, 'Normative tension between state sovereignty and universal justice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__universalist_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(rome_tr_t2004, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2004, 0.2).
narrative_ontology:measurement(rome_tr_t2010, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(rome_tr_t2016, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2016, 0.35).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 1998, 0.5).
narrative_ontology:measurement(rome_be_t2004, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2004, 0.58).
narrative_ontology:measurement(rome_be_t2010, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(rome_be_t2016, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2016, 0.7).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 1998, 0.45).
narrative_ontology:measurement(rome_su_t2004, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2004, 0.52).
narrative_ontology:measurement(rome_su_t2010, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(rome_su_t2016, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, international_human_rights_law).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, un_security_council_authority).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, international_customary_law).

% DUAL FORMULATION NOTE:
% This constraint is the 'universalist_reading' of the 'rome_statute_jurisdiction' kernel. It is structurally distinct from the 'sovereigntist_reading' and 'hybrid_complementarity_reading' due to differing interpretations of ICC jurisdiction and state consent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
