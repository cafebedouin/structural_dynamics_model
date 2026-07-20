% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Nation-to-Nation Treaty Framework (Indigenous-Settler International Law Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint instantiates the nation_to_nation_reading of the
 *   contested kernel 'historical_treaty_substrate' â the interpretation of
 *   historical treaties between Indigenous nations and settler states. Under
 *   this reading, treaties are ongoing international agreements between
 *   co-equal sovereigns requiring ongoing consent, subject to modern treaty
 *   law principles. Indigenous nations are positioned as beneficiaries with
 *   consent rights, while settler states are constrained by international
 *   obligations. In practice, the framework coordinates some territorial
 *   decision-making while asymmetrically extracting enforcement labor from
 *   Indigenous nations, who must continually activate the framework against
 *   state domestication.
 *
 * KEY AGENTS:
 *   - Indigenous nations (beneficiary/payer): organized, constrained exit â receive sovereignty recognition but bear enforcement costs across generations.
 *   - Settler state governments (agenda_setter/beneficiary): institutional, arbitrage exit â administer the framework, gain legitimacy, retain interpretive control.
 *   - Resource extraction corporations (payer): powerful, mobile exit â bear consent-obtaining costs and project delays.
 *   - Domestic appellate judiciary (agenda_setter): institutional, analytical exit â determines whether international principles bind domestically.
 *   - International monitoring bodies (observer): institutional, analytical exit â provide normative frameworks without enforcement.
 *   - Extinguishment advocates (excluded): moderate, constrained exit â excluded from the interpretive framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.58).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.44).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Nation-to-Nation Treaty Framework (Indigenous-Settler International Law Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, '8bb53b7a-43dd-4d5d-befd-c301f9a8dea3').
narrative_ontology:cs_kernel_codification('8bb53b7a-43dd-4d5d-befd-c301f9a8dea3', fixed_text).
narrative_ontology:cs_authority_grounding('8bb53b7a-43dd-4d5d-befd-c301f9a8dea3', lineage).
narrative_ontology:cs_interpretation_layer_present('8bb53b7a-43dd-4d5d-befd-c301f9a8dea3').
narrative_ontology:cs_reading_relation('8bb53b7a-43dd-4d5d-befd-c301f9a8dea3', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('8bb53b7a-43dd-4d5d-befd-c301f9a8dea3', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('8bb53b7a-43dd-4d5d-befd-c301f9a8dea3', foundational, ongoing_sovereign_consent_obligates_state).
narrative_ontology:cs_axiom_status(ongoing_sovereign_consent_obligates_state, holdable).
narrative_ontology:cs_axiom_grounding('8bb53b7a-43dd-4d5d-befd-c301f9a8dea3', ongoing_sovereign_consent_obligates_state, conventional).
narrative_ontology:cs_axiom('8bb53b7a-43dd-4d5d-befd-c301f9a8dea3', foundational, indigenous_nations_as_co_equal_sovereigns_under_international_law).
narrative_ontology:cs_axiom_status(indigenous_nations_as_co_equal_sovereigns_under_international_law, holdable).
narrative_ontology:cs_axiom_grounding('8bb53b7a-43dd-4d5d-befd-c301f9a8dea3', indigenous_nations_as_co_equal_sovereigns_under_international_law, conventional).
narrative_ontology:cs_reference_frame('8bb53b7a-43dd-4d5d-befd-c301f9a8dea3', international_sovereign_equality).
narrative_ontology:cs_drift_state('8bb53b7a-43dd-4d5d-befd-c301f9a8dea3', contemporary_state_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8bb53b7a-43dd-4d5d-befd-c301f9a8dea3', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_corporations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold inherent sovereignty and treaty rights recognized under the nation-to-nation framework. Receive formal standing to consent or withhold consent to territorial changes and resource extraction. Simultaneously bear the burden of activating enforcement mechanisms through litigation, treaty commissions, and international advocacy, often across generational timeframes.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, payer).

% Control the legislative, executive, and administrative apparatus that recognizes or narrows treaty obligations. Gain domestic and international legitimacy from the nation-to-nation framework. Retain interpretive power to determine the scope of consent and the procedural requirements for obtaining it.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments, beneficiary).

% Seek licenses and permits for resource development on treaty territories. Must negotiate with Indigenous nations or obtain state approval under the constraint that treaties require ongoing consent. Face project delays, legal uncertainty, and added transaction costs when the nation-to-nation framework is operative.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_corporations, payer,
    powerful, biographical, mobile, regional).

% Review state compliance with international treaty obligations toward Indigenous peoples through periodic reports and recommendations. Lack direct enforcement authority but provide the normative framework that sustains the nation-to-nation reading in international discourse.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_monitoring_bodies, observer,
    institutional, civilizational, analytical, global).

% Hear appeals on treaty rights and interpret the domestic legal effect of the nation-to-nation framework. Their interpretive methods determine whether international treaty principles bind the executive or are merely persuasive.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, domestic_appellate_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Advance the position that historical treaties effected complete cessions of Indigenous territorial sovereignty, leaving only domestic privileges. Structurally excluded from the nation-to-nation interpretive framework, which treats sovereignty as inalienable and ongoing.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, extinguishment_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal architecture for territorial coexistence and shared governance between sovereign Indigenous nations and settler states, replacing unilateral state assertions with a bilateral consent requirement grounded in international law.
% TRANSFER_FUNCTION: Moves territorial and resource decision-making authority from unilateral settler state control to a bilateral framework; transfers the primary enforcement burden and procedural costs to Indigenous nations who must continually activate the framework through litigation and advocacy.
% ABSENT_VOICES: Advocates for treaty extinguishment and domestic dependency frameworks are structurally excluded from the nation-to-nation paradigm; future generations and non-human territorial relations are not directly represented in the consent framework.
% DISAPPEARANCE_RATIONALE: If the nation-to-nation treaty framework disappeared, settler states would revert to unilateral territorial and resource decision-making; Indigenous nations would lose internationally recognized standing to withhold consent, and the legal architecture supporting Indigenous sovereignty claims against state intrusion would collapse.
% FOUNDING_PROBLEM: How to establish legitimate, ongoing governance relations between existing Indigenous sovereign nations and incoming settler states without continuous warfare or total Indigenous subjugation.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal historians and international human rights bodies attest that the problem of co-equal coexistence remains unresolved; settler state governments often assert the problem was resolved through constitutional supremacy or historical absorption, but independent UN special rapporteurs and non-state human rights monitors corroborate the live tension.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the framework extracts significant enforcement and advocacy labor from Indigenous nations while delivering incomplete territorial protection. Suppression (0.44) reflects the structural suppression of Indigenous international-law claims by domestic legal orders. Theater ratio (0.36) captures the growing gap between nation-to-nation rhetoric and domestic practice. Resistance (0.62) is high because Indigenous nations and allied legal advocates actively resist state domestication. Accessibility collapse (0.48) is moderate because alternatives (extinguishment, domestic dependency) remain live in legal discourse. The measurement series share one time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (settler state governments) experiences the constraint as a source of international legitimacy and a manageable administrative burden; the payer/beneficiary seat (Indigenous nations) experiences it as a perpetual uphill battle for recognition. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are declared in both beneficiary and victim sets because the same structure coordinates their standing (benefit) and extracts enforcement costs from them (cost). Their constrained exit and generational time horizon push their effective directionality toward the target end despite the beneficiary declaration. Settler state governments are beneficiaries of legitimacy and agenda-setters controlling interpretation; their arbitrage exit and institutional power push them toward the beneficiary end. Resource extraction corporations are payers facing delayed projects. The structural asymmetry is between the institutional power of the settler state and the organized but constrained position of Indigenous nations.
 *
 * MANDATROPHY ANALYSIS:
 *   The nation-to-nation reading avoids mandatrophy mislabeling because it retains a genuine coordination function (bilateral consent replaces unilateral seizure) while admitting asymmetric extraction (enforcement burden falls disproportionately on Indigenous nations). A pure rope classification would ignore the extraction; a pure snare classification would ignore the real sovereignty protections the framework provides. Tangled rope captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_burden_asymmetry,
    'Does the nation-to-nation framework genuinely constrain settler state resource extraction, or does it primarily extract enforcement labor and procedural compliance costs from Indigenous nations while allowing states to maintain effective territorial control?',
    'Comparative case study analysis of treaty implementation outcomes: measure rates of successful Indigenous consent-based veto versus rates of state-approved extraction over Indigenous objection within nation-to-nation jurisdictions.',
    'If enforcement is asymmetrically borne by Indigenous nations with low veto success rates, the constraint computes as more extractive and the coordination function operates as cover for state legitimacy; if veto rates are high and state compliance is genuine, the constraint moves toward rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_burden_asymmetry, empirical, 'Asymmetric enforcement burden and actual constraint on state extraction').

omega_variable(
    domestic_international_law_gap,
    'Is the nation-to-nation reading structurally enforceable as international law, or has it been captured by domestic legal orders that systematically narrow Indigenous sovereignty into administrative consultation rights?',
    'Jurisprudential mapping of domestic court interpretations against Vienna Convention on the Law of Treaties standards and UN Declaration on the Rights of Indigenous Peoples principles.',
    'If domestic capture is total, the constraint''s coordination function is illusory and it functions as a snare of legitimation; if international law retains independent force, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_international_law_gap, conceptual, 'Domestic legal capture of international treaty law framework').

omega_variable(
    consent_as_stewardship_boundary,
    'Does the nation-to-nation reading''s emphasis on sovereign statehood and international law foreclose Indigenous legal traditions that frame treaty relationships through relational stewardship rather than Westphalian sovereignty?',
    'Ethnographic and legal analysis of whether Indigenous treaty signatories historically understood themselves as sovereign equals in the Westphalian sense or as relational partners bound by mutual obligation to land.',
    'If the Westphalian framing misrepresents Indigenous legal traditions, the nation-to-nation reading imposes an alien conceptual framework that may extract cultural specificity in exchange for legal standing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_as_stewardship_boundary, conceptual, 'Westphalian sovereignty versus Indigenous relational legal traditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(n2n_treaty_tr_t0, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(n2n_treaty_tr_t6, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(n2n_treaty_tr_t12, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement(n2n_treaty_tr_t18, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement(n2n_treaty_tr_t24, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(n2n_treaty_tr_t30, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 30, 0.36).

% Extraction over time
narrative_ontology:measurement(n2n_treaty_be_t0, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(n2n_treaty_be_t6, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(n2n_treaty_be_t12, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(n2n_treaty_be_t18, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 18, 0.54).
narrative_ontology:measurement(n2n_treaty_be_t24, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(n2n_treaty_be_t30, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(n2n_treaty_su_t0, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(n2n_treaty_su_t6, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 6, 0.36).
narrative_ontology:measurement(n2n_treaty_su_t12, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(n2n_treaty_su_t18, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 18, 0.41).
narrative_ontology:measurement(n2n_treaty_su_t24, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement(n2n_treaty_su_t30, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 30, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'historical_treaty_substrate' â the interpretation of historical treaties between Indigenous nations and settler states. The nation_to_nation_reading interprets treaties as ongoing international agreements between sovereign equals; the extinguishment_reading treats them as completed property transactions; the stewardship_reading treats them as relational pacts for mutual territorial care. These are not the same constraint viewed from different angles â their epsilon values, beneficiary structures, and empirical operations differ. They form a constraint family linked by shared kernel but distinct structural instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
