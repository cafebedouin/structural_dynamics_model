% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Israeli Territorial Control via Security Necessity (1967+ Strategic Depth)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'security necessity' reading of
 *   territorial legitimacy, specifically regarding Israeli control over
 *   territories captured in 1967. This reading posits that such control,
 *   including strategic depth and conditional Palestinian sovereignty, is
 *   legitimate due to ongoing security imperatives. It justifies military
 *   presence, administrative control, and settlement expansion as essential
 *   for the State of Israel's defense. The constraint operates as a Tangled
 *   Rope, providing security coordination for Israel while extracting land,
 *   resources, and self-determination from Palestinians, maintained through
 *   active enforcement.
 *
 * KEY AGENTS:
 *   - State_of_Israel: Primary agenda_setter (institutional/arbitrage) — benefits from territorial control.
 *   - Palestinian_Population: Primary payer (powerless/trapped) — bears the costs of occupation.
 *   - Palestinian_Authority: Payer (organized/constrained) — exercises conditional, limited sovereignty.
 *   - Israeli_Settlers: Beneficiary (powerful/mobile) — benefits from state protection and resources in contested territories.
 *   - International_Community: Observer (institutional/analytical) — divided on the legitimacy of the security claim.
 *   - International_Law_Bodies: Observer (institutional/analytical) — often finds the occupation and settlements illegal.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.85).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.92).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Israeli Territorial Control via Security Necessity (1967+ Strategic Depth)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, 'e9f3a64e-e63c-4df8-a748-612ad53247b2').
narrative_ontology:cs_kernel_codification('e9f3a64e-e63c-4df8-a748-612ad53247b2', formalized).
narrative_ontology:cs_authority_grounding('e9f3a64e-e63c-4df8-a748-612ad53247b2', extraction).
narrative_ontology:cs_interpretation_layer_present('e9f3a64e-e63c-4df8-a748-612ad53247b2').
narrative_ontology:cs_reading_relation('e9f3a64e-e63c-4df8-a748-612ad53247b2', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('e9f3a64e-e63c-4df8-a748-612ad53247b2', territorial_legitimacy__indigenous_continuity_reading, forecloses).
narrative_ontology:cs_axiom('e9f3a64e-e63c-4df8-a748-612ad53247b2', foundational, territorial_depth_is_security_imperative).
narrative_ontology:cs_axiom_status(territorial_depth_is_security_imperative, holdable).
narrative_ontology:cs_axiom_grounding('e9f3a64e-e63c-4df8-a748-612ad53247b2', territorial_depth_is_security_imperative, empirically_contingent).
narrative_ontology:cs_axiom('e9f3a64e-e63c-4df8-a748-612ad53247b2', foundational, unilateral_security_action_is_justified).
narrative_ontology:cs_axiom_status(unilateral_security_action_is_justified, holdable).
narrative_ontology:cs_axiom_grounding('e9f3a64e-e63c-4df8-a748-612ad53247b2', unilateral_security_action_is_justified, instrumental).
narrative_ontology:cs_reference_frame('e9f3a64e-e63c-4df8-a748-612ad53247b2', post_six_day_war_security_doctrine).
narrative_ontology:cs_drift_state('e9f3a64e-e63c-4df8-a748-612ad53247b2', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e9f3a64e-e63c-4df8-a748-612ad53247b2', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and enforces control over territories captured in 1967 (West Bank, Golan Heights) as essential for national security and strategic depth. Benefits from the territorial buffer and control over resources.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, state_of_israel, agenda_setter,
    institutional, generational, arbitrage, national).

% Lives under military occupation and administrative control, experiencing restrictions on movement, land use, and self-determination. Bears the direct costs of territorial control justified by security.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_population, payer,
    powerless, biographical, trapped, regional).

% Exercises limited self-governance in fragmented areas, with its sovereignty and operational capacity constrained by Israeli security control. Bears the political and economic costs of conditional sovereignty.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_authority, payer,
    organized, biographical, constrained, regional).

% Resides in settlements in the West Bank and Golan Heights, benefiting from state protection, infrastructure, and access to land and resources, justified by the broader security narrative.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_settlers, beneficiary,
    powerful, biographical, mobile, local).

% Observes the conflict, often expressing concern over human rights and international law violations, but is divided on the legitimacy of the security necessity claim versus other readings of territorial rights.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_community, observer,
    institutional, generational, analytical, global).

% Interprets and applies international law, often finding Israeli settlements illegal and the occupation prolonged, but lacks direct enforcement power against the security necessity claim.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_law_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__security_necessity_reading, state_of_israel).
narrative_ontology:fixing_cost_class(territorial_legitimacy__security_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a perceived security buffer and strategic depth for the State of Israel against potential external threats and internal unrest, ensuring the safety of its citizens.
% TRANSFER_FUNCTION: Transfers control over land, water resources, and administrative authority from the Palestinian population and Authority to the State of Israel, in exchange for a claimed guarantee of security.
% ABSENT_VOICES: Palestinian voices advocating for full, unconditional sovereignty over the 1967 territories, and for the right of return for refugees, are largely excluded from the security-centric framing of legitimacy.
% DISAPPEARANCE_RATIONALE: If the claim of security necessity for territorial control vanished overnight, the entire framework of Israeli occupation and settlement in the West Bank and Golan Heights would lose its primary justification, leading to a rapid and profound reorganization of political control, military presence, and demographic realities in the region.
% FOUNDING_PROBLEM: The existential threat to the State of Israel from hostile neighboring states and non-state actors, necessitating territorial control for defensive purposes and strategic depth after the 1967 Six-Day War.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli government and military consistently attest to the ongoing live status of the security threat. This is contested by the Palestinian Authority and many international bodies, who argue that the threat is exaggerated or that the response is disproportionate, citing UN resolutions and human rights reports as counter-evidence.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the constraint enables significant transfers of land, resources, and sovereignty from Palestinians to Israel, far exceeding any 'coordination cost'. Suppression is also very high (0.92) due to the extensive military and administrative control required to maintain the status quo and prevent Palestinian self-determination. Theater ratio is moderate (0.45): while genuine security concerns exist, a substantial portion of the enforcement and justification serves to maintain and expand territorial control beyond immediate defensive needs. Accessibility collapse is high (0.78) as Palestinian alternatives for self-governance and movement are severely restricted. Resistance is high (0.88) reflecting ongoing Palestinian opposition and international criticism.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of the security necessity reading (e.g., the State of Israel) perceive the constraint as a legitimate and necessary 'Rope' for survival, where any extraction is a regrettable but unavoidable cost of security. From the perspective of Palestinians and many international observers, the same structure operates as a 'Snare' or 'Tangled Rope', primarily serving to extract land and rights under the guise of security, with the coordination function largely serving the occupier.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and Israeli settlers are clear beneficiaries, gaining security, land, and resources. The Palestinian population and Authority are clear targets, losing land, resources, and sovereignty. The international community and international law bodies act as observers, analyzing the situation without direct structural benefit or cost, though their pronouncements can influence the constraint's legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'Rope' (as proponents might claim) by highlighting the substantial extraction and suppression. It also avoids mislabeling it as a pure 'Snare' by acknowledging the genuine, albeit contested, security coordination function for the State of Israel. The 'Tangled Rope' classification captures the hybrid nature where a coordination function for one party is inextricably linked with asymmetric extraction from another, requiring active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_threat_assessment_ambiguity,
    'Is the security threat genuinely existential and requiring the current extent of territorial control, or is it exaggerated/managed to justify ongoing occupation and expansion?',
    'Independent, declassified intelligence assessments from multiple international security agencies, coupled with a comprehensive review of historical and contemporary threat levels and the efficacy of alternative security measures.',
    'If the threat is found to be exaggerated or manageable with less territorial control, the ''extractiveness'' and ''suppression'' metrics would be re-evaluated upwards, and the ''claimed_type'' would shift closer to a ''Snare''. If the threat is corroborated as existential, it would strengthen the ''Tangled Rope'' classification by emphasizing the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_threat_assessment_ambiguity, empirical, 'Ambiguity regarding the true nature and scale of the security threat justifying territorial control.').

omega_variable(
    proportionality_of_control_vs_necessity,
    'Is the extent of territorial control, including settlement expansion and restrictions on Palestinian life, strictly proportional to the stated security necessity?',
    'Detailed, independent spatial and demographic analysis comparing security requirements (e.g., early warning, defensive lines) with actual territorial control and settlement patterns, alongside legal review of international humanitarian law principles of proportionality.',
    'If disproportionate, the ''theater_ratio'' would increase significantly, indicating that security justifications are increasingly performative cover for expansion, pushing the classification closer to a ''Snare''. If proportional, the ''theater_ratio'' would decrease, reinforcing the ''Tangled Rope'' as a genuine, albeit harsh, coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_control_vs_necessity, conceptual, 'Whether the means of control are proportional to the security ends.').

omega_variable(
    alternative_security_arrangements_viability,
    'Are there viable alternative security arrangements (e.g., international guarantees, demilitarized Palestinian state, advanced surveillance) that could meet Israel''s security needs without extensive territorial control?',
    'Feasibility studies and diplomatic negotiations exploring and testing alternative security paradigms, potentially involving third-party security forces or advanced technological solutions.',
    'If viable alternatives exist and are rejected, it would indicate a preference for territorial control over pure security, increasing ''extractiveness'' and ''suppression'' and shifting the classification towards a ''Snare''. If no viable alternatives are found, it would reinforce the ''Tangled Rope'' classification by underscoring the perceived necessity of the current arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_security_arrangements_viability, preference, 'The existence and political acceptability of alternative security solutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__security_necessity_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(terr_tr_t1977, territorial_legitimacy__security_necessity_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy__security_necessity_reading, theater_ratio, 1987, 0.3).
narrative_ontology:measurement(terr_tr_t1997, territorial_legitimacy__security_necessity_reading, theater_ratio, 1997, 0.35).
narrative_ontology:measurement(terr_tr_t2007, territorial_legitimacy__security_necessity_reading, theater_ratio, 2007, 0.4).
narrative_ontology:measurement(terr_tr_t2017, territorial_legitimacy__security_necessity_reading, theater_ratio, 2017, 0.43).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__security_necessity_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(terr_be_t1977, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1977, 0.75).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1987, 0.8).
narrative_ontology:measurement(terr_be_t1997, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1997, 0.82).
narrative_ontology:measurement(terr_be_t2007, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2007, 0.83).
narrative_ontology:measurement(terr_be_t2017, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2017, 0.84).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(terr_su_t1977, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1977, 0.8).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1987, 0.85).
narrative_ontology:measurement(terr_su_t1997, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1997, 0.88).
narrative_ontology:measurement(terr_su_t2007, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2007, 0.9).
narrative_ontology:measurement(terr_su_t2017, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2017, 0.91).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
