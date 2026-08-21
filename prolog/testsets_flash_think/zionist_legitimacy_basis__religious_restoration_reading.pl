% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__religious_restoration_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Zionism: Religious Restoration Reading (Post-1967)
 *   domain: political_history/nationalism/religious_studies/settler_colonialism
 *
 * SUMMARY:
 *   This constraint story analyzes Zionism through the lens of its religious
 *   Zionist interpretation, particularly post-1967, where it is understood as
 *   the fulfillment of a divine promise and an active messianic process. This
 *   reading mandates territorial maximalism and prioritizes religious
 *   obligation over secular political considerations. The constraint is
 *   claimed as a 'mountain' by its adherents, reflecting its perceived divine
 *   origin and immutability. However, the authored metrics reflect a high
 *   degree of extraction and suppression, indicating a significant divergence
 *   between the claimed naturalness and its operational reality, which the
 *   False Summit Mountain (FSM) detection is designed to identify.
 *
 * KEY AGENTS:
 *   - religious_zionist_settlers: Primary agenda-setters and beneficiaries (institutional/identity_locked)
 *   - israeli_state_institutions_aligned_with_religious_zionism: Institutional beneficiaries and enforcers (institutional/constrained)
 *   - palestinians: Primary targets and victims (powerless/trapped)
 *   - secular_israelis_opposed_to_settlements: Bear costs and face internal opposition (moderate/constrained)
 *   - international_legal_bodies: Analytical observers whose authority is often rejected (institutional/analytical)
 *   - anti_occupation_activists: Excluded voices who resist the constraint (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.85).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.9).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, mountain).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Zionism: Religious Restoration Reading (Post-1967)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/religious_studies/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).
domain_priors:emerges_naturally(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, '1d8e952a-72d1-48c2-a283-0c940663d482').
narrative_ontology:cs_kernel_codification('1d8e952a-72d1-48c2-a283-0c940663d482', fixed_text).
narrative_ontology:cs_authority_grounding('1d8e952a-72d1-48c2-a283-0c940663d482', lineage).
narrative_ontology:cs_interpretation_layer_present('1d8e952a-72d1-48c2-a283-0c940663d482').
narrative_ontology:cs_reading_relation('1d8e952a-72d1-48c2-a283-0c940663d482', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d8e952a-72d1-48c2-a283-0c940663d482', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('1d8e952a-72d1-48c2-a283-0c940663d482', foundational, divine_command_to_settle_land).
narrative_ontology:cs_axiom_status(divine_command_to_settle_land, holdable).
narrative_ontology:cs_axiom_grounding('1d8e952a-72d1-48c2-a283-0c940663d482', divine_command_to_settle_land, theological).
narrative_ontology:cs_axiom('1d8e952a-72d1-48c2-a283-0c940663d482', foundational, messianic_redemption_through_territorial_control).
narrative_ontology:cs_axiom_status(messianic_redemption_through_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('1d8e952a-72d1-48c2-a283-0c940663d482', messianic_redemption_through_territorial_control, theological).
narrative_ontology:cs_reference_frame('1d8e952a-72d1-48c2-a283-0c940663d482', biblical_covenant_and_messianic_era).
narrative_ontology:cs_drift_state('1d8e952a-72d1-48c2-a283-0c940663d482', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1d8e952a-72d1-48c2-a283-0c940663d482', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_institutions_aligned_with_religious_zionism).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinians).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israelis_opposed_to_settlements).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, divine_covenant_with_israel).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, messianic_redemption_process).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, biblical_land_of_israel_borders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively expand and consolidate settlements in the West Bank, Gaza, and Golan Heights, viewing this as a direct fulfillment of divine command and a catalyst for messianic redemption. They are deeply invested in the theological justification and often hold significant political influence.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers, agenda_setter,
    institutional, generational, identity_locked, regional).

% Government ministries, military units, and legal bodies that implement policies supporting settlement expansion, land expropriation, and the application of Israeli law in occupied territories, often drawing on or accommodating religious Zionist ideology for legitimacy and political support.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_institutions_aligned_with_religious_zionism, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_institutions_aligned_with_religious_zionism, agenda_setter).

% Experience displacement, land confiscation, movement restrictions, and military occupation as a direct consequence of the expansion driven by this religious-nationalist ideology. Their existence and claims are often delegitimized by the divine mandate narrative.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinians, payer,
    powerless, generational, trapped, regional).

% Bear the economic and security costs of maintaining the occupation and settlements, and often face internal political and social pressure for opposing policies justified by religious claims. They view the expansion as detrimental to Israel's democratic and security future.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israelis_opposed_to_settlements, payer,
    moderate, biographical, constrained, national).

% Monitor and issue rulings on the legality of settlements under international law, consistently finding them to be in violation. Their authority is often rejected by adherents of this reading, who prioritize divine law over international statutes.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% Work to document and resist the expansion of settlements and the associated human rights abuses. They are often marginalized in mainstream discourse and face legal and physical suppression from state and settler actors.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, anti_occupation_activists, excluded,
    organized, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__religious_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a significant segment of the Israeli population and political leadership around a shared religious-national vision, coordinating political action, settlement expansion, and military presence in territories beyond the 1967 lines, all framed as fulfilling divine prophecy.
% TRANSFER_FUNCTION: Transfers land, water resources, and political sovereignty from Palestinians to Jewish settlers and Israeli state control, justified by a theological claim to the entire biblical Land of Israel.
% ABSENT_VOICES: Palestinians, international legal experts, and secular Israeli critics who challenge the divine mandate as a basis for territorial claims or state policy are systematically excluded from the legitimizing discourse, their perspectives dismissed as irrelevant or hostile to the messianic process.
% DISAPPEARANCE_RATIONALE: If the belief in Zionism as a divine promise and messianic process vanished overnight, the primary ideological justification for the settlement enterprise and territorial maximalism would collapse. This would fundamentally alter Israeli domestic politics, its relationship with Palestinians, and its standing in the international community, leading to a profound reordering of territorial claims and national identity.
% FOUNDING_PROBLEM: The perceived historical exile and persecution of the Jewish people, and the theological imperative to restore the biblical Land of Israel as part of a messianic process leading to ultimate redemption.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as 'live' primarily by religious Zionist leaders, rabbis, and their adherents, who interpret contemporary events as stages in the messianic process. This interpretation is not corroborated by secular historians, political scientists, or international bodies, who view the conflict through political, national, and human rights lenses.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, ExtMetricName, E),
    domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zionist_legitimacy_basis__religious_restoration_reading),
    narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is very high (0.85) due to the systematic transfer of land, resources, and rights from Palestinians, justified by a theological framework. `Suppression` is also very high (0.90) as the constraint relies on active military and legal enforcement to maintain control and suppress Palestinian resistance and alternative political solutions. `Theater_ratio` is moderate (0.40); while there is genuine religious conviction, the invocation of divine mandate often serves to legitimize political actions and deflect criticism, creating a performative aspect. `Accessibility_collapse` is high (0.80) because for adherents, divine command leaves no legitimate alternatives, and for Palestinians, structural barriers severely limit options. `Resistance` is extremely high (0.95) due to ongoing Palestinian struggle and international condemnation.
 *
 * PERSPECTIVAL GAP:
 *   Adherents of this reading perceive the constraint as a 'mountain'—a divinely ordained, unchangeable reality. From their perspective, the actions taken are not extractive but are righteous acts of restoration. Conversely, Palestinians and international observers experience this same structure as a highly extractive and suppressive 'snare' or 'tangled_rope', enforced through state power and military occupation. The engine's classification will highlight this divergence between the claimed type and the operational metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious Zionist settlers and aligned Israeli state institutions are clear beneficiaries and agenda-setters, directly gaining land, resources, and political power, and shaping the narrative. Palestinians are the primary victims, bearing the brunt of displacement and dispossession. Secular Israelis opposed to settlements also bear costs (e.g., security, international isolation) but have more constrained exit options than Palestinians. International legal bodies are analytical observers, while anti-occupation activists are excluded voices whose resistance is suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandate, rooted in divine promise, is considered 'live' by its adherents, preventing a mandatrophy resolution from their perspective. However, the high and increasing extractiveness and suppression, coupled with the contested status of the 'founding problem' (divine mandate vs. political conflict), suggest that the constraint's function has shifted from a purely theological imperative to a mechanism for territorial control and resource acquisition, maintained through active enforcement rather than inherent naturalness. The FSM detection will flag this 'claimed mountain' with beneficiaries as a potential false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is Zionism as fulfillment of divine promise a genuine natural law (mountain) or a constructed constraint that benefits identifiable agents (snare/tangled_rope)?',
    'Analysis of the constraint''s persistence: if it requires active enforcement and suppresses alternatives, it is constructed; if it would persist without human intervention, it is natural.',
    'If constructed, the classification shifts from mountain to a more extractive type (e.g., snare or tangled_rope), reflecting its reliance on human agency and coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Ambiguity between divine mandate as natural law and as a human-enforced ideology.').

omega_variable(
    divine_mandate_empirical_status,
    'Is the divine promise and messianic process, as interpreted by this reading, empirically verifiable or solely a matter of faith?',
    'Theological and philosophical analysis of the nature of prophecy and divine intervention, and whether its fulfillment can be objectively measured or is subject to interpretive frameworks.',
    'If solely a matter of faith, its ''naturalness'' claim is weakened for non-adherents, increasing its perceived extractiveness and suppression for those outside the belief system. If empirically verifiable, its mountain claim gains external validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_empirical_status, conceptual, 'Whether the divine mandate has an empirical basis or is purely theological.').

omega_variable(
    territorial_maximalism_necessity,
    'Is territorial maximalism (control over all biblical lands) a necessary and non-negotiable consequence of the divine promise, or is it an interpretation that could be revised?',
    'Internal theological debate and reinterpretation within religious Zionist thought, or a shift in the political-religious consensus regarding the boundaries of the messianic process.',
    'If revisable, the constraint''s rigidity and suppression of alternative territorial arrangements would decrease, potentially lowering its extractiveness. If non-negotiable, its current high extractiveness and suppression are structurally inherent to this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_maximalism_necessity, preference, 'Whether territorial maximalism is an immutable part of the divine promise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(zion_tr_t1977, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement(zion_tr_t1987, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1987, 0.3).
narrative_ontology:measurement(zion_tr_t1997, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1997, 0.35).
narrative_ontology:measurement(zion_tr_t2007, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2007, 0.38).
narrative_ontology:measurement(zion_tr_t2017, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2017, 0.42).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.65).
narrative_ontology:measurement(zion_be_t1977, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1977, 0.72).
narrative_ontology:measurement(zion_be_t1987, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1987, 0.78).
narrative_ontology:measurement(zion_be_t1997, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1997, 0.82).
narrative_ontology:measurement(zion_be_t2007, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2007, 0.84).
narrative_ontology:measurement(zion_be_t2017, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2017, 0.86).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(zion_su_t1977, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1977, 0.78).
narrative_ontology:measurement(zion_su_t1987, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1987, 0.85).
narrative_ontology:measurement(zion_su_t1997, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1997, 0.88).
narrative_ontology:measurement(zion_su_t2007, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2007, 0.9).
narrative_ontology:measurement(zion_su_t2017, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2017, 0.92).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, israeli_settlement_expansion_policy).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, palestinian_right_of_return).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'zionist_legitimacy_basis' kernel. This 'religious_restoration_reading' focuses on divine promise and messianic process, distinct from the 'national_liberation_reading' (secular self-determination) and the 'settler_colonial_reading' (indigenous displacement). Each reading presents a different structural justification and consequence for the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
