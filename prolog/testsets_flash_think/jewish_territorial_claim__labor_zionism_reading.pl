% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__labor_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__labor_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__labor_zionism_reading
 *   human_readable: Labor Zionism's 'Conquest of Labor' and Settlement
 *   domain: political_history/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   This constraint story analyzes the 'Labor Zionism' reading of the Jewish
 *   territorial claim in Palestine, focusing on its core tenets of socialist
 *   transformation, 'conquest of labor,' and building 'facts on the ground'
 *   through settlement. This reading emphasizes the creation of a
 *   self-sufficient Jewish economy and society, often at the expense of the
 *   existing Palestinian Arab population. The constraint is framed as a
 *   Tangled Rope, reflecting its dual function of coordinating Jewish
 *   national regeneration while simultaneously extracting resources and
 *   opportunities from Palestinians.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.85).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.9).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionism's 'Conquest of Labor' and Settlement").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political_history/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, 'ee1bb378-fd1b-4821-b682-c3176e4eda50').
narrative_ontology:cs_kernel_codification('ee1bb378-fd1b-4821-b682-c3176e4eda50', formalized).
narrative_ontology:cs_authority_grounding('ee1bb378-fd1b-4821-b682-c3176e4eda50', practice).
narrative_ontology:cs_interpretation_layer_present('ee1bb378-fd1b-4821-b682-c3176e4eda50').
narrative_ontology:cs_reading_relation('ee1bb378-fd1b-4821-b682-c3176e4eda50', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee1bb378-fd1b-4821-b682-c3176e4eda50', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee1bb378-fd1b-4821-b682-c3176e4eda50', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('ee1bb378-fd1b-4821-b682-c3176e4eda50', foundational, hebrew_labor_is_national_redemption).
narrative_ontology:cs_axiom_status(hebrew_labor_is_national_redemption, holdable).
narrative_ontology:cs_axiom_grounding('ee1bb378-fd1b-4821-b682-c3176e4eda50', hebrew_labor_is_national_redemption, instrumental).
narrative_ontology:cs_axiom('ee1bb378-fd1b-4821-b682-c3176e4eda50', foundational, land_conquest_through_settlement).
narrative_ontology:cs_axiom_status(land_conquest_through_settlement, holdable).
narrative_ontology:cs_axiom_grounding('ee1bb378-fd1b-4821-b682-c3176e4eda50', land_conquest_through_settlement, conventional).
narrative_ontology:cs_reference_frame('ee1bb378-fd1b-4821-b682-c3176e4eda50', socialist_pioneering_ethos).
narrative_ontology:cs_drift_state('ee1bb378-fd1b-4821-b682-c3176e4eda50', post_1948_statehood, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ee1bb378-fd1b-4821-b682-c3176e4eda50', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, zionist_organizations).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_laborers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_landowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively participate in agricultural and industrial settlement, adhering to the principle of 'Hebrew labor' and building a self-sufficient Jewish economy. They benefit from land acquisition and exclusive employment opportunities, but are deeply committed ideologically.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_settlers, beneficiary,
    organized, biographical, identity_locked, local).

% Direct the acquisition of land, fund settlement projects, and promote the ideology of 'conquest of labor' and socialist transformation. They mobilize international Jewish support and negotiate with mandatory powers, benefiting from the expansion of Jewish presence and economic control.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, zionist_organizations, agenda_setter,
    institutional, generational, arbitrage, global).

% Are systematically excluded from employment in Jewish-owned enterprises and agricultural settlements due to the 'Hebrew labor' policy. They face diminishing economic opportunities and increasing competition for scarce resources, with few alternatives for livelihood.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_laborers, payer,
    powerless, immediate, trapped, local).

% Experience pressure to sell land, often through intermediaries, to Zionist organizations. While some receive payment, the sales contribute to the fragmentation of Palestinian communities and the loss of ancestral lands, with limited legal or political recourse.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_landowners, payer,
    moderate, biographical, constrained, local).

% Administer Palestine under a League of Nations mandate, balancing conflicting Jewish and Arab claims. Their policies, though often ambiguous, facilitate Jewish immigration and land acquisition, while also attempting to maintain order, often through coercive means.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, british_mandate_authorities, agenda_setter,
    institutional, biographical, mobile, regional).

% Observe and sometimes support Labor Zionism, often viewing it through the lens of socialist pioneering and national liberation, while sometimes overlooking or downplaying its exclusionary and colonial aspects. Their influence is primarily ideological and rhetorical.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, international_socialist_movements, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__labor_zionism_reading, zionist_organizations).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__labor_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective effort of Jewish immigrants to build a new national society and economy in Palestine, fostering self-sufficiency, mutual aid, and a shared national identity through labor and settlement.
% TRANSFER_FUNCTION: Transfers land, economic opportunities, and political control from the existing Palestinian Arab population to Jewish settlers and Zionist organizations, facilitated by land purchases and exclusionary labor policies.
% ABSENT_VOICES: Palestinian Arab political leaders and civil society organizations, who consistently opposed Zionist settlement and land acquisition, were largely excluded from decision-making processes by the British Mandate and Zionist institutions. Their objections to displacement and economic marginalization were systematically suppressed.
% DISAPPEARANCE_RATIONALE: If the principles and enforcement of Labor Zionism vanished overnight, the trajectory of Jewish settlement and state-building would have been fundamentally altered. The exclusive 'Hebrew labor' economy would collapse, opening opportunities for Palestinian workers, and the pace and nature of land acquisition would change dramatically, leading to a different demographic and political landscape.
% FOUNDING_PROBLEM: The 'Jewish Question' in Europe: antisemitism, pogroms, and the lack of a secure national home, combined with a desire for Jewish national and personal regeneration through productive labor.
% FOUNDING_PROBLEM_CORROBORATION: Zionist organizations and many Jewish communities attest that the problem of Jewish insecurity and the need for national self-determination remains live. Palestinian and post-colonial scholars, however, argue that while the founding problem for Jews was real, its 'solution' through Labor Zionism created a new problem of dispossession for Palestinians, and that the original problem's status is now contested by its consequences.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__labor_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__labor_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the 'conquest of labor' policy systematically excluded Palestinian Arab workers from the developing Jewish economy, and land acquisition led to dispossession. Suppression is very high (0.90) as this exclusion was actively enforced by Zionist institutions and supported by British Mandate policies, often through coercive means to prevent Palestinian resistance. Theater ratio is moderate (0.40) because the socialist and pioneering ideals provided a powerful ideological justification for practices that were inherently extractive and exclusionary, masking the full extent of the impact on the indigenous population. Accessibility collapse is high (0.75) as the goal was to create an exclusive economic sphere, limiting alternatives for Palestinians. Resistance is also high (0.70) reflecting significant Palestinian opposition throughout the period.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish settlers and Zionist organizations, this constraint was a necessary and just act of national liberation and socialist construction, a 'conquest of labor' to build a new society. From the perspective of Palestinian Arabs, it was a process of dispossession, economic marginalization, and colonial settlement. The engine's classification as Tangled Rope captures this fundamental divergence, where coordination for one group entails extraction for another.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish settlers and Zionist organizations are the primary beneficiaries, gaining land, employment, and a foundation for national self-determination. Palestinian Arab laborers and landowners are the primary targets/victims, losing economic opportunities and land. The British Mandate authorities act as a secondary agenda-setter, facilitating the process while attempting to manage the resulting conflict. International socialist movements are observers, often ideologically aligned but not directly involved in the local extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of Labor Zionism was Jewish national regeneration through socialist ideals and productive labor. However, the implementation of 'Hebrew labor' and land acquisition policies, while fulfilling the internal mandate for Jewish settlers, became increasingly extractive and exclusionary for Palestinians. The socialist rhetoric served to legitimize the creation of an ethnically exclusive economy, preventing a clear recognition of the constraint's extractive nature by many of its proponents. The 'mandate' of self-sufficiency and national building became intertwined with, and often obscured, the 'trophy' of land and economic control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'How does this ''Labor Zionism'' reading of the Jewish territorial claim interact with other Zionist readings (Political, Cultural, Revisionist) in shaping the overall constraint?',
    'Comparative historical analysis of policy implementation, resource allocation, and political discourse across different Zionist factions and their influence on the British Mandate and international community.',
    'Understanding the inter-reading dynamics would clarify how different ideological emphases contributed to or mitigated the constraint''s extractive and suppressive elements, potentially reclassifying specific policy implementations as distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'This constraint is a specific reading of the ''Jewish Territorial Claim'' kernel, with sibling readings including Political, Cultural, and Revisionist Zionism.').

omega_variable(
    conquest_of_labor_ambiguity,
    'To what extent was ''conquest of labor'' a genuine socialist ideal for Jewish self-sufficiency, versus a strategic tool for economic separation and displacement of Palestinian Arab labor?',
    'Analysis of internal Zionist debates, economic data on labor market dynamics, and the documented impact on Palestinian livelihoods, distinguishing between stated intent and practical outcome.',
    'If primarily a strategic tool, the constraint''s extractiveness and suppression would be understood as more deliberate and less a byproduct of coordination; if genuinely ideal-driven, it highlights the inherent tension between national self-sufficiency and indigenous rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conquest_of_labor_ambiguity, empirical, 'Ambiguity of ''conquest of labor'' as coordination or extraction.').

omega_variable(
    long_term_viability_of_economic_separation,
    'Was the goal of a fully separate Jewish economy in Palestine ever economically viable or was it always dependent on external funding and political enforcement?',
    'Economic modeling of the pre-1948 Yishuv economy, accounting for capital inflows, trade balances, and labor market structures, compared to a counterfactual scenario without exclusionary policies.',
    'If not viable, it suggests the high suppression and extractiveness were not merely a means to an end, but a perpetual necessity to maintain an artificial economic structure, further solidifying its Snare-like qualities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_viability_of_economic_separation, empirical, 'Economic viability of the exclusive ''Hebrew economy''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1904, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1904, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1904, 0.2).
narrative_ontology:measurement(jewi_tr_t1918, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1918, 0.25).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1929, 0.3).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1936, 0.35).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1948, 0.4).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1904, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1904, 0.6).
narrative_ontology:measurement(jewi_be_t1918, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1918, 0.7).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1929, 0.78).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1936, 0.82).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1948, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1904, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1904, 0.55).
narrative_ontology:measurement(jewi_su_t1918, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1918, 0.65).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1929, 0.75).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1936, 0.85).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1948, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
