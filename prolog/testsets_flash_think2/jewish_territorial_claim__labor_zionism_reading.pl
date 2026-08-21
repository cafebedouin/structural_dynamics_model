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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_territorial_claim__labor_zionism_reading
 *   human_readable: Labor Zionism's Jewish National Regeneration through 'Conquest of Labor'
 *   domain: Political History / Settler Colonialism / Nationalism Studies
 *
 * SUMMARY:
 *   This constraint describes the Labor Zionist reading of the Jewish
 *   territorial claim in Palestine, emphasizing national regeneration through
 *   socialist transformation and the 'conquest of labor'. This involved
 *   actively building a separate Jewish economy and society, often at the
 *   expense of the indigenous Palestinian Arab population, through land
 *   acquisition and the exclusion of Arab workers. The constraint is framed
 *   by its proponents as a necessary act of self-determination and social
 *   renewal, while critics view it as a settler-colonial project involving
 *   significant extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.75).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.85).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionism's Jewish National Regeneration through 'Conquest of Labor'").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "Political History / Settler Colonialism / Nationalism Studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, 'e2f85037-2087-4f2e-adb7-51d2be4640f4').
narrative_ontology:cs_kernel_codification('e2f85037-2087-4f2e-adb7-51d2be4640f4', formalized).
narrative_ontology:cs_authority_grounding('e2f85037-2087-4f2e-adb7-51d2be4640f4', lineage).
narrative_ontology:cs_interpretation_layer_present('e2f85037-2087-4f2e-adb7-51d2be4640f4').
narrative_ontology:cs_reading_relation('e2f85037-2087-4f2e-adb7-51d2be4640f4', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2f85037-2087-4f2e-adb7-51d2be4640f4', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('e2f85037-2087-4f2e-adb7-51d2be4640f4', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('e2f85037-2087-4f2e-adb7-51d2be4640f4', foundational, hebrew_labor_as_national_regeneration).
narrative_ontology:cs_axiom_status(hebrew_labor_as_national_regeneration, holdable).
narrative_ontology:cs_axiom_grounding('e2f85037-2087-4f2e-adb7-51d2be4640f4', hebrew_labor_as_national_regeneration, instrumental).
narrative_ontology:cs_axiom('e2f85037-2087-4f2e-adb7-51d2be4640f4', foundational, socialist_settlement_as_national_building).
narrative_ontology:cs_axiom_status(socialist_settlement_as_national_building, holdable).
narrative_ontology:cs_axiom_grounding('e2f85037-2087-4f2e-adb7-51d2be4640f4', socialist_settlement_as_national_building, conventional).
narrative_ontology:cs_reference_frame('e2f85037-2087-4f2e-adb7-51d2be4640f4', socialist_pioneer_ethos).
narrative_ontology:cs_drift_state('e2f85037-2087-4f2e-adb7-51d2be4640f4', post_1948_statehood, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2f85037-2087-4f2e-adb7-51d2be4640f4', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, zionist_organizations).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_settlers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_workers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_landowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Planned, funded, and directed Jewish immigration, land acquisition, and the establishment of a separate Jewish economy based on 'Hebrew labor'. They articulated the ideology of national regeneration through socialist transformation and enforced policies of excluding Arab labor from Jewish enterprises.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, zionist_organizations, agenda_setter,
    institutional, generational, arbitrage, global).

% Participated in agricultural and urban settlements, benefiting from land acquisition, employment in the Jewish-only economy, and the social infrastructure built by Zionist organizations. They were the direct agents of 'conquest of labor' and territorial expansion.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_settlers, beneficiary,
    organized, biographical, constrained, local).

% Were systematically excluded from employment in Jewish agricultural and industrial enterprises due to the 'Hebrew labor' policy. This led to economic marginalization, loss of traditional livelihoods, and increased competition for scarce jobs in the Arab sector.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_workers, payer,
    powerless, immediate, trapped, local).

% Experienced land loss through sales (often to Zionist organizations) and later through expropriation. While some sales were voluntary, the broader economic and political pressures created a context of constrained choice, leading to the fragmentation of their traditional landholdings and social structures.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_landowners, payer,
    moderate, biographical, constrained, local).

% Administered Palestine, often balancing (or failing to balance) competing Jewish and Arab claims. Their policies on immigration, land transfers, and economic development indirectly facilitated the Labor Zionist project, despite official neutrality claims. They observed the growing tensions and resistance.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, british_mandate_authorities, observer,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, british_mandate_authorities, agenda_setter).

% Represented Palestinian Arab opposition to Zionist settlement and the 'conquest of labor' policies. They were largely excluded from decision-making processes that shaped the future of Palestine, despite their organized resistance and diplomatic efforts.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_nationalist_leaders, excluded,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__labor_zionism_reading, zionist_organizations).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__labor_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate Jewish immigration, land acquisition, and the establishment of a self-sufficient, separate Jewish economy in Palestine, fostering national regeneration and a new social order for Jewish settlers.
% TRANSFER_FUNCTION: Transfers land, labor opportunities, and economic control from Palestinian Arabs to Jewish settlers and Zionist institutions, while also transferring resources and labor from Jewish immigrants to the collective project of state-building.
% ABSENT_VOICES: Palestinian Arab political representatives and workers were systematically excluded from the Jewish economy and political structures. They would have argued for equal rights, land retention, and an integrated economy, but their voices were actively suppressed or ignored within the Zionist project's framework.
% DISAPPEARANCE_RATIONALE: If the ideology and enforcement of 'conquest of labor' and separate Jewish economy vanished overnight, the entire economic and social structure built on these foundations would collapse. Land ownership patterns, labor markets, and political power dynamics would fundamentally reorganize, leading to a vastly different historical trajectory for the region.
% FOUNDING_PROBLEM: The perceived existential threat of antisemitism in Europe and the lack of a secure national home for the Jewish people, leading to a need for self-determination, national regeneration, and economic self-sufficiency in Palestine.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is primarily attested by Zionist organizations and Jewish communities, who emphasize the historical context of persecution. Critics, including Palestinian historians and post-colonial scholars, acknowledge the historical context but dispute the framing of the 'problem' and the 'solution' as legitimate, highlighting the dispossession it entailed for the indigenous population. Legislative hearings and international reports from outside the benefiting parties have also documented the contested nature of the founding narrative versus its outcomes.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) reflects the transfer of land, resources, and economic opportunities from Palestinian Arabs to Jewish settlers. Suppression (0.85) is high due to the active enforcement of 'Hebrew labor' policies, which systematically excluded Arab workers, and the broader political and military efforts to secure Jewish control. The theater ratio (0.40) indicates that while genuine socialist and regenerative ideals were present, they increasingly served as a legitimizing narrative for policies that were fundamentally extractive and exclusionary. Accessibility collapse (0.70) for Palestinian Arabs was substantial, as their traditional economic structures were undermined and alternatives within the Jewish economy were denied. Resistance (0.80) was consistently high, manifested in Arab revolts and political opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Labor Zionists, this constraint was a 'Rope' or 'Scaffold' – a necessary, regenerative, and ultimately beneficial coordination for Jewish national revival. From the perspective of Palestinian Arabs, it was a 'Snare' – a coercive, extractive mechanism leading to dispossession and marginalization. The engine's classification as 'Tangled Rope' captures this dual nature: coordination for one group, extraction and suppression for another, held together by active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist organizations and Jewish settlers are clear beneficiaries, directing and participating in the project, gaining land, economic control, and a national home. Palestinian Arab workers and landowners are the primary targets, experiencing dispossession, economic marginalization, and loss of livelihood. The British Mandate authorities, while officially neutral, often facilitated the Zionist project, making them complex actors with elements of both agenda-setting and observation. Arab nationalist leaders are excluded, actively resisting the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    socialist_ideal_vs_nationalist_exclusion,
    'To what extent was the ''socialist transformation'' aspect of Labor Zionism a genuine ideal for universal liberation, versus a rhetorical cover for nationalist exclusion and ethnic separation?',
    'Analysis of primary sources (internal debates, policy documents) and historical outcomes: if the universalist ideals were consistently subordinated to or abandoned in favor of exclusionary practices, it supports the ''cover'' interpretation.',
    'If primarily a cover, the constraint''s theater_ratio would be higher, and its extractiveness would be more clearly understood as purely exploitative rather than a byproduct of a flawed ideal. If genuinely conflicted, it highlights the internal tension of the movement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(socialist_ideal_vs_nationalist_exclusion, conceptual, 'Ambiguity of Labor Zionism''s socialist ideals versus its exclusionary practices.').

omega_variable(
    conquest_of_labor_necessity,
    'Was the ''conquest of labor'' policy (excluding Arab workers from Jewish enterprises) a necessary step for Jewish economic self-sufficiency and national building, or an avoidable act of ethnic discrimination and economic warfare?',
    'Counterfactual historical analysis and comparative studies of other settler societies: examining alternative economic models or historical moments where integration was considered or attempted, and their potential outcomes.',
    'If necessary, it partially legitimizes the coordination function for Jewish settlers, even with high extraction from Arabs. If avoidable, it strengthens the ''pure extraction'' reading of the policy, increasing the effective extractiveness for Palestinian Arabs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conquest_of_labor_necessity, empirical, 'Whether ''conquest of labor'' was an economic necessity or discriminatory policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1904, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jewi_tr_t14, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement(jewi_tr_t25, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(jewi_tr_t32, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement(jewi_tr_t41, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 41, 0.38).
narrative_ontology:measurement(jewi_tr_t44, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 44, 0.4).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jewi_be_t14, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 14, 0.62).
narrative_ontology:measurement(jewi_be_t25, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(jewi_be_t32, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 32, 0.72).
narrative_ontology:measurement(jewi_be_t41, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 41, 0.74).
narrative_ontology:measurement(jewi_be_t44, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 44, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(jewi_su_t14, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 14, 0.7).
narrative_ontology:measurement(jewi_su_t25, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(jewi_su_t32, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 32, 0.82).
narrative_ontology:measurement(jewi_su_t41, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 41, 0.84).
narrative_ontology:measurement(jewi_su_t44, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 44, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Jewish territorial claim' kernel, focusing on Labor Zionism's approach. Other readings (political, cultural, revisionist) represent distinct constraints with different structural properties and outcomes, but are linked by their shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
