% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__labor_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Labor Zionist 'Conquest of Labor' and Hebrew Economic Separation
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint models the Labor Zionist reading of the contested Jewish
 *   territorial claim kernel: national regeneration achieved not primarily
 *   through diplomacy (political Zionism) or spiritual center-building
 *   (cultural Zionism) or maximalist force (revisionism), but through
 *   socialist economic transformation — Jewish workers physically laboring on
 *   Jewish-owned land, organized through the Histadrut, purchasing land
 *   inalienably through the Jewish National Fund, and building incremental
 *   'facts on the ground.' The mechanism (Hebrew labor / conquest of labor)
 *   is economically separatist by design: it deliberately excludes Arab
 *   workers from the Jewish agricultural and industrial economy as a matter
 *   of ideological principle, not incidental market outcome. This produces a
 *   genuine coordination function (building a self-sufficient national
 *   working class) fused to a genuine extraction structure (displacement of
 *   Arab tenants and exclusion of Arab wage labor) operating through the same
 *   land-purchase and hiring apparatus.
 *
 * KEY AGENTS:
 *   - histadrut_labor_federation: agenda_setter (organized/arbitrage) — sets and enforces Hebrew labor policy
 *   - jewish_agricultural_settlers and kibbutz_movement_members: beneficiaries (moderate/constrained) — receive land, employment, ideological purpose
 *   - arab_wage_laborers, displaced_tenant_farmers, arab_landless_peasantry: payers (powerless/trapped) — lose employment, tenancy, and long-term land access
 *   - ottoman_then_british_mandate_authorities: observer (institutional/analytical) — documents but does not halt the process
 *   - diaspora_jewish_donors: beneficiary/excluded (organized/arbitrage) — funds from abroad without local exposure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.62).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.58).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionist 'Conquest of Labor' and Hebrew Economic Separation").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, 'a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56').
narrative_ontology:cs_kernel_codification('a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56', distributed).
narrative_ontology:cs_authority_grounding('a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56', practice).
narrative_ontology:cs_reading_relation('a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56', foundational, national_regeneration_through_manual_labor).
narrative_ontology:cs_axiom_status(national_regeneration_through_manual_labor, holdable).
narrative_ontology:cs_axiom_grounding('a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56', national_regeneration_through_manual_labor, instrumental).
narrative_ontology:cs_axiom('a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56', foundational, hebrew_labor_as_precondition_for_sovereignty).
narrative_ontology:cs_axiom_status(hebrew_labor_as_precondition_for_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56', hebrew_labor_as_precondition_for_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56', socialist_pioneering_settlement_ethos).
narrative_ontology:cs_drift_state('a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56', post_1948_state_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a5f98f28-b4ee-454e-a7c2-8fd81d3c4d56', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_agricultural_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, kibbutz_movement_members).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_wage_laborers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, displaced_tenant_farmers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_landless_peasantry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, diaspora_jewish_donors).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, kibbutz_movement_members).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, socialist_national_regeneration_doctrine).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, hebrew_labor_as_national_duty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coordinates Hebrew labor campaigns, land purchase through the Jewish National Fund, and cooperative settlement, systematically directing employment and land toward Jewish workers and away from cheaper Arab labor. Frames this as necessary for building an autonomous Jewish working class and national economy, and enforces the boycott of Arab labor through picketing, social pressure, and control of hiring on JNF-owned land.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation, agenda_setter,
    organized, generational, arbitrage, regional).

% Settle on land purchased and often previously cultivated by others, form kibbutzim and moshavim organized around Hebrew labor, and receive employment, land access, and ideological purpose through the movement. Many are recent immigrants with few alternatives outside the settlement project, but they are structurally the ones for whom the land and jobs are secured.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_agricultural_settlers, beneficiary,
    moderate, generational, constrained, regional).

% Live communally on settlements built on newly purchased land, doing physical labor themselves as an ideological practice ('conquest of labor') rather than employing cheaper local labor. They absorb real economic cost and hardship for the ideological commitment, while gaining permanent territorial presence and political standing within the movement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, kibbutz_movement_members, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, kibbutz_movement_members, payer).

% Previously available for agricultural and construction work on land now under Jewish ownership, they are systematically excluded from employment as Hebrew-labor policy takes hold, losing income they depended on with no equivalent alternative employment structure offered to replace it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_wage_laborers, payer,
    powerless, biographical, trapped, local).

% Cultivated land under absentee landlords who sell to Jewish National Fund purchasers; the sale extinguishes their tenancy without their consent or compensation for lost livelihood. They have no land title to defend and no political voice in the transaction that displaces them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, displaced_tenant_farmers, payer,
    powerless, biographical, trapped, local).

% As land increasingly transfers to Jewish national ownership held inalienably (JNF land is not to be resold or leased to non-Jews), this population's long-term access to agricultural land and rural livelihood contracts generation over generation, with no reversal mechanism built into the land regime.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_landless_peasantry, payer,
    powerless, generational, trapped, regional).

% Administer land registration, permit land sales, and periodically investigate the socioeconomic effects of land transfer and labor exclusion (e.g., the Hope Simpson and Peel inquiries), producing findings that document displacement without halting the underlying process.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, ottoman_then_british_mandate_authorities, observer,
    institutional, biographical, analytical, regional).

% Fund land purchase and settlement infrastructure from abroad, benefiting from the ideological and eventual political outcome without bearing local costs, but with limited direct say over how conquest-of-labor policy is implemented on the ground.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, diaspora_jewish_donors, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, diaspora_jewish_donors, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__labor_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed, newly arriving immigrant population into a self-sufficient national working class and territorial base, solving the genuine collective-action problem of building durable communal institutions, land tenure, and economic self-reliance from scratch under socialist organizing principles.
% TRANSFER_FUNCTION: Moves land tenure, agricultural employment, and wage income from the existing Arab peasant and laboring population to Jewish settlers and settlement institutions, financed by diaspora capital and organized through the Histadrut's control of hiring and land allocation.
% ABSENT_VOICES: Arab tenant farmers and wage laborers displaced by land sales and hiring exclusion have no seat in the Zionist institutions making these decisions and limited standing within Mandate administrative processes; their objections surface mainly through periodic British commissions of inquiry and through resistance and unrest rather than through direct participation in the constraint's own governance.
% DISAPPEARANCE_RATIONALE: If Hebrew labor policy and JNF inalienable land tenure had not operated, land and employment would likely have remained more economically integrated between Jewish and Arab populations, the trajectory toward a demographically and economically separate Jewish national economy would have been substantially slower or different in character, and the settlement movement's capacity to build the institutional and territorial 'facts on the ground' that shaped later state formation would have been significantly reduced.
% FOUNDING_PROBLEM: Jewish immigrants arriving with little capital or agricultural experience needed a way to build sustainable livelihoods and a national economic base without being permanently dependent on Arab landowners and cheap Arab labor, given the ideological goal of socialist national regeneration through physical labor rather than absentee ownership.
% FOUNDING_PROBLEM_CORROBORATION: Labor Zionist institutions and their historians attest the founding problem (economic dependency, lack of a Jewish working class, need for national self-reliance) as real and as substantially resolved by the 1930s-40s. British Mandate commissions of inquiry (Hope Simpson 1930, Peel 1937) and Arab political representatives attest from outside the movement that the same policy simultaneously produced displacement and landlessness among the Arab population as a direct and foreseeable consequence, a cost the movement's own account does not treat as part of 'the problem' it was solving.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction rises from 0.35 to 0.62 across the interval as land purchase and Hebrew labor enforcement intensify from scattered early settlement (1880s-1900s) toward the more organized, ideologically disciplined Histadrut-led boycotts and JNF inalienability regime of the interwar period. Suppression tracks a similar rise (0.30 to 0.58) as picketing of Arab-employing farms, organized hiring exclusion, and land-transfer restrictions harden into settled institutional practice requiring active enforcement rather than ad hoc individual choice. Theater ratio stays comparatively low (0.28 at endpoint) because the coordination function — building durable Jewish agricultural and industrial institutions — was substantively real, not primarily performative; the extraction rides on genuine functional infrastructure (cooperative farms, labor exchanges, national funds) rather than empty ritual.
 *
 * DIRECTIONALITY LOGIC:
 *   Histadrut and the settlement institutions sit at the agenda-setting, low-d end: they design and enforce the Hebrew labor policy and collect its coordination benefits directly. Jewish settlers, especially kibbutz members, are beneficiaries who nonetheless bear real personal cost (physical labor, hardship) — hence the dual role for kibbutz members. Arab wage laborers, displaced tenants, and the broader landless peasantry sit at the high-d target end: land tenure and employment access move away from them through the same institutional mechanism that builds up the Jewish national economy, and their trapped exit options (no alternative employer network, no legal title, no political voice in Mandate land policy) push their effective extraction toward the full-target pole regardless of the modest scope discount for a regional constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Jewish economic dependency and lack of a national working class — was substantially resolved by the late Mandate period as Jewish agricultural and industrial self-sufficiency matured; yet Hebrew labor exclusivity and inalienable land tenure persisted and hardened rather than relaxing, continuing to structure Arab economic exclusion even as the original scarcity-driven justification weakened. This is the mismatch the R5 corroboration surfaces: the movement's own account treats the founding problem as still live (national self-reliance, later national security), while outside observers (Mandate commissions) document the same period as one where the mechanism's costs to the excluded population were rising even as its coordination necessity for the beneficiary population was arguably declining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_exclusion_inseparability,
    'Is Hebrew labor''s national-building coordination function separable from its Arab-exclusion mechanism, or are they the same act described from two sides?',
    'Comparative analysis of contemporaneous Jewish settlement projects that pursued agricultural self-sufficiency without labor exclusivity (e.g., some early Bilu-era mixed-labor farms) to test whether national economic self-reliance could have been achieved without displacing Arab wage labor.',
    'If separable, the exclusion is a policy choice layered onto a genuine coordination need, sharpening the tangled_rope classification toward its extraction pole. If inseparable — if ideological conquest-of-labor required exclusivity by definition — the coordination and extraction functions are structurally fused rather than merely co-occurring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_exclusion_inseparability, conceptual, 'Whether Hebrew labor''s coordination and exclusion functions are structurally separable.').

omega_variable(
    sibling_reading_boundary_location,
    'Where exactly does the labor_zionism_reading''s economic-separation mechanism diverge from the political_zionism_reading''s sovereignty-first mechanism, given that Histadrut leadership (e.g., Ben-Gurion) moved fluidly between both frames over time?',
    'Track institutional decision points (e.g., transition from Histadrut-led settlement to statehood-oriented diplomacy in the 1930s-40s) to identify where the same historical actors shifted primary reliance from labor/settlement mechanisms to explicit sovereignty claims.',
    'If the boundary is porous with the same actors and institutions operating under both readings sequentially, the labor_zionism_reading should be understood as a phase or mechanism within a broader trajectory rather than a wholly independent strand — this affects how network.affects_constraints edges to political_zionism_reading should be weighted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary_location, conceptual, 'Structural boundary and overlap between the labor and political Zionism readings.').

omega_variable(
    counterfactual_arab_economic_integration,
    'Absent Hebrew labor exclusivity, would Arab and Jewish economies in Mandate Palestine have integrated, and would that integration have been more or less stable than the separatist path actually taken?',
    'Comparative study of mixed-labor agricultural enterprises that persisted despite Histadrut pressure, and economic historiography of binational economic integration attempts in analogous colonial contexts.',
    'Bears on whether the exclusion should be read as a necessary component of the national project''s success (as the movement''s own account holds) or as an avoidable choice that generated most of the constraint''s extractive character without correspondingly necessary coordination benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_arab_economic_integration, empirical, 'Counterfactual viability of an integrated rather than separatist labor economy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jewi_tr_t8, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(jewi_tr_t16, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(jewi_tr_t24, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(jewi_tr_t32, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(jewi_tr_t40, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jewi_be_t8, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(jewi_be_t16, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(jewi_be_t24, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(jewi_be_t32, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(jewi_be_t40, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(jewi_su_t8, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(jewi_su_t16, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(jewi_su_t24, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(jewi_su_t32, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(jewi_su_t40, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the jewish_territorial_claim kernel, each instantiating a structurally distinct mechanism for achieving Jewish national regeneration in Palestine: labor_zionism_reading (this story — economic separation and incremental settlement), political_zionism_reading (diplomatic sovereignty-seeking), cultural_zionism_reading (spiritual center without political majority), and revisionist_zionism_reading (maximalist territorial claim via compelled acceptance). Each carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked here as a constraint family rather than merged into one variable-mechanism story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
