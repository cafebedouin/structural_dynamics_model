% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__labor_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Labor Zionist 'Conquest of Labor' and Hebrew-Economy Settlement Building
 *   domain: political/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   This story instantiates the Labor Zionist reading of the contested
 *   'Jewish territorial claim' kernel: national regeneration achieved through
 *   socialist economic transformation, physical labor on reclaimed land, and
 *   the incremental construction of an exclusive Hebrew economic and
 *   settlement infrastructure, rather than through immediate political
 *   sovereignty claims (political Zionism), cultural-spiritual centering
 *   (cultural Zionism), or maximalist military assertion (revisionist
 *   Zionism). The mechanism this reading is specifically about is economic
 *   separation — 'Hebrew labor' and 'Hebrew produce' campaigns and
 *   cooperative land purchase — functioning simultaneously as a genuine
 *   coordination solution for a stateless, economically distorted diaspora
 *   population and as an extractive mechanism displacing Arab tenants and
 *   wage laborers from land and employment they had held under prior
 *   arrangements. Per the ε-invariance principle, this story's ε is fixed to
 *   this specific mechanism (economic separation and incremental settlement)
 *   and does not average across the sibling readings' different mechanisms
 *   (state sovereignty demand, cultural presence, military compulsion), which
 *   are authored as separate constraint stories.
 *
 * KEY AGENTS:
 *   - jewish_agricultural_settlers: Primary organized beneficiary (organized/constrained) — receives land, capital, ideological legitimacy
 *   - histadrut_labor_federation: Primary agenda-setter (institutional/arbitrage) — designs and enforces the Hebrew-labor exclusion mechanism
 *   - displaced_arab_tenant_farmers: Primary target of land transfer (powerless/trapped) — loses tenancy without consent
 *   - arab_wage_laborers_excluded_from_hebrew_economy: Primary target of labor exclusion (powerless/trapped) — loses employment via organized boycott
 *   - historians_of_mandate_palestine: Analytical observer — reconstructs the dual coordination/extraction structure from archival record
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
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionist 'Conquest of Labor' and Hebrew-Economy Settlement Building").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, '43e8bdf4-950e-4a86-805e-fe5d3a4ec08b').
narrative_ontology:cs_kernel_codification('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b', distributed).
narrative_ontology:cs_authority_grounding('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b', practice).
narrative_ontology:cs_interpretation_layer_present('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b').
narrative_ontology:cs_reading_relation('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b', foundational, national_regeneration_requires_manual_labor).
narrative_ontology:cs_axiom_status(national_regeneration_requires_manual_labor, holdable).
narrative_ontology:cs_axiom_grounding('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b', national_regeneration_requires_manual_labor, instrumental).
narrative_ontology:cs_axiom('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b', secondary, incremental_economic_facts_ground_sovereignty).
narrative_ontology:cs_axiom_status(incremental_economic_facts_ground_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b', incremental_economic_facts_ground_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b', diaspora_economic_distortion_thesis).
narrative_ontology:cs_drift_state('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b', post_1936_arab_revolt_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('43e8bdf4-950e-4a86-805e-fe5d3a4ec08b', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_agricultural_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, kibbutz_and_moshav_movement).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, displaced_arab_tenant_farmers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_wage_laborers_excluded_from_hebrew_economy).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, national_regeneration_through_manual_labor).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, socialist_zionist_synthesis_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Immigrate under the banner of national and personal transformation through physical labor on the land, forming collective and cooperative settlements funded by national institutions. They receive land access, credit, and ideological legitimacy in exchange for committing to Hebrew-only labor on their holdings; their own exit from the movement's discipline is constrained by the totalizing character of the settlement project and communal life.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_agricultural_settlers, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, jewish_agricultural_settlers, agenda_setter).

% Organizes and enforces the 'conquest of labor' and 'Hebrew labor' campaigns, picketing citrus groves and construction sites that employ Arab workers, running labor exchanges that route jobs exclusively to Jewish workers, and building the parallel economic infrastructure (cooperatives, banks, construction companies) that makes the incremental state-building strategy possible. Sets the enforcement agenda and absorbs no cost from it; can adjust tactics without losing institutional position.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation, agenda_setter,
    institutional, generational, arbitrage, regional).

% Collective and smallholder settlements that receive preferential access to purchased land (often bought from absentee landlords over the heads of resident tenants) and national fundraising support, in exchange for realizing the ideological program of Hebrew labor and territorial contiguity. Their land tenure and communal identity become inseparable from the settlement project's success.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, kibbutz_and_moshav_movement, beneficiary,
    organized, generational, constrained, regional).

% Cultivate land that absentee landowners sell to Jewish national land-purchasing bodies; the sale extinguishes their tenancy without their consent or, typically, adequate compensation or alternative livelihood. They have no legal standing in transactions conducted between landlord and purchaser and no meaningful recourse once the land changes hands.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, displaced_arab_tenant_farmers, payer,
    powerless, biographical, trapped, local).

% Previously employed on Jewish-owned citrus groves, construction sites, and farms at competitive wages; systematically displaced as Hebrew-labor enforcement campaigns picket and boycott employers who hire them, on the explicit rationale that Jewish national regeneration requires Jews performing all labor themselves. Lose income and the informal economic integration that had existed under mixed-labor arrangements.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_wage_laborers_excluded_from_hebrew_economy, payer,
    powerless, biographical, trapped, local).

% Administers land registration and sale under successive Ottoman and British Mandate legal regimes that formalize absentee ownership and permit land transfer without tenant consultation. Its own legal categories make the displacement possible but the administration is not a party the settlers or Histadrut answer to in the labor-conquest program itself.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, ottoman_and_mandatory_land_administration, excluded,
    institutional, biographical, analytical, regional).

% Criticizes the labor movement's incrementalist, economic-separation strategy as too slow and insufficiently assertive of sovereignty claims; argues for immediate maximalist territorial and military assertion instead. Their voice is present in the broader Zionist movement but structurally marginal to the labor-Zionist institutions actually administering settlement and labor allocation in this period.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, revisionist_zionist_faction, excluded,
    organized, generational, constrained, regional).

% Study land sale records, Histadrut archives, and labor exchange documentation to reconstruct how much displacement resulted from market transactions versus coercion, and how the socialist-national synthesis functioned simultaneously as genuine collective liberation project for Jewish settlers and as an exclusionary economic mechanism toward Arab labor.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, historians_of_mandate_palestine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__labor_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem for the immigrant Jewish settler population: pooling capital, land, agricultural expertise, and labor to build viable communities from scratch under conditions of statelessness, without access to a state's redistributive or protective machinery, using cooperative and collective structures instead.
% TRANSFER_FUNCTION: Moves land tenure from Arab tenant cultivators to Jewish national land-purchasing institutions and settlers (via absentee-landlord sales), and moves wage-labor opportunities away from Arab laborers toward Jewish workers (via organized boycott and exclusive labor exchanges), in service of building a self-sufficient, territorially contiguous Hebrew economy and society.
% ABSENT_VOICES: Arab tenant farmers whose land was sold out from under them had no seat in the land-transaction process; Arab wage laborers displaced by Hebrew-labor picketing had no representation in the Histadrut's labor allocation decisions. Both groups' economic interests were treated as external to the coordination problem the movement was solving.
% DISAPPEARANCE_RATIONALE: Had the Hebrew-labor and land-redemption program not operated as it did, the demographic and economic 'facts on the ground' enabling later territorial and political claims would not have accumulated in the same way; the trajectory toward a Jewish-majority economic base in specific regions depended materially on this incremental settlement and labor-exclusion mechanism, not merely on political negotiation or cultural presence alone.
% FOUNDING_PROBLEM: Diaspora Jewish life was seen by the movement's founders as economically distorted (concentrated in trade, finance, and intellectual professions, absent from agriculture and manual labor) and as leaving Jews perpetually vulnerable, landless, and dependent on host-society tolerance; 'conquest of labor' was designed to normalize the Jewish national body through direct physical relationship to land and labor, while simultaneously building irreversible territorial and economic presence.
% FOUNDING_PROBLEM_CORROBORATION: Labor Zionist institutions and their intellectual heirs continue to attest the founding problem (Jewish economic distortion and vulnerability) was real and substantially addressed by the program. Independent historians working from British Mandate land records, contemporaneous Arab press accounts, and Histadrut's own internal correspondence corroborate that displacement and labor exclusion were real and substantial effects, though they dispute whether these effects were a necessary cost of legitimate national regeneration or the actual operative function beneath the coordination rhetoric — no fully external corroboration exists that treats the founding problem as resolved without also documenting the transfer to Arab communities.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.28 at the movement's early cooperative-settlement phase (1904) to 0.62 by 1939, tracking the scaling of land purchase and labor-exchange enforcement from scattered agricultural colonies to a near-comprehensive parallel economy. Suppression (0.58 by 1939) reflects the escalation from persuasive boycott appeals to organized picketing, blacklisting of employers, and eventually more coercive tactics against both Jewish employers who hired Arab labor and Arab laborers seeking work in the Jewish sector. Theater ratio stays comparatively low (0.28) throughout because the coordination function — building functioning collective settlements, cooperative credit, and agricultural expertise — remained substantively real and productive across the interval; this is not a constraint whose function had hollowed into performance, which is why tangled_rope rather than snare or piton is the structurally appropriate claim. Accessibility collapse (0.5) and resistance (0.68) reflect that Arab economic alternatives (informal labor markets, agricultural tenancy under the prior landlord system) did not fully vanish but were significantly narrowed, and met real, escalating resistance in the form of Arab labor organizing, land-sale opposition, and periodic uprising.
 *
 * PERSPECTIVAL GAP:
 *   From the settler and Histadrut seats, the arrangement reads as pure coordination: solving Jewish landlessness and economic distortion through dignified labor and collective self-sufficiency, a rope by their own lights. From the displaced tenant and excluded-laborer seats, the identical structure reads as organized economic displacement enforced through boycott and exclusion — a mechanism whose coordination benefit accrues entirely to one national community at the direct expense of another's prior economic position. The engine computes both seats from the same structural data; this divergence is exactly what a tangled_rope classification is built to register, rather than forcing either seat's reading to stand for the whole.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish agricultural settlers and the kibbutz/moshav movement sit near the beneficiary end: they receive land, capital, and communal infrastructure, though their own constrained exit (deep ideological and communal commitment) tempers how purely beneficiary their position reads. The Histadrut sits furthest toward beneficiary/agenda-setter: institutional power with arbitrage-grade exit — it can recalibrate tactics without bearing the costs of either side. Displaced Arab tenant farmers and excluded Arab wage laborers sit at the full-target end: powerless, trapped, bearing the land and income loss the mechanism produces, with no leverage to redirect it structurally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora economic distortion and vulnerability) was genuinely live at the movement's founding and substantially addressed by 1939 for the Jewish settler population specifically — this is not a constraint whose mandate had already died and persisted only by inertia; that is why 'tangled_rope' rather than 'piton' is the correct claim despite the escalating suppression trajectory. The classification prevents two symmetric mislabelings: treating the entire program as pure coordination (which would erase the documented, escalating displacement of Arab tenants and laborers) and treating it as pure extraction with no coordination function (which would erase the genuine, functioning cooperative and collective-settlement achievement for the Jewish population it organized). Both the coordination and the extraction are structurally real and operate through the same mechanism — Hebrew labor and land redemption — which is the defining signature of tangled_rope rather than either rope or snare alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_inseparability,
    'Was Hebrew-labor economic separation a necessary component of building a genuinely self-sufficient, non-dependent Jewish national economy under diaspora-vulnerability conditions, or was the exclusion of Arab labor a separable extraction layered onto a coordination function that could have proceeded with mixed labor?',
    'Comparative study of contemporaneous mixed-labor Jewish agricultural enterprises (which existed and in some cases were economically successful) against Hebrew-labor-exclusive enterprises, controlling for capital access and land quality, to test whether exclusivity was economically necessary to the stated coordination goal or ideologically additional to it.',
    'If mixed-labor enterprises performed comparably, the exclusion mechanism reads as a separable extractive/nationalist layer rather than an inherent requirement of the coordination function, weakening the tangled_rope coordination claim toward snare; if exclusivity was economically load-bearing for the stated national-transformation goal, the tangled_rope reading holds more firmly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_inseparability, empirical, 'Whether Hebrew-labor exclusivity was economically necessary or an additional extractive layer.').

omega_variable(
    land_sale_consent_structure,
    'To what extent did absentee-landlord land sales to Jewish national institutions proceed through legally valid transactions under Ottoman/Mandate law versus through transactions that, while legally valid, systematically extinguished tenant rights the legal system itself failed to protect?',
    'Land registry and Mandate court record analysis distinguishing sales with documented tenant compensation or resettlement from those without, across the interval.',
    'A high proportion of uncompensated displacement strengthens the victim/payer characterization and the extraction reading; a high proportion of compensated or negotiated transitions would moderate the extractiveness score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_sale_consent_structure, empirical, 'Whether land transactions typically compensated displaced tenants or not.').

omega_variable(
    kernel_reading_boundary_conditions,
    'Is the economic-separation mechanism this reading isolates genuinely distinct from the territorial-sovereignty mechanism claimed by political_zionism_reading, or does labor Zionism''s ''facts on the ground'' strategy simply implement political Zionism''s sovereignty goal through economic means — making the two readings sequential phases of one mechanism rather than structurally independent claims?',
    'Trace whether Labor Zionist institutional leadership (Histadrut, Mapai) explicitly subordinated the economic program to an eventual sovereignty goal in internal deliberation, versus treating economic and social transformation as terminal goals independent of statehood.',
    'If economic separation was consistently instrumentalized toward sovereignty, the labor_zionism_reading and political_zionism_reading may exhibit an ''influences'' rather than ''coexists_with'' relationship at the mechanism level, though they remain separately authored per the ε-invariance principle given their different immediate mechanisms and victim structures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_conditions, conceptual, 'Whether the economic-separation and sovereignty mechanisms are independent or sequential/instrumental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1904, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1904, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1904, 0.1).
narrative_ontology:measurement(jewi_tr_t1911, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1911, 0.14).
narrative_ontology:measurement(jewi_tr_t1920, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1920, 0.18).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1929, 0.22).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1936, 0.26).
narrative_ontology:measurement(jewi_tr_t1939, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1939, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1904, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1904, 0.28).
narrative_ontology:measurement(jewi_be_t1911, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1911, 0.38).
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1920, 0.48).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1929, 0.56).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1936, 0.6).
narrative_ontology:measurement(jewi_be_t1939, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1939, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1904, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1904, 0.25).
narrative_ontology:measurement(jewi_su_t1911, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1911, 0.34).
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1920, 0.42).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1929, 0.51).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1936, 0.56).
narrative_ontology:measurement(jewi_su_t1939, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1939, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__labor_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the jewish_territorial_claim kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. labor_zionism_reading isolates the economic-separation and incremental-settlement mechanism (this file); political_zionism_reading isolates the sovereignty-demand mechanism; cultural_zionism_reading isolates the spiritual-center-without-sovereignty mechanism; revisionist_zionism_reading isolates the maximalist military-compulsion mechanism. Each carries its own ε, victim set, and classification; they are linked here via affects_constraints rather than merged, because averaging their extraction profiles would misrepresent all four as one indeterminate constraint rather than four structurally distinct claims sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
