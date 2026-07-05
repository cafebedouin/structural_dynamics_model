% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionism as European Settler-Colonial Ethno-State Formation (Settler-Colonial Reading)
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   This story instantiates the settler-colonial reading of the contested
 *   Zionist legitimacy kernel: the claim that Zionism functioned structurally
 *   as a European settler-colonial movement whose founding and persistence
 *   required indigenous Palestinian displacement as a constitutive act, not
 *   an incidental wartime casualty. This is one of three sibling readings of
 *   the same underlying kernel (national liberation, religious restoration,
 *   settler-colonial); each is authored as its own ε-invariant constraint per
 *   the decomposition principle, because measuring 'Zionist legitimacy'
 *   through the lens of colonial land-transfer mechanics yields a
 *   structurally different extraction profile than measuring it through the
 *   lens of persecuted-minority return or covenantal fulfillment. This file
 *   holds only the settler-colonial reading's internal logic; the sibling
 *   claims are not represented or averaged here.
 *
 * KEY AGENTS:
 *   - jewish_israeli_settler_society: primary beneficiary (organized/constrained) — inherits consolidated land and sovereignty
 *   - israeli_state_apparatus: agenda_setter (institutional/arbitrage) — administers the legal architecture that converted displacement into permanent structure
 *   - palestinian_1948_refugees: primary target (powerless/trapped) — bear the founding transfer with no return mechanism
 *   - palestinian_present_day_residents_under_occupation: ongoing target (powerless/trapped) — bear the continuation of the same displacement logic
 *   - land_development_institutions: institutional beneficiary/agenda_setter (institutional/arbitrage) — captures and administers the transferred asset base
 *   - international_historiography_community: analytical observer — evaluates the archival record underlying the constitutive-vs-incidental dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.81).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.78).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionism as European Settler-Colonial Ethno-State Formation (Settler-Colonial Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, 'ea2f4a4b-87f9-4f85-a068-d043fb183b93').
narrative_ontology:cs_kernel_codification('ea2f4a4b-87f9-4f85-a068-d043fb183b93', distributed).
narrative_ontology:cs_authority_grounding('ea2f4a4b-87f9-4f85-a068-d043fb183b93', distributed).
narrative_ontology:cs_reading_relation('ea2f4a4b-87f9-4f85-a068-d043fb183b93', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea2f4a4b-87f9-4f85-a068-d043fb183b93', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('ea2f4a4b-87f9-4f85-a068-d043fb183b93', foundational, displacement_is_constitutive_not_incidental).
narrative_ontology:cs_axiom_status(displacement_is_constitutive_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('ea2f4a4b-87f9-4f85-a068-d043fb183b93', displacement_is_constitutive_not_incidental, empirically_contingent).
narrative_ontology:cs_axiom('ea2f4a4b-87f9-4f85-a068-d043fb183b93', foundational, colonial_structure_overrides_settler_persecution_history_in_legitimacy_determination).
narrative_ontology:cs_axiom_status(colonial_structure_overrides_settler_persecution_history_in_legitimacy_determination, holdable).
narrative_ontology:cs_axiom_grounding('ea2f4a4b-87f9-4f85-a068-d043fb183b93', colonial_structure_overrides_settler_persecution_history_in_legitimacy_determination, conventional).
narrative_ontology:cs_reference_frame('ea2f4a4b-87f9-4f85-a068-d043fb183b93', pre_1917_ottoman_demographic_status_quo).
narrative_ontology:cs_drift_state('ea2f4a4b-87f9-4f85-a068-d043fb183b93', post_new_historians_archival_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ea2f4a4b-87f9-4f85-a068-d043fb183b93', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_settler_society).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, land_development_institutions).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_1948_refugees).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_present_day_residents_under_occupation).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, internally_displaced_palestinian_citizens_of_israel).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, national_self_determination_via_territorial_sovereignty).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, demographic_engineering_as_state_founding_technique).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Descendants and successors of Zionist settlement waves who acquired land, citizenship, and state protection through a founding process that displaced the prior population. Benefits from consolidated territorial control, state-backed land allocation, and demographic majority secured through the 1948 and subsequent displacements. Exit from the beneficiary position would require renouncing the material and legal gains of statehood; most experience the arrangement as simply the normal order of their lives rather than as an ongoing structural relationship.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_settler_society, beneficiary,
    organized, generational, constrained, national).

% Administers land law, citizenship law, military governance, and settlement expansion. Sets and enforces the legal architecture (Absentee Property Law, Law of Return, military administration of occupied territory) that this reading identifies as the mechanism converting displacement into permanent structure. Has full institutional capacity to alter or entrench the arrangement and derives its own legitimating narrative partly from denying the settler-colonial characterization.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Population and descendants expelled or fled during the 1947-49 war, barred by Israeli law from returning to properties and villages now inside the state, many remaining stateless or in protracted refugee status across neighboring states for over 75 years. No legal exit toward restitution exists inside the constraint's own institutions; the displacement is treated as this reading's constitutive rather than incidental event.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_1948_refugees, payer,
    powerless, civilizational, trapped, regional).

% Residents of the West Bank and Gaza living under continued military administration, settlement expansion, and movement restriction. Bear the ongoing, present-tense continuation of the founding displacement logic through land expropriation and permit regimes; have no path to exit the constraint short of a political settlement they do not control.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_present_day_residents_under_occupation, payer,
    powerless, generational, trapped, regional).

% Hold Israeli citizenship but were internally displaced from destroyed or depopulated villages within the 1948 borders; barred from returning to original land now designated state or Jewish National Fund land. Formal citizenship provides some legal standing to contest specific administrative decisions but not to reverse the founding land transfer.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, internally_displaced_palestinian_citizens_of_israel, payer,
    moderate, generational, constrained, national).

% Quasi-governmental bodies (Jewish National Fund and successors) that acquired, administer, and allocate land vacated by displacement, structurally excluding non-Jewish claimants from lease and purchase in large categories of state land. Directly capture and administer the transferred asset base the founding displacement produced.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, land_development_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, land_development_institutions, agenda_setter).

% Absorbed large refugee populations and have periodically pressed the displacement claim diplomatically, but are not party to Israel's internal legal architecture and have no standing within it to compel restitution or recognition of the settler-colonial framing.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, arab_host_states, excluded,
    moderate, generational, constrained, regional).

% Historians, comparative settler-colonial studies scholars, and international legal bodies who evaluate archival evidence (including post-1980s access to Israeli state archives) to assess whether displacement was policy-driven and constitutive or a contingent wartime byproduct. Their scholarship shapes but does not control the constraint's operation.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_historiography_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_settler_society).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coordinated legal and demographic mechanism for a settling population to establish sovereign territorial control, citizenship rights, and land tenure in a defined territory — solving the settling population's problem of secure, permanent, majority-controlled statehood.
% TRANSFER_FUNCTION: Moves land title, residency rights, water and resource access, and political sovereignty from the pre-1948 indigenous Arab population to the incoming and established Jewish-Israeli population and its state institutions, principally through the 1947-49 depopulation of approximately 400-600 Palestinian villages and subsequent legal consolidation of the transferred land.
% ABSENT_VOICES: Palestinian refugees and their descendants, the primary bearers of the founding transfer, have no seat in Israeli domestic legal or political processes that adjudicate land and citizenship law; their claims are routed instead through UN mechanisms (UNRWA registration, non-binding General Assembly resolutions) that carry no enforcement power inside the constraint.
% DISAPPEARANCE_RATIONALE: If the settler-colonial characterization were universally adopted as the operative legal and political frame, restitution, right-of-return, and land-reallocation claims would gain force that the current framework structurally forecloses — proponents of this reading hold the world would substantially rearrange (property regimes, citizenship law, and possibly sovereignty itself). Defenders of sibling readings hold the material facts on the ground (existing state, existing population distribution) would persist regardless of which historical characterization prevails, since the reading is retrospective interpretation rather than the mechanism itself. This is exactly the site of the kernel's contest.
% FOUNDING_PROBLEM: European Zionist movement leaders (late 19th-early 20th century) identified the problem as the persistence of antisemitic persecution and the impossibility of Jewish safety within European nation-states, and proposed sovereign Jewish territorial concentration as the structural solution — pursued through land purchase, immigration, and eventually armed statehood in Ottoman/British Mandate Palestine, a territory already inhabited by an Arab majority.
% FOUNDING_PROBLEM_CORROBORATION: Comparative settler-colonial scholars (e.g., Patrick Wolfe's framework of settler colonialism as a structure rather than an event, applied to Zionism by scholars including Nadim Rouhana and Rashid Khalidi) attest from outside Israeli state institutions that the founding displacement was a constitutive and in significant part deliberately pursued outcome documented in Zionist leadership correspondence and Israeli state archives (notably Benny Morris's archival work, itself produced from inside the Israeli academic establishment but drawing on state military archives). Israeli state institutions and mainstream Zionist historiography dispute the constitutive characterization, holding displacement was substantially a contingent wartime byproduct rather than founding policy — corroboration exists but is contested across the same institutional lines the reading itself concerns.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, contested).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.81 at 2024) because, under this reading, land tenure, citizenship privilege, and demographic majority status were transferred from an indigenous population to a settling one through a legally consolidated process (Absentee Property Law, military governance, differential citizenship access) that persists in the present-day occupation regime. Suppression is authored high (0.78) because the reading holds the arrangement's persistence depends on active legal, administrative, and at times military enforcement against return, restitution, and equal land access claims — not on the voluntary consent of the displaced population. Theater ratio is moderate (0.42): substantial genuine coordination function exists (a real state was built, real institutions administer real services for the beneficiary population), but a meaningful share of legitimating activity (archival access restriction, 'no such thing as Palestinian people' framings, denial of Nakba-era planning documents until Israeli New Historians' archival work in the 1980s-2000s) functions to obscure rather than resolve the constitutive-displacement question. Accessibility collapse is moderate (0.58): for the displaced population, legal alternatives to the current land/citizenship regime have been substantially foreclosed inside Israeli domestic law, though international legal avenues remain nominally open without enforcement power. Resistance is authored high (0.88), reflecting sustained Palestinian political mobilization, international solidarity movements, BDS, and ongoing legal challenges — this is not a settled, unresisted arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish-Israeli settler society and the state apparatus sit near the beneficiary end: the constraint (understood as the land/citizenship/sovereignty transfer mechanism) subsidizes their security, land tenure, and political standing, and their exit from the beneficiary position would require renouncing material gains most do not experience as extractive at all — this is the seat-divergence the engine is built to surface. Palestinian refugees and present-day residents under occupation sit near the full-target end: trapped exit options, no legal path to reversal inside the constraint's own institutions, and the extraction (loss of land, residency, and often citizenship) runs directly through the same legal structure that benefits the settler society. Land development institutions are declared beneficiaries with agenda-setting capacity because they administer, not merely receive, the transferred asset base. Arab host states are excluded rather than victimized directly by this constraint's domestic mechanics, though they absorbed downstream refugee costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists a simple mandatrophy verdict because the founding_problem_status is authored as contested: proponents of the settler-colonial reading hold the founding coordination function (Jewish safety via sovereign territory) has been achieved and the arrangement now persists primarily through ongoing extraction and enforcement against the displaced population rather than through any live coordination need — a mandatrophy pattern. The israeli_state_apparatus and allied historiography dispute this, holding the founding problem (regional and existential security threat) remains live, which would characterize the arrangement as an active tangled rope rather than an atrophied one. The classification as tangled_rope (not snare) in this reading reflects that a genuine, still-functioning coordination function exists for the beneficiary population (state services, security, political community) even as this reading holds it depends on continuing extraction from a defined victim population through active enforcement — both gates required by the classifier are authored as present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_incidental_displacement,
    'Was the 1947-49 Palestinian displacement a constitutive, substantially planned feature of Zionist state-building (as this reading holds), or a contingent byproduct of wartime conditions that Zionist leadership did not systematically pursue as policy?',
    'Continued archival research into pre-1948 Zionist leadership planning documents, military orders, and population transfer discussions (building on Benny Morris, Ilan Pappe, and subsequent New Historian archival work), weighed against competing archival interpretations from Israeli military historians who characterize the same documents as contingency planning rather than expulsion policy.',
    'If substantially constitutive, this reading''s tangled_rope classification with high extraction is well-supported and the founding coordination function is more clearly parasitic on displacement from the outset. If substantially incidental, this reading''s severity is overstated relative to the national_liberation_reading, which would better capture the structural reality even while displacement remains a genuine historical harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_incidental_displacement, empirical, 'Whether Zionist leadership planning treated displacement as a means to an end or as an unintended consequence.').

omega_variable(
    settler_colonial_framework_applicability,
    'Does the comparative settler-colonial studies framework (developed primarily from Australian, North American, and Southern African cases of colonizing power exercising continuous metropolitan backing) apply cleanly to a movement whose settlers were themselves fleeing persecution and lacked a sustaining metropole after 1948?',
    'Comparative historical-sociological analysis of the structural fit between Zionism and canonical settler-colonial cases (Wolfe''s ''logic of elimination'' framework), attentive to disanalogies (absence of a continuing metropolitan sponsor state, refugee status of many settlers) that proponents of the national_liberation_reading and religious_restoration_reading raise as reasons the settler-colonial frame miscategorizes the movement''s core character.',
    'If the framework fits well, this reading''s classification of the founding act as colonial extraction is strongly supported. If the disanalogies are structurally significant, the settler-colonial reading may capture real extractive dynamics in the present-day occupation while overstating the framework''s fit to the 1917-1948 founding period specifically — suggesting the kernel may itself decompose further by time period.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_colonial_framework_applicability, conceptual, 'Whether the settler-colonial analytical framework, developed from other cases, structurally fits the Zionist case as cleanly as this reading assumes.').

omega_variable(
    cross_reading_evidentiary_overlap,
    'To what extent do the three sibling readings (settler-colonial, national-liberation, religious-restoration) dispute the same underlying facts versus interpreting largely agreed facts through incompatible legitimating frameworks?',
    'Systematic comparison of each reading''s factual claims about the founding period against the archival record, isolating genuine empirical disputes (what happened, who ordered it) from purely normative disputes (whether what happened was justified, and by what standard).',
    'If the disputes are primarily normative rather than factual, the kernel''s readings are better understood as competing legitimating frameworks over substantially agreed facts, which would support classifying all three readings as coexisting interpretive positions (as authored in reading_relations) rather than any one reading foreclosing the others on factual grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_reading_evidentiary_overlap, conceptual, 'Whether the three kernel readings diverge on facts or on normative interpretation of shared facts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1917, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1993, 0.48).
narrative_ontology:measurement(zion_tr_t2005, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(zion_be_t1917, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.72).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.78).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1993, 0.7).
narrative_ontology:measurement(zion_be_t2005, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1917, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1917, 0.3).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.82).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1993, 0.72).
narrative_ontology:measurement(zion_su_t2005, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, israeli_land_law_regime).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, palestinian_right_of_return_claim).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the zionist_legitimacy_basis kernel, each authored as a separate ε-invariant constraint per the decomposition principle: settler_colonial_reading (this file, ε=0.81, tangled_rope), national_liberation_reading (sibling, expected lower ε, coordination-dominant framing of return from persecution), religious_restoration_reading (sibling, expected framing centered on covenantal legitimacy claims largely orthogonal to material extraction metrics). The three do not average into a single 'Zionism' constraint; each captures a structurally distinct legitimacy claim with its own beneficiary/victim mapping and its own persistence logic. This file's extraction and suppression scores should not be read as describing Zionism generically, only as describing what follows structurally if the settler-colonial premise is adopted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
