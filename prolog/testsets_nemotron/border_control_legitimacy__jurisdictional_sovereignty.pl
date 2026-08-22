% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Jurisdictional Sovereignty: Balanced Border Authority with Protection and Consent Constraints
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates the 'jurisdictional sovereignty' reading of
 *   the border_control_legitimacy kernel: sovereignty is the power to
 *   regulate rights and obligations within territory, but this does not
 *   entail absolute border closure authority. Legitimacy requires balancing
 *   three claims — protection obligations to the displaced, labor needs of
 *   the economy, and democratic consent of the citizen polity. The constraint
 *   is the institutional architecture (asylum law, visa policy, integration
 *   systems, enforcement machinery) that attempts this balance. It is a
 *   tangled rope: genuine coordination (managing mobility, protecting rights,
 *   sustaining polity) with asymmetric extraction (excluded migrants bear
 *   costs, displaced citizens bear adjustment costs, state institutions
 *   extract legitimacy from both enforcement and admission).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.58).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.42).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Jurisdictional Sovereignty: Balanced Border Authority with Protection and Consent Constraints").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '51096ca6-814e-47aa-81ea-787b2a4b15ae').
narrative_ontology:cs_kernel_codification('51096ca6-814e-47aa-81ea-787b2a4b15ae', formalized).
narrative_ontology:cs_authority_grounding('51096ca6-814e-47aa-81ea-787b2a4b15ae', lineage).
narrative_ontology:cs_interpretation_layer_present('51096ca6-814e-47aa-81ea-787b2a4b15ae').
narrative_ontology:cs_reading_relation('51096ca6-814e-47aa-81ea-787b2a4b15ae', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('51096ca6-814e-47aa-81ea-787b2a4b15ae', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('51096ca6-814e-47aa-81ea-787b2a4b15ae', foundational, sovereignty_is_jurisdictional_not_absolute_exclusion).
narrative_ontology:cs_axiom_status(sovereignty_is_jurisdictional_not_absolute_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('51096ca6-814e-47aa-81ea-787b2a4b15ae', sovereignty_is_jurisdictional_not_absolute_exclusion, conventional).
narrative_ontology:cs_axiom('51096ca6-814e-47aa-81ea-787b2a4b15ae', foundational, legitimacy_requires_tripartite_balance_protection_labor_consent).
narrative_ontology:cs_axiom_status(legitimacy_requires_tripartite_balance_protection_labor_consent, holdable).
narrative_ontology:cs_axiom_grounding('51096ca6-814e-47aa-81ea-787b2a4b15ae', legitimacy_requires_tripartite_balance_protection_labor_consent, instrumental).
narrative_ontology:cs_reference_frame('51096ca6-814e-47aa-81ea-787b2a4b15ae', post_war_refugee_convention_compromise).
narrative_ontology:cs_drift_state('51096ca6-814e-47aa-81ea-787b2a4b15ae', contemporary_migration_governance_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('51096ca6-814e-47aa-81ea-787b2a4b15ae', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, citizen_polity).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, domestic_labor_market).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_institutions).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_economic_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens_from_migration_pressure).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, stateless_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, domestic_labor_market).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, state_legitimacy_requires_balancing).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, proportionality_in_enforcement).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, territorial_jurisdiction_without_absolute_exclusion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers immigration law, border enforcement, and asylum systems. Must balance international legal obligations (non-refoulement, human rights treaties) against domestic political pressure for restriction. Legitimacy depends on maintaining both legal compliance and public consent. Can adjust policy but faces structural constraints from courts, treaties, and electoral accountability.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Collective political community whose consent legitimizes border policy. Benefits from labor market protection, public service sustainability, and cultural cohesion. Exit from the polity (emigration) is possible but identity-locked: national membership is constitutive of political identity and social rights. Bears costs when migration pressures strain services or wages, but also benefits from migrant labor and demographic renewal.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, citizen_polity, beneficiary,
    organized, biographical, identity_locked, national).

% Workers and unions in sectors affected by migration. Benefit from wage protection and job access when borders restrict low-wage competition. Pay costs when labor shortages in care, agriculture, and construction raise prices or degrade services. Exit is constrained by sector-specific skills and geographic mobility limits. Internal divisions: high-skill vs low-skill, unionized vs precarious.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, domestic_labor_market, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, domestic_labor_market, payer).

% Persons fleeing persecution who are denied access to territory or fair procedure. Bear the full cost of exclusion: return to danger, detention, family separation, or indefinite limbo in transit countries. No meaningful exit — cannot return home, cannot access protection. Structural powerlessness is compounded by legal invisibility: many lack documentation to claim rights.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Workers from low-income countries denied legal pathways matching labor demand. Bear costs of irregular migration (smuggling fees, exploitation, death), deportation, and foreclosed life chances. Exit options are structurally trapped: home economies offer no viable livelihood, legal channels are rationed or non-existent, irregular routes are lethal. Their labor is needed but their persons are excluded.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_economic_migrants, payer,
    powerless, biographical, trapped, global).

% Citizens in communities experiencing rapid demographic change, wage depression, or service strain from migration. Bear costs of adjustment without consent mechanisms. Exit is constrained: geographic mobility limited by housing, employment, family ties. Political voice exists but is mediated by party systems that may not represent their interests. Distinct from citizen_polity as a whole — localized, specific harms rather than abstract collective benefit.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens_from_migration_pressure, payer,
    moderate, biographical, constrained, regional).

% Persons without recognized nationality, excluded from both territorial protection and legal personality. Bear compounding costs: no state to claim rights from, no travel documents, intergenerational transmission of statelessness. The ultimate trapped subjects of the border regime — sovereignty's jurisdictional authority has no purchase on them, yet border closure authority excludes them entirely.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, stateless_persons, payer,
    powerless, generational, trapped, global).

% Adjudicate state compliance with human rights treaties, refugee law, and non-refoulement. Provide the external legal constraint that shapes the proportionality/necessity test. Their rulings create legitimacy pressure but lack direct enforcement — rely on state consent and reputational cost. Analytical seat: see the full structure of competing obligations.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_courts_treaty_bodies, observer,
    institutional, generational, analytical, global).

% Monitor, document, and litigate border enforcement abuses. Provide witness testimony and legal representation to excluded migrants. Excluded from formal decision-making but shape the normative environment. Mobile exit options (international networks, funding), but their operational space is constrained by state hostility (NGO laws, surveillance, criminalization of solidarity).
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, civil_society_human_rights_orgs, observer,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, civil_society_human_rights_orgs, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates three legitimate demands: (1) territorial jurisdiction to regulate rights/obligations within borders (labor standards, public services, security), (2) protection obligations to persons fleeing persecution (non-refoulement, asylum), (3) democratic consent of the governed polity. The constraint is the institutional architecture that attempts to balance these without collapsing any into the others.
% TRANSFER_FUNCTION: Transfers protection and mobility rights from excluded migrants (asylum seekers, economic migrants, stateless persons) to the citizen polity and domestic labor market in the form of wage protection, service access, and political consent. Simultaneously transfers legitimacy costs from the state to migrants when enforcement violates proportionality, and transfers adjustment costs to displaced citizens when admission outpaces integration capacity.
% ABSENT_VOICES: Future generations (who inherit demographic and ecological consequences), migrants in transit countries (Libya, Turkey, Mexico) who bear externalized enforcement, climate-displaced persons not yet recognized in law. These voices are structurally excluded — no institutional channel for their claims, no vote, no standing in courts that adjudicate border policy.
% DISAPPEARANCE_RATIONALE: If the balanced jurisdictional sovereignty constraint vanished overnight, two opposite reorganizations would compete: (1) sovereign_primary reading would expand — states would assert absolute exclusion, asylum systems would collapse, protection obligations would be repudiated; (2) freedom_of_movement_primary reading would expand — borders would open toward free movement, labor markets would globalize, citizen polity consent would fracture. The world rearranges because the constraint is the active site of contestation between these poles.
% FOUNDING_PROBLEM: Post-WWII: how to reconcile state sovereignty (the only legitimate political form) with the moral horror of turning away refugees (the Holocaust, the Refugee Convention), while maintaining democratic consent in welfare states that depend on bounded membership for redistribution. The 1951 Convention and its 1967 Protocol were the founding compromise: sovereignty retains jurisdictional authority but surrenders absolute exclusion for a defined class (refugees).
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and human rights bodies attest the protection problem is live and worsening (record displacement, climate displacement unaddressed). Migration-skeptic parties and some legal realists attest the consent problem is live and worsening (democratic backlash, welfare chauvinism). Economic historians attest the labor-need problem is live (aging populations, sectoral shortages). No single corridor corroborates the founding balance — each sees one leg of the tripod as broken.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the constraint transfers substantial protections and life-chances from excluded persons to the citizen polity and labor market, while the state extracts legitimacy rents from managing the balance. Suppression (0.42) is moderate — enforcement is active (detention, deportation, border walls, visa regimes) but constrained by courts and treaties; the proportionality test is real, not performative. Theater ratio (0.28) captures that security theater and deterrence signaling exist but the core coordination function (asylum adjudication, labor migration channels) remains operational. Accessibility collapse (0.35) is low-moderate: alternatives (irregular migration, smuggling, informal work) persist despite the constraint. Resistance (0.55) is high: legal challenges, migrant solidarity movements, political backlash, and state non-compliance all contest the constraint from multiple directions.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat types: from state_institutions and citizen_polity seats, the constraint computes toward rope (coordination with manageable extraction); from excluded_migrant and stateless_persons seats, it computes toward snare (extraction with no exit, suppression of alternatives); from displaced_citizens and domestic_labor_market seats, it computes toward tangled_rope (real coordination benefit mixed with real extraction cost). This divergence IS the measurement — the constraint's legitimacy crisis is precisely that it occupies different types for different seats simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   State institutions (agenda_setter, institutional power, constrained exit) sit near the beneficiary end of directionality (d ~0.25) — they administer the constraint and collect legitimacy rents, but are bound by legal and electoral constraints. Citizen polity (beneficiary, organized, identity_locked) has d ~0.2 — genuine coordination benefit, identity-locked membership makes exit costly, but they bear adjustment costs. Domestic labor market (beneficiary/payer, organized, constrained) sits near symmetric d ~0.5 — net benefit/loss varies by sector and skill. Excluded asylum seekers and economic migrants (payers, powerless, trapped) sit at d ~0.95 — full targets, no exit, bear the constraint's extraction directly. Displaced citizens (payers, moderate, constrained) at d ~0.7 — bear localized costs with limited voice. Stateless persons (payers, powerless, trapped) at d ~1.0 — the constraint has no jurisdictional purchase on them yet excludes them completely. International courts and civil society (observers, analytical/mobile) at d ~0.1 — analytical seats outside the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (balancing sovereignty, protection, consent) remains live but the balance has shifted: protection obligations have expanded (gender persecution, gang violence, climate displacement) while consent has contracted (welfare chauvinism, cultural anxiety) and labor needs have polarized (high-skill shortages, low-skill surpluses). The constraint does not suffer classic mandatrophy (original function gone, form persists) — rather, it suffers 'mandate stretch': the same architecture is asked to balance demands that have diverged beyond its design parameters. The theater ratio rise (0.15→0.28) reflects performative enforcement substituting for unresolved balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_test_operationalization,
    'Does the proportionality/necessity test in border enforcement operate as a genuine constraint on state power, or as a legitimating ritual that ratifies predetermined exclusion?',
    'Comparative analysis of court outcomes: do proportionality reviews regularly invalidate enforcement actions, or do they overwhelmingly defer? Track win rates for state vs. migrant across jurisdictions and over time.',
    'If the test is ritual, suppression is higher than measured (0.42 → 0.6+), theater ratio rises, and the constraint reclassifies toward snare for powerless seats. If genuine, the tangled_rope classification holds — coordination survives enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_operationalization, empirical, 'Whether legal proportionality review materially constrains enforcement or performs legitimacy').

omega_variable(
    climate_displacement_recognition,
    'Will the international legal system recognize climate displacement as triggering protection obligations equivalent to persecution-based refugee status?',
    'Treaty negotiation (new protocol), customary law evolution, or judicial interpretation (Ioane Teitiota v. New Zealand precedent). Track UNHCR guidance, regional instruments (e.g., Cartagena Declaration expansion), and national court rulings.',
    'If recognized, the protection-obligation leg of the tripod expands dramatically — extractiveness rises (more persons entitled to admission), suppression must rise to maintain exclusion, or the constraint restructures toward scaffold (transitional protection regime). If not recognized, climate-displaced persons join the stateless_persons victim set, extraction deepens without coordination expansion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_displacement_recognition, conceptual, 'Whether the protection-obligation category expands to include climate displacement').

omega_variable(
    dual_victim_set_coalition_possibility,
    'Can excluded_migrants and displaced_citizens form a structural coalition against the constraint''s current extraction pattern, or are their interests irreconcilably opposed by the constraint''s design?',
    'Political sociology of migration politics: analyze voting behavior, union positions, NGO coalitions, and policy proposals that link migrant regularization with worker protections (e.g., sectoral bargaining, portable benefits, pathways to citizenship).',
    'If coalition forms, the power atom for displaced_citizens shifts from moderate toward organized/powerful, directionality shifts (d decreases), effective extraction for both victim sets falls — constraint may restructure toward rope. If irreconcilable, the constraint''s extraction is stabilized by divided victims — classic divide-and-extract.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_victim_set_coalition_possibility, preference, 'Whether the two victim sets can structurally align or are locked in constraint-designed opposition').

omega_variable(
    reading_foreclosure_conditions,
    'Under what conditions would this jurisdictional_sovereignty reading foreclose the sovereignty_primary or freedom_of_movement_primary readings, rather than coexist with them?',
    'Constitutional and international law scholarship: identify the doctrinal thresholds where balanced sovereignty becomes logically incompatible with absolute sovereignty (e.g., non-derogable non-refoulement) or with open borders (e.g., welfare state sustainability). Track supreme/constitutional court jurisprudence for foreclosure signals.',
    'If foreclosure conditions are met, the kernel collapses from three live readings to one or two — the constraint family reduces. This would register in the engine as cs_axiom_contradiction between sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_conditions, conceptual, 'Conditions under which this reading logically excludes its siblings within a single framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 1951, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1951, 0.15).
narrative_ontology:measurement(bord_tr_t1975, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(bord_tr_t1990, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(bord_tr_t2001, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2001, 0.26).
narrative_ontology:measurement(bord_tr_t2015, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(bord_tr_t2025, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1951, 0.25).
narrative_ontology:measurement(bord_be_t1975, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement(bord_be_t1990, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(bord_be_t2001, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement(bord_be_t2015, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(bord_be_t2025, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1951, 0.2).
narrative_ontology:measurement(bord_su_t1975, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1975, 0.28).
narrative_ontology:measurement(bord_su_t1990, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(bord_su_t2001, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement(bord_su_t2015, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(bord_su_t2025, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__jurisdictional_sovereignty, 0.12).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, asylum_non_refoulement_obligation).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, labor_migration_channel_governance).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, citizenship_acquisition_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the border_control_legitimacy kernel. The sovereignty_primary reading (absolute exclusion) and freedom_of_movement_primary reading (movement as right) are sibling constraints. All three share the kernel but instantiate different ε, different victim/beneficiary structures, and different claimed types. This reading (jurisdictional_sovereignty) claims tangled_rope; sovereignty_primary claims mountain (natural state prerogative) or snare (extraction from migrants); freedom_of_movement_primary claims mountain (natural right) or rope (coordination of free movement). The ε-invariance principle requires separate stories because measuring 'border legitimacy' through protection, consent, or sovereignty observables yields different ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__jurisdictional_sovereignty, institutional, 0.25).
constraint_indexing:directionality_override(border_control_legitimacy__jurisdictional_sovereignty, organized, 0.2).
constraint_indexing:directionality_override(border_control_legitimacy__jurisdictional_sovereignty, moderate, 0.7).
constraint_indexing:directionality_override(border_control_legitimacy__jurisdictional_sovereignty, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
