% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Federation Membership Treaty — Sovereignty-Primary Reading: State Consent Condition on Free Movement
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The sovereignty-primary reading of the federation membership treaty
 *   interprets free movement as a conditional privilege granted by member
 *   states, not an absolute right. States retain authority to restrict
 *   movement to protect national labor markets and welfare systems. This
 *   reading was essential to the treaty's original ratification but has
 *   become a structural brake on integration. The constraint operates as a
 *   tangled rope: it coordinates a federal market (genuine coordination)
 *   while extracting mobility rights from workers to preserve state autonomy
 *   (asymmetric extraction). The beneficiary set is local labor markets and
 *   national welfare systems; the victim set is mobile workers and
 *   cross-border commuters. Active enforcement is required — border checks,
 *   permit systems, welfare access conditions — and suppression is moderate
 *   because alternatives (irregular migration, third-country relocation)
 *   exist but are costly.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.58).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.45).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Federation Membership Treaty — Sovereignty-Primary Reading: State Consent Condition on Free Movement").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '6181b5eb-afc4-45b3-8fdb-2d285fa2c679').
narrative_ontology:cs_kernel_codification('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', formalized).
narrative_ontology:cs_authority_grounding('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', lineage).
narrative_ontology:cs_interpretation_layer_present('6181b5eb-afc4-45b3-8fdb-2d285fa2c679').
narrative_ontology:cs_reading_relation('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', foundational, state_consent_required_for_movement_restrictions).
narrative_ontology:cs_axiom_status(state_consent_required_for_movement_restrictions, holdable).
narrative_ontology:cs_axiom_grounding('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', state_consent_required_for_movement_restrictions, conventional).
narrative_ontology:cs_axiom('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', foundational, national_welfare_autonomy_preserved).
narrative_ontology:cs_axiom_status(national_welfare_autonomy_preserved, holdable).
narrative_ontology:cs_axiom_grounding('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', national_welfare_autonomy_preserved, conventional).
narrative_ontology:cs_axiom('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', secondary, free_movement_not_absolute_right).
narrative_ontology:cs_axiom_status(free_movement_not_absolute_right, holdable).
narrative_ontology:cs_axiom_grounding('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', free_movement_not_absolute_right, conventional).
narrative_ontology:cs_reference_frame('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', rome_treaty_1957_sovereignty_bargain).
narrative_ontology:cs_drift_state('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', post_eastern_enlargements_2004_2007, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6181b5eb-afc4-45b3-8fdb-2d285fa2c679', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_states).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, local_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, cross_border_commuters).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, national_regulatory_autonomy).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, subsidiarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate, ratify, and enforce treaty provisions. Retain veto over free movement restrictions to protect national labor markets and welfare systems. Exercise authority through national parliaments, constitutional courts, and executive action. Collect regulatory autonomy as the primary benefit.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_states, agenda_setter,
    institutional, generational, analytical, continental).

% Domestic workers and unions in member states. Gain protection from wage competition and displacement through state-imposed restrictions on incoming mobile workers. Their political voice is amplified through national electoral systems. Exit means accepting open competition or emigrating.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, local_labor_markets, beneficiary,
    organized, biographical, constrained, national).

% Public insurance, healthcare, pension, and social assistance schemes financed by national contributions. State restrictions on mobile worker access prevent fiscal strain from non-contributors. The systems collect preserved financial sustainability. Exit is not available — they are territorially bound.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_welfare_systems, beneficiary,
    institutional, generational, analytical, national).

% Workers seeking employment across member state borders. Face permit requirements, quota systems, wage floors, and welfare access delays imposed by destination states. Bear the cost of restricted mobility in lost opportunities and compliance burdens. Exit options: accept restrictions, return home, or move to third countries — all costly.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    moderate, biographical, constrained, continental).

% Workers living in one member state and employed in a neighboring state. Uniquely vulnerable to bilateral restriction regimes — daily life depends on uninterrupted movement. Bear concentrated costs when states reimpose controls. Exit means relocating residence or job, disrupting family and community ties.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, cross_border_commuters, payer,
    moderate, biographical, constrained, regional).

% Civil society, business federations, and political actors advocating for deeper free movement. Would object to sovereignty-based restrictions as undermining the single market. Their exclusion from national decision-making on restriction triggers is structural — they hold no veto in member state capitals.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, integration_advocates, excluded,
    organized, generational, mobile, continental).

% Supranational guardian of treaty compliance. Monitors whether state restrictions meet proportionality and non-discrimination tests. Can launch infringement proceedings but lacks direct enforcement — depends on Court rulings and political pressure. Sees full structure but cannot unilaterally alter it.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, eu_commission, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a federal framework for economic cooperation and political integration while preserving member state authority to regulate labor market access and welfare eligibility — the coordination is the treaty itself, the sovereignty clause is the price of ratification.
% TRANSFER_FUNCTION: Moves regulatory authority over mobility from supranational to national level; moves economic opportunity from mobile workers to local incumbents; moves fiscal risk from national welfare systems onto excluded workers.
% ABSENT_VOICES: Mobile workers and cross-border commuters who would object to restrictions but lack voting rights in destination states; their interests are filtered through supranational institutions with limited enforcement power. Integration advocates in civil society and business are structurally excluded from the national consent decisions that trigger restrictions.
% DISAPPEARANCE_RATIONALE: If the state consent condition vanished overnight, free movement would become absolute. National labor markets would face immediate wage and displacement pressures; welfare systems would confront unfunded eligibility expansion; member states would lose their primary sovereignty safeguard. The federal bargain would collapse or require radical renegotiation.
% FOUNDING_PROBLEM: Post-war European integration required a federal treaty that could secure ratification by sovereign states. The sovereignty clause — making free movement conditional on state consent for labor and welfare protection — was the political price of agreement. It balanced economic cooperation against the fear of uncontrolled migration undermining national social contracts.
% FOUNDING_PROBLEM_CORROBORATION: Treaty negotiation records (Messina 1955, Rome 1957) show sovereignty protections were demanded by smaller states and accepted by larger ones as the condition for a common market. Constitutional court rulings (German Bundesverfassungsgericht, French Conseil Constitutionnel) affirm national identity clauses as treaty-compliant. Academic federalism literature (Weiler, Majone, Scharpf) documents the contested legacy: some argue the clause enabled integration; others argue it embedded a structural deficit that prevents completion of the single market.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the constraint transfers substantial regulatory value from mobile workers to states — but not pure extraction because the federal framework itself provides coordination value. Suppression (0.45) is moderate: states actively enforce restrictions but the Schengen acquis and ECJ jurisprudence create counter-pressure. Theater ratio (0.22) is low: restrictions are substantively enforced, not performative. Accessibility collapse (0.48) is partial: workers can move but face friction; resistance (0.52) is significant from integration advocates and affected workers. The measurement series shows extraction rising as the single market deepened while sovereignty clauses remained — the coordination function expanded but the extraction clause did not sunset.
 *
 * PERSPECTIVAL GAP:
 *   From the member state seat, the constraint is a necessary sovereignty safeguard enabling the federation. From the mobile worker seat, it is a barrier extracting opportunity. From the EU Commission seat, it is a treaty exception that must be narrowly interpreted. The engine will compute these divergent seat classifications from the structural data — the authored claim (tangled_rope) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states are agenda_setters with analytical exit — they designed the constraint and can veto changes. Local labor markets and welfare systems are beneficiaries with constrained exit — they gain protection but are territorially bound. Mobile workers and cross-border commuters are payers with constrained exit — they bear costs but cannot easily escape the federal labor market. Integration advocates are excluded — they would challenge restrictions but lack national veto power. The EU Commission is an observer with analytical exit — it monitors but cannot override state consent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ratification of a federal treaty by sovereign states) was live in 1957. Whether it remains live is contested: integration_primary argues the sovereignty clause has outlived its function and now obstructs the single market; sovereignty_primary argues the social contract still requires national control. The mandate has not been formally resolved — no sunset clause exists — creating a classic mandatrophy tension where the arrangement persists without consensus on its continued justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_structure,
    'Does the sovereignty_primary reading represent a genuine coordination function (enabling federation by assuring states) or an extraction mechanism (preserving state power at worker expense)?',
    'Counterfactual analysis: would the treaty have been ratified without the sovereignty clause? If yes, the clause was extraction from the start. If no, it was coordination. Historical negotiation records and comparative federalism (US, Swiss, Canadian cases) provide evidence.',
    'If genuine coordination, the constraint is a rope with residual extraction. If extraction cover, it is a snare masquerading as a tangled rope. Determines whether the mandated sovereignty protection is a feature or a bug.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_structure, conceptual, 'Whether the sovereignty clause is a founding coordination necessity or an extractive relic.').

omega_variable(
    labor_protection_vs_incumbent_protection,
    'Do state restrictions on mobile workers actually protect local labor market outcomes (wages, employment) or primarily protect incumbent workers from competition?',
    'Empirical studies of restriction regimes (transitional arrangements post-2004/2007 enlargements, posting of workers directive enforcement). Compare wage/employment trajectories in restricted vs. open sectors and regions.',
    'If restrictions improve aggregate local outcomes, the beneficiary claim for local_labor_markets is substantiated. If they only protect incumbents, the beneficiary set narrows and extractiveness toward mobile workers increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_protection_vs_incumbent_protection, empirical, 'Whether labor market protection benefits the collective or captures rents for insiders.').

omega_variable(
    welfare_fiscal_impact_of_mobile_workers,
    'What is the net fiscal contribution of mobile workers to destination-state welfare systems, relative to the restrictions imposed?',
    'Longitudinal fiscal studies tracking mobile worker cohorts'' contributions vs. benefits across member states. OECD/EU Commission fiscal incidence analyses.',
    'If mobile workers are net contributors, welfare_system beneficiary claim weakens and extractiveness rises. If net beneficiaries, the restriction has fiscal coordination logic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_fiscal_impact_of_mobile_workers, empirical, 'Whether welfare access restrictions are fiscally justified or extractive.').

omega_variable(
    sovereignty_clause_forecloses_integration_reading,
    'Does the sovereignty_primary reading logically foreclose the integration_primary reading within a single legal framework, or do they coexist as competing interpretations?',
    'ECJ case law analysis: does the Court treat sovereignty exceptions as narrow derogations (coexistence) or as structural principles that can override free movement (foreclosure)? Treaty amendment history: have reforms expanded or contracted the sovereignty clause?',
    'If forecloses, the kernel has a structural fracture — no single framework can satisfy both readings. If coexists_with, the tension is political, not logical. Determines reading_relations declaration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_clause_forecloses_integration_reading, conceptual, 'Structural relationship between sovereignty_primary and integration_primary readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 1957, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1957, federation_membership_treaty__sovereignty_primary, theater_ratio, 1957, 0.1).
narrative_ontology:measurement(fede_tr_t1970, federation_membership_treaty__sovereignty_primary, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(fede_tr_t1985, federation_membership_treaty__sovereignty_primary, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(fede_tr_t1992, federation_membership_treaty__sovereignty_primary, theater_ratio, 1992, 0.18).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_treaty__sovereignty_primary, theater_ratio, 2004, 0.2).
narrative_ontology:measurement(fede_tr_t2015, federation_membership_treaty__sovereignty_primary, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_treaty__sovereignty_primary, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(fede_be_t1957, federation_membership_treaty__sovereignty_primary, base_extractiveness, 1957, 0.35).
narrative_ontology:measurement(fede_be_t1970, federation_membership_treaty__sovereignty_primary, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement(fede_be_t1985, federation_membership_treaty__sovereignty_primary, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement(fede_be_t1992, federation_membership_treaty__sovereignty_primary, base_extractiveness, 1992, 0.52).
narrative_ontology:measurement(fede_be_t2004, federation_membership_treaty__sovereignty_primary, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement(fede_be_t2015, federation_membership_treaty__sovereignty_primary, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(fede_be_t2024, federation_membership_treaty__sovereignty_primary, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1957, federation_membership_treaty__sovereignty_primary, suppression_requirement, 1957, 0.3).
narrative_ontology:measurement(fede_su_t1970, federation_membership_treaty__sovereignty_primary, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(fede_su_t1985, federation_membership_treaty__sovereignty_primary, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement(fede_su_t1992, federation_membership_treaty__sovereignty_primary, suppression_requirement, 1992, 0.4).
narrative_ontology:measurement(fede_su_t2004, federation_membership_treaty__sovereignty_primary, suppression_requirement, 2004, 0.42).
narrative_ontology:measurement(fede_su_t2015, federation_membership_treaty__sovereignty_primary, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(fede_su_t2024, federation_membership_treaty__sovereignty_primary, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__sovereignty_primary, 0.12).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, eu_single_market_completion).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, posting_of_workers_directive).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, schengen_border_code).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, european_social_charter).

% DUAL FORMULATION NOTE:
% Part of the federation_membership_treaty constraint family. This reading (sovereignty_primary) and its siblings (integration_primary, subsidiarity_balance) decompose the single treaty label into structurally distinct constraints with different ε values, beneficiary/victim sets, and enforcement logics. The sovereignty clause is the coordination-extraction boundary; integration_primary treats it as a narrow exception, subsidiarity_balance as a proportionality test.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, institutional, 0.15).
constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
