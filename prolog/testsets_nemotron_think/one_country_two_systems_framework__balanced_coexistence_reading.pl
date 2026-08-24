% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country Two Systems: Balanced Coexistence Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   The balanced coexistence reading of One Country Two Systems treats the
 *   arrangement as a medium-extraction constraint regime where sovereignty
 *   and autonomy are neither absolute nor hierarchically ordered, but
 *   functionally divided through ongoing political negotiation. Contested
 *   boundaries (national security, electoral design, judicial interpretation)
 *   are resolved through accommodation rather than legal supremacy — meaning
 *   neither the NPCSC's interpretive power nor the HK judiciary's common law
 *   reasoning automatically prevails; instead, crises trigger renegotiation.
 *   Civil society retains bargaining power through mass mobilization,
 *   international attention, and economic leverage. The arrangement persists
 *   because both systems need each other: the PRC needs Hong Kong's global
 *   connectivity and rule-of-law brand; Hong Kong needs mainland market
 *   access and sovereign cover. This reading instantiates a tangled_rope:
 *   genuine coordination (managing two systems in one sovereignty) coexisting
 *   with asymmetric extraction (central sovereignty extracts political
 *   conformity; local autonomy extracts economic rents).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.48).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.42).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country Two Systems: Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '541c534b-ab72-4408-bba6-7b839d559f4b').
narrative_ontology:cs_kernel_codification('541c534b-ab72-4408-bba6-7b839d559f4b', formalized).
narrative_ontology:cs_authority_grounding('541c534b-ab72-4408-bba6-7b839d559f4b', extraction).
narrative_ontology:cs_interpretation_layer_present('541c534b-ab72-4408-bba6-7b839d559f4b').
narrative_ontology:cs_reading_relation('541c534b-ab72-4408-bba6-7b839d559f4b', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('541c534b-ab72-4408-bba6-7b839d559f4b', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('541c534b-ab72-4408-bba6-7b839d559f4b', foundational, political_accommodation_over_legal_supremacy).
narrative_ontology:cs_axiom_status(political_accommodation_over_legal_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('541c534b-ab72-4408-bba6-7b839d559f4b', political_accommodation_over_legal_supremacy, conventional).
narrative_ontology:cs_axiom('541c534b-ab72-4408-bba6-7b839d559f4b', foundational, mutual_limit_acknowledgment).
narrative_ontology:cs_axiom_status(mutual_limit_acknowledgment, holdable).
narrative_ontology:cs_axiom_grounding('541c534b-ab72-4408-bba6-7b839d559f4b', mutual_limit_acknowledgment, conventional).
narrative_ontology:cs_reference_frame('541c534b-ab72-4408-bba6-7b839d559f4b', negotiated_settlement_framework).
narrative_ontology:cs_drift_state('541c534b-ab72-4408-bba6-7b839d559f4b', post_2019_protests, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('541c534b-ab72-4408-bba6-7b839d559f4b', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_sar_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elites).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, mainland_business_interests).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, mainland_citizens_affected_by_asymmetry).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__balanced_coexistence_reading, political_accommodation_over_legal_supremacy).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__balanced_coexistence_reading, mutual_limit_acknowledgment).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__balanced_coexistence_reading, functional_division_of_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate sovereignty authority under the Basic Law; interprets constitutional boundaries through NPCSC interpretations; maintains control over foreign affairs, defense, and national security; benefits from Hong Kong's economic contribution and international connectivity while constraining political autonomy that could challenge central authority.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government, agenda_setter,
    institutional, generational, arbitrage, continental).

% Administers Hong Kong with high degree of autonomy in local affairs per Basic Law; controls policy domains including finance, trade, immigration, and legal system (except national security); depends on central government for constitutional legitimacy and national security apparatus; negotiates boundary cases through political channels rather than pure legal adjudication.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_sar_government, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_sar_government, beneficiary).

% Exercises rights to protest, free press, assembly, and judicial review within the autonomy space; bears costs when autonomy boundaries contract (national security law, electoral reforms); retains bargaining power through mass mobilization, international attention, and economic disruption capacity; not fully excluded but structurally disadvantaged in constitutional interpretation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society, payer,
    organized, biographical, constrained, local).

% Benefits from Hong Kong's unique position as gateway between mainland and global markets; common law system, free capital flow, and low taxation sustained by autonomy arrangement; lobbies both SAR and central governments to preserve commercial autonomy; can relocate capital and operations if autonomy erodes substantially.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elites, beneficiary,
    powerful, biographical, mobile, global).

% Uses Hong Kong as offshore financing, listing, and trade platform; benefits from separate customs territory and currency regime; advocates for stability and predictability in cross-border rules; pressures central government to maintain Hong Kong's institutional distinctiveness for economic utility.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, mainland_business_interests, beneficiary,
    powerful, biographical, mobile, global).

% Experiences perceived inequities from Hong Kong's separate systems (tax advantages, visa-free travel, capital account openness); bears fiscal costs of central subsidies and infrastructure integration; limited voice in Hong Kong's governance but affected by its policy externalities; nationalist sentiment frames autonomy as privilege rather than right.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, mainland_citizens_affected_by_asymmetry, payer,
    moderate, biographical, trapped, national).

% Monitors compliance with Sino-British Joint Declaration and Basic Law; includes foreign governments, NGOs, legal scholars, and rating agencies; applies diplomatic and economic pressure when autonomy commitments appear breached; provides external enforcement layer that both constrains central overreach and validates SAR autonomy claims.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_observers, observer,
    institutional, generational, analytical, global).

% Adjudicates boundary disputes between autonomy and sovereignty through common law reasoning; issues rulings that interpret Basic Law provisions; subject to NPCSC interpretive override but maintains institutional legitimacy through professional independence; identity_locked because judicial role is constituted by the very autonomy framework it interprets.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_judiciary, observer,
    institutional, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_judiciary, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the relationship between a unitary sovereign state and a semi-autonomous region with distinct legal, economic, and political systems — solving the coordination problem of how two systems operate within one sovereignty without either collapsing into the other.
% TRANSFER_FUNCTION: Moves political authority over boundary-definition from pure legal adjudication to political negotiation; transfers economic rents from Hong Kong's gateway status to both central and local elites; transfers legitimacy costs to civil society when accommodation fails; transfers enforcement discretion to crisis moments rather than routine governance.
% ABSENT_VOICES: Hong Kong residents without organized representation (migrant workers, ethnic minorities, youth not in established civil society groups); mainland residents who would prefer deeper integration but lack political channels; Taiwanese public whose 'one country two systems' reference point is shaped by Hong Kong's trajectory but who have no formal role in the framework.
% DISAPPEARANCE_RATIONALE: If the balanced coexistence framework vanished overnight, Hong Kong would either face immediate central imposition of mainland legal-political system (ending separate systems) or unilateral declaration of maximal autonomy (triggering sovereignty crisis); the economic integration, legal distinctiveness, and international status all depend on the negotiated boundary maintenance.
% FOUNDING_PROBLEM: How to transfer sovereignty over Hong Kong from Britain to China while preserving the capitalist system, way of life, and international confidence that made Hong Kong valuable — without either freezing the territory in 1997 conditions or allowing autonomy to become a vehicle for secession.
% FOUNDING_PROBLEM_CORROBORATION: The PRC and UK governments attest the Joint Declaration solved the 1997 transfer problem; Hong Kong legal scholars (Benny Tai, Johannes Chan) attest the founding problem has evolved into a governance problem of managing pluralism within unity; Beijing-aligned scholars (Wang Zhenmin) attest the founding problem was sovereignty recovery, now complete; international legal commentators (Simon Young) corroborate the problem has shifted from transfer to ongoing constitutional adaptation.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is medium (0.48 at interval end) because the arrangement transfers real value: central government extracts political compliance and narrative control; SAR government and business elites extract economic rents from unique institutional position; civil society bears costs when boundaries shift. Suppression is moderate (0.42) — national security law and electoral reforms raised it sharply (2020 peak 0.72), but the system still permits contested politics within narrowed space. Theater ratio is low-moderate (0.28) — negotiation is real, not performative, though performative elements increased during 2019-2020 crisis. Accessibility collapse is moderate (0.52) — alternatives (full integration, independence) exist but are structurally blocked. Resistance is moderate-high (0.55) — civil society mobilizes, business lobbies, international pressure operates; not crushed but constrained.
 *
 * PERSPECTIVAL GAP:
 *   From PRC seat: arrangement is generous autonomy delegation within sovereign right — extraction is low, coordination high. From civil society seat: arrangement is eroding autonomy under sovereign pressure — extraction is high, coordination is cover. From business seat: arrangement is valuable institutional arbitrage — extraction is negative (subsidy), coordination is essential. From judiciary seat: arrangement is constitutional interpretation under sovereign override threat — extraction varies case-by-case. The engine computes these divergences from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's structural assessment, not any single stakeholder's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   PRC central government is structural beneficiary on sovereignty dimension (d ~ 0.2) but payer on economic dimension (subsidizes integration, bears reputational costs). Hong Kong SAR government is dual: agenda-setter on local governance, beneficiary of autonomy rents, payer of political conformity. Hong Kong civil society is primary payer (bears autonomy erosion costs, constrained exit) but retains organized power (exit_options: constrained not trapped). Business elites on both sides are beneficiaries with mobile exit. Mainland citizens are payers with trapped exit. Judiciary is identity_locked — its institutional role is constituted by the autonomy framework it interprets. International observers are analytical with arbitrage-grade exit (can disengage attention).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1997 sovereignty transfer) is substantially solved but has generated a successor problem: how to govern a pluralist jurisdiction within a unitary authoritarian state without either assimilating or separating. The arrangement persists not because the original mandate lives, but because both systems extract value from the status quo and neither can afford the transition costs of change. This is mandatrophy in the precise sense: the coordination function (managing transition) has atrophied into a rent-distribution mechanism, yet the constraint persists because the cost of renegotiation exceeds the benefit for all agenda-setters. The balanced reading captures this by treating the current regime as the constraint itself — not the transition it was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the balanced_coexistence_reading a distinct structural constraint, or a descriptive midpoint between the sovereignty_primacy and autonomy_primacy readings that would collapse if either extreme prevailed?',
    'Track whether political actors explicitly advocate for ''balanced coexistence'' as a normative position, or whether it only exists as an analytical description of the de facto equilibrium. If no institutional actor claims it as their reading, it may be an observer construct rather than a live constraint.',
    'If observer construct, the constraint should be decomposed into the two primacy readings with a network edge; if live reading, it merits independent classification with its own stakeholder coalitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether balanced coexistence is a live constitutional position or an analytical midpoint').

omega_variable(
    coordination_extraction_separability,
    'Can the coordination function (managing two systems) be separated from the extraction functions (sovereignty rents, autonomy rents) in practice, or are they structurally fused such that reducing extraction degrades coordination?',
    'Examine historical episodes where extraction was reduced (e.g., 2003 Article 23 withdrawal, 2010 electoral reform package) — did coordination capacity improve, degrade, or stay stable? If coordination degrades when extraction falls, they are fused; if coordination improves, they are separable.',
    'If fused, the tangled_rope classification is structurally necessary — the constraint cannot be a pure rope. If separable, the extraction component could be reduced without losing coordination, suggesting a scaffold or rope trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether coordination and extraction are structurally separable in the OCTS framework').

omega_variable(
    renegotiation_mechanism_durability,
    'Does the ''political accommodation'' mechanism have institutionalized procedures (regularized dialogue, joint committees, predefined crisis protocols) or does it depend on ad hoc crisis bargaining that could fail under stress?',
    'Catalog all instances of boundary renegotiation since 1997: identify whether they followed standing procedures or were improvised; assess whether new institutions (e.g., Hong Kong National Security Committee, GBA cooperation mechanisms) have formalized accommodation.',
    'If ad hoc, the constraint is fragile — a crisis could break the accommodation logic and trigger sovereignty_primacy or autonomy_primacy collapse. If institutionalized, the tangled_rope has stable coordination infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renegotiation_mechanism_durability, empirical, 'Whether political accommodation is institutionalized or ad hoc crisis bargaining').

omega_variable(
    civil_society_bargaining_power_trajectory,
    'Is civil society''s bargaining power (mobilization, international attention, economic leverage) structurally durable or eroding irreversibly under national security law and electoral restructuring?',
    'Measure protest frequency/size, international NGO access, foreign capital flow sensitivity to political events, and voter turnout in post-2021 elections over 5-year windows. Structural erosion shows monotonic decline despite trigger events; cyclical pattern shows resilience.',
    'If eroding irreversibly, the constraint drifts toward snare (extraction without coordination). If durable, the tangled_rope maintains its coordination function through countervailing power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_society_bargaining_power_trajectory, empirical, 'Whether civil society''s countervailing power is structurally durable or eroding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 1997, 2047).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(octs_balanced_tr_t1997, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 1997, 0.12).
narrative_ontology:measurement(octs_balanced_tr_t2003, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2003, 0.15).
narrative_ontology:measurement(octs_balanced_tr_t2012, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(octs_balanced_tr_t2014, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2014, 0.25).
narrative_ontology:measurement(octs_balanced_tr_t2019, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement(octs_balanced_tr_t2020, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(octs_balanced_tr_t2023, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2023, 0.38).
narrative_ontology:measurement(octs_balanced_tr_t2047, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2047, 0.28).

% Extraction over time
narrative_ontology:measurement(octs_balanced_be_t1997, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 1997, 0.22).
narrative_ontology:measurement(octs_balanced_be_t2003, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2003, 0.28).
narrative_ontology:measurement(octs_balanced_be_t2012, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2012, 0.35).
narrative_ontology:measurement(octs_balanced_be_t2014, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2014, 0.38).
narrative_ontology:measurement(octs_balanced_be_t2019, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2019, 0.52).
narrative_ontology:measurement(octs_balanced_be_t2020, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement(octs_balanced_be_t2023, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2023, 0.55).
narrative_ontology:measurement(octs_balanced_be_t2047, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2047, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(octs_balanced_su_t1997, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 1997, 0.18).
narrative_ontology:measurement(octs_balanced_su_t2003, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2003, 0.25).
narrative_ontology:measurement(octs_balanced_su_t2012, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2012, 0.32).
narrative_ontology:measurement(octs_balanced_su_t2014, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2014, 0.38).
narrative_ontology:measurement(octs_balanced_su_t2019, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(octs_balanced_su_t2020, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(octs_balanced_su_t2023, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2023, 0.65).
narrative_ontology:measurement(octs_balanced_su_t2047, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2047, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__balanced_coexistence_reading, 0.12).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_national_security_law).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_electoral_reform_2021).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, greater_bay_area_integration_framework).

% DUAL FORMULATION NOTE:
% This constraint family decomposes 'One Country Two Systems' into three readings with distinct ε values and stakeholder structures. The balanced reading (this file) has medium ε (0.48) and tangled_rope structure. Sovereignty primacy reading has higher ε (est. 0.7+) and snare/tangled_rope structure from civil society seat. Autonomy primacy reading has lower ε (est. 0.3-) and rope/scaffold structure from central government seat. All three share the Basic Law / Joint Declaration kernel but instantiate different constraints because ε is reading-indexed (OQ-26).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__balanced_coexistence_reading, institutional, 0.25).
constraint_indexing:directionality_override(one_country_two_systems_framework__balanced_coexistence_reading, organized, 0.7).
constraint_indexing:directionality_override(one_country_two_systems_framework__balanced_coexistence_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
