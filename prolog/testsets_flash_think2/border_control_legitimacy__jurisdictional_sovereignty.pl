% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Jurisdictional Sovereignty in Border Control
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'jurisdictional sovereignty' reading of
 *   border control legitimacy, which posits that state sovereignty grants
 *   authority to regulate rights and obligations within its territory, but
 *   does not automatically confer absolute border closure authority. Instead,
 *   legitimate border control requires a balancing act between protection
 *   obligations (e.g., refugees), labor needs, and public consent. The
 *   constraint is classified as a Tangled Rope because it serves a genuine
 *   coordination function (managing territorial integrity and societal needs)
 *   but also involves asymmetric extraction from excluded migrants and
 *   potentially from citizens whose consent is undermined or whose rights are
 *   violated by disproportionate enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.65).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.7).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Jurisdictional Sovereignty in Border Control").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, 'ceff981e-24d3-4150-bd41-cba18ba27680').
narrative_ontology:cs_kernel_codification('ceff981e-24d3-4150-bd41-cba18ba27680', formalized).
narrative_ontology:cs_authority_grounding('ceff981e-24d3-4150-bd41-cba18ba27680', lineage).
narrative_ontology:cs_interpretation_layer_present('ceff981e-24d3-4150-bd41-cba18ba27680').
narrative_ontology:cs_reading_relation('ceff981e-24d3-4150-bd41-cba18ba27680', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_reading_relation('ceff981e-24d3-4150-bd41-cba18ba27680', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('ceff981e-24d3-4150-bd41-cba18ba27680', foundational, sovereignty_is_jurisdictional_not_absolute_border_control).
narrative_ontology:cs_axiom_status(sovereignty_is_jurisdictional_not_absolute_border_control, holdable).
narrative_ontology:cs_axiom_grounding('ceff981e-24d3-4150-bd41-cba18ba27680', sovereignty_is_jurisdictional_not_absolute_border_control, deontological).
narrative_ontology:cs_axiom('ceff981e-24d3-4150-bd41-cba18ba27680', foundational, border_legitimacy_requires_balancing_obligations_needs_consent).
narrative_ontology:cs_axiom_status(border_legitimacy_requires_balancing_obligations_needs_consent, holdable).
narrative_ontology:cs_axiom_grounding('ceff981e-24d3-4150-bd41-cba18ba27680', border_legitimacy_requires_balancing_obligations_needs_consent, instrumental).
narrative_ontology:cs_reference_frame('ceff981e-24d3-4150-bd41-cba18ba27680', westphalian_state_system_with_human_rights_overlay).
narrative_ontology:cs_drift_state('ceff981e-24d3-4150-bd41-cba18ba27680', contemporary_migration_crises_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ceff981e-24d3-4150-bd41-cba18ba27680', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_authorities).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, citizens).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, employers_seeking_labor).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining territorial integrity, managing public resources, and ensuring national security. They interpret and enforce border policies, balancing various domestic and international obligations. They benefit from the ability to regulate entry and exit, but are constrained by international law and domestic political pressures.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from perceived security, social cohesion, and managed labor markets. They bear costs if policies lead to labor shortages, human rights violations in their name, or if their 'public consent' is undermined by policies they disagree with. Their ability to influence policy is through democratic processes.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, citizens, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, citizens, payer).

% Bear the direct costs of exclusion, including denial of entry, separation from family, and precarious legal status. Their options are limited to irregular entry, asylum claims (often difficult), or remaining in unsafe conditions in their home countries. They are often identity_locked to their need for safety or economic opportunity.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Monitor state border practices against international human rights law and humanitarian principles. They advocate for the rights of migrants and refugees, challenging policies that violate proportionality or necessity tests. They influence policy through public pressure, legal challenges, and international bodies.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Benefit from access to a flexible labor pool, often at lower wages, which can be facilitated or constrained by border policies. They exert pressure on state authorities to adjust immigration policies to meet economic demands. Their exit options include relocating production or lobbying for policy changes.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, employers_seeking_labor, beneficiary,
    powerful, immediate, mobile, national).

% Provide frameworks and oversight for state conduct regarding borders and migration, including refugee law and human rights conventions. They assess state compliance and issue recommendations, but lack direct enforcement power over sovereign states.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_law_bodies, observer,
    institutional, civilizational, analytical, global).

% Citizens who are displaced or negatively impacted by migration policies, for example, through perceived strain on public services or cultural changes, whose 'public consent' is not adequately addressed by state policies. They bear social and economic costs without clear recourse.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens, payer,
    powerless, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To manage the movement of people across national borders, balancing the state's legitimate interests in territorial integrity, security, and economic needs with its international human rights obligations and the need for public consent.
% TRANSFER_FUNCTION: Transfers the ultimate authority over who enters and resides in a territory from individuals to the state. It transfers the costs of exclusion to migrants and the costs of managing social integration or perceived societal changes to citizens.
% ABSENT_VOICES: Migrants themselves, particularly those without legal status or political representation, are often absent from policy debates. Their experiences and perspectives are mediated through advocates or ignored, leading to policies that may not reflect their needs or rights.
% DISAPPEARANCE_RATIONALE: If this framework vanished, states would either revert to an absolute, unconstrained border closure (sovereignty_primary) or move towards open borders (freedom_of_movement_primary). Both scenarios would fundamentally alter global migration patterns, state-citizen relations, and international legal norms, leading to a complete reorganization of how human mobility is governed.
% FOUNDING_PROBLEM: How to reconcile the traditional concept of state sovereignty over territory with the realities of global interdependence, human rights, and forced migration, ensuring that border control is legitimate and humane.
% FOUNDING_PROBLEM_CORROBORATION: International organizations (UN, IOM), human rights bodies (Amnesty International, Human Rights Watch), academic migration scholars, and ongoing national and international policy debates consistently highlight the persistent and evolving challenges of legitimate and humane border governance. This corroboration comes from outside the immediate beneficiaries of state power.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is moderate-to-high because even with balancing, the act of controlling borders and excluding individuals imposes significant costs on those denied entry. Suppression (0.70) is high due to the active enforcement required to maintain border controls and the limited alternatives for those seeking entry. The theater ratio (0.25) is relatively low, reflecting a genuine, albeit contested, function in managing complex societal needs, though some performative aspects of enforcement may exist. The rising trend in extractiveness and suppression over the interval reflects increasing migration pressures and the hardening of border regimes, even within this balancing framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state authorities, this framework is a necessary and legitimate tool for governance. From the perspective of excluded migrants and human rights advocates, the 'balancing' often falls short, leading to significant extraction and rights violations. The engine's computation of per-seat classification will highlight this divergence, showing the constraint as more extractive for migrants and more coordinative for the state, despite the shared structural claim.
 *
 * DIRECTIONALITY LOGIC:
 *   State authorities are beneficiaries as they maintain control and manage the territory, but also bear the costs of balancing. Citizens are beneficiaries of perceived order and security, but can be payers if their consent is undermined or if policies lead to social costs. Excluded migrants are clear targets/payers, bearing the brunt of the constraint. Employers seeking labor are beneficiaries when policies allow access to needed workers. Human rights advocates and international law bodies act as observers, challenging the constraint's operation when it fails to meet its legitimacy conditions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_balancing_threshold,
    'What constitutes a legitimate ''balance'' between protection obligations, labor needs, and public consent, and at what point does the balance tip into illegitimacy?',
    'Consensus among international legal scholars, human rights bodies, and representative democratic processes on specific thresholds or criteria for balancing these competing demands.',
    'If the threshold is defined, it would provide clearer grounds for challenging or upholding specific border policies, potentially reclassifying policies that fail the test as pure extraction (Snare) rather than a legitimate balancing act.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_balancing_threshold, conceptual, 'Ambiguity in defining the ''legitimate balance'' in border control.').

omega_variable(
    proportionality_enforcement_genuineness,
    'Are the proportionality and necessity tests applied to border enforcement genuinely constraining state action, or are they primarily rhetorical justifications for existing practices?',
    'Empirical analysis of judicial review outcomes, independent audits of border enforcement practices, and the actual impact of legal challenges on policy implementation.',
    'If the tests are found to be largely rhetorical, the effective suppression and extractiveness of the constraint would be higher, pushing it closer to a Snare by removing a key legitimizing condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_enforcement_genuineness, empirical, 'Effectiveness of proportionality tests in constraining border enforcement.').

omega_variable(
    public_consent_authenticity,
    'Is ''public consent'' for border policies genuinely informed and deliberative, or is it shaped by political narratives that suppress dissent, exploit fear, or misrepresent the costs and benefits of migration?',
    'Sociological studies of public opinion formation, media analysis, and deliberative democracy experiments to assess the quality and authenticity of public discourse on migration.',
    'If public consent is found to be manipulated or uninformed, a key pillar of the constraint''s legitimacy would be undermined, potentially increasing its effective extractiveness for citizens and reclassifying it as more coercive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_consent_authenticity, empirical, 'Authenticity and deliberative quality of public consent for border policies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t2000, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(bord_tr_t2005, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2005, 0.21).
narrative_ontology:measurement(bord_tr_t2010, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(bord_tr_t2015, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2015, 0.23).
narrative_ontology:measurement(bord_tr_t2020, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(bord_tr_t2025, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(bord_be_t2000, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(bord_be_t2005, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(bord_be_t2010, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(bord_be_t2015, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(bord_be_t2020, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(bord_be_t2025, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t2000, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(bord_su_t2005, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement(bord_su_t2010, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(bord_su_t2015, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2015, 0.67).
narrative_ontology:measurement(bord_su_t2020, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement(bord_su_t2025, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'border_control_legitimacy' kernel. It represents the view that sovereignty is jurisdictional authority, not absolute border closure, requiring a balance of obligations. It is linked to the 'freedom_of_movement_primary' and 'sovereignty_primary' readings as competing interpretations of the same underlying kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
