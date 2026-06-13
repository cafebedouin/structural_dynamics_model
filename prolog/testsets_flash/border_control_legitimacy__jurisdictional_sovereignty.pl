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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Jurisdictional Sovereignty Reading of Border Control Legitimacy
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'jurisdictional sovereignty' reading of
 *   border control legitimacy, where state sovereignty grants authority to
 *   regulate within its territory but not necessarily absolute border
 *   closure. Legitimacy is contingent on balancing protection obligations
 *   (for citizens and human rights of migrants) with labor needs and public
 *   consent. It acknowledges dual victim sets: both excluded migrants and,
 *   potentially, citizens displaced by unmanaged migration. The enforcement
 *   apparatus is constrained by proportionality and necessity, and legitimacy
 *   crises arise from violations of human rights or undermining public
 *   consent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.6).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.7).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.6).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Jurisdictional Sovereignty Reading of Border Control Legitimacy").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '3843562c-a50a-40d1-abf1-82f3fb9cef83').
narrative_ontology:cs_kernel_codification('3843562c-a50a-40d1-abf1-82f3fb9cef83', formalized).
narrative_ontology:cs_authority_grounding('3843562c-a50a-40d1-abf1-82f3fb9cef83', lineage).
narrative_ontology:cs_interpretation_layer_present('3843562c-a50a-40d1-abf1-82f3fb9cef83').
narrative_ontology:cs_reading_relation('3843562c-a50a-40d1-abf1-82f3fb9cef83', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('3843562c-a50a-40d1-abf1-82f3fb9cef83', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('3843562c-a50a-40d1-abf1-82f3fb9cef83', foundational, sovereignty_is_jurisdictional_not_absolute).
narrative_ontology:cs_axiom_status(sovereignty_is_jurisdictional_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('3843562c-a50a-40d1-abf1-82f3fb9cef83', sovereignty_is_jurisdictional_not_absolute, deontological).
narrative_ontology:cs_axiom('3843562c-a50a-40d1-abf1-82f3fb9cef83', foundational, legitimacy_requires_balancing_obligations_and_consent).
narrative_ontology:cs_axiom_status(legitimacy_requires_balancing_obligations_and_consent, holdable).
narrative_ontology:cs_axiom_grounding('3843562c-a50a-40d1-abf1-82f3fb9cef83', legitimacy_requires_balancing_obligations_and_consent, deontological).
narrative_ontology:cs_reference_frame('3843562c-a50a-40d1-abf1-82f3fb9cef83', post_westphalian_constrained_sovereignty).
narrative_ontology:cs_drift_state('3843562c-a50a-40d1-abf1-82f3fb9cef83', contemporary_globalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3843562c-a50a-40d1-abf1-82f3fb9cef83', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, citizenry_seeking_stability).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, labor_market_sectors).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary actor responsible for defining and enforcing border policies, balancing internal stability, economic needs, and international obligations. Its legitimacy is derived from its ability to protect its citizens and manage its territory, but also from upholding human rights.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, nation_state, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the state's ability to manage its borders to maintain social cohesion, public services, and perceived security. Their consent is a key component of the state's legitimacy in this reading.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, citizenry_seeking_stability, beneficiary,
    organized, biographical, constrained, national).

% Benefits from the state's ability to regulate labor migration to meet economic demands, fill labor shortages, or control wage levels. They exert pressure for policies that align with their economic interests.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, labor_market_sectors, beneficiary,
    powerful, immediate, mobile, national).

% Bear the direct costs of border controls, including denial of entry, separation from families, and precarious legal status. Their human rights are a critical consideration for the legitimacy of the state's actions.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Citizens who may experience negative impacts from migration policies, such as increased competition for resources or changes in local demographics, if not managed effectively. Their consent is crucial for the state's legitimacy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens, payer,
    moderate, biographical, constrained, local).

% Monitor state compliance with international human rights law, providing critical oversight and challenging policies that violate fundamental rights, particularly for migrants and asylum seekers.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's internal capacity to govern its territory and population, manage its economy, and fulfill its protection obligations, while also integrating labor needs and public consent regarding migration.
% TRANSFER_FUNCTION: Transfers the right to reside and work within a territory, along with associated social benefits and obligations, from the state to admitted individuals, while denying these to excluded individuals. It also transfers the burden of managing social and economic impacts to the state, which then distributes these to citizens.
% ABSENT_VOICES: Stateless persons and those with no legal claim to any territory are often entirely absent from the conversation, bearing the full weight of exclusion without representation. Future generations, who will inherit the long-term demographic and economic consequences of current policies, also lack a direct voice.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, states would lose a key mechanism for managing their populations, economies, and social contracts. Borders would become porous, leading to massive demographic shifts, potential collapse of social services, and a fundamental redefinition of national identity and governance. The world would be profoundly reorganized.
% FOUNDING_PROBLEM: The need for states to define and control their territorial boundaries to establish a stable political order, protect their citizens, and manage resources, while also acknowledging the human rights of non-citizens and the economic realities of a globalized world.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and various national commissions corroborate that the problem of balancing sovereign control with human rights and economic realities remains live and highly contested. This is supported by ongoing debates in international forums and national legislatures, as well as by the continuous flow of migrants and refugees.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the state's ability to govern its territory and population (benefiting citizens and labor markets) while simultaneously extracting from excluded migrants and potentially from citizens facing displacement. The extractiveness (0.6) reflects the significant costs borne by those denied entry or facing precarious status, as well as the social costs of unmanaged migration. Suppression (0.7) is high due to the active enforcement required to maintain borders and control entry. The theater ratio (0.2) is relatively low, indicating that while there's some performative aspect to border security, the core functions are real and actively maintained.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the nation-state and its citizenry, this constraint is a necessary coordination mechanism for maintaining order and protecting national interests. From the perspective of excluded migrants, it is a highly extractive and suppressive barrier. International human rights bodies view it as a system requiring constant scrutiny to ensure proportionality and adherence to international law. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The nation-state, citizenry seeking stability, and labor market sectors are beneficiaries (low d) as they gain from regulated borders. Excluded migrants and displaced citizens are victims (high d) as they bear the costs of exclusion or unmanaged migration impacts. International human rights bodies are observers (analytical d). The constraint's legitimacy hinges on the state's ability to manage these conflicting interests.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling legitimate state functions as pure extraction by emphasizing the coordination role of sovereignty in maintaining a stable political and social order. However, it also prevents mislabeling extraction as pure coordination by requiring a balance with human rights and public consent. Mandatrophy would occur if the state's actions became solely extractive (e.g., using border control purely for labor exploitation or political repression) while claiming to uphold protection obligations, or if the founding problem of balancing these interests ceased to be live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_of_enforcement,
    'Is the state''s border enforcement apparatus proportionate to the threats it addresses and necessary for achieving legitimate aims, or does it exceed these bounds?',
    'Independent audits of border enforcement practices, judicial review of individual cases, and comparative analysis with states employing less restrictive measures while achieving similar security outcomes.',
    'If enforcement is found disproportionate or unnecessary, the constraint''s suppression and extractiveness would be re-evaluated upwards, potentially shifting its classification towards a Snare due to excessive coercion beyond legitimate coordination needs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_enforcement, empirical, 'Assesses whether border enforcement measures align with the ''jurisdictional sovereignty'' principle of constrained authority.').

omega_variable(
    public_consent_measurement,
    'How is ''public consent'' for migration policies genuinely measured, and does it reflect informed deliberation or is it susceptible to manipulation and xenophobia?',
    'Deliberative polling, citizen assemblies on migration policy, and analysis of media narratives to distinguish genuine public opinion from manufactured consent or fear-driven reactions.',
    'If public consent is found to be manipulated, the legitimacy grounding of the constraint would be undermined, increasing its effective extractiveness from migrants and potentially shifting the ''displaced_citizens'' role towards a victim of manipulation rather than a beneficiary of protection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_consent_measurement, conceptual, 'Examines the quality and authenticity of public consent as a legitimacy factor for border control.').

omega_variable(
    sovereignty_scope_ambiguity,
    'Does ''jurisdictional authority'' inherently include the right to control entry, or is entry control a separate power that must be justified independently?',
    'Further development in international legal doctrine and state practice, potentially through advisory opinions from international courts or new multilateral treaties.',
    'If entry control is deemed a separate power, the ''jurisdictional sovereignty'' reading would be weakened, potentially requiring a re-evaluation of the constraint''s foundational axioms and its relationship to the ''freedom_of_movement_primary'' reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_scope_ambiguity, conceptual, 'Clarifies the scope of sovereignty regarding border control within this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(bord_tr_t1968, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(bord_tr_t1988, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1988, 0.18).
narrative_ontology:measurement(bord_tr_t2008, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(bord_tr_t2024, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(bord_be_t1968, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1968, 0.48).
narrative_ontology:measurement(bord_be_t1988, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1988, 0.55).
narrative_ontology:measurement(bord_be_t2008, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement(bord_be_t2024, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(bord_su_t1968, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1968, 0.58).
narrative_ontology:measurement(bord_su_t1988, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1988, 0.65).
narrative_ontology:measurement(bord_su_t2008, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2008, 0.72).
narrative_ontology:measurement(bord_su_t2024, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
