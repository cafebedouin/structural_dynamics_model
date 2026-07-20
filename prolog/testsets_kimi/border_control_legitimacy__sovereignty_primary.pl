% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: Territorial Sovereignty as Absolute Border Control
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   The sovereignty_primary reading treats state territorial sovereignty as
 *   necessarily including absolute discretion to exclude non-citizens, making
 *   border control constitutive of statehood itself. This constraint story
 *   models that reading as a governance arrangement: it coordinates
 *   membership and collective self-determination for citizens while
 *   asymmetrically extracting from excluded non-citizens through border
 *   violence, detention, and legal exclusion. The enforcement apparatus
 *   justifies itself as defending the very possibility of political order,
 *   treating human rights limitations as external moral irritants rather than
 *   internal legitimacy conditions.
 *
 * KEY AGENTS:
 *   - border_enforcement_apparatus: Agenda-setter (institutional/constrained) â administers exclusion and collects institutional legitimacy
 *   - citizen_electorate: Primary beneficiary (organized/constrained) â receives bounded membership goods and labor market protection
 *   - excluded_non_citizens: Primary target (powerless/trapped) â bears exclusion costs, detention, and violence
 *   - human_rights_advocates: Excluded voice (moderate/mobile) â structurally marginalized in sovereignty forums
 *   - international_legal_observers: Analytical observer (institutional/analytical) â monitors without enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.78).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.82).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Territorial Sovereignty as Absolute Border Control").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, '8ba74b54-1388-4f6f-a01b-33586377ccb9').
narrative_ontology:cs_kernel_codification('8ba74b54-1388-4f6f-a01b-33586377ccb9', fixed_text).
narrative_ontology:cs_authority_grounding('8ba74b54-1388-4f6f-a01b-33586377ccb9', lineage).
narrative_ontology:cs_interpretation_layer_present('8ba74b54-1388-4f6f-a01b-33586377ccb9').
narrative_ontology:cs_reading_relation('8ba74b54-1388-4f6f-a01b-33586377ccb9', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_reading_relation('8ba74b54-1388-4f6f-a01b-33586377ccb9', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('8ba74b54-1388-4f6f-a01b-33586377ccb9', foundational, territorial_exclusion_constitutive).
narrative_ontology:cs_axiom_status(territorial_exclusion_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('8ba74b54-1388-4f6f-a01b-33586377ccb9', territorial_exclusion_constitutive, conventional).
narrative_ontology:cs_axiom('8ba74b54-1388-4f6f-a01b-33586377ccb9', foundational, human_rights_as_external_limit).
narrative_ontology:cs_axiom_status(human_rights_as_external_limit, holdable).
narrative_ontology:cs_axiom_grounding('8ba74b54-1388-4f6f-a01b-33586377ccb9', human_rights_as_external_limit, deontological).
narrative_ontology:cs_reference_frame('8ba74b54-1388-4f6f-a01b-33586377ccb9', westphalian_sovereignty_authority).
narrative_ontology:cs_drift_state('8ba74b54-1388-4f6f-a01b-33586377ccb9', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8ba74b54-1388-4f6f-a01b-33586377ccb9', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_electorate).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, border_enforcement_apparatus).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_non_citizens).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers border control, detention, and removal operations. Derives budget, legal mandate, and institutional legitimacy from the claim that territorial sovereignty requires absolute exclusion discretion. Frames enforcement as defense of statehood itself.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, border_enforcement_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Receives bounded membership benefits including access to territorial public goods, labor market protections, and collective self-determination framed against outsiders. Politically supports or tacitly accepts enforcement as necessary to statehood.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_electorate, beneficiary,
    organized, generational, constrained, national).

% Bear the costs of exclusion: denied entry, family separation, precarious legal status, detention, pushbacks, and death at borders. Cannot access the territory's protections or labor markets on equal terms. Exit from the constraint means either returning to origin conditions or seeking ever-more-dangerous irregular routes.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, excluded_non_citizens, payer,
    powerless, immediate, trapped, global).

% Assert that freedom of movement and asylum are fundamental rights that limit sovereignty claims. Structurally marginalized in sovereignty-centric policy forums; their voices are treated as external moral constraints rather than constitutive of legitimate border policy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, human_rights_advocates, excluded,
    moderate, biographical, mobile, global).

% Monitor compliance with refugee conventions and human rights law. Record violations but lack enforcement power against sovereign states asserting absolute discretion. Produce reports that are often ignored by the agenda-setting state.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_legal_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__sovereignty_primary, citizen_electorate).
narrative_ontology:fixing_cost_class(border_control_legitimacy__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines the membership boundary of the political community, enabling collective self-determination and the provision of public goods to a bounded population. Coordinates who is inside the sphere of mutual obligation and who is outside.
% TRANSFER_FUNCTION: Transfers security, public goods access, and labor market position from non-citizens to citizens and the enforcement apparatus, while transferring the costs of exclusionâdetention, pushbacks, legal precarity, and bodily riskâonto non-citizens.
% ABSENT_VOICES: Asylum seekers and rejected migrants are physically and legally excluded from the deliberative forums where border policy is set. Human rights advocates are present in discourse but structurally excluded from sovereignty-centric decision venues.
% DISAPPEARANCE_RATIONALE: If the absolute discretion to exclude vanished overnight, state membership boundaries would lose their coercive anchor; labor markets, welfare systems, and political communities would reorganize around inclusive or differentiated membership models rather than territorial exclusion.
% FOUNDING_PROBLEM: The emergence of modern statehood required distinguishing internal populations subject to law and protection from external actors; border control was institutionalized to secure territory, regulate labor, and constitute the 'we' of democratic self-determination.
% FOUNDING_PROBLEM_CORROBORATION: State sovereignty theorists and enforcement agencies attest the problem is live, citing external threats and unregulated migration. Human rights advocates, migration economists, and some international legal scholars attest the founding problem has shifted: territorial control is now decoupled from genuine security needs and persists as a mechanism of exclusion and labor stratification. Independent demographic and economic analysis from outside the benefiting parties supports the shifted-function reading.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint systematically transfers life chances, security, and legal standing from non-citizens to the citizen body. Suppression is higher (0.82) because the arrangement depends on active enforcementâpushbacks, detention, criminalization of solidarityâto prevent entry and suppress alternatives. Theater_ratio rises to 0.50 over the interval as enforcement becomes increasingly performative (walls, surveillance spectacle) relative to its coordination function. Accessibility_collapse is moderate (0.60): open-border alternatives are thinkable and exist regionally, but are heavily politically suppressed. Resistance is moderate (0.55): migrants resist by crossing, NGOs challenge legally, but state coercion largely prevails.
 *
 * PERSPECTIVAL GAP:
 *   The citizen_electorate and border_enforcement_apparatus seats should compute as low-directionality beneficiaries: the constraint subsidizes their security, membership, and budget. The excluded_non_citizens seat should compute as high-directionality target: the same structure extracts bodily risk, legal precarity, and family separation. The human_rights_advocates seat, though excluded, is not a victim of direct extraction but of epistemic exclusion. The engine will register seat divergence between the beneficiary and payer positions from identical structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The border_enforcement_apparatus is structurally a beneficiary-agenda_setter: it receives budget and legitimacy from the sovereignty narrative, giving it a low derived d. The citizen_electorate is a pure beneficiary: it receives bounded public goods and labor market protection without administering the constraint, also yielding low d. Excluded_non_citizens are declared victims with trapped exit options and powerless status, producing a high derived d near the full-target end. No override is needed because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this arrangement could be misread as either a pure rope (border coordination for public goods) or a pure snare (cynical extraction with no coordination function). The tangled_rope gate forces both the coordination function (genuine membership boundary for democratic self-determination) and the asymmetric extraction (non-citizens pay with exclusion and violence) to be present. This prevents the false benignity of treating all border control as coordination, and the false malice of treating all sovereignty claims as pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_natural_law_or_construct,
    'Is the absolute discretion to exclude non-citizens a constitutive feature of statehood itself, or a historically contingent political doctrine that serves particular beneficiaries?',
    'Historical comparative analysis of state forms that functioned without territorial exclusion (pre-modern empires, contemporary free-movement zones) and empirical assessment of whether statehood collapses without border control.',
    'If statehood does not require absolute exclusion, the constraint is a constructed snare or tangled rope rather than a mountain, and its victims are not natural casualties of political order but targets of policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_natural_law_or_construct, conceptual, 'Whether sovereignty-as-exclusion is natural or constructed').

omega_variable(
    enforcement_security_vs_extraction,
    'Does the escalation of border enforcement track genuine security threats, or does it function as performative sovereignty theater that extracts from non-citizens while delivering diminishing security returns?',
    'Correlate enforcement expenditure and violence rates at borders with objective security indicators; compare outcomes in high-enforcement vs. low-enforcement jurisdictions with similar migration pressures.',
    'If enforcement is largely theatrical, the theater_ratio is higher than functional necessity suggests, and the constraint drifts toward piton or snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_security_vs_extraction, empirical, 'Whether enforcement escalation tracks security or theater').

omega_variable(
    human_rights_as_constitutive_or_external,
    'Are human rights constraints on border control external limitations on sovereign authority, or are they constitutive of legitimate statehood in the contemporary international order?',
    'Jurisprudential analysis of international court rulings and state practice: do states that accept rights-based limitations lose sovereign recognition, or retain it?',
    'If rights are constitutive, the sovereignty_primary reading rests on a foreclosed axiom; if external, the reading remains live but contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_rights_as_constitutive_or_external, conceptual, 'Whether human rights are internal or external to sovereignty legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_sov_prim_tr_t0, border_control_legitimacy__sovereignty_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement(border_sov_prim_tr_t10, border_control_legitimacy__sovereignty_primary, theater_ratio, 10, 0.32).
narrative_ontology:measurement(border_sov_prim_tr_t20, border_control_legitimacy__sovereignty_primary, theater_ratio, 20, 0.38).
narrative_ontology:measurement(border_sov_prim_tr_t30, border_control_legitimacy__sovereignty_primary, theater_ratio, 30, 0.42).
narrative_ontology:measurement(border_sov_prim_tr_t40, border_control_legitimacy__sovereignty_primary, theater_ratio, 40, 0.46).
narrative_ontology:measurement(border_sov_prim_tr_t50, border_control_legitimacy__sovereignty_primary, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(border_sov_prim_be_t0, border_control_legitimacy__sovereignty_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(border_sov_prim_be_t10, border_control_legitimacy__sovereignty_primary, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(border_sov_prim_be_t20, border_control_legitimacy__sovereignty_primary, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(border_sov_prim_be_t30, border_control_legitimacy__sovereignty_primary, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(border_sov_prim_be_t40, border_control_legitimacy__sovereignty_primary, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(border_sov_prim_be_t50, border_control_legitimacy__sovereignty_primary, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(border_sov_prim_su_t0, border_control_legitimacy__sovereignty_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(border_sov_prim_su_t10, border_control_legitimacy__sovereignty_primary, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(border_sov_prim_su_t20, border_control_legitimacy__sovereignty_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(border_sov_prim_su_t30, border_control_legitimacy__sovereignty_primary, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(border_sov_prim_su_t40, border_control_legitimacy__sovereignty_primary, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(border_sov_prim_su_t50, border_control_legitimacy__sovereignty_primary, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, identity_coordination).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% The kernel border_control_legitimacy decomposes into three structurally distinct readings because the label 'territorial sovereignty' conflates claims with different epsilon values, victim sets, and legitimacy conditions. This reading asserts high extraction justified by state necessity; siblings deny that sovereignty entails closure authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
