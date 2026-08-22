% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Territorial Legitimacy via Security Necessity and Defensive Territorial Control
 *   domain: political/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the security-necessity reading of the
 *   territorial legitimacy kernel: the claim that Israeli control of the West
 *   Bank and Golan Heights is legitimated by defensive security requirements
 *   and strategic depth, that Palestinian sovereignty must remain conditional
 *   and demilitarized, and that civilian settlements function as
 *   security-presence assets. The reading is contested by the partition
 *   reading (grounded in UN Resolution 181 and 1948 borders) and the
 *   indigenous continuity reading (grounded in uninterrupted Palestinian
 *   habitation and anti-colonial self-determination). As a kernel reading,
 *   this JSON authors ONLY the security-necessity constraint; sibling
 *   readings are separate files linked via cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - israeli_security_establishment (agenda_setter, institutional/arbitrage): Sets the security doctrine and territorial administration.
 *   - settler_population (beneficiary, organized/identity_locked): Collects housing, land, and state protection through the security frame.
 *   - palestinian_civilians (payer, powerless/trapped): Bear daily extraction of movement rights, land, and autonomy.
 *   - palestinian_polity (payer, moderate/constrained): Bears truncated sovereignty and diplomatic exclusion.
 *   - international_community (observer, institutional/analytical): Monitors but cannot override.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.82).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.88).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity and Defensive Territorial Control").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, 'f113a9af-edc8-4d8d-bbe6-0c5284bc8395').
narrative_ontology:cs_kernel_codification('f113a9af-edc8-4d8d-bbe6-0c5284bc8395', formalized).
narrative_ontology:cs_authority_grounding('f113a9af-edc8-4d8d-bbe6-0c5284bc8395', lineage).
narrative_ontology:cs_interpretation_layer_present('f113a9af-edc8-4d8d-bbe6-0c5284bc8395').
narrative_ontology:cs_reading_relation('f113a9af-edc8-4d8d-bbe6-0c5284bc8395', territorial_legitimacy__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('f113a9af-edc8-4d8d-bbe6-0c5284bc8395', territorial_legitimacy__indigenous_continuity_reading, forecloses).
narrative_ontology:cs_axiom('f113a9af-edc8-4d8d-bbe6-0c5284bc8395', foundational, existential_territorial_depth).
narrative_ontology:cs_axiom_status(existential_territorial_depth, holdable).
narrative_ontology:cs_axiom_grounding('f113a9af-edc8-4d8d-bbe6-0c5284bc8395', existential_territorial_depth, empirically_contingent).
narrative_ontology:cs_axiom('f113a9af-edc8-4d8d-bbe6-0c5284bc8395', foundational, security_override_of_sovereignty).
narrative_ontology:cs_axiom_status(security_override_of_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('f113a9af-edc8-4d8d-bbe6-0c5284bc8395', security_override_of_sovereignty, deontological).
narrative_ontology:cs_reference_frame('f113a9af-edc8-4d8d-bbe6-0c5284bc8395', defensive_territorial_integrity_1967).
narrative_ontology:cs_drift_state('f113a9af-edc8-4d8d-bbe6-0c5284bc8395', post_normalization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f113a9af-edc8-4d8d-bbe6-0c5284bc8395', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_citizenry).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, settler_population).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_civilians).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_polity).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, security_first_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, defensible_borders_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formalizes and administers the security-necessity doctrine through military orders, territorial planning, and threat assessments. Determines where settlements are strategically necessary, where checkpoints are deployed, and what land is designated as state land or firing zones. Can pivot threat assessments but is institutionally committed to territorial depth as an existential imperative.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_security_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the perceived security provided by territorial control and from state subsidies to settlement regions. Pays taxes that fund the occupation infrastructure and provides military manpower for reserve duty in the territories. Cannot easily opt out of the security framing without exiting the national political consensus.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_citizenry, beneficiary,
    organized, biographical, constrained, national).

% Resides in settlements justified by the security-necessity frame, receiving housing, infrastructure, and legal protection from the state. Identifies personal and religious destiny with the territorial project. Exit means abandoning community, home, and ideological framework; relocation is politically and personally costly.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, settler_population, beneficiary,
    organized, generational, identity_locked, regional).

% Live under military administration, movement restrictions, and land confiscation justified by security necessity. Bear daily costs of checkpoints, permit regimes, and settlement expansion. Exit options are severely restricted by citizenship laws, economic dependency, and border controls; leaving means refugeehood.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_civilians, payer,
    powerless, immediate, trapped, local).

% Claims sovereignty over the 1967 territories but exercises only conditional, delegated authority under the Oslo framework. Bears the cost of truncated sovereignty, fiscal dependence on Israel, and diplomatic exclusion from final-status decisions. Constrained by the security-necessity frame which treats full sovereignty as a security threat.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_polity, payer,
    moderate, biographical, constrained, national).

% Monitors through UN bodies, ICJ opinions, and human rights reports. Issues resolutions and occasional sanctions but does not directly enforce territorial withdrawal. Can alter legitimacy conditions and economic incentives but lacks coercive capacity to override the occupying power's security framing.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_community, observer,
    institutional, civilizational, analytical, global).

% Hold diplomatic and normalization interests that are increasingly decoupled from Palestinian territorial claims. Structurally excluded from final-status sovereignty negotiations by the bilateral Oslo framework and by the security-necessity reading's focus on direct Israeli security assessments rather than regional guarantees.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, regional_arab_states, excluded,
    institutional, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__security_necessity_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy__security_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective security of a state perceived as existentially threatened by maintaining territorial buffers that complicate conventional military attack, centralizing threat assessment and territorial administration under a single security architecture.
% TRANSFER_FUNCTION: Transfers territorial control, water resources, and planning authority from the Palestinian population and polity to Israeli military-civilian administration, and moves civilian populations into settlements framed as security assets, under the justification that the territory functions as a necessary defensive buffer.
% ABSENT_VOICES: Palestinian refugees displaced in 1948 and 1967 are structurally absent from territorial negotiations; their claims to return or restitution are excluded by the security-necessity frame which treats the territory as a military asset rather than a populated homeland. Israeli anti-occupation dissenters who reject the security frame are marginalized domestically. Regional Arab states with normalization interests are present diplomatically but excluded from sovereignty adjudication.
% DISAPPEARANCE_RATIONALE: If the security-necessity territorial claim vanished overnight, Israeli military withdrawal from the West Bank and Golan would become legally and politically imperative, Palestinian statehood or full sovereignty would become actionable, settlement infrastructure would lose its legal justification, and the regional security architecture would reorganize around recognized borders rather than occupied buffer zones.
% FOUNDING_PROBLEM: The 1967 war and preceding Arab-Israeli hostilities created a perceived existential vulnerability in Israel's narrow pre-1967 borders, with enemy armies able to mass at short distances from major population centers; the arrangement was built to secure territorial depth against conventional military invasion and non-state attack.
% FOUNDING_PROBLEM_CORROBORATION: Israeli military historians and strategists outside the direct benefiting parties (e.g., retired generals in Israeli security think tanks) attest that the conventional military threat that justified the 1967 territorial expansion has substantially shifted with peace treaties, changing warfare technology, and normalization agreements. Palestinian scholars and international legal bodies attest the founding threat is now a cover for demographic and territorial expansion. No neutral party uncontestedly corroborates the live status of the founding problem in its original form.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint systematically transfers territorial control, planning authority, and civilian settlement rights from the Palestinian population to Israeli administration under a security rationale. Suppression is higher (0.88) because the arrangement depends on military occupation, legal discrimination, and active exclusion of Palestinian sovereignty alternatives. Theater is moderate (0.45): the security function is not purely performativeâgenuine attack threats existâbut a substantial share of territorial and settlement activity is justified post-hoc by security rather than driven by it. Accessibility collapse is high (0.75) because Palestinian statehood and return are structurally blocked by the security frame. Resistance is high (0.78) due to persistent Palestinian opposition and international legal challenge.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (security establishment, settlers, citizenry) experience the constraint as legitimate defensive coordination; the payer seats (Palestinian civilians and polity) experience it as asymmetric extraction and dispossession. The engine computes this divergence from the structural data: identical territory, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli security establishment is the structural beneficiary-agenda_setter (sets rules, low d); the settler population and Israeli citizenry are beneficiaries (collect security and territorial goods, low d). Palestinian civilians and polity are the structural targets (pay with truncated sovereignty and daily occupation costs, high d). The international community sits at analytical distance (neutral d). Regional Arab states are excluded (no d derivation).
 *
 * MANDATROPHY ANALYSIS:
 *   The security-necessity frame was built to solve a genuine 1967 conventional-war vulnerability. Over time, peace treaties, changing military technology, and normalization agreements have eroded the founding threat, while settlement expansion and territorial entrenchment have deepened. The constraint persists because it now serves demographic and political functions beyond its original security mandate, meeting the mandatrophy pattern: a coordination rationale outlived by its extraction function. The high theater ratio and temporal measurements showing rising extractiveness confirm this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_threat_liveness,
    'Is the conventional military threat that justified the 1967 territorial expansion still live, or has it been superseded by peace treaties, changing warfare technology, and normalization?',
    'Comparative strategic analysis of current threat vectors versus 1967-era conventional invasion risk; assessment of whether territorial depth remains militarily decisive.',
    'If the threat is dead, the security-necessity frame is a mandatrophied cover for extraction, supporting reclassification toward snare or piton. If live, the coordination function is genuine and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_threat_liveness, empirical, 'Whether the founding security threat is still operative.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of Palestinian sovereignty structural (military occupation, legal barriers) or internalized (acceptance of conditional sovereignty frameworks by Palestinian leadership)?',
    'Post-Oslo trajectory analysis: if Palestinian Authority assertiveness increases when external constraints relax, suppression is primarily structural; if compliance persists regardless of external openings, internalization is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggestsâthe target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    settlement_security_nexus,
    'Is civilian settlement in the territories genuinely integral to the security architecture, or is the security frame a post-hoc legitimization of demographic expansion?',
    'Independent military-strategic review of settlement placement against actual defense requirements; comparison with alternative low-population security-presence models.',
    'If settlements are decoupled from security needs, the theater_ratio is higher than authored and the coordination story is thinner, strengthening extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_security_nexus, conceptual, 'Whether settlements are security-integral or security-justified.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__security_necessity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(terr_tr_t8, territorial_legitimacy__security_necessity_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(terr_tr_t16, territorial_legitimacy__security_necessity_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(terr_tr_t24, territorial_legitimacy__security_necessity_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(terr_tr_t32, territorial_legitimacy__security_necessity_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy__security_necessity_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(terr_tr_t48, territorial_legitimacy__security_necessity_reading, theater_ratio, 48, 0.48).
narrative_ontology:measurement(terr_tr_t56, territorial_legitimacy__security_necessity_reading, theater_ratio, 56, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__security_necessity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(terr_be_t8, territorial_legitimacy__security_necessity_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(terr_be_t16, territorial_legitimacy__security_necessity_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(terr_be_t24, territorial_legitimacy__security_necessity_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(terr_be_t32, territorial_legitimacy__security_necessity_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy__security_necessity_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(terr_be_t48, territorial_legitimacy__security_necessity_reading, base_extractiveness, 48, 0.78).
narrative_ontology:measurement(terr_be_t56, territorial_legitimacy__security_necessity_reading, base_extractiveness, 56, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__security_necessity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(terr_su_t8, territorial_legitimacy__security_necessity_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(terr_su_t16, territorial_legitimacy__security_necessity_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(terr_su_t24, territorial_legitimacy__security_necessity_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(terr_su_t32, territorial_legitimacy__security_necessity_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy__security_necessity_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(terr_su_t48, territorial_legitimacy__security_necessity_reading, suppression_requirement, 48, 0.85).
narrative_ontology:measurement(terr_su_t56, territorial_legitimacy__security_necessity_reading, suppression_requirement, 56, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
