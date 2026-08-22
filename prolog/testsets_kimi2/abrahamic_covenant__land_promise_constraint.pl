% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant Land Promise as Territorial Legitimacy Constraint
 *   domain: religious_studies/political_geography
 *
 * SUMMARY:
 *   This constraint instantiates the land-promise reading of the
 *   abrahamic_covenant kernel. It holds that Genesis contains a divine
 *   territorial grant of Canaan to Abraham's descendants, a claim whose
 *   statusâconditional, fulfilled, or ongoingâremains contested within
 *   and across Jewish, Christian, and Islamic interpretive traditions. In its
 *   modern institutional form, the reading is leveraged by state and
 *   non-state actors to sacralize territorial control, settlement expansion,
 *   and the displacement of Palestinian populations. The constraint is
 *   structurally extractive: it converts theological text into geopolitical
 *   enclosure, actively suppressing alternative land-sharing frameworks by
 *   rendering them theologically illegitimate. Sibling readings
 *   (isaac_covenant_reading, ishmael_covenant_reading) handle filial
 *   transmission; this reading isolates the territorial-deployment claim and
 *   its material violence.
 *
 * KEY AGENTS:
 *   - israeli_state_territorial_project: Agenda-setter (institutional/regional) â enforces the territorial claim through state law, military occupation, and settlement administration.
 *   - religious_zionist_settler_bloc: Beneficiary (organized/regional) â receives land, subsidies, and sacralized legitimacy for settlement; exit is identity-locked to the covenant narrative.
 *   - displaced_palestinian_communities: Primary target (powerless/regional) â bear refugee status, property loss, and exclusion from return; exit is trapped.
 *   - occupied_palestinian_population: Primary target (powerless/regional) â live under military rule and land confiscation; exit constrained by closure regime.
 *   - international_human_rights_institutions: Observer (institutional/global) â document violations but lack enforcement leverage.
 *   - ishmael_lineage_communities: Excluded (organized/regional) â represent the Islamic covenant tradition structurally absent from this reading's legitimacy framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.82).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.78).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.82).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant Land Promise as Territorial Legitimacy Constraint").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious_studies/political_geography").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, '761989f2-a791-49ae-a013-31972e072d2e').
narrative_ontology:cs_kernel_codification('761989f2-a791-49ae-a013-31972e072d2e', fixed_text).
narrative_ontology:cs_authority_grounding('761989f2-a791-49ae-a013-31972e072d2e', lineage).
narrative_ontology:cs_interpretation_layer_present('761989f2-a791-49ae-a013-31972e072d2e').
narrative_ontology:cs_reading_relation('761989f2-a791-49ae-a013-31972e072d2e', abrahamic_covenant__isaac_covenant_reading, influences).
narrative_ontology:cs_reading_relation('761989f2-a791-49ae-a013-31972e072d2e', abrahamic_covenant__ishmael_covenant_reading, influences).
narrative_ontology:cs_axiom('761989f2-a791-49ae-a013-31972e072d2e', foundational, territorial_grant_irrevocable).
narrative_ontology:cs_axiom_status(territorial_grant_irrevocable, holdable).
narrative_ontology:cs_axiom_grounding('761989f2-a791-49ae-a013-31972e072d2e', territorial_grant_irrevocable, theological).
narrative_ontology:cs_axiom('761989f2-a791-49ae-a013-31972e072d2e', foundational, divine_title_supersedes_native_occupancy).
narrative_ontology:cs_axiom_status(divine_title_supersedes_native_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('761989f2-a791-49ae-a013-31972e072d2e', divine_title_supersedes_native_occupancy, theological).
narrative_ontology:cs_reference_frame('761989f2-a791-49ae-a013-31972e072d2e', sacred_territorial_grant_to_abrahams_descendants).
narrative_ontology:cs_drift_state('761989f2-a791-49ae-a013-31972e072d2e', contemporary_nation_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('761989f2-a791-49ae-a013-31972e072d2e', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, israeli_state_territorial_project).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, religious_zionist_settler_bloc).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, displaced_palestinian_communities).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, occupied_palestinian_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces territorial policy through military occupation, settlement administration, and legal architecture that translates biblical geography into state land regimes. Controls the coercive apparatus that displaces incumbent populations and allocates confiscated land to settler use.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, israeli_state_territorial_project, agenda_setter,
    institutional, generational, constrained, regional).

% Receives state-subsidized housing, infrastructure, and sacralized legitimacy for settlement on land confiscated under covenant theology. Their collective identity is fused with the theological claim that settlement is fulfillment of prophecy; exit from the settlement project is experienced as apostasy.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_zionist_settler_bloc, beneficiary,
    organized, generational, identity_locked, regional).

% Bear the cost of the constraint as refugee populations exiled from their homes and barred from return by laws and theological-political maps that reclassify their presence as illegitimate. Exit is blocked by absence of citizenship, military cordons, and denial of re-entry rights.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, displaced_palestinian_communities, payer,
    powerless, generational, trapped, regional).

% Live under military rule, movement permits, and land-confiscation orders that reserve territory for settler expansion. Daily life is structured by a permit regime and separation infrastructure that enforces the covenant-based territorial map.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, occupied_palestinian_population, payer,
    powerless, immediate, constrained, regional).

% Document displacement, occupation, and settlement illegality under international law. They issue reports and legal opinions but lack enforcement leverage to alter the territorial constraint.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_human_rights_institutions, observer,
    institutional, generational, analytical, global).

% Represent the Islamic Abrahamic tradition that reads covenant continuity through Ishmael. Their hermeneutic framework is structurally excluded from the theological-political discourse that adjudicates land title in this reading, rendering their objections illegitimate a priori.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, ishmael_lineage_communities, excluded,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__land_promise_constraint, israeli_state_territorial_project).
narrative_ontology:fixing_cost_class(abrahamic_covenant__land_promise_constraint, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Purports to coordinate the return of a dispersed people to a promised homeland by providing a sacred territorial anchor; structurally, it does not coordinate a multi-party collective-action problem but authorizes a unilateral demographic and territorial claim against incumbent populations.
% TRANSFER_FUNCTION: Moves land, housing, water rights, and political sovereignty from Palestinian incumbent and refugee populations to the Israeli state and its settler clientele, mediated by biblical territorial mapping.
% ABSENT_VOICES: Palestinian refugees and their descendants, Palestinian Christian and Muslim communities, and the Ishmaelite/Islamic covenant tradition are structurally excluded from the theological-political framework that adjudicates legitimacy; their counter-claims are ruled illegitimate a priori by the covenant reading.
% DISAPPEARANCE_RATIONALE: If the land-promise constraint vanished overnight as a legitimacy structure, the territorial claims it underwrites would lose their primary theological anchor, settlement expansion would shed its sacred justification, and the political configuration of the Israeli-Palestinian conflict would likely shift toward secular-legal, binational, or internationally brokered land-sharing frameworks. The arrangement depends on this constraint for its sacralized endurance.
% FOUNDING_PROBLEM: Jewish communal statelessness, diaspora, and vulnerability in the wake of Roman dispersion and twentieth-century European genocide; the absence of a territorial base for collective self-determination and physical security.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and Israeli state institutions assert the problem remains live due to ongoing security threats and diaspora assimilation. Palestinian historians, anti-Zionist Jewish scholars, and international legal experts attest that the founding problem was substantially resolved by state formation in 1948 and that the constraint now perpetuates territorial expansion rather than solving statelessness; corroboration from outside the benefiting party set exists but is politically suppressed.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint transfers land, sovereignty, and demographic control from an incumbent population to a settler-colonial project under sacred cover. Suppression (0.78) reflects active enforcement: military occupation, legal discrimination, denial of refugee return, and the disqualification of counter-claims as heretical or anti-Semitic. Theater ratio (0.45) captures the performative dimensionâarchaeological claims, biblical tourism, and state ritual that maintain a theological veneer over a secular-nationalist territorial project. Accessibility collapse (0.65) is substantial: once the covenant frame is accepted, secular or binational alternatives appear theologically void, though they persist in international law discourse. Resistance (0.75) is high and growing, ranging from grassroots Palestinian protest to international legal challenges. Temporal measurements show extraction and suppression ratcheting upward as the state project hardens from early settlement to full-spectrum occupation.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli state and settler bloc experience this constraint as restorative justice and sacred homeland recovery; the engine will compute their seat near the beneficiary pole. Displaced and occupied Palestinian populations experience the identical arrangement as dispossession and military domination; their seats compute near full target. The divergence is not perspectival illusion but structural asymmetry in power, exit options, and directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The state territorial project and religious nationalist movements are structural beneficiaries: the constraint subsidizes their land acquisition and sovereignty claims with divine authority, yielding low directionality. Palestinian communities are structural targets: the constraint extracts their land, political status, and right of return, while trapping them in refugee camps or occupied enclaves with no viable exit, yielding high directionality toward 1.0. International observers sit at analytical exit and moderate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâJewish statelessness and diasporaâwas substantially addressed by Israeli statehood in 1948 and its subsequent military capacity for self-defense. Yet the land-promise constraint persists and has expanded its territorial demands beyond the 1948 armistice lines. This is a classic unresolved mandatrophy: the arrangement's justification (safe haven) has been achieved, but the constraint continues to extract land in the name of an ever-fulfilling promise. The R5 genealogy mismatch (founding_problem_status contested + disappearance_verdict world_rearranges) flags it as a zombie snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_conditionality_ambiguity,
    'Is the land promise conditional on ethical obedience (as prophetic and rabbinic traditions often hold) or unconditional and irrevocable (as religious nationalist theology claims)?',
    'Historical-critical and theological analysis of covenantal conditionality across Genesis, Deuteronomy, and prophetic literature; comparison with Second Temple and rabbinic interpretive traditions.',
    'If the promise is conditional, the constraint''s current extraction is theologically unauthorized and the snare structure destabilizes; if unconditional, the extraction is internally coherent with the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_conditionality_ambiguity, conceptual, 'Ambiguity about whether the territorial promise is conditional or unconditional.').

omega_variable(
    filial_lineage_dependency,
    'Does the land-promise constraint logically depend on the Isaac-exclusive reading, or can it stand independently of any filial lineage claim?',
    'Comparative theology tracing how land promise and filial transmission are coupled or decoupled in Jewish, Christian, and Islamic interpretive histories.',
    'If dependent, the constraint''s legitimacy is tied to the Isaac-Ishmael contest and may be reclassified as a family-internal rope or tangled rope; if independent, it is a standalone snare with its own victim structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(filial_lineage_dependency, conceptual, 'Whether the land promise reading is separable from filial exclusivity.').

omega_variable(
    secular_nationalist_drift,
    'Has the theological land-promise reading been functionally superseded by secular nationalist territorial logic, leaving only a theatrical residue?',
    'Sociological and discourse analysis of Knesset debates, state education curricula, and settlement movement rhetoric to measure the ratio of theological to security/economic justification.',
    'If superseded, the constraint is a piton (theater_ratio should rise toward 0.8+ and claimed_type would be piton); if theological justification remains operationally necessary for recruitment and legitimation, it remains a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_nationalist_drift, empirical, 'Whether the constraint is genuinely theological or a secular project with sacred veneer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__land_promise_constraint, theater_ratio, 0, 0.2).
narrative_ontology:measurement(abra_tr_t10, abrahamic_covenant__land_promise_constraint, theater_ratio, 10, 0.25).
narrative_ontology:measurement(abra_tr_t20, abrahamic_covenant__land_promise_constraint, theater_ratio, 20, 0.32).
narrative_ontology:measurement(abra_tr_t30, abrahamic_covenant__land_promise_constraint, theater_ratio, 30, 0.38).
narrative_ontology:measurement(abra_tr_t40, abrahamic_covenant__land_promise_constraint, theater_ratio, 40, 0.42).
narrative_ontology:measurement(abra_tr_t50, abrahamic_covenant__land_promise_constraint, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__land_promise_constraint, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(abra_be_t10, abrahamic_covenant__land_promise_constraint, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(abra_be_t20, abrahamic_covenant__land_promise_constraint, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(abra_be_t30, abrahamic_covenant__land_promise_constraint, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(abra_be_t40, abrahamic_covenant__land_promise_constraint, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(abra_be_t50, abrahamic_covenant__land_promise_constraint, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__land_promise_constraint, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(abra_su_t10, abrahamic_covenant__land_promise_constraint, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(abra_su_t20, abrahamic_covenant__land_promise_constraint, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(abra_su_t30, abrahamic_covenant__land_promise_constraint, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(abra_su_t40, abrahamic_covenant__land_promise_constraint, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(abra_su_t50, abrahamic_covenant__land_promise_constraint, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).

% DUAL FORMULATION NOTE:
% The abrahamic_covenant kernel decomposes into at least three readings: filial transmission (isaac, ishmael) and territorial promise (land_promise). Each reading carries a distinct epsilon, beneficiary structure, and classification. This reading isolates the territorial-deployment claim and its extraction surface.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
