% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Covenant Territorial Claim (Religious Zionist Reading)
 *   domain: political/nationalism/theological
 *
 * SUMMARY:
 *   This constraint story instantiates the religious_zionist_reading of the
 *   jewish_sovereignty_palestine kernel. The standing arrangement under
 *   contest is the claim that a divine promise to the Jewish people grants an
 *   inalienable, non-negotiable territorial title to Eretz Yisrael, with
 *   modern Israeli statehood understood as theological fulfillment rather
 *   than merely political self-determination. From this reading, Palestinian
 *   territorial claims are structurally subordinate or absent: the land is
 *   held in trust by divine grant and cannot be partitioned, shared, or
 *   renounced. The constraint coordinates Jewish collective identity around a
 *   sovereign territorial anchor while extracting sovereignty and restitution
 *   from Palestinian inhabitants and refugees. It is claimed as covenantal
 *   identity coordination but operates through active enforcement and
 *   suppression of alternatives.
 *
 * KEY AGENTS:
 *   - religious_zionist_institutions: Primary agenda_setter (institutional/constrained) â administer the divine-title framework through state and settlement institutions
 *   - jewish_covenanted_people: Primary beneficiary (organized/identity_locked) â receive territorial fulfillment as covenantal entitlement
 *   - palestinian_inhabitants: Primary target (powerless/trapped) â bear sovereignty denial and territorial dispossession
 *   - palestinian_refugees: Secondary target (powerless/trapped) â bear exclusion from return and restitution
 *   - international_community: Analytical observer (institutional/analytical) â cannot adjudicate the theological title claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.93).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.9).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.93).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Covenant Territorial Claim (Religious Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political/nationalism/theological").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, 'aec2a3bd-d8fc-4a24-8563-caa414106711').
narrative_ontology:cs_kernel_codification('aec2a3bd-d8fc-4a24-8563-caa414106711', fixed_text).
narrative_ontology:cs_authority_grounding('aec2a3bd-d8fc-4a24-8563-caa414106711', lineage).
narrative_ontology:cs_interpretation_layer_present('aec2a3bd-d8fc-4a24-8563-caa414106711').
narrative_ontology:cs_reading_relation('aec2a3bd-d8fc-4a24-8563-caa414106711', jewish_sovereignty_palestine__cultural_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('aec2a3bd-d8fc-4a24-8563-caa414106711', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('aec2a3bd-d8fc-4a24-8563-caa414106711', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('aec2a3bd-d8fc-4a24-8563-caa414106711', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('aec2a3bd-d8fc-4a24-8563-caa414106711', foundational, divine_grant_everlasting_covenant).
narrative_ontology:cs_axiom_status(divine_grant_everlasting_covenant, holdable).
narrative_ontology:cs_axiom_grounding('aec2a3bd-d8fc-4a24-8563-caa414106711', divine_grant_everlasting_covenant, theological).
narrative_ontology:cs_axiom('aec2a3bd-d8fc-4a24-8563-caa414106711', foundational, statehood_as_theological_fulfillment).
narrative_ontology:cs_axiom_status(statehood_as_theological_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('aec2a3bd-d8fc-4a24-8563-caa414106711', statehood_as_theological_fulfillment, theological).
narrative_ontology:cs_reference_frame('aec2a3bd-d8fc-4a24-8563-caa414106711', divine_grant_everlasting_covenant).
narrative_ontology:cs_drift_state('aec2a3bd-d8fc-4a24-8563-caa414106711', contemporary_secular_statehood_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aec2a3bd-d8fc-4a24-8563-caa414106711', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenanted_people).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_inhabitants).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugees).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, biblical_land_title_doctrine).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_as_redemption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the divine-title framework through state institutions, religious courts, settlement planning, and education systems. Derives authority from continuity with biblical promise and rabbinic interpretation. Cannot negotiate territorial compromise without violating the covenantal foundation of its legitimacy.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Receives territorial fulfillment of the divine promise as a constitutive element of collective identity. The covenant relationship to Eretz Yisrael is non-negotiable within the religious framework; exit would require apostasy or communal abandonment, which is structurally unavailable.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenanted_people, beneficiary,
    organized, generational, identity_locked, global).

% Bears the cost of sovereignty denial, military administration, territorial fragmentation, and discriminatory legal regimes. Their presence is treated as temporary or tolerated but never legitimate; partition or equal citizenship is blocked by the inalienable divine-title claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_inhabitants, payer,
    powerless, biographical, trapped, local).

% Bears the cost of exclusion from return and restitution. Their territorial claim is structurally erased because the land is held to be divinely entailed to the Jewish covenant community, rendering refugee return theologically and politically impossible.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Observes a claim it cannot adjudicate theologically. Secular international law treats territory as negotiable political space, which is subordinated here to a divine-title framework that sits outside conventional arbitration.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenanted_people).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__religious_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Jewish people as a covenant community around territorial sovereignty in Eretz Yisrael, solving diasporic dispersion and collective purpose by anchoring religious identity to a specific divinely promised land.
% TRANSFER_FUNCTION: Moves territorial sovereignty, demographic control, and exclusive land title from Palestinian inhabitants and refugees to the Jewish covenanted people, under a theological title that cannot be sold, partitioned, or shared.
% ABSENT_VOICES: Palestinian refugees and their descendants are structurally excluded from the legitimacy conversation; secular international-law frameworks that treat territory as negotiable political space are subordinated to divine title.
% DISAPPEARANCE_RATIONALE: If the divine-title constraint vanished, the territorial claim would lose its non-negotiable status, partition and land-sharing would become politically viable, and the sovereign framework would shift from theological entitlements to civic-territorial negotiation.
% FOUNDING_PROBLEM: Jewish statelessness and diasporic vulnerability in the absence of a sovereign homeland; the perceived need for a divinely sanctioned refuge and collective self-realization after centuries of dispersion and persecution.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist theologians and historians attest the founding problem as live, arguing full redemption is incomplete. Palestinian historians, post-Zionist Israeli scholars, and international human-rights monitors attest the founding problem is superseded and the arrangement now functions as territorial extraction and permanent subordination; these sources sit outside the beneficiary set.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.93, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.93 because the constraint transfers territorial sovereignty and demographic control nearly completely from Palestinians to the covenanted community, with no mechanism for compensation or power-sharing. Suppression is 0.90 because the arrangement's persistence requires actively suppressing Palestinian statehood, refugee return, and equal territorial claims. Theater ratio at 0.60 reflects that while the theological commitment is genuinely held, an increasing share of state activity performs sovereignty without fully formalizing it, masking the gap between divine-title ambition and actual control. Accessibility collapse at 0.92 because within this framework partition or shared sovereignty is delegitimized as covenantal betrayal; resistance at 0.85 due to persistent Palestinian and international opposition. The reading's internal framework treats Palestinian claims as illegitimate or temporary; the analytical observer restores them as the structurally identified payer class.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (religious Zionist institutions, Jewish covenanted people) experience the constraint as covenantal fulfillment and necessary identity coordination. The payer seats (Palestinian inhabitants, refugees) experience the same arrangement as sovereignty denial and permanent exclusion. The engine computes this divergence from the structural data: same constraint, opposite directionalities, producing divergent per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish covenanted people are declared beneficiaries (low directionality: the constraint subsidizes their collective identity and territorial control). Palestinian inhabitants and refugees are declared victims (high directionality: the constraint extracts sovereignty, land, and return rights from them). Religious Zionist institutions are agenda-setters with constrained exit because their authority is fused to the covenant framework; deviating from the divine title would delegitimize their role. The international community sits at analytical scope with no directional stake in the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both beneficiary and victim declarations for Tangled Rope. A purely nationalist reading might class the arrangement as Rope (coordination for Jewish self-determination). A purely anti-colonial reading might class it as Snare (pure extraction). The Tangled Rope classification captures that religious Zionism provides genuine identity coordination for the in-group while simultaneously functioning as asymmetric extraction from the out-group, with active enforcement binding both functions together. Mandatrophy would appear if the coordination function atrophied (e.g., if Jewish identity no longer needed territorial sovereignty) but the territorial claim persisted by inertia; current measurements show theater_ratio rising but not dominant, suggesting coordination remains live while extraction intensifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_title_vs_political_construction,
    'Is the divine promise a genuine theological commitment or a political-ideological construction leveraging scripture for territorial expansion?',
    'Historical-sociological analysis of the religious Zionist movement''s development; theological exegesis comparing mainstream Jewish legal traditions that accepted partition versus maximalist readings.',
    'If constructed, the constraint is a false summit naturalizing a human choice; if genuine theological commitment, it functions as identity coordination with high extraction but distinct authority grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_title_vs_political_construction, conceptual, 'Whether the divine title is authentic covenant theology or ideological cover.').

omega_variable(
    palestinian_subordination_mechanism,
    'Does the constraint structurally require Palestinian exclusion, or does it merely tolerate it as a side effect of Jewish self-determination?',
    'Analysis of religious Zionist legal rulings on non-Jewish residency; whether the framework affords structural pathways to equal citizenship or permanent subordination.',
    'If structural exclusion is required, extractiveness is higher and the constraint moves toward snare; if incidental, extraction is lower and the coordination function more separable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_subordination_mechanism, conceptual, 'Whether Palestinian exclusion is constitutive or incidental to the constraint.').

omega_variable(
    enforcement_secularization_drift,
    'As Israeli state institutions secularize, does enforcement of the divine-title claim rely increasingly on state power or religious communal authority?',
    'Comparative analysis of state land policy versus rabbinical land rulings over the measurement interval.',
    'If state power supplants theological authority, the constraint drifts toward secular nationalism; if religious authority maintains control, the covenant structure persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_secularization_drift, empirical, 'Secularization of enforcement machinery over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsp_rz_tr_t0, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jsp_rz_tr_t20, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(jsp_rz_tr_t40, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(jsp_rz_tr_t60, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(jsp_rz_tr_t80, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 80, 0.55).
narrative_ontology:measurement(jsp_rz_tr_t100, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(jsp_rz_be_t0, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(jsp_rz_be_t20, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(jsp_rz_be_t40, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(jsp_rz_be_t60, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(jsp_rz_be_t80, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 80, 0.9).
narrative_ontology:measurement(jsp_rz_be_t100, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 100, 0.93).

% Suppression requirement over time
narrative_ontology:measurement(jsp_rz_su_t0, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jsp_rz_su_t20, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(jsp_rz_su_t40, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(jsp_rz_su_t60, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(jsp_rz_su_t80, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 80, 0.88).
narrative_ontology:measurement(jsp_rz_su_t100, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, post_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_sovereignty_palestine kernel. It decomposes the colloquial label 'Zionism' into structurally distinct commitments: religious Zionist (divine title), liberal nationalist (self-determination right), cultural Zionist (spiritual center), post-Zionist (civic transcendence), and settler colonial (displacement regime). Each has distinct epsilon, beneficiary/victim structures, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
