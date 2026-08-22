% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Territorial Legitimacy via Security Necessity and Defensive Control (1967 Borders Plus Strategic Depth)
 *   domain: political/legal/territorial
 *
 * SUMMARY:
 *   This constraint describes one reading of the contested territorial
 *   legitimacy kernel: the view that Israeli control of the West Bank, Gaza,
 *   and Golan Heights is legitimate as a security-necessity measure—that 1948
 *   borders were militarily indefensible, that the 1967 war created a new
 *   security environment requiring strategic depth, and that Palestinian
 *   sovereignty must be conditional on demilitarization and acceptance of
 *   Israeli security arrangements. This reading is institutionalized in
 *   Israeli military doctrine, settlement policy, and diplomatic claims. The
 *   constraint is CLAIMED as tangled_rope (real coordination
 *   function—security provision—plus asymmetric extraction) while the
 *   authored metrics describe substantially extractive, actively suppressive
 *   operation with growing theater (performance masking the erosion of the
 *   original security rationale). The claim/metric gap is deliberate: the
 *   engine measures whether the coordination narrative survives the
 *   structural data.
 *
 * KEY AGENTS:
 *   - Israeli security establishment (institutional): defines and enforces the control regime; claims security necessity
 *   - Israeli state apparatus (institutional): administers territories, expands settlements, collects rents
 *   - Palestinian population West Bank (powerless): subject to military law, checkpoints, land loss; trapped exit
 *   - Palestinian population Gaza (powerless): blockade-controlled; trapped exit
 *   - Palestinian diaspora (moderate): excluded from return; stateless limbo
 *   - Israeli settler population (moderate): identity-locked beneficiaries; settlements as security infrastructure
 *   - Neighboring Arab states (institutional): militarily excluded; would argue for Palestinian self-determination
 *   - International legal community (institutional, observer): divided on proportionality and necessity
 *   - US security guarantor (institutional): benefits from partnership; provides diplomatic and military support
 *   - Palestinian National Authority (moderate, observer/payer): nominally self-governing; constrained by occupation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.82).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.79).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity and Defensive Control (1967 Borders Plus Strategic Depth)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political/legal/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, 'c5250b33-adcc-4469-bb3d-672767df8fb1').
narrative_ontology:cs_kernel_codification('c5250b33-adcc-4469-bb3d-672767df8fb1', fixed_text).
narrative_ontology:cs_authority_grounding('c5250b33-adcc-4469-bb3d-672767df8fb1', extraction).
narrative_ontology:cs_interpretation_layer_present('c5250b33-adcc-4469-bb3d-672767df8fb1').
narrative_ontology:cs_reading_relation('c5250b33-adcc-4469-bb3d-672767df8fb1', territorial_legitimacy__partition_reading, influences).
narrative_ontology:cs_reading_relation('c5250b33-adcc-4469-bb3d-672767df8fb1', territorial_legitimacy__indigenous_continuity_reading, forecloses).
narrative_ontology:cs_axiom('c5250b33-adcc-4469-bb3d-672767df8fb1', foundational, strategic_depth_necessity).
narrative_ontology:cs_axiom_status(strategic_depth_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c5250b33-adcc-4469-bb3d-672767df8fb1', strategic_depth_necessity, empirically_contingent).
narrative_ontology:cs_axiom('c5250b33-adcc-4469-bb3d-672767df8fb1', foundational, palestinian_sovereignty_demilitarization_conditionality).
narrative_ontology:cs_axiom_status(palestinian_sovereignty_demilitarization_conditionality, holdable).
narrative_ontology:cs_axiom_grounding('c5250b33-adcc-4469-bb3d-672767df8fb1', palestinian_sovereignty_demilitarization_conditionality, instrumental).
narrative_ontology:cs_reference_frame('c5250b33-adcc-4469-bb3d-672767df8fb1', post_1967_indefensible_borders_paradigm).
narrative_ontology:cs_drift_state('c5250b33-adcc-4469-bb3d-672767df8fb1', contemporary_settlement_infrastructure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c5250b33-adcc-4469-bb3d-672767df8fb1', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population_west_bank).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population_gaza).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_diaspora).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_settler_population).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, us_security_guarantor).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_national_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the territorial control regime, justifying it as necessary for state security against hostile neighbors and non-state actors. Controls checkpoints, settlement policy, resource allocation, and military deployment. Argues that 1948-1967 borders were indefensible and that strategic depth (control of the Jordan Valley, Golan Heights, high ground) is existentially necessary. Frames settlements as security infrastructure and civilian presence as deterrence.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_security_establishment, agenda_setter,
    institutional, generational, trapped, regional).

% Consolidates territorial control through administration, resource distribution, and international diplomatic legitimacy claims grounded in security necessity. Collects tax and resource rents from occupied territories, expands settlement infrastructure, and maintains legal structures that privilege Israeli presence. Claims the occupation is defensive and temporary (pending secure final-status agreement); maintains it indefinitely.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_state_apparatus, beneficiary,
    institutional, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, israeli_state_apparatus, agenda_setter).

% Subject to military law, checkpoints, movement restrictions, and settlement expansion. Cannot vote in the political process that governs them. Bear costs through land loss, resource scarcity, economic dependency, administrative control of permits and travel, and military enforcement. Exit is territorially and legally blocked; exit to Gaza or diaspora is not a genuine option for most.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_population_west_bank, payer,
    powerless, biographical, trapped, local).

% Experience control via blockade, military operations, and external administration of key utilities. Trapped in a small territory with no exit; survival depends on goods allowed through controlled corridors. Subject to periodic military enforcement when security threats are declared.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_population_gaza, payer,
    powerless, biographical, trapped, local).

% Excluded from return to territories controlled under the security-necessity regime, justified by security and settlement demographics. Hold residual legal and historical claims but no mechanism to enforce them. Bear the cost of statelessness and legal limbo across refugee camps and neighboring states; many lack citizenship.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_diaspora, payer,
    moderate, generational, constrained, global).

% Benefit from subsidized housing, security provision, and territorial access justified as security settlements. Their presence is presented as deterrence and control infrastructure. Many have fused their identity with the territorial claim and view exit as abandonment of historical right; ideological commitment to the settlement project makes exit psychologically identity-shattering.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_settler_population, beneficiary,
    moderate, biographical, identity_locked, regional).

% Militarily excluded from the territories by Israeli control; their voices are entirely outside the internal governance of the constraint. Would argue for Palestinian self-determination and territorial integrity but are kept out by the military enforcement structure. Egypt and Jordan have treaty relationships that constrain their options; others lack direct leverage.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, neighboring_arab_states, excluded,
    institutional, generational, constrained, regional).

% Examines the constraint under international humanitarian law and human rights law. UN bodies, human rights organizations, and legal scholars assess whether the security-necessity justification meets proportionality and necessity tests. Consensus is deeply divided; enforcement mechanisms (ICJ, ICC) are contested and asymmetrically deployed.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_legal_community, observer,
    institutional, generational, analytical, global).

% Benefits from regional military partnership, intelligence sharing, and a key strategic ally. Provides military aid, diplomatic cover, and security coordination that sustains the occupation regime. Has sufficient exit options (could withdraw support) but chooses to maintain the relationship, treating regional stability and Israeli security as aligned with its interests.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, us_security_guarantor, beneficiary,
    institutional, generational, mobile, global).

% Nominally self-governing but with extremely limited authority (Area A only, ~3% of West Bank). Depends on Israeli permission for revenue collection, security operations, and territorial expansion. Faces impossible choice: accept security-necessity framing and negotiate reduced sovereignty, or reject it and lose whatever institutional capacity exists. Treated as both observer (analyzes the constraint) and payer (bears the costs of its constraints).
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_national_authority, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, palestinian_national_authority, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__security_necessity_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_legitimacy__security_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes territorial control that the reading frames as necessary for state security: control of high ground, water resources, and strategic depth provides defense against hostile states and non-state actors. Claims to solve the problem of indefensible 1948 borders and the security threats that emerged from 1967 and after. Provides military, administrative, and settlement coordination under a unified security framework.
% TRANSFER_FUNCTION: Moves territorial control, resource access (water, arable land, minerals), settlement rights, and administrative authority from Palestinian populations to Israeli state and security apparatus. Transfers security costs to Palestinian subjects (surveillance, checkpoints, movement restrictions, military law) while Palestinian security interests remain subordinated to Israeli strategic interests. Transfers legitimacy narratives claiming occupation as defensive rather than expansionist.
% ABSENT_VOICES: Palestinian voices are structurally excluded from the governance of the territories they inhabit (no voting in Israeli military administration, limited voice in Palestinian Authority under occupation). Neighboring Arab states are militarily excluded. International law critics exist but lack enforcement mechanisms. Voices that would argue for Palestinian territorial sovereignty and self-determination are kept outside the decision-making apparatus that justifies the constraint.
% DISAPPEARANCE_RATIONALE: If this constraint and the security-necessity justification it rides on were abandoned overnight, territorial control would shift to Palestinian-majority governance (under current demographic and settlement patterns), settlement status would be legally contested, Israeli strategic doctrine would require fundamental revision, and the regional balance of power would reorganize. The constraint sustains a specific allocation of control; its removal triggers political reorganization.
% FOUNDING_PROBLEM: Israel faced military threats from neighboring states after 1948; the 1967 war and subsequent security incidents created the reading that pre-1967 borders were strategically indefensible and that control of the West Bank and Golan Heights was necessary for state survival. The founding problem is framed as existential: how to defend a small state surrounded by hostile neighbors when borders are militarily vulnerable.
% FOUNDING_PROBLEM_CORROBORATION: Israeli security scholars and military analysts attest the founding problem is still live, citing persistent security threats and rocket attacks. Palestinian scholars and international human rights organizations attest the problem has been substantially transformed: modern military threats do not require civilian territorial occupation; settlements compound rather than reduce security risk; the constraint persists for territorial expansion and resource control, not defense. Empirical assessments of threat severity diverge sharply across the reading divide.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high and rising (0.55→0.82 over 57 years) because: (1) control and settlement expansion have progressively appropriated Palestinian land, water, and resource access far beyond what minimal military defense requires; (2) the cost transferred to Palestinian subjects (administrative control, checkpoint regimes, movement restrictions, dispossession) has grown as settlements have expanded; (3) the gap between stated security justification and actual territorial extent has widened. Suppression is high (0.79) and required actively: the constraint depends on military enforcement, administrative subordination, and resource denial to keep Palestinian populations from exercising territorial claims. The measured theater ratio (0.41 and rising) reflects the erosion of the original security rationale: when the constraint was imposed (1967), the security case was temporally plausible (weeks after an existential military threat). Decades later, with settlements deep in Palestinian territory, with civilian presence as the primary control mechanism, and with security incidents often triggered by settlement expansion itself, the theater ratio has risen—the performative maintenance of the security narrative has become more prominent relative to genuine security function. The time-series shows extraction accumulation (T17 trigger, mountain_extraction_accumulation): a constraint claimed as natural defense gradually reveals itself as constructed appropriation.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli security establishment and state apparatus experience this as coordination (a legitimate security arrangement they built and maintain for defense). The Palestinian populations experience it as enforced extraction and dispossession. The engine should compute different types from these seats: the beneficiary seat should compute a weaker type (the coordination story they tell), while the victim seats compute a stronger extraction reading. This is not a defect in the constraint's classification—it is exactly the phenomenon tangled_rope is built to model: one arrangement, two incompatible situational readings, one holding power over the other through institutional control.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli security establishment (institutional, trapped exit, generational time horizon) benefits from control and collects the rents; they have substantial power but no way out without risking their security narrative. Directionality is high (d→0.9+) beneficiary ward: they benefit, they set the rules, they enforce them. The Palestinian populations (powerless, trapped exit) bear all the costs—administrative subordination, land loss, resource scarcity, surveillance, military law—with no exit and no voice in the governance. Directionality is high (d→0.9+) target ward. The settler population (moderate power, identity-locked exit) benefits from subsidized access and security provision but has fused identity with the territorial claim; exit is identity-shattering. Directionality for settlers is moderate beneficiary (d~0.2-0.3) because power is lower than institutions, but exit is as severe as trapped once identity is considered. The US security guarantor (institutional, mobile exit) benefits from the partnership and provides support, but could exit without existential cost; directionality is beneficiary-ward but with genuine alternatives (d~0.1-0.2). This stakeholder-level divergence is what the engine computes per-seat; the authored base_properties score reflects the dominant structural asymmetry (high extraction favoring institutional beneficiaries).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential security threat from hostile neighbors with indefensible borders) had live status in 1967. By 2024, the founding problem is contested: Israeli security scholars argue it is still live (persistent rocket threats, terrorism risk); Palestinian scholars and international law critics argue it is dead (territorial occupation does not require civilian settlements; security concerns are now used to justify territorial appropriation; the constraint persists through institutional inertia and settlement investment, not ongoing security necessity). The constraint shows mandatrophy markers: theater ratio rising (0.12→0.41), suggesting performative maintenance has become dominant; extraction rising while the security rationale degrades; the apparatus of enforcement (settlements, checkpoints, military law) becoming more theatrical and less functionally tied to the original security problem. If the founding problem is classified as dead and the constraint persists by institutional inertia, this is a candidate for mandatrophy resolution. However, the status remains contested because both sides claim the security problem is still live. The tangled_rope classification reflects this: there IS a coordination function (security provision), but the asymmetry and extraction have grown far beyond what the coordination justifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_necessity_vs_territorial_expansion,
    'Does the territorial control claimed as security necessity match the minimal geographic footprint required for military defense, or does the extent of control and settlement expansion exceed what military doctrine requires?',
    'Military strategic analysis from independent defense experts comparing the declared security threats against the territorial extent of Israeli control; comparison with defensive security arrangements in other states facing similar threat profiles; analysis of settlement patterns and their military justification.',
    'If control significantly exceeds military necessity, the constraint would reclassify toward snare (pure extraction) from tangled_rope (coordination with asymmetric extraction). If control aligns with military necessity, the tangled_rope reading (genuine security function plus extraction) holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_necessity_vs_territorial_expansion, empirical, 'Whether territorial extent matches security doctrine or exceeds it.').

omega_variable(
    civilian_settlement_as_security_infrastructure,
    'Are Israeli civilian settlements actually security infrastructure, or does claiming them as such provide post-hoc legitimacy cover for territorial appropriation driven by ideological and resource motives?',
    'Analysis of settlement placement relative to military defensive positions; military doctrine texts and strategic planning documents; comparison with security settlements in other occupation regimes; post-settlement security outcome data.',
    'If settlements are primarily security infrastructure, they are part of the coordination-function cost; if they are primarily territorial appropriation legitimized as security, they shift the constraint toward snare. The boundary is empirically contestable but structurally determinative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_settlement_as_security_infrastructure, empirical, 'Whether settlements serve military strategy or ideological/resource territorial goals.').

omega_variable(
    conditional_sovereignty_doctrine_enforceability,
    'The reading frames Palestinian sovereignty as conditional on demilitarization and security arrangements. Can such conditional sovereignty ever be enforced by the conditional party (Israel) fairly, or does the conditionality structure itself guarantee indefinite extension of the occupation?',
    'Examination of historical conditional-sovereignty arrangements (demilitarization conditions that were lifted vs. indefinitely extended); analysis of incentive structure (what would cause Israeli security establishment to voluntarily cede territory); institutional mechanisms that would make the conditions verifiable and enforceable by Palestinians rather than unilaterally by Israel.',
    'If conditions can be genuinely satisfied and lifted, the tangled_rope framing (temporary asymmetric extraction for security coordination) holds. If conditions are structurally irreversible once imposed, the constraint reclassifies toward indefinite snare. This is the central sustainability question for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_sovereignty_doctrine_enforceability, conceptual, 'Whether conditional sovereignty doctrine is genuine constraint or indefinite occupation cover.').

omega_variable(
    reading_foreclosure_via_demographic_facts,
    'Does this reading''s dependence on settlement expansion as security presence foreclose the indigenous_continuity_reading by creating irreversible demographic facts on the ground?',
    'Examination of settlement growth rates and territorial takeover; analysis of whether demographic changes create facts-on-ground that Palestinian self-determination cannot accommodate; investigation of whether the security_necessity_reading actively uses settlement expansion as a tool to foreclose alternatives.',
    'If settlement expansion is a tool to foreclose alternative readings, the reading_relations entry to indigenous_continuity should be forecloses, not coexists_with. If settlements are incidental to the reading''s core claim, the relationship is coexists_with or influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_via_demographic_facts, empirical, 'Whether this reading''s implementation strategy forecloses sibling readings.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.79) primarily structural (military law, checkpoints, resource control) or partially internalized (Palestinian acceptance of subordination as natural or necessary)?',
    'Post-occupation trajectory analysis: if suppression persists after military control is removed, it is partially internalized; if suppression drops sharply, it was structural. Survey data and qualitative testimony about perceived necessity and alternatives.',
    'If suppression is partially internalized, the constraint''s effective suppressive force is higher than the structural measure suggests, and the reading''s claim to be coordination (even asymmetric) is undermined. If suppression is primarily structural, the reading''s tangled_rope framing (real coordination function plus enforcement) is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in occupation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__security_necessity_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(terr_tr_t1980, territorial_legitimacy__security_necessity_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(terr_tr_t1995, territorial_legitimacy__security_necessity_reading, theater_ratio, 1995, 0.27).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__security_necessity_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy__security_necessity_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__security_necessity_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(terr_be_t1980, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1980, 0.62).
narrative_ontology:measurement(terr_be_t1995, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1995, 0.71).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2005, 0.76).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2015, 0.8).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(terr_su_t1980, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1980, 0.64).
narrative_ontology:measurement(terr_su_t1995, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1995, 0.72).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2005, 0.76).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2024, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__security_necessity_reading, 0.18).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, israeli_settlement_expansion_constraint).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, palestinian_resource_scarcity_constraint).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, checkpoint_regime_constraint).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-story kernel family decomposing contested territorial legitimacy. The security_necessity_reading shares the same territorial referent with the partition_reading and indigenous_continuity_reading but grounds legitimacy in different axioms (military necessity vs. international law vs. indigenous rights). Each reading has its own ε and stakeholder structure because the beneficiaries and victims differ across readings. The readings coexist as live institutional and scholarly positions; this story models the security reading's structural logic, not the disputed empirical claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
