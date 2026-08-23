% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems â Balanced Coexistence Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the balanced_coexistence_reading of the
 *   one_country_two_systems_framework kernel. It models the
 *   constitutional-political arrangement governing Hong Kong as a persistent
 *   negotiation between PRC sovereignty and Hong Kong autonomy, in which
 *   neither principle is absolute and contested boundaries are resolved
 *   through political accommodation rather than legal supremacy. The
 *   framework coordinates a genuine dual-system equilibriumâpreserving Hong
 *   Kong's common-law economy and international access under Chinese
 *   sovereigntyâwhile asymmetrically extracting compliance and sovereignty
 *   recognition from Hong Kong civil society, which retains limited but real
 *   bargaining power through protest, judicial review, and international
 *   leverage. Periodic crises (2003 national security legislation attempt,
 *   2014 Umbrella Movement, 2019 protests, 2020 National Security Law)
 *   function as renegotiation triggers that shift the boundary without
 *   collapsing the framework entirely.
 *
 * KEY AGENTS:
 *   - beijing_authority (institutional/arbitrage): Primary beneficiary and ultimate agenda-setter â retains sovereignty override, interprets the framework's outer limits, and absorbs legitimating value from territorial reunification.
 *   - hong_kong_establishment (organized/constrained): Secondary beneficiary â administers local governance under Beijing's shadow, captures stability rents and patronage access.
 *   - hong_kong_business_elite (powerful/mobile): Beneficiary â captures economic continuity and cross-border arbitrage, retains capital-flight exit.
 *   - hong_kong_civil_society (organized/constrained): Primary payer â bears constraints on democratic self-governance and civil liberties, retains limited bargaining power through protest and international voice.
 *   - pro_democracy_movement (moderate/constrained): Payer â directly targeted by sovereignty overrides, excluded from formal boundary negotiation, subject to prosecution or exile.
 *   - international_community (institutional/analytical): Observer â monitors, reports, and sanctions symbolically but lacks enforceable authority over Beijing's sovereignty decisions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.58).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.65).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems â Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, 'c5c6a3c1-e50a-4323-8ef3-84828a95397d').
narrative_ontology:cs_kernel_codification('c5c6a3c1-e50a-4323-8ef3-84828a95397d', formalized).
narrative_ontology:cs_authority_grounding('c5c6a3c1-e50a-4323-8ef3-84828a95397d', lineage).
narrative_ontology:cs_interpretation_layer_present('c5c6a3c1-e50a-4323-8ef3-84828a95397d').
narrative_ontology:cs_reading_relation('c5c6a3c1-e50a-4323-8ef3-84828a95397d', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5c6a3c1-e50a-4323-8ef3-84828a95397d', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('c5c6a3c1-e50a-4323-8ef3-84828a95397d', foundational, neither_sovereignty_nor_autonomy_absolute).
narrative_ontology:cs_axiom_status(neither_sovereignty_nor_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('c5c6a3c1-e50a-4323-8ef3-84828a95397d', neither_sovereignty_nor_autonomy_absolute, conventional).
narrative_ontology:cs_axiom('c5c6a3c1-e50a-4323-8ef3-84828a95397d', foundational, boundary_resolution_through_political_accommodation).
narrative_ontology:cs_axiom_status(boundary_resolution_through_political_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('c5c6a3c1-e50a-4323-8ef3-84828a95397d', boundary_resolution_through_political_accommodation, conventional).
narrative_ontology:cs_reference_frame('c5c6a3c1-e50a-4323-8ef3-84828a95397d', handover_settlement_equilibrium).
narrative_ontology:cs_drift_state('c5c6a3c1-e50a-4323-8ef3-84828a95397d', post_national_security_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5c6a3c1-e50a-4323-8ef3-84828a95397d', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, beijing_authority).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_establishment).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elite).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, pro_democracy_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains final interpretive authority over the Basic Law and the power to legislate for Hong Kong on matters of sovereignty, national security, and territorial integrity. Controls the boundary of what autonomy is permitted and intervenes when local governance is perceived to threaten core interests. Benefits from the legitimating frame of reunification without the full cost of assimilation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, beijing_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Comprises the local executive, pro-establishment political parties, and affiliated civil service elites who administer day-to-day governance under Beijing's oversight. Benefit from political stability, preferential access to mainland economic opportunities, and maintenance of the status quo. Their institutional survival is tied to the framework's persistence.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_establishment, beneficiary,
    organized, generational, constrained, regional).

% Controls cross-border finance, real estate, and trade gateways. Benefits from Hong Kong's common-law commercial system and international access while operating within the PRC economic orbit. Retains significant capital mobility and can relocate operations or family members if the framework deteriorates, making their support conditional rather than locked.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elite, beneficiary,
    powerful, biographical, mobile, regional).

% Includes professional bodies, religious organizations, trade unions, and NGOs that historically exercised autonomous advocacy. Bears the cost of narrowing freedoms as the sovereignty-autonomy boundary shifts. Retains limited bargaining power through protest, judicial review, and international visibility, but faces institutional erosion of those channels.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society, payer,
    organized, biographical, constrained, regional).

% Advocates for expanded self-determination, universal suffrage, and preservation of civil liberties. Directly targeted by sovereignty overrides and national security enforcement. Excluded from formal boundary negotiation; members face prosecution, disqualification, or exile. Emigration is possible but involves severing professional and family networks.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, pro_democracy_movement, payer,
    moderate, biographical, constrained, regional).

% Monitors the framework through diplomatic statements, human rights reports, and targeted sanctions. Provides reputational and symbolic leverage but lacks enforceable authority over Beijing's sovereignty decisions. Does not bear direct costs or collect direct benefits from the arrangement.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__balanced_coexistence_reading, diffuse).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__balanced_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the coexistence of a socialist mainland legal-political system and a capitalist common-law jurisdiction under a single sovereign state, providing a mechanism for territorial reunification without immediate systemic assimilation and preserving economic continuity.
% TRANSFER_FUNCTION: Transfers discretionary local governance authority from Beijing to Hong Kong institutions, while transferring sovereignty recognition and political loyalty from Hong Kong population to Beijing; transfers stability and cross-border economic advantage to the business elite.
% ABSENT_VOICES: Pro-independence advocates and radical localists who reject the 'One Country' premise are structurally excluded from formal negotiation. Some international human rights monitors are present but non-binding, and their recommendations are disregarded in sovereignty-sensitive domains.
% DISAPPEARANCE_RATIONALE: If the framework vanished, Hong Kong would face either full integration into the PRC legal system or a sovereignty rupture. The current functional division, legal pluralism, and bargaining equilibrium would collapse, triggering capital flight, constitutional crisis, and likely mass emigration.
% FOUNDING_PROBLEM: How to reunify Hong Kong with China after British colonial rule without destroying its economic value, triggering mass emigration, or inviting Western sanctions, while preserving Chinese territorial integrity.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and the Hong Kong establishment attest the problem remains live due to national security threats and foreign interference. Pro-democracy movements and independent historians attest the colonial handover problem was resolved in 1997 and the framework now serves to defer full integration. The UK Parliament Foreign Affairs Committee and UN human rights mechanisms provide external corroboration that the original transition has concluded but the arrangement's current function is disputed.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.58 (medium) because the constraint coordinates a genuine and valuable dual-system arrangement while simultaneously extracting sovereignty compliance and suppressing full self-determination. Suppression (0.65) reflects the structural closure of independence and full democracy as alternatives, rising sharply after the 2020 National Security Law. Theater_ratio (0.50 at interval end) captures the growing gap between the performative maintenance of 'two systems' language and the substantive contraction of autonomous practice. Accessibility_collapse (0.70) is high because the understood alternativesâfull integration or independenceâare both blocked by geopolitical and structural barriers. Resistance (0.60) reflects sustained protest, electoral mobilization, and emigration waves that contest the constraint from the payer seats.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Beijing) experiences the framework as generous coordination that preserves Hong Kong's distinctiveness under Chinese sovereignty. The payer seats (civil society, pro-democracy movement) experience the same structure as progressively extractive sovereignty override with shrinking autonomy. The business elite experiences it as a conditional benefit set that remains valuable only so long as exit remains open. The engine computes this divergence from the identical structural data through directionality and scope scaling.
 *
 * DIRECTIONALITY LOGIC:
 *   Beijing_authority is the primary structural beneficiary (low directionality, subsidized by the constraint's legitimating function). Hong_kong_establishment and business_elite are secondary beneficiaries (low-to-moderate directionality). Hong_kong_civil_society and pro_democracy_movement are the structural targets (high directionality): they bear the costs of constrained autonomy, identity-locked or constrained exit, and regional scope that amplifies effective extraction. International_community sits at the analytical extreme with negligible directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcolonial handover without systemic collapseâwas substantially resolved in 1997. The framework persists, but its function is contested: Beijing claims it remains necessary for national security and stability, while opponents argue it now serves primarily to defer full integration and extract political compliance. The founding_problem_status is contested and the disappearance_verdict is world_rearranges, producing a mandatrophy mismatch signal. However, the constraint is not a piton because the agenda-setter actively renegotiates the boundary during crises (theater is performative but not purely inertial), and beneficiaries continue to capture real coordination value from the dual-system arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_status,
    'Does the balanced coexistence reading accurately model the constraint, or does sovereignty-unilateralism drift push the kernel toward the sovereignty_primacy reading?',
    'Longitudinal observation of crisis resolution patterns: if future disputes are resolved through bilateral accommodation, the balanced reading holds; if through unilateral Beijing assertion, the reading drifts toward sovereignty_primacy.',
    'Would reclassify the constraint as more extractive and potentially shift computed type from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_status, conceptual, 'Whether the balanced reading remains structurally accurate as the kernel evolves').

omega_variable(
    sovereignty_autonomy_sustainability,
    'Can a ''neither absolute'' equilibrium persist indefinitely, or does sovereignty structurally absorb autonomy over time?',
    'Comparative historical analysis of autonomy arrangements within the PRC (Tibet, Inner Mongolia, Xinjiang) and tracking of legal override frequency in Hong Kong post-2020.',
    'If sovereignty inherently absorbs autonomy, the balanced reading describes a temporary performance rather than a stable structural equilibrium, implying higher theater_ratio and eventual piton or snare transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_autonomy_sustainability, empirical, 'Whether the sovereignty-autonomy balance is structurally stable or transient').

omega_variable(
    cs_framing_underdetermination,
    'Should the authority grounding be framed as lineage (continuity with Joint Declaration/Basic Law) or practice (ongoing political accommodation without fixed textual supremacy)?',
    'Discourse analysis of Beijing''s public justifications: appeals to fixed text and historical treaty versus appeals to contemporary political necessity and sovereignty.',
    'Lineage framing with a functioning interpretation_layer supports tangled_rope classification; practice framing without fixed kernel suggests distributed authority and a potentially more fragile coordination structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framing between textual lineage and living practice').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is Hong Kong civil society''s compliance with constrained autonomy driven by structural barriers (economic dependency, emigration costs, legal threat) or by internalized acceptance of the ''one country'' identity frame?',
    'Post-opening behavioral analysis: if resistance resurges rapidly when structural barriers lower, suppression is primarily structural; if resistance remains muted even when barriers ease, internalization is significant.',
    'If internalized, effective extraction exceeds structural measures and the constraint is more resilient; if structural, the constraint is brittle and may face sudden legitimacy collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in civil society compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oc2s_balanced_tr_t0, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(oc2s_balanced_tr_t5, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(oc2s_balanced_tr_t10, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(oc2s_balanced_tr_t15, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(oc2s_balanced_tr_t20, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(oc2s_balanced_tr_t25, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(oc2s_balanced_tr_t30, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 30, 0.5).

% Extraction over time
narrative_ontology:measurement(oc2s_balanced_be_t0, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(oc2s_balanced_be_t5, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(oc2s_balanced_be_t10, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(oc2s_balanced_be_t15, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(oc2s_balanced_be_t20, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(oc2s_balanced_be_t25, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(oc2s_balanced_be_t30, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(oc2s_balanced_su_t0, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(oc2s_balanced_su_t5, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(oc2s_balanced_su_t10, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(oc2s_balanced_su_t15, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(oc2s_balanced_su_t20, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(oc2s_balanced_su_t25, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(oc2s_balanced_su_t30, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, identity_coordination).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'One Country, Two Systems' conflates three structurally distinct constraints. The autonomy_primacy_reading treats the framework as a guarantee with low extraction; the sovereignty_primacy_reading treats it as delegated authority with high extraction; this balanced reading treats it as an ongoing negotiated compromise with medium extraction. Each reading has a distinct epsilon, stakeholder directionality, and classification. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
