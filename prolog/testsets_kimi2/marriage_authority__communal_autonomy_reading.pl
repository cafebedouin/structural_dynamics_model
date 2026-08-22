% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: State-Enforced Communal Marriage Authority (Communal Autonomy Reading)
 *   domain: legal/constitutional/family_law
 *
 * SUMMARY:
 *   In a plural legal order, the state enforces marriage and family law norms
 *   authored by religious communities rather than by the democratic
 *   legislature. This communal-autonomy reading of the marriage_authority
 *   kernel treats the arrangement as a constitutional settlement that
 *   protects minority identity by delegating family governance to religious
 *   leadership. The state acts as a deferential enforcer: courts apply Hindu,
 *   Muslim, Christian, or other personal codes, and legislative amendments to
 *   those codes require community consent. Religious leadership benefits from
 *   state-backed gatekeeping authority, while intra-community
 *   dissentersâespecially women and gender minoritiesâbear the costs of
 *   norms they cannot exit without social expulsion. The constraint is
 *   claimed as tangled rope because it carries a genuine coordination
 *   function (minority protection against majoritarian assimilation)
 *   alongside asymmetric extraction (state-backed enforcement of patriarchal
 *   communal control).
 *
 * KEY AGENTS:
 *   - religious_leadership: Primary beneficiary (organized/constrained) â retains normative authority over family status because the state delegates enforcement and requires community consent for reform.
 *   - intra_community_dissenters: Primary target (powerless/identity_locked) â bear the extraction of patriarchal personal law; exit is locked by religious identity and kinship costs.
 *   - state_enforcement_apparatus: Agenda-setter (institutional/constrained) â enforces communal norms without authoring them; politically blocked from unilateral reform by the community-consent requirement.
 *   - excluded_secular_reformists: Excluded voice (moderate/constrained) â advocate uniform civil code but are kept out of the legislative conversation by the political economy of communal consent.
 *   - legal_pluralism_scholars: Analytical observer (analytical/analytical) â documents the divergence between coordination narrative and extractive outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.45).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.62).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "State-Enforced Communal Marriage Authority (Communal Autonomy Reading)").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal/constitutional/family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '9d80a976-681a-45c9-adf7-cb5b52839c4f').
narrative_ontology:cs_kernel_codification('9d80a976-681a-45c9-adf7-cb5b52839c4f', fixed_text).
narrative_ontology:cs_authority_grounding('9d80a976-681a-45c9-adf7-cb5b52839c4f', lineage).
narrative_ontology:cs_interpretation_layer_present('9d80a976-681a-45c9-adf7-cb5b52839c4f').
narrative_ontology:cs_reading_relation('9d80a976-681a-45c9-adf7-cb5b52839c4f', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('9d80a976-681a-45c9-adf7-cb5b52839c4f', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d80a976-681a-45c9-adf7-cb5b52839c4f', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d80a976-681a-45c9-adf7-cb5b52839c4f', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('9d80a976-681a-45c9-adf7-cb5b52839c4f', foundational, communal_autonomy_as_constitutional_imperative).
narrative_ontology:cs_axiom_status(communal_autonomy_as_constitutional_imperative, holdable).
narrative_ontology:cs_axiom_grounding('9d80a976-681a-45c9-adf7-cb5b52839c4f', communal_autonomy_as_constitutional_imperative, conventional).
narrative_ontology:cs_axiom('9d80a976-681a-45c9-adf7-cb5b52839c4f', foundational, religious_leadership_as_normative_gatekeeper).
narrative_ontology:cs_axiom_status(religious_leadership_as_normative_gatekeeper, holdable).
narrative_ontology:cs_axiom_grounding('9d80a976-681a-45c9-adf7-cb5b52839c4f', religious_leadership_as_normative_gatekeeper, conventional).
narrative_ontology:cs_reference_frame('9d80a976-681a-45c9-adf7-cb5b52839c4f', communal_autonomy_default).
narrative_ontology:cs_drift_state('9d80a976-681a-45c9-adf7-cb5b52839c4f', contemporary_constitutional_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9d80a976-681a-45c9-adf7-cb5b52839c4f', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Community religious authorities whose norms on marriage, divorce, and inheritance are enforced by the state. They benefit from institutionalized deference that preserves their role as gatekeepers of family status and community boundaries, and they retain a veto over legislative amendments through the community-consent requirement.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_leadership, beneficiary,
    organized, generational, constrained, national).

% Members of the religious community, especially women and gender dissenters, who reject patriarchal family law norms but remain bound by them because the state enforces communal personal law. Exit requires apostasy or civil conversion, which carries severe social ostracism and loss of kinship networks.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, biographical, identity_locked, national).

% State courts and administrative bodies that apply personal law in marriage and family disputes. They do not author the norms but are structurally required to enforce them; legislative amendments require community consent, locking the state into a delegated enforcement role that it cannot easily alter.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Advocates of a uniform civil code and secular family law who are excluded from effective legislative influence because reform is blocked by the political need for community consent and the constitutional framing of communal autonomy.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, excluded_secular_reformists, excluded,
    moderate, biographical, constrained, national).

% Academic analysts who study the intersection of religious personal law and state enforcement, documenting the divergence between the coordination narrative of minority protection and the extraction experienced by intra-community dissenters.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, legal_pluralism_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:fixing_cost_class(marriage_authority__communal_autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves religious minority community identity and inter-group peace by delegating family law authority to communal institutions, avoiding majoritarian legislative imposition and providing a constitutional shelter for plural marriage norms.
% TRANSFER_FUNCTION: Transfers authority over marriage, divorce, and inheritance from the secular legislature to religious community institutions, and transfers the compliance costs of restrictive patriarchal norms from religious leadership to intra-community dissenters who are bound by state-enforced personal law.
% ABSENT_VOICES: Secular reformists advocating for a uniform civil code and gender-equality advocates within religious communities are structurally excluded; their objections are muted because the state treats community consent as a veto over personal law reform.
% DISAPPEARANCE_RATIONALE: If state enforcement of communal personal law vanished overnight, religious communities would lose state-backed coercion over family status, intra-community dissenters could access civil family law, and the state's deferential role would collapse into direct democratic legislation; the political settlement around minority protection would unravel.
% FOUNDING_PROBLEM: How to integrate religious minority communities into a democratic state without forcing assimilation to majoritarian family norms, and how to prevent communal conflict over marriage and inheritance.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial constitutional framers and minority political representatives attest the problem was live at founding. Feminist legal historians and intra-community dissenters attest the problem has been superseded by constitutional equality commitments and that the arrangement now functions as communal power preservation. Corroboration exists from outside the beneficiary set.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).
:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint genuinely coordinates minority identity preservation but imposes significant costs on dissenters. Suppression is elevated (0.62) because the arrangement depends on state courts actively blocking civil-law alternatives for community members. Theater ratio is moderate-low (0.30): some enforcement is performative maintenance of tradition, but the coordination function is not hollow. Accessibility collapse is moderate (0.58): alternatives like civil marriage or uniform code are culturally and politically visible but legally blocked. Resistance is moderate (0.55): ongoing feminist litigation and dissent generate pushback. The measurement series share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the religious leadership seat, the constraint is protective coordination that shields a minority culture from majoritarian erasure; from the intra-community dissenter seat, it is state-backed extraction that forces compliance with patriarchal norms; from the state seat, it is a pragmatic deferral that buys political stability at the cost of individual equality. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious leadership sits near the beneficiary end: they collect authority, status, and boundary-maintenance benefits without paying the direct costs of enforcement. Intra-community dissenters sit near the full-target end: they are identity-locked, powerless, and the state channels extraction directly onto them. The state enforcement apparatus sits near symmetric: it bears the administrative cost and political risk of enforcement without capturing the communal benefit. Excluded secular reformists would sit at the target end if they were inside the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâintegrating minorities without forced assimilationâwas genuine, but its current status is contested. If the problem is dead and the arrangement persists purely to preserve communal elite power, the constraint would drift toward snare or piton. The active presence of a coordination function (genuine minority protection) and the political reality of community consent prevent a pure extraction reading, placing it in tangled rope rather than snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    communal_autonomy_vs_gender_equality,
    'Does the constitutional commitment to communal autonomy override individual gender equality guarantees within personal law, or vice versa?',
    'Supreme Court constitutional bench ruling or legislative amendment explicitly settling the hierarchy of fundamental rights in the personal law domain.',
    'If gender equality overrides, this constraint shifts toward a scaffold or snare (if reforms are blocked by community veto); if autonomy holds, it remains a tangled rope with persistent asymmetric extraction on dissenters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_autonomy_vs_gender_equality, conceptual, 'Hierarchy conflict between group autonomy and individual equality.').

omega_variable(
    kernel_reading_sibling_structure,
    'This constraint is one reading of the marriage_authority kernel; how would the secularist or gender-rights readings change the beneficiary and victim structure?',
    'Comparative analysis of sibling constraint stories within this kernel family.',
    'Sibling readings would redistribute directionality: the secularist reading eliminates religious_leadership as beneficiary and makes the democratic legislature the agenda-setter; the gender-rights reading makes intra-community women the beneficiaries and religious leadership the payers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_structure, conceptual, 'Sibling reading structural variation for committer frame.').

omega_variable(
    enforcement_as_coordination_or_extraction,
    'Is state enforcement of communal personal law primarily a coordination mechanism protecting minority identity, or an extraction mechanism that offloads the costs of patriarchal control onto intra-community dissenters?',
    'Comparative outcome analysis across jurisdictions with varying degrees of state enforcement of personal law, measuring community persistence versus individual welfare outcomes.',
    'If primarily coordination, classification moves toward rope; if primarily extraction with coordination as cover, classification moves toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_as_coordination_or_extraction, empirical, 'Coordination-extraction ambiguity in state-deferred personal law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ma_car_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ma_car_tr_t10, marriage_authority__communal_autonomy_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(ma_car_tr_t20, marriage_authority__communal_autonomy_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(ma_car_tr_t30, marriage_authority__communal_autonomy_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(ma_car_tr_t40, marriage_authority__communal_autonomy_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(ma_car_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ma_car_be_t10, marriage_authority__communal_autonomy_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(ma_car_be_t20, marriage_authority__communal_autonomy_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(ma_car_be_t30, marriage_authority__communal_autonomy_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(ma_car_be_t40, marriage_authority__communal_autonomy_reading, base_extractiveness, 40, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ma_car_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ma_car_su_t10, marriage_authority__communal_autonomy_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(ma_car_su_t20, marriage_authority__communal_autonomy_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(ma_car_su_t30, marriage_authority__communal_autonomy_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(ma_car_su_t40, marriage_authority__communal_autonomy_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority kernel. The communal_autonomy_reading instantiates a constraint where personal law variation is institutionalized with state enforcement and community consent requirements, differing from siblings in its authority grounding (lineage), beneficiary structure (religious leadership), and victim structure (intra-community dissenters). Each sibling reading carries a distinct epsilon, stakeholder set, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
