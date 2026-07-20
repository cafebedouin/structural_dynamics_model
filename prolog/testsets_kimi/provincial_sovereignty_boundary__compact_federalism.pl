% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Compact Federalism Reading of Provincial Sovereignty Boundary
 *   domain: political/federalism
 *
 * SUMMARY:
 *   This constraint story instantiates the compact_federalism reading of the
 *   provincial_sovereignty_boundary kernel in Canadian constitutional
 *   political economy. It treats the constitutional order as a compact among
 *   sovereign provinces retaining residual sovereignty, with federal
 *   authority conditional on provincial consent, equalization as negotiable,
 *   climate policy subject to provincial override, and secession as requiring
 *   negotiation under duress rather than federal permission. The reading
 *   competes with constitutional_subordination (federal supremacy) and
 *   resource_sovereignty_primacy (absolute provincial resource control). Key
 *   agents include provincial governments that benefit from residual
 *   sovereignty and conditional federalism, the federal government whose
 *   authority is constrained, resource-rich and net-recipient provinces with
 *   asymmetric fiscal interests, secessionist movements caught in
 *   duress-laden exit procedures, and the Supreme Court as adjudicator.
 *
 * KEY AGENTS:
 *   - provincial_governments: Primary beneficiary (institutional/constrained) â collects residual sovereignty and veto leverage over federal policy
 *   - federal_government: Primary payer (institutional/constrained) â bears cost of constitutionally limited authority and conditional supremacy
 *   - resource_rich_provinces: Secondary beneficiary (powerful/constrained) â leverages provincial sovereignty to retain resource revenues and resist equalization
 *   - net_recipient_provinces: Secondary payer (moderate/constrained) â exposed to negotiable equalization rather than guaranteed transfers
 *   - secessionist_movements: Tertiary payer (organized/identity_locked) â promised negotiated exit but trapped by procedural duress and identity-fusion
 *   - supreme_court: Analytical observer (institutional/analytical) â adjudicates which reading prevails in specific disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.58).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.52).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.58).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Compact Federalism Reading of Provincial Sovereignty Boundary").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political/federalism").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, 'e692ac5d-40fd-4a10-bec4-f7888cf106fc').
narrative_ontology:cs_kernel_codification('e692ac5d-40fd-4a10-bec4-f7888cf106fc', fixed_text).
narrative_ontology:cs_authority_grounding('e692ac5d-40fd-4a10-bec4-f7888cf106fc', lineage).
narrative_ontology:cs_interpretation_layer_present('e692ac5d-40fd-4a10-bec4-f7888cf106fc').
narrative_ontology:cs_reading_relation('e692ac5d-40fd-4a10-bec4-f7888cf106fc', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('e692ac5d-40fd-4a10-bec4-f7888cf106fc', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('e692ac5d-40fd-4a10-bec4-f7888cf106fc', foundational, residual_provincial_sovereignty).
narrative_ontology:cs_axiom_status(residual_provincial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('e692ac5d-40fd-4a10-bec4-f7888cf106fc', residual_provincial_sovereignty, conventional).
narrative_ontology:cs_axiom('e692ac5d-40fd-4a10-bec4-f7888cf106fc', foundational, negotiated_exit_right).
narrative_ontology:cs_axiom_status(negotiated_exit_right, holdable).
narrative_ontology:cs_axiom_grounding('e692ac5d-40fd-4a10-bec4-f7888cf106fc', negotiated_exit_right, conventional).
narrative_ontology:cs_reference_frame('e692ac5d-40fd-4a10-bec4-f7888cf106fc', confederation_compact_sovereignty).
narrative_ontology:cs_drift_state('e692ac5d-40fd-4a10-bec4-f7888cf106fc', contemporary_centralization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e692ac5d-40fd-4a10-bec4-f7888cf106fc', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, net_recipient_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, secessionist_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise residual sovereignty over constitutional jurisdictions; use compact theory to resist federal intrusion into provincial affairs; negotiate equalization and federal transfers from a position of claimed constitutional equality.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provincial_governments, beneficiary,
    institutional, generational, constrained, national).

% Leverage residual sovereignty and provincial crown ownership of natural resources to resist federal climate and taxation policy; benefit from constitutional ambiguity that allows resource revenue retention; bear political costs of resisting federal equalization demands.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces, beneficiary,
    powerful, generational, constrained, regional).

% Bears the cost of constitutionally conditional authority; cannot unilaterally implement national climate, social, or economic policy in areas of provincial jurisdiction; must negotiate with provinces even where federal supremacy would be more efficient.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, payer,
    institutional, generational, constrained, national).

% Rely on equalization and federal transfers that are negotiable under the compact reading rather than constitutionally guaranteed entitlements; vulnerable to provincial override by donor provinces in federal-provincial bargaining.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, net_recipient_provinces, payer,
    moderate, biographical, constrained, regional).

% Seek provincial exit from the federation; the compact reading promises a right to negotiate secession but embeds structural duress (clear majority, clear question, duty to negotiate balanced against federal integrity) that makes exit costly and uncertain; identity-fusion with sovereignty movement traps members in the negotiation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, secessionist_movements, payer,
    organized, biographical, identity_locked, regional).

% Adjudicates federalism disputes and the constitutional amending formula; its interpretations determine whether the compact reading or competing readings prevail in specific cases; acts as final interpreter of the constitutional text.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, supreme_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__compact_federalism, diffuse).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__compact_federalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a multi-provincial federation by preserving provincial autonomy over local matters, providing a constitutional mechanism for diverse regions to share a state without unitary centralization.
% TRANSFER_FUNCTION: Transfers jurisdictional authority from the federal government to provincial governments; transfers fiscal risk from resource-rich provinces to net recipient provinces by making equalization negotiable; transfers exit costs to secessionist populations through duress-laden negotiation requirements.
% ABSENT_VOICES: First Nations and Indigenous governments whose territorial sovereignty predates the provincial-federal compact are excluded from the constitutional narrative; federal supremacists and unitary-state advocates are marginalized in constitutional interpretation; citizens preferring uniform national standards in climate or social policy lack a seat when provinces override.
% DISAPPEARANCE_RATIONALE: Federal authority would expand unilaterally, equalization would become either a guaranteed federal entitlement or disappear, provincial resource control would weaken, and secession would be either prohibited unconditionally or permitted without procedural duress â the federation would reorganize around a fundamentally different sovereignty boundary.
% FOUNDING_PROBLEM: How to unite British North American colonies in 1867 without imposing unitary centralization on distinct religious, linguistic, and economic communities, while preserving sufficient central authority for defense, trade, and macroeconomic stability.
% FOUNDING_PROBLEM_CORROBORATION: Provincial governments and compact historians attest the founding problem was preserving provincial sovereignty; federal officials and centralist historians attest the founders intended a strong central government to avoid American-style fragmentation. The Privy Council's early 20th-century provincial-rights rulings and the Supreme Court's later centralizing jurisprudence corroborate ongoing contestation. No neutral arbiter unanimously confirms one status.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is moderate-high because the reading structurally transfers authority and fiscal capacity from the federal government to provinces, and embeds duress in exit procedures that exact costs from secessionist movements without providing genuine sovereignty. Suppression (0.52) reflects the structural suppression of federal supremacy and unitary alternatives through constitutional interpretation and provincial veto. Theater (0.35) captures the performative maintenance of the 'sovereign compact' historical narrative despite historical evidence of colonial orchestration. Accessibility collapse (0.42) is moderate: alternatives such as federal supremacy or unitary state are theoretically available but politically collapsed by the constitutional amending formula and path dependence. Resistance (0.60) is substantial because the federal government and centralist constituencies actively resist this reading in favor of constitutional subordination.
 *
 * PERSPECTIVAL GAP:
 *   The federal government seat experiences the constraint as a structural limitation on democratically elected national authority, while provincial governments experience it as a legitimate guarantee of autonomy. Resource-rich provinces experience it as a shield for resource rents, while net-recipient provinces experience the same reading as exposure in fiscal bargaining. Secessionist movements experience the 'negotiable exit' promise as either a lifeline or a trap depending on whether they focus on the nominal right or the operational duress. The engine should compute these divergent seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Provincial governments and resource-rich provinces are structural beneficiaries (low d, subsidized by the constraint's empowerment of provincial jurisdiction). The federal government, net-recipient provinces, and secessionist movements are structural targets (high d, extraction of authority, fiscal certainty, and exit possibility respectively). The Supreme Court sits at analytical distance. The primary directionality driver is the beneficiary/victim split combined with exit options: beneficiaries are constrained but not trapped, while secessionist movements are identity-locked, amplifying their effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and extraction. The compact reading genuinely coordinates a multi-national federation without centralizing coercion â this is the coordination function. However, the same structure asymmetrically extracts from federal capacity and secessionist self-determination, and equalization negotiability exposes net-recipient provinces. If the coordination story were taken alone, it would read as rope; if the extraction from federal authority and secessionist movements were taken alone, it would read as snare. The tangled_rope classification captures that both are structurally present and operationally coupled through the same constitutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_historicity,
    'Was Confederation in 1867 a voluntary compact among sovereign equals, or a colonially managed arrangement with asymmetric power?',
    'Historical archival analysis of the Charlottetown and Quebec conferences; examination of colonial office records and provincial legislative debates in 1867.',
    'If the compact was historically fictitious, the residual sovereignty claim loses its genealogical warrant and the constraint shifts toward pure extraction or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compact_historicity, empirical, 'Historical authenticity of the compact narrative').

omega_variable(
    duress_exit_legitimacy,
    'Does the ''negotiation under duress'' framework for secession constitute a genuine right of exit or a procedural trap that extracts legitimacy from secessionist movements while preserving the federation?',
    'Comparative analysis of secession negotiations under the clarity framework; measurement of actual exit outcomes versus procedural costs imposed.',
    'If the duress makes exit effectively impossible, the constraint is more extractive toward secessionist populations than the compact reading admits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_exit_legitimacy, conceptual, 'Whether negotiated exit under duress is genuine or extractive').

omega_variable(
    equalization_negotiability,
    'Is fiscal equalization a constitutionally guaranteed federal duty under the compact reading, or merely a discretionary inter-provincial bargain?',
    'Constitutional text analysis of section 36 and related jurisprudence; federal and provincial fiscal bargaining records.',
    'If negotiable, net-recipient provinces are structurally exposed, increasing the extractiveness of the constraint from their seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equalization_negotiability, empirical, 'Constitutional status of equalization transfers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prov_tr_t10, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 10, 0.25).
narrative_ontology:measurement(prov_tr_t20, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 20, 0.28).
narrative_ontology:measurement(prov_tr_t30, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 30, 0.3).
narrative_ontology:measurement(prov_tr_t40, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 40, 0.33).
narrative_ontology:measurement(prov_tr_t50, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(prov_be_t10, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(prov_be_t20, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(prov_be_t30, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(prov_be_t40, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(prov_be_t50, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 50, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(provincial_sovereignty_boundary__compact_federalism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% The provincial_sovereignty_boundary kernel decomposes into three structurally distinct constraints: constitutional_subordination (federal supremacy reading), compact_federalism (provincial residual sovereignty reading), and resource_sovereignty_primacy (absolute resource control reading). Each reading has a different epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
