% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority under Indian Christian Marriage Act 1872
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint instantiates the christian_canonical_reading of the
 *   marriage_authority_kernel: marriage and family-law authority for Indian
 *   Christians is sourced from Christian canonical law as codified in the
 *   Indian Christian Marriage Act 1872. The reading enforces a fault-based
 *   divorce regime, reserves annulment jurisdiction to church tribunals, and
 *   encodes moderate gender inequity. It operates within India's plural
 *   personal-law system, coexisting with parallel Hindu, Muslim, Parsi, and
 *   secular civil readings. The claim/metric independence is maintained: the
 *   constraint is claimed as tangled_rope because a genuine coordination
 *   function (communal marriage validation, inheritance order) coexists with
 *   asymmetric extraction (restricted exit, gendered costs, institutional
 *   gatekeeping).
 *
 * KEY AGENTS:
 *   - Christian Ecclesiastical Authority (agenda_setter/institutional): Controls annulment and canonical validity, benefits from concentrated jurisdiction.
 *   - Traditional Christian Community (beneficiary/organized): Receives identity-boundary maintenance and social cohesion.
 *   - Indian State (beneficiary/institutional): Delegates governance burden, maintains plural order.
 *   - Christian Women (payer/powerless): Bear asymmetric exit costs under fault-based divorce and tribunal control.
 *   - Christian Dissenters (excluded/moderate): Excluded from canonical deliberations, advocate for secular reform.
 *   - Supreme Constitutional Bench (observer/institutional): Monitors but largely defers, unresolved tension with constitutional morality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.62).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.58).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Marriage Authority under Indian Christian Marriage Act 1872").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, 'ac946d0e-ba57-4096-b1cc-0450ad3cff92').
narrative_ontology:cs_kernel_codification('ac946d0e-ba57-4096-b1cc-0450ad3cff92', fixed_text).
narrative_ontology:cs_authority_grounding('ac946d0e-ba57-4096-b1cc-0450ad3cff92', lineage).
narrative_ontology:cs_interpretation_layer_present('ac946d0e-ba57-4096-b1cc-0450ad3cff92').
narrative_ontology:cs_reading_relation('ac946d0e-ba57-4096-b1cc-0450ad3cff92', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac946d0e-ba57-4096-b1cc-0450ad3cff92', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac946d0e-ba57-4096-b1cc-0450ad3cff92', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac946d0e-ba57-4096-b1cc-0450ad3cff92', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('ac946d0e-ba57-4096-b1cc-0450ad3cff92', foundational, sacramental_indissolubility).
narrative_ontology:cs_axiom_status(sacramental_indissolubility, holdable).
narrative_ontology:cs_axiom_grounding('ac946d0e-ba57-4096-b1cc-0450ad3cff92', sacramental_indissolubility, theological).
narrative_ontology:cs_axiom('ac946d0e-ba57-4096-b1cc-0450ad3cff92', foundational, ecclesiastical_jurisdiction_over_matrimony).
narrative_ontology:cs_axiom_status(ecclesiastical_jurisdiction_over_matrimony, holdable).
narrative_ontology:cs_axiom_grounding('ac946d0e-ba57-4096-b1cc-0450ad3cff92', ecclesiastical_jurisdiction_over_matrimony, conventional).
narrative_ontology:cs_reference_frame('ac946d0e-ba57-4096-b1cc-0450ad3cff92', canonical_sacramental_framework).
narrative_ontology:cs_drift_state('ac946d0e-ba57-4096-b1cc-0450ad3cff92', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ac946d0e-ba57-4096-b1cc-0450ad3cff92', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, traditional_christian_community).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, indian_state).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers annulment proceedings under canonical rules and the Indian Christian Marriage Act 1872. Determines validity of Christian marriages and grants decrees of nullity or dissolution on limited fault grounds. Derives institutional authority from the claim that matrimonial jurisdiction originates in sacramental canonical law rather than secular state delegation alone.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_ecclesiastical_authority, agenda_setter,
    institutional, generational, constrained, national).

% Receives social cohesion and boundary maintenance from a distinct matrimonial regime that differentiates Christian marriage from other personal laws. Community leaders and families reference canonical tribunal rulings to regulate membership, inheritance, and social standing.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, traditional_christian_community, beneficiary,
    organized, generational, identity_locked, national).

% Must prove adultery, cruelty, desertion, or other statutory faults to obtain civil divorce under the Act. Annulment routes require submitting to church tribunal procedures that may not recognize modern gender-equality norms. They bear the primary asymmetry in exit costs compared to men under the moderate gender-inequity structure.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women, payer,
    powerless, biographical, constrained, national).

% Advocate for no-fault divorce, gender parity, or secular marriage governance within the Christian community. Their views are not represented in canonical tribunal deliberations or in the ICMA framework, and they face social pressure to conform to the canonical regime.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_dissenters, excluded,
    moderate, biographical, constrained, national).

% Codified the 1872 Act and continues to enforce its provisions through civil courts while deferring to church tribunals on annulment. Benefits from delegated community governance that reduces direct state burden and manages pluralism, at the cost of ceding some matrimonial jurisdiction.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, indian_state, beneficiary,
    institutional, generational, mobile, national).

% Reviews personal law for compatibility with constitutional Articles 14, 15, and 21. Has intervened in other personal laws but has largely deferred to the Christian canonical framework under the Act, creating an unresolved tension between canonical lineage and constitutional morality.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, supreme_constitutional_bench, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__christian_canonical_reading, christian_ecclesiastical_authority).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, church-recognized framework for contracting and validating Christian marriages, ensuring communal legitimacy and orderly inheritance and lineage within the community.
% TRANSFER_FUNCTION: Moves authority over marital validity and dissolution from individual spouses to church tribunals and canonical doctrine, restricting unilateral exit and concentrating gatekeeping power in ecclesiastical hands.
% ABSENT_VOICES: Secular feminists, Christian women seeking no-fault divorce, and proponents of a Uniform Civil Code are structurally excluded from canonical tribunal deliberations; their objections are heard only in civil constitutional challenges, not in the normative framework itself.
% DISAPPEARANCE_RATIONALE: If the canonical authority vanished, Christian marriage and divorce would shift to civil secular adjudication or other personal-law frameworks; church tribunals would lose jurisdiction, and the community's distinct matrimonial identity would reorganize around state or individual-choice frameworks.
% FOUNDING_PROBLEM: Colonial and post-colonial need to govern a religiously plural society by delegating marriage regulation to identifiable community authorities, preventing inter-religious conflict and providing legitimacy through familiar canonical forms.
% FOUNDING_PROBLEM_CORROBORATION: Colonial legislative history attests the delegation motive; contemporary constitutional scholars and women's rights advocates attest the problem has mutated, while church authorities attest it remains live. Independent comparative-law scholarship outside the benefiting parties documents the plural-governance rationale.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects substantial but not total extraction: the fault-based regime and tribunal gatekeeping restrict autonomy, yet the Act still provides a recognizable marriage framework. Suppression (0.58) captures the combined legal enforcement through civil courts and social enforcement through community norms. Theater ratio (0.45) registers the performative weight of sacramental language and colonial-era statutory forms that now exceed their functional necessity. Accessibility collapse (0.68) is high because once married under this regime, secular exit routes are legally and socially truncated. Resistance (0.52) is moderate: feminist and constitutional challenges exist but have not yet dislodged the canonical framework. Measurements track monotonic drift from 1872 to the present: extraction and theater gradually rise as constitutional norms and global rights discourse make the canonical restrictions increasingly contested.
 *
 * PERSPECTIVAL GAP:
 *   The ecclesiastical seat experiences the constraint as necessary coordination preserving sacramental order and community identity. The payer seat (Christian women) experiences the identical structure as enforced extraction of marital autonomy. The engine computes this divergence from the structural asymmetry in power and exit options: the agenda setter has institutional power and generational time horizon, while the payer is powerless with biographical time horizon and constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authority and the traditional community are structural beneficiaries: the constraint subsidizes their institutional authority and boundary maintenance (low d, low effective extraction). The Indian State is a diffuse beneficiary through delegated governance stability. Christian women are the primary structural targets: fault-based divorce and tribunal control fall disproportionately on them, amplifying effective extraction (high d). Christian dissenters are excluded rather than coordinated; their exclusion is part of the suppression structure. Directionality is derived from these declarations without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâplural governance of a multi-religious societyâpersists in a general sense, but the specific canonical form is contested. The constraint is classified as tangled_rope rather than piton because the coordination function (community marriage validation) has not atrophied; it remains live for the beneficiary community. It is not a snare because the coordination story is not merely coverâthe canonical framework genuinely solves information and legitimacy problems for community members. The mandatrophy risk is resolved by the coexistence of live coordination with identifiable, asymmetric victimization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    canonical_colonial_freeze_ambiguity,
    'Is the Indian Christian Marriage Act 1872 a genuine codification of immutable canonical law, or a colonial construct that freezes a particular historical Christian tradition?',
    'Historical-theological analysis comparing pre-1872 canonical practice in Indian Christian communities against the codified statutory text and subsequent church tribunal rulings.',
    'If the Act is a colonial freeze, the constraint is more constructed and extractive than its lineage claim suggests; if it is genuine canonical continuity, the lineage grounding is stronger and extraction may read as necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canonical_colonial_freeze_ambiguity, conceptual, 'Colonial construction versus genuine canonical continuity').

omega_variable(
    gender_inequity_source_ambiguity,
    'Does the moderate gender inequity in this reading stem from canonical theological necessity or from cultural patriarchy layered onto canonical form?',
    'Comparative analysis of gender outcomes across jurisdictions with similar canonical readings and across different Christian denominations under the same Act.',
    'If the inequity is theological, reform requires kernel revision or axiom overriding; if cultural, it may be reformable through interpretive drift without structural collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_inequity_source_ambiguity, empirical, 'Theological versus cultural source of gender asymmetry').

omega_variable(
    state_church_authority_overlap,
    'When civil constitutional morality conflicts with canonical tribunal rulings, which authority structurally prevails, and does this ambiguity create a hidden extraction layer?',
    'Systematic review of Supreme Court judgments and High Court rulings that touch on Christian personal law, tracking whether civil courts defer, override, or create exceptions.',
    'If civil authority ultimately overrides canonical authority, effective extraction is lower than the structural measure suggests; if canonical authority prevails in practice despite constitutional rhetoric, extraction is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_church_authority_overlap, empirical, 'Civil constitutional versus canonical tribunal supremacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(icma1872_tr_t0, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(icma1872_tr_t30, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(icma1872_tr_t60, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(icma1872_tr_t90, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 90, 0.35).
narrative_ontology:measurement(icma1872_tr_t120, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 120, 0.4).
narrative_ontology:measurement(icma1872_tr_t150, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 150, 0.45).

% Extraction over time
narrative_ontology:measurement(icma1872_be_t0, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(icma1872_be_t30, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(icma1872_be_t60, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(icma1872_be_t90, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 90, 0.56).
narrative_ontology:measurement(icma1872_be_t120, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 120, 0.6).
narrative_ontology:measurement(icma1872_be_t150, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 150, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_authority_kernel__christian_canonical_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% This reading is part of the marriage_authority_kernel constraint family. The kernel decomposes into five structurally distinct readings (christian_canonical, hindu_codified, muslim_shariat, parsi_communal, secular_civil) because the source of authority, divorce regime, and gender equity profile differ across readings. Each reading carries its own epsilon and stakeholder map.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
