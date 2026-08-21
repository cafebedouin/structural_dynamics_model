% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope: Enforcement Vacuum Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint describes the 'enforcement vacuum' reading of the GPL
 *   copyleft scope. It posits that the absence of definitive judicial
 *   precedent regarding the precise boundaries of GPL's copyleft provisions
 *   (e.g., what constitutes a 'derivative work' in dynamic linking scenarios)
 *   creates a state of 'licensed plurality.' In this state, different
 *   interpretive communities (e.g., FSF-aligned projects vs.
 *   industry-dominated ecosystems) operate under their preferred
 *   interpretations, with the actual effective constraint depending on which
 *   community has the capacity and willingness to enforce its view in a
 *   specific context. This uncertainty itself becomes a structural feature of
 *   the software licensing landscape.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.35).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.45).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope: Enforcement Vacuum Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, 'f72474e8-9e9c-488f-949d-c4ad54406a87').
narrative_ontology:cs_kernel_codification('f72474e8-9e9c-488f-949d-c4ad54406a87', distributed).
narrative_ontology:cs_authority_grounding('f72474e8-9e9c-488f-949d-c4ad54406a87', distributed).
narrative_ontology:cs_reading_relation('f72474e8-9e9c-488f-949d-c4ad54406a87', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('f72474e8-9e9c-488f-949d-c4ad54406a87', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('f72474e8-9e9c-488f-949d-c4ad54406a87', foundational, judicial_ambiguity_is_structural).
narrative_ontology:cs_axiom_status(judicial_ambiguity_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('f72474e8-9e9c-488f-949d-c4ad54406a87', judicial_ambiguity_is_structural, conventional).
narrative_ontology:cs_axiom('f72474e8-9e9c-488f-949d-c4ad54406a87', foundational, enforcement_capacity_defines_de_facto_scope).
narrative_ontology:cs_axiom_status(enforcement_capacity_defines_de_facto_scope, holdable).
narrative_ontology:cs_axiom_grounding('f72474e8-9e9c-488f-949d-c4ad54406a87', enforcement_capacity_defines_de_facto_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('f72474e8-9e9c-488f-949d-c4ad54406a87', interpretive_pluralism_by_default).
narrative_ontology:cs_drift_state('f72474e8-9e9c-488f-949d-c4ad54406a87', contemporary_software_ecosystem, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f72474e8-9e9c-488f-949d-c4ad54406a87', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These projects adhere to a strong interpretation of copyleft, but face uncertainty and increased transaction costs due to the lack of definitive legal clarity. They must navigate potential legal challenges or compromise on their interpretation to ensure broader adoption.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects, payer,
    organized, generational, constrained, global).

% These ecosystems often operate under a narrow interpretation of GPL scope, leveraging the legal ambiguity to integrate GPL-licensed software with proprietary components. Their de facto enforcement capacity (e.g., through large legal teams) shapes the practical scope of copyleft within their sphere.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems, agenda_setter,
    institutional, biographical, arbitrage, global).

% Developers and companies who prioritize legal certainty and seek clear guidance on GPL compliance. They incur higher legal review costs and may avoid certain integrations due to the ambiguity, effectively paying a 'clarity premium'.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    moderate, immediate, constrained, global).

% These adopters exploit the legal vacuum to adopt more flexible interpretations of GPL scope, allowing them to integrate GPL-licensed code in ways that might be challenged by strong copyleft advocates but are not definitively prohibited by law. They benefit from reduced compliance friction.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Academics and legal experts who analyze the ongoing debate and lack of judicial precedent, contributing to the discourse but not directly enforcing or benefiting from the ambiguity.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% The ultimate arbiters of legal precedent, but have largely refrained from issuing definitive rulings on the precise scope of GPL copyleft, thus perpetuating the enforcement vacuum. Their inaction is a form of agenda-setting.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows diverse interpretations of GPL copyleft scope to coexist without immediate legal gridlock, enabling a broader range of software integrations and business models than a single, strict interpretation might allow.
% TRANSFER_FUNCTION: Transfers transaction costs (legal analysis, risk assessment, potential litigation) to clarity-seeking adopters and FSF-aligned projects, while transferring flexibility and reduced compliance burden to pragmatic adopters and industry-dominated ecosystems.
% ABSENT_VOICES: A definitive, universally accepted judicial ruling on GPL scope. Such a ruling would clarify the boundaries of derivative works and linking, resolving the current interpretive plurality.
% DISAPPEARANCE_RATIONALE: If definitive judicial precedent on GPL scope emerged overnight, the landscape of software licensing and development would immediately shift. Compliance strategies would be clarified, legal risks re-evaluated, and many existing integrations would either become definitively compliant or non-compliant, forcing a reorganization of the mobile software economy.
% FOUNDING_PROBLEM: The inherent ambiguity in copyright law's 'derivative work' concept when applied to software linking, combined with the lack of judicial clarity regarding the GPL's specific terms and enforcement mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Ongoing legal analyses, continued debates in open-source communities, and the persistent existence of projects and companies operating under different, often conflicting, interpretations of GPL scope, all corroborate that the problem remains unresolved.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it facilitates a form of coordination (allowing diverse interpretations to coexist, preventing immediate legal gridlock) but does so with asymmetric extraction. The 'extraction' comes from the elevated transaction costs for clarity-seeking adopters and FSF-aligned projects, who must invest in legal analysis and risk assessment, or compromise their principles. Conversely, pragmatic adopters and industry benefit from the flexibility afforded by the ambiguity. Suppression is moderate, as it's not active coercion but the suppression of legal certainty and clear alternatives. Theater ratio is low because the ambiguity is a genuine, unresolved legal state, not a performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of industry and pragmatic adopters, the enforcement vacuum offers valuable flexibility, allowing them to innovate without being strictly bound by the most expansive copyleft interpretations. From the perspective of FSF-aligned projects and clarity-seeking adopters, this same vacuum represents a significant burden, undermining the intended protective function of copyleft and creating an uneven playing field where those with greater legal resources can effectively dictate the interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Industry-dominated ecosystems and pragmatic adopters are beneficiaries (low directionality) as they gain flexibility and potentially reduce compliance burdens by exploiting the ambiguity. FSF-aligned projects and clarity-seeking adopters are targets (high directionality) as they bear the costs of uncertainty, legal analysis, and potential enforcement actions. Courts, by their inaction, act as agenda-setters, maintaining the status quo of ambiguity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_inherence,
    'Is the ambiguity in GPL copyleft scope an inherent feature of software copyright law, or is it resolvable through judicial or legislative action?',
    'A landmark judicial ruling or new legislation specifically addressing software linking and derivative works under copyleft licenses.',
    'If resolvable, the constraint would shift from a ''tangled rope of uncertainty'' to a clearer, potentially more extractive (if the ruling favors one side) or less extractive (if it clarifies coordination) form. If inherent, the current state is a stable feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_inherence, conceptual, 'Whether the legal ambiguity is fundamental or contingent.').

omega_variable(
    enforcement_capacity_impact,
    'To what extent do the differing enforcement capacities of FSF-aligned projects and industry-dominated ecosystems actually shape the de facto scope of GPL copyleft?',
    'Empirical study of licensing practices, litigation outcomes, and compliance strategies across different software ecosystems and company sizes.',
    'If enforcement capacity is the primary driver, the constraint''s extractiveness and suppression are directly tied to power asymmetries. If other factors (e.g., community norms, ethical considerations) play a larger role, the constraint is less extractive than it appears.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_impact, empirical, 'How power dynamics influence effective copyleft scope.').

omega_variable(
    transaction_cost_quantification,
    'What is the quantifiable cost (in legal fees, delayed development, missed opportunities) incurred by clarity-seeking adopters due to the GPL copyleft enforcement vacuum?',
    'Economic analysis and surveys of legal departments and open-source projects, comparing costs in ambiguous vs. clear licensing environments.',
    'A high quantifiable cost would strengthen the ''extraction'' component of this Tangled Rope classification, highlighting the burden on certain stakeholders. A low cost would suggest the ''coordination'' aspect (flexibility) is more dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transaction_cost_quantification, empirical, 'Quantification of costs imposed by legal ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 2000, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t2000, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(gpl__tr_t2005, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(gpl__tr_t2010, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(gpl__tr_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(gpl__tr_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t2000, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(gpl__be_t2005, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement(gpl__be_t2010, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(gpl__be_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2015, 0.34).
narrative_ontology:measurement(gpl__be_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2020, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t2000, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(gpl__su_t2005, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement(gpl__su_t2010, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2010, 0.43).
narrative_ontology:measurement(gpl__su_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement(gpl__su_t2020, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2020, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, information_standard).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'GPL copyleft scope' kernel. This 'enforcement vacuum' reading describes the state of legal ambiguity and its practical implications, coexisting with the 'strong copyleft' and 'narrow scope' readings, which represent specific interpretations of the license.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
