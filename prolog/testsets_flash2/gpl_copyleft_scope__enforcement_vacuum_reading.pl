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
 *   This constraint describes the situation where the scope of GPL copyleft
 *   is ambiguous due to a lack of definitive judicial precedent. This
 *   'enforcement vacuum' allows multiple interpretations to coexist, with the
 *   actual constraint experienced by adopters depending on the specific
 *   interpretive community's enforcement capacity (e.g., FSF-aligned projects
 *   vs. industry-dominated ecosystems). The ambiguity itself becomes a
 *   structural feature, creating a low-epsilon tangled_rope where some
 *   benefit from flexibility while others bear the cost of uncertainty.
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
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope: Enforcement Vacuum Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '0cefaec4-224b-4982-b52e-75cdc332f05a').
narrative_ontology:cs_kernel_codification('0cefaec4-224b-4982-b52e-75cdc332f05a', fixed_text).
narrative_ontology:cs_authority_grounding('0cefaec4-224b-4982-b52e-75cdc332f05a', distributed).
narrative_ontology:cs_reading_relation('0cefaec4-224b-4982-b52e-75cdc332f05a', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('0cefaec4-224b-4982-b52e-75cdc332f05a', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('0cefaec4-224b-4982-b52e-75cdc332f05a', foundational, interpretive_pluralism_is_structural).
narrative_ontology:cs_axiom_status(interpretive_pluralism_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('0cefaec4-224b-4982-b52e-75cdc332f05a', interpretive_pluralism_is_structural, conventional).
narrative_ontology:cs_reference_frame('0cefaec4-224b-4982-b52e-75cdc332f05a', gpl_text_ambiguity).
narrative_ontology:cs_drift_state('0cefaec4-224b-4982-b52e-75cdc332f05a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0cefaec4-224b-4982-b52e-75cdc332f05a', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, legal_counsel).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These projects adhere strictly to the Free Software Foundation's interpretation of the GPL, viewing any ambiguity as a threat to software freedom. They bear the cost of uncertainty and the effort to enforce their interpretation in a vacuum of clear precedent, often through community pressure rather than litigation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects, payer,
    organized, generational, identity_locked, global).

% These ecosystems often operate with a more permissive interpretation of copyleft, sometimes exploiting the lack of definitive precedent to integrate GPL code in ways that strong copyleft advocates would consider non-compliant. They benefit from the flexibility and reduced legal risk due to the enforcement vacuum.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems, agenda_setter,
    institutional, biographical, mobile, global).

% Developers and companies who prioritize clear legal boundaries to avoid future litigation. They face elevated transaction costs in seeking legal advice and risk assessment due to the interpretive pluralism, or choose more permissively licensed alternatives to avoid the ambiguity entirely.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    moderate, biographical, constrained, global).

% Developers and companies who are willing to operate within the ambiguity, assessing risk based on community norms and the likelihood of enforcement. They benefit from the flexibility that the lack of definitive precedent provides, allowing them to combine code in ways that might be challenged under a stricter interpretation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Lawyers specializing in open-source licensing benefit from the ongoing ambiguity, as it creates a demand for their advisory services in risk assessment, compliance strategies, and potential litigation. They navigate the interpretive pluralism on behalf of their clients.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, legal_counsel, beneficiary,
    organized, biographical, arbitrage, national).

% The courts are the ultimate arbiters but have largely remained silent on the specific scope of GPL copyleft, leading to the current enforcement vacuum. They observe the ongoing disputes and community practices, waiting for a case that forces a definitive ruling.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, judicial_systems, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows diverse software projects to coexist under the GPL by providing a flexible, albeit ambiguous, framework for interpreting copyleft obligations, enabling a broader range of code coupling scenarios than a strict interpretation might allow.
% TRANSFER_FUNCTION: Transfers the burden of legal certainty from the licensing text to individual adopters and their legal counsel, who must assess risk based on community norms and enforcement capacity. It also transfers flexibility to pragmatic adopters at the cost of clarity for others.
% ABSENT_VOICES: A unified, globally recognized body with the authority to issue binding interpretations of the GPL's scope. Such a body would reduce ambiguity but is absent due to the decentralized nature of open-source governance and the reluctance of judicial systems to intervene definitively.
% DISAPPEARANCE_RATIONALE: If the enforcement vacuum vanished overnight (e.g., through a universally accepted, definitive judicial precedent), the software licensing landscape would immediately polarize. Projects currently operating in the ambiguous space would be forced to re-license or re-architect, leading to significant disruption and potentially fracturing existing ecosystems.
% FOUNDING_PROBLEM: The GPL was designed to ensure software freedom by requiring derivative works to also be free, but its precise scope regarding code coupling (e.g., dynamic linking, plugins) was not fully defined, leading to interpretive challenges as software architectures evolved.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation continues to advocate for a strong interpretation, while various industry groups and legal scholars publish conflicting guidance. The ongoing debate in legal journals and developer forums, alongside the absence of clear judicial rulings, corroborates that the problem of scope definition remains live and contested by parties outside the direct beneficiaries of ambiguity.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.35) is moderate, reflecting the transaction costs for clarity-seeking adopters and the strategic advantage for pragmatic ones. Suppression (0.45) is also moderate, as the constraint is enforced not by clear legal rulings but by community pressure, risk assessment, and the threat of potential (but rare) litigation. The theater ratio is low (0.1) because the ambiguity is a genuine structural feature, not a performance masking a clear underlying function. The claimed type is tangled_rope because it provides a coordination function (allowing diverse code coupling) but with asymmetric extraction (cost of uncertainty for some, flexibility for others) that requires active, albeit diffuse, enforcement through community norms and legal risk management.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pragmatic adopters, the ambiguity is a feature, allowing for innovation and flexible code integration. From the perspective of FSF-aligned projects, it's a bug, undermining the core intent of copyleft. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing a rope-like constraint and payers experiencing a more snare-like one, even within the same overall tangled_rope structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Pragmatic adopters and legal counsel are beneficiaries, gaining flexibility or advisory fees from the ambiguity. Clarity-seeking adopters and FSF-aligned projects are payers, bearing the costs of uncertainty, risk assessment, or the effort to maintain a strict interpretation. Industry-dominated ecosystems act as agenda-setters by shaping de facto interpretations through their practices and resources, while judicial systems are observers, passively allowing the vacuum to persist.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_intervention_likelihood,
    'What is the likelihood of a definitive judicial ruling on GPL copyleft scope in the near future, and how would it impact the current interpretive pluralism?',
    'Analysis of ongoing litigation, legislative efforts, and judicial trends in intellectual property law. A high-profile case reaching a supreme court equivalent could resolve it.',
    'A definitive ruling would collapse the interpretive pluralism, potentially reclassifying the constraint as a strong_copyleft_reading (if broad) or narrow_scope_reading (if limited), and significantly altering the extractiveness and suppression for all stakeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_intervention_likelihood, empirical, 'Uncertainty about future judicial intervention and its impact on GPL scope.').

omega_variable(
    community_enforcement_efficacy,
    'How effective is community-driven enforcement (e.g., FSF compliance efforts, public shaming) in shaping de facto GPL scope in the absence of judicial precedent?',
    'Empirical study of compliance rates in projects targeted by community enforcement actions versus those that are not. Case studies of projects that changed licensing or architecture due to community pressure.',
    'If community enforcement is highly effective, the ''enforcement vacuum'' is less of a vacuum and more of a distributed, informal enforcement mechanism, potentially increasing the effective suppression and extractiveness for non-compliant actors. If ineffective, the ambiguity persists with lower real-world impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_enforcement_efficacy, empirical, 'The actual power of community norms to enforce GPL scope.').

omega_variable(
    identity_lock_strength_for_fsf_aligned,
    'To what extent is the ''identity_locked'' exit option for FSF-aligned projects a genuine structural constraint versus an ideological preference?',
    'Longitudinal studies of FSF-aligned developers who transition to more permissively licensed projects, examining the psychological and professional costs incurred. Analysis of ''forking'' events where ideological splits lead to new projects.',
    'If identity lock is primarily ideological, the effective suppression is lower than structural measures suggest, as exit is conceptually available but chosen against. If structural (e.g., career path dependence within the FSF ecosystem), then suppression is genuinely high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_strength_for_fsf_aligned, conceptual, 'Structural vs. ideological basis of identity lock for FSF-aligned projects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, identity_coordination).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_copyleft_scope' kernel. This 'enforcement vacuum' reading describes the current state of interpretive pluralism, influencing how both the 'strong_copyleft_reading' and 'narrow_scope_reading' are practically applied and contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
