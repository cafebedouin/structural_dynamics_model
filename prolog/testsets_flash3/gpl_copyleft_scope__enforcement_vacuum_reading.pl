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
 *   This constraint describes the practical reality of GPL copyleft
 *   enforcement in the absence of definitive judicial precedent. The
 *   ambiguity around what constitutes a 'derivative work' (GPL Section 2(b))
 *   creates an 'enforcement vacuum' where different interpretive communities
 *   (e.g., FSF-aligned projects vs. industry-dominated ecosystems) operate
 *   with varying degrees of risk and enforcement capacity. This reading
 *   highlights how the uncertainty itself becomes a structural feature,
 *   allowing a licensed plurality of interpretations to coexist, with actual
 *   constraint depending on context-specific enforcement dynamics. It is a
 *   low-epsilon tangled_rope because it provides a coordination function
 *   (allowing diverse interpretations to coexist without constant litigation)
 *   but also extracts costs from those seeking clarity or strict adherence.
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
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '9c332fb3-6080-420b-86be-7ac0f68ac403').
narrative_ontology:cs_kernel_codification('9c332fb3-6080-420b-86be-7ac0f68ac403', fixed_text).
narrative_ontology:cs_authority_grounding('9c332fb3-6080-420b-86be-7ac0f68ac403', distributed).
narrative_ontology:cs_reading_relation('9c332fb3-6080-420b-86be-7ac0f68ac403', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c332fb3-6080-420b-86be-7ac0f68ac403', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('9c332fb3-6080-420b-86be-7ac0f68ac403', foundational, interpretive_pluralism_is_de_facto_norm).
narrative_ontology:cs_axiom_status(interpretive_pluralism_is_de_facto_norm, holdable).
narrative_ontology:cs_axiom_grounding('9c332fb3-6080-420b-86be-7ac0f68ac403', interpretive_pluralism_is_de_facto_norm, conventional).
narrative_ontology:cs_axiom('9c332fb3-6080-420b-86be-7ac0f68ac403', foundational, enforcement_capacity_defines_scope).
narrative_ontology:cs_axiom_status(enforcement_capacity_defines_scope, holdable).
narrative_ontology:cs_axiom_grounding('9c332fb3-6080-420b-86be-7ac0f68ac403', enforcement_capacity_defines_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('9c332fb3-6080-420b-86be-7ac0f68ac403', gpl_text_ambiguity).
narrative_ontology:cs_drift_state('9c332fb3-6080-420b-86be-7ac0f68ac403', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9c332fb3-6080-420b-86be-7ac0f68ac403', '').
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

% These projects adhere strictly to the Free Software Foundation's interpretation of the GPL, viewing any ambiguity as a threat to software freedom. They bear the cost of uncertainty and the effort to maintain a 'pure' copyleft environment, often through community pressure rather than legal action. Their identity is fused with the strong copyleft principle.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects, payer,
    organized, generational, identity_locked, global).

% These ecosystems, often led by large corporations, interpret the GPL narrowly to maximize proprietary integration. They benefit from the lack of definitive precedent, allowing them to operate in a 'gray area' without direct legal challenge, shaping de facto norms through market power and selective enforcement.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems, agenda_setter,
    institutional, biographical, mobile, global).

% Developers and companies who desire clear legal boundaries for their software projects. They face elevated transaction costs due to the need for extensive legal review and risk assessment to navigate the ambiguous copyleft scope, often opting for more permissive licenses or avoiding GPL-licensed components altogether.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    moderate, biographical, constrained, global).

% Developers and companies who prioritize flexibility and speed. They exploit the ambiguity to integrate GPL-licensed components in ways that might be challenged under a strong copyleft interpretation, but are unlikely to face enforcement in their specific context. They benefit from reduced friction in development and deployment.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters, beneficiary,
    moderate, immediate, mobile, global).

% Lawyers specializing in open-source licensing. The interpretive vacuum creates a constant demand for their services, advising clients on risk mitigation, license compliance strategies, and potential litigation. They benefit from the complexity and uncertainty of the legal landscape.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, legal_counsel, beneficiary,
    organized, biographical, arbitrage, national).

% The courts, which have largely avoided issuing definitive rulings on the precise scope of GPL copyleft, particularly regarding dynamic linking and aggregation. Their inaction maintains the enforcement vacuum, allowing the interpretive plurality to persist. They observe, but do not actively resolve, the ambiguity.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, judicial_system, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows diverse software projects to coexist under the GPL by providing a flexible, albeit ambiguous, framework for interpreting derivative works, enabling a broader range of code coupling than a strict interpretation might permit without explicit legal challenge.
% TRANSFER_FUNCTION: Transfers the burden of legal interpretation and risk assessment from a centralized authority to individual adopters and their legal counsel. It also transfers flexibility to pragmatic adopters at the cost of clarity for others.
% ABSENT_VOICES: A unified, globally recognized body with the authority to issue binding interpretations of the GPL's scope. Such a body would provide clarity but is absent due to the distributed nature of open-source governance and the reluctance of courts to intervene definitively.
% DISAPPEARANCE_RATIONALE: If the enforcement vacuum vanished overnight (e.g., through a definitive global judicial ruling), the software licensing landscape would immediately polarize. Projects relying on ambiguous interpretations would face immediate compliance crises, while strong copyleft advocates would gain significant leverage. The entire open-source ecosystem would undergo a major restructuring.
% FOUNDING_PROBLEM: The GPL was designed to ensure software freedom by requiring derivative works to also be free. The 'derivative work' boundary, however, was left open to interpretation, leading to ambiguity in complex software architectures.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, open-source advocates (including FSF-aligned projects), and industry legal departments all corroborate that the ambiguity around 'derivative work' remains a live and unresolved problem, leading to ongoing debates and risk assessments.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.35) is moderate, reflecting the transaction costs for clarity-seeking adopters and the strategic advantage for pragmatic adopters, but not outright rent-seeking. Suppression (0.45) is also moderate, as it's maintained by the absence of clear legal resolution rather than active coercion, though community pressure and selective enforcement play a role. Theater ratio (0.1) is low, as the ambiguity is a genuine structural feature, not a performance. The constraint is a tangled_rope because it coordinates coexistence of interpretations while extracting costs from those who desire a single, clear interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of FSF-aligned projects, this constraint is a snare, undermining the core principle of copyleft. From pragmatic adopters, it's a rope, offering flexibility. The engine's classification as tangled_rope reflects the hybrid nature: a coordination of interpretive plurality with asymmetric extraction of clarity and certainty.
 *
 * DIRECTIONALITY LOGIC:
 *   Pragmatic adopters and legal counsel are beneficiaries, as they navigate and profit from the ambiguity. Clarity-seeking adopters and FSF-aligned projects are payers, bearing the costs of uncertainty and the effort to maintain their preferred interpretation. Industry-dominated ecosystems act as agenda-setters, leveraging their power to shape de facto interpretations. The judicial system is an observer, whose inaction maintains the vacuum.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_intervention_likelihood,
    'What is the likelihood of a definitive judicial ruling on GPL copyleft scope in a major jurisdiction, and how would it impact the current enforcement vacuum?',
    'Analysis of ongoing litigation, legislative efforts, and judicial trends in key jurisdictions (e.g., US, EU, Germany).',
    'A definitive ruling would collapse the enforcement vacuum, likely shifting the constraint towards a strong_copyleft_reading (if broad) or narrow_scope_reading (if restrictive), fundamentally altering the extractiveness and suppression dynamics for all stakeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_intervention_likelihood, empirical, 'Uncertainty regarding future legal clarity on GPL scope.').

omega_variable(
    community_enforcement_efficacy,
    'How effective is community-driven enforcement (e.g., FSF compliance efforts, community shaming) in shaping de facto GPL interpretations in the absence of judicial precedent?',
    'Empirical study of compliance rates, project forks, and developer behavior in response to community pressure versus formal legal threats.',
    'If community enforcement is highly effective, the ''vacuum'' is less pronounced, and the constraint leans more towards a strong_copyleft_reading in certain ecosystems. If ineffective, the vacuum is deeper, reinforcing the current reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_enforcement_efficacy, empirical, 'The actual power of non-judicial enforcement mechanisms.').

omega_variable(
    interpretive_pluralism_sustainability,
    'Is the current state of interpretive pluralism a stable equilibrium, or does it inherently generate pressure for resolution?',
    'Longitudinal study of developer sentiment, legal costs, and project failures/successes under ambiguity. Conceptual analysis of ''licensed plurality'' as a stable state versus a transitional phase.',
    'If stable, the current tangled_rope classification holds. If inherently unstable, the constraint is a scaffold, building towards a future, more definitive state (either strong or narrow).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_pluralism_sustainability, conceptual, 'Whether the ambiguity is a stable feature or a temporary phase.').


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
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
