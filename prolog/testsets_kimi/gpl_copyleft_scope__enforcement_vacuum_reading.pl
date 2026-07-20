% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: GPL Copyleft Scope â Enforcement Vacuum Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL copyleft scope kernel is contested between strong copyleft,
 *   narrow scope, and enforcement vacuum readings. This story instantiates
 *   the enforcement vacuum reading: the absence of definitive judicial
 *   precedent on what constitutes a derivative work under GPL Section 2(b)
 *   creates a licensed plurality where the actual constraint on any given
 *   actor depends on which interpretive community holds enforcement capacity
 *   in their specific context. FSF-aligned projects maintain strong copyleft
 *   norms through selective enforcement and community sanction, while
 *   industry-dominated ecosystems exploit the ambiguity to integrate GPL code
 *   with proprietary systems. The uncertainty itself becomes a structural
 *   feature that differentially shapes behavior across contexts.
 *
 * KEY AGENTS:
 *   - fsf_enforcement_network: Primary agenda-setter (organized/global) â promotes strong copyleft interpretation, enforces selectively in their sphere.
 *   - industry_legal_consortia: Secondary agenda-setter (institutional/global) â advances narrow interpretations to enable proprietary integration.
 *   - pragmatic_integrators: Primary beneficiary (powerful/global) â exploit ambiguity for licensing flexibility.
 *   - risk_averse_adopters: Primary payer (moderate/global) â bear transaction costs and compliance paralysis from uncertainty.
 *   - copyleft_compliant_distributors: Secondary payer (moderate/global) â bear competitive disadvantage when others exploit the vacuum.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.32).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope â Enforcement Vacuum Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, 'b5c7c0db-e5bc-4f2b-b450-1ffe3d3b9628').
narrative_ontology:cs_kernel_codification('b5c7c0db-e5bc-4f2b-b450-1ffe3d3b9628', fixed_text).
narrative_ontology:cs_authority_grounding('b5c7c0db-e5bc-4f2b-b450-1ffe3d3b9628', distributed).
narrative_ontology:cs_reading_relation('b5c7c0db-e5bc-4f2b-b450-1ffe3d3b9628', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5c7c0db-e5bc-4f2b-b450-1ffe3d3b9628', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('b5c7c0db-e5bc-4f2b-b450-1ffe3d3b9628', foundational, scope_as_enforcement_construct).
narrative_ontology:cs_axiom_status(scope_as_enforcement_construct, holdable).
narrative_ontology:cs_axiom_grounding('b5c7c0db-e5bc-4f2b-b450-1ffe3d3b9628', scope_as_enforcement_construct, empirically_contingent).
narrative_ontology:cs_reference_frame('b5c7c0db-e5bc-4f2b-b450-1ffe3d3b9628', enforceable_scope_as_validity).
narrative_ontology:cs_drift_state('b5c7c0db-e5bc-4f2b-b450-1ffe3d3b9628', contemporary_ecosystem_divergence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b5c7c0db-e5bc-4f2b-b450-1ffe3d3b9628', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_integrators).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_enforcement_network).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, risk_averse_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, copyleft_compliant_distributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promotes and selectively enforces a strong copyleft interpretation of the GPL through litigation threats, license stewardship, and community sanction. Their enforcement capacity is real but geographically and sectorally limited to spheres where they have standing and resources.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_enforcement_network, agenda_setter,
    organized, generational, mobile, global).

% Advances narrow interpretations of derivative work scope through legal opinions, lobbying, and amicus briefing. Enables proprietary integration of GPL components by producing doctrinal cover that reduces perceived legal risk for commercial adopters.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_legal_consortia, agenda_setter,
    institutional, biographical, arbitrage, global).

% Large technology firms and vendors who combine GPL code with proprietary systems, relying on ambiguity and narrow legal opinions to avoid triggering copyleft obligations. They capture flexibility value from the absence of definitive precedent.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_integrators, beneficiary,
    powerful, biographical, arbitrage, global).

% Small and medium enterprises, startups, and individual developers who face elevated legal costs and compliance paralysis due to uncertainty about derivative work boundaries. They either pay for expensive legal review or avoid valuable GPL software entirely.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, risk_averse_adopters, payer,
    moderate, biographical, constrained, global).

% Distributors and developers who fully comply with source-release obligations, but compete against pragmatic integrators who exploit the vacuum to free-ride on GPL code without corresponding source distribution. They bear a competitive disadvantage created by uneven enforcement.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, copyleft_compliant_distributors, payer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common legal text (the GPL) that enables software sharing and modification while preserving source-code availability norms, serving as a reference point across heterogeneous development communities with divergent enforcement capacities.
% TRANSFER_FUNCTION: Moves legal risk and compliance flexibility from clarity-seeking adopters to pragmatic integrators; moves ideological enforcement leverage from the judicial void to FSF-aligned communities who can credibly threaten action in their sphere.
% ABSENT_VOICES: Definitive appellate courts in major jurisdictions are structurally absent from the interpretive conversation; small downstream users without legal counsel are excluded; independent legal scholars are drowned out by institutional legal departments and advocacy organizations.
% DISAPPEARANCE_RATIONALE: A definitive judicial ruling on derivative work scope under GPL Section 2(b) would collapse the current licensed plurality into a single legal meaning. Pragmatic integrators would face clarified compliance costs or freedoms; risk-averse adopters would re-enter GPL markets; and the present ecosystem structured around interpretive ambiguity would reorganize around a settled rule.
% FOUNDING_PROBLEM: How to ensure software freedoms propagate to downstream users while enabling commercial adoption and derivative development, in the absence of a judiciary specialized in software licensing and before widespread industry legal infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: FSF attests the founding problem remains live and the text is sufficient. Industry legal consortia and risk-averse adopters attest the ambiguity has created a secondary problem of legal uncertainty that undermines the original coordination goal. Independent legal scholarship outside both beneficiary camps corroborates the persistent interpretive gap.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.32, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.32) because the primary effect is interpretive uncertainty rather than direct rent extraction; the constraint extracts via transaction costs and risk asymmetry. Suppression is moderate (0.38): the FSF's selective enforcement suppresses some narrow-interpretation behavior in their sphere, but industry spheres operate with low suppression. Theater ratio is moderate-high (0.45) because both camps maintain strong rhetorical positions that exceed their enforcement capacity â FSF asserts universal strong copyleft while litigating rarely; industry asserts compliance while relying on weak legal opinions. Accessibility collapse is moderate (0.42) because alternatives (BSD, MIT, proprietary) exist but network effects and GPL's prevalence in certain stacks make exit costly. Resistance is moderate-high (0.58) due to active legal and ideological contestation between the two interpretive communities.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF enforcement network seat, the constraint is a coordination mechanism preserving software freedom against proprietary capture; the vacuum is a regrettable but temporary feature pending better enforcement. From the pragmatic integrator seat, the vacuum is a beneficial flexibility that enables valuable commercial innovation. From the risk-averse adopter seat, the same structure is an extractive uncertainty tax that forces expensive legal review or avoidance of useful code. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Pragmatic integrators and the FSF enforcement network both sit on the beneficiary side of the directionality derivation, but for different reasons: integrators capture flexibility rents from the absence of clarity, while FSF captures ideological coordination leverage and selective enforcement power. Risk-averse adopters and compliant distributors are the targets, bearing transaction costs and competitive disadvantage respectively. The industry legal consortia sit closer to the beneficiary end due to arbitrage-grade exit and institutional power, though their role is agenda-setting rather than direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the enforcement vacuum as a pure Mountain (natural law of licensing) or pure Snare (intentional extraction). The genuine coordination function â providing a shared legal framework for copyleft â is real but entangled with the asymmetric extraction imposed on clarity-seeking actors. The absence of judicial precedent is not a natural fact but a constructed institutional gap that differentially benefits actors with legal resources. Mandatrophy would occur if a definitive precedent rendered the vacuum obsolete but the interpretive communities maintained the old rhetoric; current measurements show active, not atrophied, contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint a reading of the GPL copyleft scope kernel, and how do sibling readings alter the structural classification?',
    'Comparison with sibling constraints strong_copyleft_reading and narrow_scope_reading in the compiled corpus; evaluate whether the epsilon difference justifies separate stories.',
    'If sibling readings collapse into one constraint, reclassify as a single contested constraint with higher conceptual variance; if they remain distinct, the enforcement vacuum reading retains its low-epsilon tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Commit-frame uncertainty: this constraint is one reading of a contested kernel.').

omega_variable(
    beneficiary_of_vacuum,
    'Do the gains from interpretive ambiguity accrue primarily to pragmatic industry integrators, or to FSF-aligned enforcers who maintain ideological leverage?',
    'Empirical study of enforcement actions, legal opinion markets, and compliance tooling revenue; map which actors capture value from the persistence of uncertainty.',
    'If industry captures the gains, the directionality vector points from risk-averse adopters toward powerful integrators; if FSF captures them, the vector points toward ideological enforcement networks. This affects the computed extraction asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_of_vacuum, empirical, 'Uncertainty rent capture: who benefits from the enforcement vacuum.').

omega_variable(
    vacuum_mechanism_ambiguity,
    'Is the enforcement vacuum maintained intentionally by actors who benefit from ambiguity, or is it an inertial failure of the judicial system to address a technical question?',
    'Trace funding and litigation decisions of major GPL enforcers to determine whether test cases are avoided strategically or resource constraints prevent them.',
    'If intentional, the constraint is actively maintained extraction; if inertial, it may be a piton or degraded scaffold. This informs whether the theater ratio represents performative maintenance or institutional incapacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vacuum_mechanism_ambiguity, empirical, 'Intentional ambiguity versus judicial inertia as the vacuum source.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 0, 33).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_envac_tr_t0, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gpl_envac_tr_t6, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(gpl_envac_tr_t12, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(gpl_envac_tr_t18, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(gpl_envac_tr_t24, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement(gpl_envac_tr_t33, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 33, 0.45).

% Extraction over time
narrative_ontology:measurement(gpl_envac_be_t0, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gpl_envac_be_t6, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 6, 0.25).
narrative_ontology:measurement(gpl_envac_be_t12, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(gpl_envac_be_t18, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 18, 0.3).
narrative_ontology:measurement(gpl_envac_be_t24, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement(gpl_envac_be_t33, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 33, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gpl_copyleft_scope__enforcement_vacuum_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, identity_coordination).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).

% DUAL FORMULATION NOTE:
% This constraint is one decomposition of the gpl_copyleft_scope kernel. The kernel conflates three structurally distinct claims: strong copyleft (high extraction on integrators), narrow scope (low extraction, high permissiveness), and enforcement vacuum (uncertainty as structural feature). Each reading has distinct epsilon, stakeholder directionalities, and victim/beneficiary structures. This story models the enforcement vacuum reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
