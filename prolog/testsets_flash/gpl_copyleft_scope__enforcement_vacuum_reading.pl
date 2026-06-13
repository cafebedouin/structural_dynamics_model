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
 *   This constraint describes the GPL copyleft's scope as defined by an
 *   'enforcement vacuum' – the absence of definitive judicial precedent. This
 *   allows for a 'licensed plurality' of interpretations, where the actual
 *   constraint experienced by adopters depends on the specific interpretive
 *   community and its enforcement capacity (e.g., FSF-aligned projects vs.
 *   industry-dominated ecosystems). The uncertainty itself becomes a
 *   structural feature, benefiting pragmatic adopters who exploit ambiguity
 *   and creating elevated transaction costs for those seeking clarity. This
 *   is one reading of the 'gpl_copyleft_scope' kernel.
 *
 * KEY AGENTS:
 *   - pragmatic_adopters: Beneficiary (moderate/mobile) – benefits from interpretive flexibility.
 *   - clarity_seeking_adopters: Payer (moderate/constrained) – bears costs of uncertainty and legal review.
 *   - gpl_enforcement_advocates: Agenda Setter (organized/constrained) – seeks to enforce a strong interpretation but is limited by lack of precedent.
 *   - legal_counsel: Beneficiary (powerful/arbitrage) – profits from advising on interpretive ambiguity.
 *   - judicial_system: Observer (institutional/analytical) – has not provided definitive rulings, thus maintaining the vacuum.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.35).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.2).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.2).
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
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '77dc4a05-9c3c-4aea-bd8c-dbe47b20107a').
narrative_ontology:cs_kernel_codification('77dc4a05-9c3c-4aea-bd8c-dbe47b20107a', fixed_text).
narrative_ontology:cs_authority_grounding('77dc4a05-9c3c-4aea-bd8c-dbe47b20107a', distributed).
narrative_ontology:cs_reading_relation('77dc4a05-9c3c-4aea-bd8c-dbe47b20107a', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('77dc4a05-9c3c-4aea-bd8c-dbe47b20107a', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('77dc4a05-9c3c-4aea-bd8c-dbe47b20107a', foundational, interpretive_pluralism_is_operative).
narrative_ontology:cs_axiom_status(interpretive_pluralism_is_operative, holdable).
narrative_ontology:cs_axiom_grounding('77dc4a05-9c3c-4aea-bd8c-dbe47b20107a', interpretive_pluralism_is_operative, conventional).
narrative_ontology:cs_axiom('77dc4a05-9c3c-4aea-bd8c-dbe47b20107a', foundational, judicial_precedent_is_absent).
narrative_ontology:cs_axiom_status(judicial_precedent_is_absent, holdable).
narrative_ontology:cs_axiom_grounding('77dc4a05-9c3c-4aea-bd8c-dbe47b20107a', judicial_precedent_is_absent, empirically_contingent).
narrative_ontology:cs_reference_frame('77dc4a05-9c3c-4aea-bd8c-dbe47b20107a', gpl_text_without_judicial_gloss).
narrative_ontology:cs_drift_state('77dc4a05-9c3c-4aea-bd8c-dbe47b20107a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('77dc4a05-9c3c-4aea-bd8c-dbe47b20107a', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, legal_counsel).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_enforcement_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These developers and companies interpret the GPL's scope flexibly, leveraging the lack of definitive precedent to integrate GPL code in ways that might be considered non-compliant by stricter interpretations, but which face low enforcement risk in their specific contexts. They benefit from the ambiguity by gaining flexibility.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters, beneficiary,
    moderate, biographical, mobile, global).

% These developers and companies desire clear legal boundaries for GPL compliance to avoid potential litigation. The enforcement vacuum creates uncertainty, leading to elevated transaction costs for legal review and conservative licensing decisions, even when stricter interpretations might not be enforced.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    moderate, biographical, constrained, global).

% Organizations like the Free Software Foundation (FSF) actively promote and enforce a strong interpretation of the GPL's copyleft. In this reading, their enforcement capacity is limited by the absence of judicial precedent, forcing them to rely on community pressure and out-of-court settlements, which reduces their effective power.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_enforcement_advocates, agenda_setter,
    organized, generational, constrained, global).

% Lawyers specializing in open-source licensing benefit from the interpretive ambiguity, as it creates a continuous demand for their services to advise clients on risk assessment, compliance strategies, and potential litigation. They navigate the 'licensed plurality' for their clients.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, legal_counsel, beneficiary,
    powerful, biographical, arbitrage, global).

% The courts are the ultimate arbiters of legal precedent. In this reading, their 'absence' of definitive rulings on GPL copyleft scope is the core structural feature, allowing the interpretive vacuum to persist. They observe disputes but have not yet provided the clarifying judgment.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, judicial_system, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, legal_counsel).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows diverse software projects and companies to interact with GPL-licensed code by providing a flexible, albeit uncertain, framework for interpretation, enabling a broader range of integration patterns than a strictly enforced interpretation might permit.
% TRANSFER_FUNCTION: Transfers flexibility and reduced immediate compliance burden to pragmatic adopters, while transferring increased legal risk and transaction costs to clarity-seeking adopters and reduced enforcement efficacy to GPL advocates.
% ABSENT_VOICES: A unified, authoritative body for open-source license interpretation, backed by clear judicial precedent, is absent. Such a body would provide the clarity that many adopters seek, but its absence is precisely what defines this reading.
% DISAPPEARANCE_RATIONALE: If the enforcement vacuum disappeared overnight (e.g., through a landmark Supreme Court ruling), the landscape of GPL adoption and integration would fundamentally shift. Pragmatic adopters would face immediate compliance challenges, clarity-seeking adopters would gain certainty, and the power dynamics of GPL enforcement would be redefined.
% FOUNDING_PROBLEM: The GPL was created to ensure software freedom and prevent proprietary enclosure of derivative works, but its precise scope regarding 'derivative works' in complex software ecosystems was never fully litigated or clarified by definitive judicial precedent.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing debates in legal scholarship, industry forums, and open-source communities, along with the continued reliance on community norms and private settlements rather than clear court rulings, corroborate that the problem of interpretive ambiguity remains live. Legal experts and industry analysts outside the FSF consistently highlight this lack of clarity.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it provides a coordination function (allowing diverse GPL code integration) but with asymmetric extraction. Pragmatic adopters benefit from flexibility (low extraction), while clarity-seeking adopters and enforcement advocates bear costs (higher extraction due to uncertainty and reduced efficacy). Extractiveness is moderate (0.35) due to the transaction costs of navigating ambiguity. Suppression is low (0.20) because the constraint's persistence relies on the *absence* of definitive enforcement, rather than active coercion. Theater ratio is low (0.10) as the core function is the actual navigation of ambiguity, not performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Pragmatic adopters experience this as a flexible Rope, allowing them to operate with less friction. Clarity-seeking adopters experience it as a Snare, trapping them in a cycle of legal uncertainty and conservative choices. GPL enforcement advocates see it as a degraded Rope or Piton, where their intended coordination function is undermined by the lack of external enforcement. The judicial system, as an observer, simply notes the absence of a definitive ruling.
 *
 * DIRECTIONALITY LOGIC:
 *   Pragmatic adopters are beneficiaries (d=0.0-0.2) as they gain flexibility. Clarity-seeking adopters are targets (d=0.7-0.9) due to elevated costs and risk. GPL enforcement advocates are also targets (d=0.6-0.8) as their enforcement efforts are blunted. Legal counsel are beneficiaries (d=0.0-0.1) as the ambiguity creates demand for their services. The judicial system is an analytical observer (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading highlights how the *absence* of a clear mandate (from judicial precedent) itself creates a constraint. It prevents mislabeling the situation as a simple Rope (ignoring the costs to clarity-seekers) or a Snare (ignoring the flexibility gained by pragmatic adopters). The constraint's 'mandate' is effectively to manage interpretive pluralism, which is still 'live' but contested in its efficacy and fairness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_intervention_likelihood,
    'What is the likelihood of a definitive judicial ruling on GPL copyleft scope emerging in the near future?',
    'Analysis of ongoing litigation, legislative efforts, and industry trends that might precipitate a landmark case.',
    'A high likelihood of intervention would shift this constraint towards either a strong_copyleft_reading (if FSF wins) or a narrow_scope_reading (if industry wins), fundamentally altering its extractiveness and suppression. A low likelihood suggests the enforcement vacuum will persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_intervention_likelihood, empirical, 'The probability of a legal event resolving the interpretive ambiguity.').

omega_variable(
    community_norm_vs_legal_precedent,
    'To what extent do community norms and social pressure effectively substitute for legal precedent in enforcing GPL copyleft?',
    'Empirical study of compliance rates in projects primarily governed by community norms versus those with higher legal scrutiny.',
    'If community norms are highly effective, the ''enforcement vacuum'' is less impactful, and the constraint leans closer to a Rope. If they are weak, the constraint is more extractive for clarity-seekers and less effective for advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_norm_vs_legal_precedent, empirical, 'The efficacy of non-judicial enforcement mechanisms.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''enforcement vacuum'' reading, or is it merely a temporary state within a broader ''strong copyleft'' or ''narrow scope'' reading?',
    'Longitudinal analysis of legal and community discourse: if the ''plurality'' persists as a stable feature, it''s a distinct reading; if it resolves into one of the others, it was a transitional phase.',
    'If it''s a distinct reading, its classification as a low-epsilon Tangled Rope is stable. If it''s transitional, the underlying classification would be either a Snare (strong copyleft) or a Rope (narrow scope), with the current state being a period of high uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Whether the ''enforcement vacuum'' is a stable reading or a transient state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 1991, 0.05).
narrative_ontology:measurement(gpl__tr_t1998, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 1998, 0.07).
narrative_ontology:measurement(gpl__tr_t2005, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2005, 0.08).
narrative_ontology:measurement(gpl__tr_t2012, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2012, 0.09).
narrative_ontology:measurement(gpl__tr_t2018, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2018, 0.09).
narrative_ontology:measurement(gpl__tr_t2024, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 1991, 0.2).
narrative_ontology:measurement(gpl__be_t1998, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 1998, 0.25).
narrative_ontology:measurement(gpl__be_t2005, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2005, 0.3).
narrative_ontology:measurement(gpl__be_t2012, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2012, 0.33).
narrative_ontology:measurement(gpl__be_t2018, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2018, 0.34).
narrative_ontology:measurement(gpl__be_t2024, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 1991, 0.1).
narrative_ontology:measurement(gpl__su_t1998, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 1998, 0.13).
narrative_ontology:measurement(gpl__su_t2005, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2005, 0.16).
narrative_ontology:measurement(gpl__su_t2012, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2012, 0.18).
narrative_ontology:measurement(gpl__su_t2018, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2018, 0.19).
narrative_ontology:measurement(gpl__su_t2024, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
