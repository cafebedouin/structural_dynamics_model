% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope — Enforcement Vacuum Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL copyleft scope has never received definitive judicial
 *   interpretation in any major jurisdiction. This absence creates a licensed
 *   plurality: FSF-aligned projects enforce a strong copyleft reading
 *   (dynamic linking triggers GPL), while industry-dominated ecosystems
 *   operate under a narrow scope reading (aggregation and plugin boundaries
 *   limit copyleft). The constraint is not the license text itself but the
 *   enforcement vacuum — adopters face a constraint whose operative boundary
 *   depends on which interpretive community can enforce its reading in their
 *   specific context. This reading treats the vacuum as the structural
 *   feature: low extractiveness overall, but with asymmetric transaction
 *   costs falling on clarity-seekers and small developers while pragmatic
 *   actors and enforcement-capable communities extract positional benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.22).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope — Enforcement Vacuum Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '8c79c9d5-2671-400c-9993-f2218c946737').
narrative_ontology:cs_kernel_codification('8c79c9d5-2671-400c-9993-f2218c946737', fixed_text).
narrative_ontology:cs_authority_grounding('8c79c9d5-2671-400c-9993-f2218c946737', lineage).
narrative_ontology:cs_interpretation_layer_present('8c79c9d5-2671-400c-9993-f2218c946737').
narrative_ontology:cs_reading_relation('8c79c9d5-2671-400c-9993-f2218c946737', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c79c9d5-2671-400c-9993-f2218c946737', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('8c79c9d5-2671-400c-9993-f2218c946737', foundational, copyleft_scope_is_enforcement_contingent).
narrative_ontology:cs_axiom_status(copyleft_scope_is_enforcement_contingent, holdable).
narrative_ontology:cs_axiom_grounding('8c79c9d5-2671-400c-9993-f2218c946737', copyleft_scope_is_enforcement_contingent, empirically_contingent).
narrative_ontology:cs_axiom('8c79c9d5-2671-400c-9993-f2218c946737', secondary, interpretive_pluralism_is_licensed_by_text).
narrative_ontology:cs_axiom_status(interpretive_pluralism_is_licensed_by_text, holdable).
narrative_ontology:cs_axiom_grounding('8c79c9d5-2671-400c-9993-f2218c946737', interpretive_pluralism_is_licensed_by_text, conventional).
narrative_ontology:cs_reference_frame('8c79c9d5-2671-400c-9993-f2218c946737', gpl_text_as_indeterminate_framework).
narrative_ontology:cs_drift_state('8c79c9d5-2671-400c-9993-f2218c946737', contemporary_licensing_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c79c9d5-2671-400c-9993-f2218c946737', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, small_independent_developers).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__enforcement_vacuum_reading, interpretive_pluralism_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__enforcement_vacuum_reading, enforcement_contingency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the GPL text and associated interpretive infrastructure; enforce strong copyleft reading within their ecosystem through compliance programs and legal action; benefit when their reading prevails in contexts where they have enforcement capacity.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects, beneficiary).

% Deploy GPL-licensed code in commercial products while asserting narrow scope reading; use legal resources to defend aggregation/plugin boundaries; benefit when their reading prevails in contexts where they control enforcement.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems, beneficiary).

% Select GPL for its ecosystem effects while exploiting scope ambiguity to combine with proprietary components; choose integration patterns (dynamic linking, plugins) that minimize compliance risk under either reading; benefit from flexibility the vacuum provides.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Require legal certainty for compliance planning; incur elevated transaction costs for legal review, dual-licensing negotiations, or architectural workarounds; cannot reliably predict enforcement outcome in their jurisdiction; bear costs of ambiguity without capturing its benefits.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    moderate, biographical, constrained, global).

% Lack legal resources to assess or defend either reading; face asymmetric risk from compliance demands by better-resourced parties; cannot afford dual-licensing or architectural segregation; effectively excluded from meaningful participation in the interpretive contest.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, small_independent_developers, payer,
    powerless, immediate, trapped, global).

% Have not produced definitive precedent on GPL scope boundaries; their eventual rulings would collapse the vacuum but none has occurred in major jurisdictions; structural absence is the constraint's enabling condition.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% Produce competing doctrinal analyses supporting each reading; no consensus emerges; their discourse maps the interpretive landscape but does not resolve it; academic debate sustains rather than settles the pluralism.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, legal_scholarship_community, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared licensing framework that enables code sharing across ideologically opposed communities without requiring agreement on derivative work boundaries; the vacuum itself coordinates by allowing both FSF-aligned and industry actors to participate in the same license ecosystem.
% TRANSFER_FUNCTION: Moves compliance certainty and legal risk from well-resourced actors (FSF projects, industry ecosystems) to clarity-seeking adopters and small developers; moves interpretive authority from the license text to whoever holds local enforcement capacity.
% ABSENT_VOICES: End users and downstream recipients who would benefit from a settled scope rule but have no standing in the interpretive contest; jurisdiction-specific regulators in Global South whose courts have never been asked to rule on GPL scope; maintainers of abandoned GPL projects who cannot update licensing terms.
% DISAPPEARANCE_RATIONALE: If the enforcement vacuum were resolved by definitive precedent, one reading would become legally binding across the relevant jurisdiction, collapsing the pluralism; FSF-aligned projects would lose enforcement leverage in industry contexts or industry would lose aggregation defenses; the entire GPL ecosystem would reorganize around the winning boundary.
% FOUNDING_PROBLEM: Create a copyright license that ensures software freedom propagates through derivative works while allowing aggregation with independent works — the derivative/aggregate boundary was left under-specified in GPLv2 Section 2 and GPLv3 Section 5, creating the interpretive space this reading describes.
% FOUNDING_PROBLEM_CORROBORATION: FSF publications attest the derivative work boundary remains live and enforceable; industry legal departments attest the boundary is functionally settled by practice toward narrow scope; academic commentary (e.g., Rosen, Moglen, Nimmer) documents the doctrinal split without resolution; no corroborating source outside the beneficiary sets agrees on status.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.22) because the vacuum does not systematically extract from a fixed victim class — it creates a landscape where extraction is contextual and contingent. Suppression is moderate (0.38) because the constraint persists through the absence of authoritative resolution, not active coercion; the 'enforcement' is the structural fact that no court has ruled. Theater ratio rises over time (0.12→0.28) as both sides perform compliance certainty while privately acknowledging ambiguity. Accessibility collapse is moderate (0.45) because alternative licenses (Apache, MIT, BSD) exist but carry different ecosystem trade-offs. Resistance is moderate (0.52) from clarity-seeking adopters who push for legal certainty or license migration.
 *
 * PERSPECTIVAL GAP:
 *   From FSF-aligned seat: the constraint is a genuine coordination mechanism (strong copyleft) temporarily obscured by judicial silence. From industry seat: the constraint is a narrow coordination tool (aggregation boundary) that FSF overreaches on. From clarity-seeker seat: the constraint is a snare — ambiguity extracts compliance costs without delivering the promised freedom propagation. From small developer seat: the constraint is a piton — the license persists but its scope function has atrophied into unnavigable risk. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   FSF-aligned projects and industry ecosystems are dual agenda-setters/beneficiaries — each benefits when their reading prevails in contexts they control. Pragmatic adopters are beneficiaries who exploit the vacuum's flexibility. Clarity-seeking adopters and small developers are payers: the former pay transaction costs for certainty, the latter pay asymmetric risk. Courts are observers whose structural absence enables the vacuum. The directionality derivation from beneficiary/victim + exit options captures this: trapped small developers sit at high d, mobile pragmatic adopters at low d, constrained clarity-seekers at middle-high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (propagate freedom through derivatives, allow aggregation) remains contested — FSF says live, industry says substantially dead. The arrangement persists because neither side can force a resolution that would disadvantage them; the vacuum serves both enforcement-capable communities. This is not classic mandatrophy (function gone, form remains) but a stable equilibrium of contested function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_vacuum_as_kernel_reading,
    'Is the enforcement vacuum a distinct reading of the GPL copyleft scope kernel, or a meta-observation about the other two readings?',
    'Determine whether adopters explicitly adopt ''enforcement-contingent scope'' as their operational interpretation, or whether the vacuum is merely the absence of adoption between the two substantive readings.',
    'If a distinct reading, it warrants its own constraint story with ε ≈ 0.22; if meta-observation, the two substantive readings should be modeled directly and the vacuum analyzed as their interaction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vacuum_as_kernel_reading, conceptual, 'Whether enforcement contingency is an adopted interpretive position or an analytical construct.').

omega_variable(
    judicial_abstention_persistence,
    'Why have courts in major jurisdictions consistently avoided ruling on GPL scope boundaries for 30+ years?',
    'Analyze procedural posture of GPL cases (settlements, dismissals, narrow grounds); assess whether abstention is strategic (courts avoiding software architecture questions) or structural (cases never present clean scope issue).',
    'If strategic abstention, the vacuum is an active judicial choice — suppression has institutional source. If structural, the vacuum reflects genuine adjudicative difficulty — the constraint emerges from legal architecture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_abstention_persistence, empirical, 'Whether judicial silence on GPL scope is active avoidance or passive absence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (absence of precedent) or internalized (adopters self-censor due to uncertainty)?',
    'Survey adopters in jurisdictions with and without GPL precedent (none exist); compare compliance behavior where enforcement threat is explicit vs. latent; track behavioral changes after high-profile compliance actions.',
    'If internalized, effective suppression is higher than structural measure suggests — adopters carry the vacuum''s chilling effect even in contexts where enforcement is unlikely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the enforcement vacuum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gpl__tr_t7, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 7, 0.18).
narrative_ontology:measurement(gpl__tr_t14, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 14, 0.22).
narrative_ontology:measurement(gpl__tr_t21, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 21, 0.26).
narrative_ontology:measurement(gpl__tr_t28, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 28, 0.28).
narrative_ontology:measurement(gpl__tr_t35, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 35, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(gpl__be_t7, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 7, 0.18).
narrative_ontology:measurement(gpl__be_t14, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 14, 0.2).
narrative_ontology:measurement(gpl__be_t21, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 21, 0.21).
narrative_ontology:measurement(gpl__be_t28, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 28, 0.22).
narrative_ontology:measurement(gpl__be_t35, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 35, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gpl__su_t7, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 7, 0.3).
narrative_ontology:measurement(gpl__su_t14, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 14, 0.34).
narrative_ontology:measurement(gpl__su_t21, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 21, 0.37).
narrative_ontology:measurement(gpl__su_t28, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 28, 0.38).
narrative_ontology:measurement(gpl__su_t35, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 35, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__enforcement_vacuum_reading, 0.08).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the GPL copyleft scope constraint family. The enforcement vacuum reading describes the meta-constraint that emerges from the absence of authoritative resolution between the two substantive readings. All three share kernel gpl_copyleft_scope but instantiate different ε values: strong_copyleft ≈ 0.45 (extraction from proprietary combiners), narrow_scope ≈ 0.15 (coordination with minimal extraction), enforcement_vacuum ≈ 0.22 (transaction costs from pluralism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, organized, 0.15).
constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, institutional, 0.1).
constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
