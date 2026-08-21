% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Hybrid of Sovereign Legitimacy
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint describes the constitutional hybrid reading of sovereign
 *   legitimacy, where authority is dual-sourced from inherited ceremonial
 *   roles and delegated political power, with constitutional law mediating
 *   the boundary. This reading is a compromise that seeks to stabilize
 *   governance by integrating historical traditions with modern democratic
 *   demands. The metrics reflect a system that has historically reduced
 *   extractiveness from pure forms but maintains a moderate level of
 *   suppression against those who advocate for either absolute monarchy or
 *   pure republicanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.45).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.55).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Hybrid of Sovereign Legitimacy").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '8f522970-7f43-4643-befe-b37bf5ea61e2').
narrative_ontology:cs_kernel_codification('8f522970-7f43-4643-befe-b37bf5ea61e2', formalized).
narrative_ontology:cs_authority_grounding('8f522970-7f43-4643-befe-b37bf5ea61e2', practice).
narrative_ontology:cs_interpretation_layer_present('8f522970-7f43-4643-befe-b37bf5ea61e2').
narrative_ontology:cs_reading_relation('8f522970-7f43-4643-befe-b37bf5ea61e2', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('8f522970-7f43-4643-befe-b37bf5ea61e2', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_axiom('8f522970-7f43-4643-befe-b37bf5ea61e2', foundational, authority_is_dual_sourced).
narrative_ontology:cs_axiom_status(authority_is_dual_sourced, holdable).
narrative_ontology:cs_axiom_grounding('8f522970-7f43-4643-befe-b37bf5ea61e2', authority_is_dual_sourced, deontological).
narrative_ontology:cs_axiom('8f522970-7f43-4643-befe-b37bf5ea61e2', foundational, constitutional_law_mediates_legitimacy_boundary).
narrative_ontology:cs_axiom_status(constitutional_law_mediates_legitimacy_boundary, holdable).
narrative_ontology:cs_axiom_grounding('8f522970-7f43-4643-befe-b37bf5ea61e2', constitutional_law_mediates_legitimacy_boundary, conventional).
narrative_ontology:cs_reference_frame('8f522970-7f43-4643-befe-b37bf5ea61e2', post_glorious_revolution_settlement).
narrative_ontology:cs_drift_state('8f522970-7f43-4643-befe-b37bf5ea61e2', contemporary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8f522970-7f43-4643-befe-b37bf5ea61e2', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_scholars).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, citizens).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_monarchists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, pure_republicans).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ceremonial and symbolic authority, along with associated status and income, but exercises political power only within constitutionally defined limits. Their legitimacy is inherited but bounded by law.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, generational, constrained, national).

% Exercise delegated political authority, including legislative and executive power, derived from popular consent. They benefit from a stable constitutional framework that legitimizes their rule alongside inherited authority.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    institutional, biographical, mobile, national).

% Benefit from the complexity and ongoing need for interpretation of the constitutional boundary between inherited and delegated authority. Their expertise is central to mediating disputes and shaping legal precedent.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_scholars, beneficiary,
    analytical, biographical, analytical, global).

% Benefit from the political stability and national unity fostered by the hybrid system, which reconciles historical traditions with modern democratic principles. They participate in delegated authority through elections.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, citizens, beneficiary,
    moderate, biographical, constrained, national).

% Bear the cost of their claim to absolute, divinely sanctioned monarchical power being denied by the constitutional framework. Their ideological commitment makes exit from this position unthinkable, but their political influence is marginalized.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_monarchists, payer,
    powerless, generational, identity_locked, national).

% Bear the cost of their claim to pure popular sovereignty, which rejects any role for inherited authority, being compromised by the constitutional hybrid. Their ideological commitment makes exit from this position unthinkable, but their political influence is marginalized.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, pure_republicans, payer,
    powerless, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles competing claims to legitimate authority (inherited vs. delegated) into a stable system of governance, preventing civil conflict over fundamental principles and fostering national unity.
% TRANSFER_FUNCTION: Transfers ceremonial status and some income to the hereditary monarch, and policy-making power to elected officials, in exchange for political stability and a defined legal framework that legitimizes both sources.
% ABSENT_VOICES: Those who advocate for a purely absolute monarchy or a purely direct democracy are structurally excluded; their foundational claims are denied by the constitutional compromise, and they are marginalized from mainstream political discourse.
% DISAPPEARANCE_RATIONALE: If the constitutional hybrid vanished, the underlying tension between inherited and delegated authority would resurface, likely leading to political instability, constitutional crises, or even civil unrest as factions vie for a pure form of legitimacy, disrupting governance and national identity.
% FOUNDING_PROBLEM: How to achieve stable governance and national unity in societies with historical monarchical traditions while also incorporating modern demands for popular representation and democratic accountability, without resorting to revolution or civil war.
% FOUNDING_PROBLEM_CORROBORATION: Political historians, constitutional lawyers, and comparative political scientists from various academic institutions corroborate the historical and ongoing nature of this problem, citing numerous examples of states navigating this tension and the challenges faced by those that failed to do so.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is moderate (0.45) because the hybrid system, while a compromise, still involves transfers of status and power that are not purely consensual, and it imposes costs on those who seek purer forms of legitimacy. Suppression is moderate (0.55) as the constitutional framework actively marginalizes absolutist and pure republican claims, requiring enforcement to maintain the delicate balance. Theater ratio is low (0.15) because the ceremonial aspects, while symbolic, genuinely contribute to national identity and stability, rather than being purely performative. The decreasing extractiveness and suppression over time reflect the historical trend in many constitutional monarchies towards greater democratic accountability and less direct political power for the monarch.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries (monarch, elected officials, citizens), the hybrid is a stable and legitimate 'rope' that coordinates diverse claims. From the perspective of the victims (absolutist monarchists, pure republicans), it is a 'snare' that suppresses their preferred form of legitimacy. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary monarch and elected officials are beneficiaries, as the constraint provides a legitimate framework for their respective roles. Constitutional scholars benefit from the ongoing need for interpretation. Citizens are beneficiaries of the stability and representation offered. Absolutist monarchists and pure republicans are victims, as their foundational claims are denied or severely constrained by the hybrid's core tenets. Their identity-locked exit options reflect their ideological commitment to positions outside the constitutional compromise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_stability_vs_contradiction,
    'Is the balance between inherited and delegated authority in the constitutional hybrid genuinely stable, or does it contain inherent contradictions that will eventually lead to a pure form (monarchical or republican)?',
    'Long-term historical and comparative political analysis of constitutional monarchies under significant internal or external stress, examining instances of successful adaptation versus collapse into pure forms.',
    'If inherently contradictory, the constraint''s classification would shift towards a ''scaffold'' (temporary support) or ''tangled_rope'' (unstable compromise) masking an unstable power dynamic. If demonstrably stable, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_stability_vs_contradiction, conceptual, 'Assessing the long-term structural stability of the constitutional hybrid.').

omega_variable(
    ceremonial_cohesion_vs_theatricality,
    'To what extent does the ceremonial authority of the monarch still provide genuine social cohesion and national identity, versus merely serving as a symbolic cover for political power?',
    'Sociological studies of public sentiment, national identity formation, and crisis response in constitutional monarchies, particularly during periods of political or social upheaval.',
    'If cohesion is minimal, the ceremonial aspect is largely theatrical, increasing the ''theater_ratio'' and potentially pushing the constraint towards a ''piton'' if its functional role atrophies. If significant, it reinforces the coordination function and legitimacy of the hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_cohesion_vs_theatricality, empirical, 'Evaluating the functional role of ceremonial authority in the hybrid system.').

omega_variable(
    constitutional_mediation_neutrality,
    'Is the constitutional law mediating the boundary between inherited and delegated authority genuinely neutral, or does its interpretation subtly favor one source of authority over the other?',
    'Detailed legal scholarship analyzing constitutional precedents, judicial decisions, and legislative reforms over time, focusing on how boundary disputes are resolved and whether a consistent bias emerges.',
    'If biased, the constraint''s ''extractiveness'' would be higher for the disfavored party, and ''suppression'' would be more targeted, potentially shifting the classification towards a ''tangled_rope'' or ''snare'' for that specific seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_mediation_neutrality, empirical, 'Assessing the impartiality of constitutional interpretation in the hybrid system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 1700, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1700, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(sove_tr_t1800, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(sove_tr_t1900, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(sove_tr_t1950, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1950, 0.14).
narrative_ontology:measurement(sove_tr_t2000, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(sove_tr_t2020, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(sove_be_t1700, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1700, 0.6).
narrative_ontology:measurement(sove_be_t1800, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1800, 0.55).
narrative_ontology:measurement(sove_be_t1900, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(sove_be_t1950, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement(sove_be_t2000, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement(sove_be_t2020, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1700, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1700, 0.7).
narrative_ontology:measurement(sove_su_t1800, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1800, 0.65).
narrative_ontology:measurement(sove_su_t1900, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(sove_su_t1950, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(sove_su_t2000, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 2000, 0.56).
narrative_ontology:measurement(sove_su_t2020, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, national_identity_formation).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, parliamentary_sovereignty_doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
