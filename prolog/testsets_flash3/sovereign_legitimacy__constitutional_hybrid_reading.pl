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
 *   human_readable: Constitutional Hybrid Reading of Sovereign Legitimacy
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint describes the 'constitutional hybrid' reading of
 *   sovereign legitimacy, where authority is dual-sourced: ceremonial power
 *   is inherited (e.g., a monarch), and political power is delegated (e.g.,
 *   elected parliament), with constitutional law mediating the boundary. This
 *   reading aims to provide stability by blending tradition with modern
 *   democratic principles. It is one reading of the 'sovereign_legitimacy'
 *   kernel, distinct from purely monarchical or republican interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.35).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.45).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Hybrid Reading of Sovereign Legitimacy").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '6b18f793-5b67-4492-8a54-ae95512bdc1d').
narrative_ontology:cs_kernel_codification('6b18f793-5b67-4492-8a54-ae95512bdc1d', formalized).
narrative_ontology:cs_authority_grounding('6b18f793-5b67-4492-8a54-ae95512bdc1d', lineage).
narrative_ontology:cs_interpretation_layer_present('6b18f793-5b67-4492-8a54-ae95512bdc1d').
narrative_ontology:cs_reading_relation('6b18f793-5b67-4492-8a54-ae95512bdc1d', sovereign_legitimacy__monarchical_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b18f793-5b67-4492-8a54-ae95512bdc1d', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_axiom('6b18f793-5b67-4492-8a54-ae95512bdc1d', foundational, legitimacy_dual_sourced).
narrative_ontology:cs_axiom_status(legitimacy_dual_sourced, holdable).
narrative_ontology:cs_axiom_grounding('6b18f793-5b67-4492-8a54-ae95512bdc1d', legitimacy_dual_sourced, conventional).
narrative_ontology:cs_axiom('6b18f793-5b67-4492-8a54-ae95512bdc1d', foundational, constitutional_mediation_necessary).
narrative_ontology:cs_axiom_status(constitutional_mediation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('6b18f793-5b67-4492-8a54-ae95512bdc1d', constitutional_mediation_necessary, instrumental).
narrative_ontology:cs_reference_frame('6b18f793-5b67-4492-8a54-ae95512bdc1d', post_glorious_revolution_settlement).
narrative_ontology:cs_drift_state('6b18f793-5b67-4492-8a54-ae95512bdc1d', contemporary_era_of_democratic_expansion, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('6b18f793-5b67-4492-8a54-ae95512bdc1d', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_scholars).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, pure_monarchists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, pure_republicans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains symbolic status, ceremonial duties, and state-funded income, but exercises no direct political power. Their legitimacy is inherited, but constrained by constitutional law. Exit means abdication and loss of status.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, generational, identity_locked, national).

% Exercise delegated political power through democratic processes, but operate within a constitutional framework that acknowledges a separate, inherited symbolic authority. Their legitimacy is popular, but bounded by constitutional precedent. Exit means electoral defeat or resignation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, agenda_setter,
    institutional, biographical, constrained, national).

% Benefit from the complexity and ongoing need for interpretation of the dual-sourced legitimacy. Their expertise is crucial in mediating boundary disputes and evolving constitutional norms. Exit means irrelevance of their field.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_scholars, beneficiary,
    analytical, generational, analytical, national).

% Bear the cost of a diluted, ceremonial monarchy, seeing it as a betrayal of inherited divine right. Their ideal of absolute sovereignty is suppressed by the constitutional framework. Exit means abandoning their core political identity.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, pure_monarchists, payer,
    moderate, generational, identity_locked, national).

% Bear the cost of retaining a hereditary head of state, seeing it as an affront to popular sovereignty and equality. Their ideal of a purely elected government is constrained by the hybrid system. Exit means abandoning their core political identity.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, pure_republicans, payer,
    moderate, generational, identity_locked, national).

% Benefit from the stability and continuity offered by the hybrid system, which often avoids the radical ruptures of pure forms. They participate in democratic processes but also identify with national symbols embodied by the monarchy. Their options are limited by the existing constitutional order.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, citizens, beneficiary,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transfer of power and national identity by blending inherited symbolic authority with delegated political authority, providing stability and continuity while allowing for democratic governance.
% TRANSFER_FUNCTION: Transfers symbolic legitimacy and national identity from the hereditary line to the state, while transferring policy-making power from the populace to elected representatives, mediated by constitutional law.
% ABSENT_VOICES: Those advocating for a purely absolute monarchy or a purely republican system are present in public discourse but are structurally excluded from the core constitutional bargain that defines this hybrid. They would argue for a simpler, ideologically consistent form of government.
% DISAPPEARANCE_RATIONALE: If this constitutional hybrid vanished, the nation would face a profound legitimacy crisis. The roles of head of state and head of government would need immediate redefinition, potentially leading to political instability, constitutional conventions, or even civil unrest as competing visions of sovereignty (pure monarchical vs. pure republican) vie for dominance.
% FOUNDING_PROBLEM: To reconcile historical claims of inherited sovereignty with modern demands for popular representation and democratic accountability, avoiding revolutionary upheaval while evolving governance.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists attest to the historical problem of reconciling these two forms of legitimacy. Contemporary constitutional lawyers and public opinion polls corroborate that the tension, while managed, remains a live issue, with ongoing debates about the monarchy's role and the extent of popular sovereignty.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low-to-moderate (0.35) because this hybrid is a compromise, reducing the extreme extraction of absolute monarchy and the potential instability of pure republicanism, but introducing costs of ambiguity and maintenance. Suppression (0.45) is moderate, as it actively constrains both pure monarchist and pure republican ideals. Theater ratio (0.20) is present due to the ceremonial aspects of inherited authority, but the political functions are real. The metrics reflect a relatively stable, albeit complex, system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the hereditary monarch and elected officials, this is a functional and legitimate system. From the perspective of pure monarchists and republicans, it is an unsatisfactory compromise that extracts from their ideals. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary monarch benefits from retained status and income (low d). Elected officials benefit from stable governance and policy power (low d). Constitutional scholars benefit from the ongoing need for interpretation (low d). Pure monarchists and pure republicans are victims, as their ideal forms of government are suppressed (high d). Citizens are beneficiaries of stability but also bear the costs of maintaining the complex system (symmetric d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_dispute_resolution,
    'How effectively does constitutional law mediate boundary disputes between inherited and delegated authority, and what is the cost of this mediation?',
    'Analysis of historical constitutional crises, judicial review outcomes, and public trust in constitutional institutions during periods of tension.',
    'If mediation is ineffective or costly, the constraint''s stability and coordination function are weaker, potentially increasing extractiveness from citizens and resistance from ideological purists. If highly effective, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_dispute_resolution, empirical, 'Effectiveness and cost of constitutional mediation in a hybrid system.').

omega_variable(
    legitimacy_source_primacy,
    'Which source of legitimacy (inherited or delegated) holds ultimate primacy in practice, despite the constitutional claim of hybridity?',
    'Analysis of constitutional amendments, judicial interpretations, and political crises where one source of authority demonstrably overrides the other.',
    'If one source consistently overrides the other, the ''hybrid'' claim is weakened, potentially reclassifying towards a ''tangled_rope'' or even ''snare'' if the dominant source is extractive. If truly balanced, it supports the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_primacy, conceptual, 'Whether the dual sources of legitimacy are truly balanced or if one holds de facto primacy.').

omega_variable(
    identity_fusion_of_citizens,
    'To what extent do citizens genuinely fuse their identity with both inherited symbols and democratic processes, or is one merely tolerated for the sake of stability?',
    'Sociological studies of national identity, public opinion surveys on attitudes towards monarchy and democracy, and analysis of political rhetoric during periods of national crisis.',
    'If identity fusion is weak, the constraint''s stability is more fragile, relying more on suppression than genuine coordination. If strong, it indicates a robust ''identity_coordination'' component, reinforcing the ''rope'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_of_citizens, empirical, 'Depth of citizen identification with both inherited and delegated authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sove_tr_t10, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(sove_tr_t30, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(sove_tr_t50, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sove_be_t10, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(sove_be_t30, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(sove_be_t50, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sove_su_t10, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(sove_su_t30, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(sove_su_t50, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
