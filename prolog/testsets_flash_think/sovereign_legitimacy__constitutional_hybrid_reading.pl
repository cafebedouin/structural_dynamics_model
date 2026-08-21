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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Hybrid of Sovereign Legitimacy
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint describes a constitutional hybrid of sovereign
 *   legitimacy, where authority is dual-sourced from inherited ceremonial
 *   power and delegated political power, with constitutional law mediating
 *   the boundary. This is one reading of the 'sovereign_legitimacy' kernel,
 *   specifically the 'constitutional_hybrid_reading'. It aims to provide
 *   stability by accommodating historical claims while integrating popular
 *   consent. The constraint is claimed as a Tangled Rope because it
 *   coordinates these two sources of authority but also extracts from those
 *   who seek a pure form of either monarchical absolutism or pure
 *   republicanism, requiring active enforcement of its constitutional limits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.35).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.5).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Hybrid of Sovereign Legitimacy").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '774154a0-da05-4faa-9520-24d909aca4b1').
narrative_ontology:cs_kernel_codification('774154a0-da05-4faa-9520-24d909aca4b1', formalized).
narrative_ontology:cs_authority_grounding('774154a0-da05-4faa-9520-24d909aca4b1', practice).
narrative_ontology:cs_interpretation_layer_present('774154a0-da05-4faa-9520-24d909aca4b1').
narrative_ontology:cs_reading_relation('774154a0-da05-4faa-9520-24d909aca4b1', sovereign_legitimacy__monarchical_reading, influences).
narrative_ontology:cs_reading_relation('774154a0-da05-4faa-9520-24d909aca4b1', sovereign_legitimacy__republican_reading, influences).
narrative_ontology:cs_axiom('774154a0-da05-4faa-9520-24d909aca4b1', foundational, dual_source_legitimacy).
narrative_ontology:cs_axiom_status(dual_source_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('774154a0-da05-4faa-9520-24d909aca4b1', dual_source_legitimacy, conventional).
narrative_ontology:cs_axiom('774154a0-da05-4faa-9520-24d909aca4b1', foundational, constitutional_supremacy).
narrative_ontology:cs_axiom_status(constitutional_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('774154a0-da05-4faa-9520-24d909aca4b1', constitutional_supremacy, conventional).
narrative_ontology:cs_reference_frame('774154a0-da05-4faa-9520-24d909aca4b1', post_revolutionary_settlement).
narrative_ontology:cs_drift_state('774154a0-da05-4faa-9520-24d909aca4b1', contemporary_political_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('774154a0-da05-4faa-9520-24d909aca4b1', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, monarchical_absolutists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, pure_republicans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, citizens).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains a symbolic head-of-state role and receives public funds, providing continuity and ceremonial authority. Their political power is strictly limited by constitutional law, which they are bound to uphold.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, agenda_setter,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary).

% Exercise delegated political authority and are responsible for governance, policy-making, and administration. Their power is derived from popular consent but is constrained by constitutional limits and the monarch's symbolic role.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary).

% Adhere to the belief in absolute monarchical rule, grounded in divine right or tradition. They view the constitutional hybrid as an illegitimate dilution of true sovereignty and bear the cost of their preferred system being suppressed.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, monarchical_absolutists, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, monarchical_absolutists, excluded).

% Advocate for a fully elected head of state and pure popular sovereignty, rejecting any inherited authority. They see the hereditary monarch as an anachronism and bear the cost of operating within a system that enshrines the hybrid.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, pure_republicans, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, pure_republicans, excluded).

% Interprets and upholds constitutional law, mediating disputes between the inherited and delegated authorities. They ensure the framework's integrity and legitimacy, acting as a check on both branches.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_judiciary, observer).

% Benefit from the political stability, historical continuity, and democratic accountability offered by the hybrid system. They also bear the costs of maintaining both institutions and the occasional ambiguities or inefficiencies inherent in the compromise.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, citizens, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, citizens, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__constitutional_hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__constitutional_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances historical continuity and popular consent by separating ceremonial and political authority, preventing either pure form from dominating and leading to instability. It provides a framework for peaceful succession and democratic governance.
% TRANSFER_FUNCTION: Transfers symbolic legitimacy from inherited tradition to the state, and political power from the people to elected representatives, with constitutional law mediating the boundary. It also transfers resources to maintain both institutions.
% ABSENT_VOICES: Absolute monarchists and pure republicans are structurally marginalized; they would argue for a singular, uncompromised source of legitimacy but are excluded from the constitutional consensus that underpins the hybrid system.
% DISAPPEARANCE_RATIONALE: If the constitutional hybrid vanished, the state would face an immediate and profound legitimacy crisis, potentially leading to civil unrest, a power vacuum, or a forced, potentially violent, transition to either a pure monarchy or a pure republic, fundamentally altering the political landscape and national identity.
% FOUNDING_PROBLEM: To resolve historical conflicts between monarchical claims of divine right and emerging demands for popular sovereignty, creating a stable political order that could accommodate both without resorting to perpetual revolution or absolutism.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists attest to the historical problem and its ongoing relevance in maintaining national unity and managing the inherent tensions between tradition and modernity. Constitutional scholars corroborate the continuous need for interpretation to manage these tensions, indicating the problem is still active.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.35) is moderate, reflecting the compromise nature of the system; it's less extractive than pure absolutism or the potential instability of a pure republic, but it imposes costs on those who desire an uncompromised form of sovereignty. Suppression (0.5) is moderate, as the constitutional framework actively limits and marginalizes challenges from both absolutist and pure republican positions. The theater ratio (0.25) is low-to-moderate, acknowledging the ceremonial aspects of inherited authority while emphasizing the functional role of constitutional mediation. The temporal measurements show relative stability with minor fluctuations, reflecting the ongoing, but generally managed, tensions within the hybrid system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the hereditary monarch and elected officials, the system is a legitimate and stable form of governance that successfully coordinates diverse claims to authority. From the perspective of absolutists and pure republicans, the same system is an illegitimate compromise that extracts from their ideals and suppresses their preferred forms of sovereignty. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary monarch benefits from retained status and income, while elected officials benefit from policy power and democratic legitimacy. Both are structural beneficiaries. Monarchical absolutists and pure republicans are victims, as their preferred systems are suppressed by the constitutional compromise. The constitutional judiciary acts as an agenda-setter for interpretation, and citizens are both beneficiaries (stability) and payers (costs of maintenance and compromise).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monarchical_vs_republican_balance,
    'Is the balance between inherited ceremonial authority and delegated political authority genuinely stable, or does one source of legitimacy subtly undermine the other over time?',
    'Longitudinal analysis of constitutional amendments, judicial interpretations, and public opinion trends regarding the powers and roles of both the monarch and elected officials.',
    'If one source consistently gains ground at the expense of the other, the hybrid''s stability is compromised, potentially leading to reclassification towards a more extractive or unstable type. If the balance holds, it reinforces the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monarchical_vs_republican_balance, empirical, 'Assesses the long-term equilibrium of the dual-sourced legitimacy.').

omega_variable(
    constitutional_interpretive_drift,
    'Does constitutional interpretation genuinely mediate the boundary between inherited and delegated authority, or does it drift to favor one source of authority (e.g., expanding executive power, diminishing symbolic roles)?',
    'Comparative legal analysis of landmark constitutional cases and scholarly commentary on the evolution of constitutional doctrine over several decades.',
    'If interpretation consistently favors one side, the constraint''s claimed coordination function is weakened, and its extractiveness from the disfavored side increases, potentially shifting its classification towards a Snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_interpretive_drift, conceptual, 'Examines the neutrality and efficacy of constitutional mediation.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the dual sourcing of legitimacy a strength (providing resilience and broad appeal) or an inherent weakness (due to conflicting foundational principles that create persistent instability)?',
    'Comparative political science studies of hybrid regimes versus pure monarchies/republics, assessing their long-term stability, adaptability, and citizen satisfaction.',
    'If ambiguity is a strength, the constraint''s coordination function is robust. If it''s a weakness, the system is inherently fragile, and its long-term viability as a stable ''Tangled Rope'' is questionable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, preference, 'Evaluates the fundamental nature of dual-sourced legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sove_tr_t10, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(sove_tr_t30, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(sove_tr_t50, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sove_be_t10, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(sove_be_t30, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(sove_be_t50, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sove_su_t10, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(sove_su_t30, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(sove_su_t50, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 50, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, national_identity_formation).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, parliamentary_procedure_rules).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'constitutional_hybrid_reading' of the 'sovereign_legitimacy' kernel, which also includes 'monarchical_reading' and 'republican_reading'. Each reading represents a distinct structural claim about the source of legitimate authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
