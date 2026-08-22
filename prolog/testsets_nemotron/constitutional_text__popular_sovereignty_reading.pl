% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Text — Popular Sovereignty Reading
 *   domain: political/legal/philosophical
 *
 * SUMMARY:
 *   This constraint story instantiates the popular_sovereignty_reading of the
 *   constitutional_text kernel. The kernel is the constitutional text itself
 *   — a stabilized commitment that different readings instantiate as
 *   different constraints. This reading holds that the text's authority
 *   derives from the constituent power of the demos, making neither courts
 *   nor legislature supreme. The people retain ultimate interpretive
 *   authority exercised through amendment, convention, or revolution. The
 *   constraint coordinates democratic legitimacy while extracting stability
 *   and expertise from institutions. It is a tangled_rope because it performs
 *   a genuine coordination function (solving the authorization regress) while
 *   asymmetrically extracting from institutional actors who lose final
 *   interpretive authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.45).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.35).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Text — Popular Sovereignty Reading").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "political/legal/philosophical").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, 'd4eb799f-85a1-4d23-b75c-28daa9aad0cb').
narrative_ontology:cs_kernel_codification('d4eb799f-85a1-4d23-b75c-28daa9aad0cb', fixed_text).
narrative_ontology:cs_authority_grounding('d4eb799f-85a1-4d23-b75c-28daa9aad0cb', lineage).
narrative_ontology:cs_interpretation_layer_present('d4eb799f-85a1-4d23-b75c-28daa9aad0cb').
narrative_ontology:cs_reading_relation('d4eb799f-85a1-4d23-b75c-28daa9aad0cb', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4eb799f-85a1-4d23-b75c-28daa9aad0cb', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('d4eb799f-85a1-4d23-b75c-28daa9aad0cb', foundational, constituent_power_supreme).
narrative_ontology:cs_axiom_status(constituent_power_supreme, holdable).
narrative_ontology:cs_axiom_grounding('d4eb799f-85a1-4d23-b75c-28daa9aad0cb', constituent_power_supreme, deontological).
narrative_ontology:cs_axiom('d4eb799f-85a1-4d23-b75c-28daa9aad0cb', foundational, institutions_subordinate_to_demos).
narrative_ontology:cs_axiom_status(institutions_subordinate_to_demos, holdable).
narrative_ontology:cs_axiom_grounding('d4eb799f-85a1-4d23-b75c-28daa9aad0cb', institutions_subordinate_to_demos, deontological).
narrative_ontology:cs_reference_frame('d4eb799f-85a1-4d23-b75c-28daa9aad0cb', constituent_power_founding).
narrative_ontology:cs_drift_state('d4eb799f-85a1-4d23-b75c-28daa9aad0cb', contemporary_judicial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d4eb799f-85a1-4d23-b75c-28daa9aad0cb', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_participants).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, social_movements).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, constituent_power_subjects).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_stability).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, constitutional_expertise).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislative_continuity).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, judicial_independence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, social_movements).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, constituent_power_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, democratic_legitimacy_primacy).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, extra_institutional_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens and organized groups who invoke constituent power through elections, referenda, protests, and constitutional conventions. They gain ultimate interpretive authority but bear the cost of mobilization and the risk of instability. Exit means disengaging from democratic participation — constrained by the fact that the constraint constitutes their political agency.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, democratic_participants, beneficiary,
    organized, biographical, constrained, national).

% Organized movements that mobilize popular sovereignty claims to challenge institutional interpretations. They benefit from the meta-authority the constraint provides but pay high mobilization costs and face repression risk. Their identity is fused to the democratic claim — exit means abandoning the constituency they represent.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, social_movements, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, social_movements, payer).

% The demos as theoretical bearer of constituent power — including those excluded from effective participation (disenfranchised, marginalized, future generations). They are the nominal beneficiaries of the constraint's founding claim but often lack effective voice. Exit from the polity is structurally trapped.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constituent_power_subjects, beneficiary,
    powerless, biographical, trapped, national).

% The continuity and predictability of constitutional governance — legislative continuity, administrative capacity, intertemporal commitment credibility. Bears the cost of periodic democratic upheaval, amendment uncertainty, and revolutionary threat. Cannot exit the constraint without ceasing to be a constitutional institution.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, institutional_stability, payer,
    institutional, generational, constrained, national).

% Judicial, academic, and professional expertise in constitutional interpretation. Loses authoritative status when extra-institutional mobilization overrides technical judgment. Can exit by shifting to advisory roles or other jurisdictions, but loses the specific authority of binding interpretation.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_expertise, payer,
    organized, biographical, mobile, national).

% The legislature's capacity to govern through stable legal frameworks. Subject to invalidation or override by popular mobilization (referenda, conventions, revolutionary mandates). Exit means abdicating legislative authority — constrained by democratic mandate.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislative_continuity, payer,
    institutional, generational, constrained, national).

% Courts' capacity to interpret the constitution without democratic reprisal. The constraint subordinates judicial finality to popular revision. Exit means accepting legislative or popular override — constrained by the constraint's own logic.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, judicial_independence, payer,
    institutional, generational, constrained, national).

% Scholars who analyze the constraint's operation across regimes and eras. They neither collect nor pay but map the structural relationships. Their analytical seat is the engine's computational position.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of constitutional legitimacy and adaptability by anchoring authority in the only source that can authorize the constitution itself — the constituent power of the people. Provides a mechanism for fundamental legal change without infinite regress (who authorizes the authorizer?) and a safety valve against institutional capture.
% TRANSFER_FUNCTION: Moves ultimate interpretive authority from courts and legislature to the mobilized demos, transferring the power to invalidate, amend, or replace constitutional meaning from institutional actors to extra-institutional democratic action. Transfers stability and expertise costs from the demos to institutions.
% ABSENT_VOICES: Future generations (who inherit the constitutional order but cannot participate in its reauthorization), the globally displaced (whose lives are shaped by constitutional orders they never consented to), and the internally excluded (disenfranchised populations within the polity). They would object to the constraint's claim to represent 'the people' but are structurally excluded from the constituent power it invokes.
% DISAPPEARANCE_RATIONALE: If the popular sovereignty constraint vanished overnight — i.e., if constitutional authority were conceded to reside solely in courts or legislature with no extra-institutional override — the legitimating foundation of the constitutional order would collapse. Amendment would become impossible without institutional consent, revolutionary resistance would lose its normative warrant, and the constitution would become a closed institutional artifact rather than a democratic project. The world would rearrange into either judicial supremacy or legislative sovereignty by default.
% FOUNDING_PROBLEM: The infinite regress of constitutional authorization: if courts interpret the constitution, who authorizes the courts? If the legislature enacts the constitution, who authorizes the legislature? The constituent power of the demos was posited as the only non-circular answer — the people authorize the constitution that authorizes the institutions that govern them.
% FOUNDING_PROBLEM_CORROBORATION: Democratic theorists (Arendt, Habermas, Negri) attest the founding problem is live — constituent power remains the only non-regressive ground of legitimacy. Institutionalists (Kelsen, Schmitt in his institutional phase, contemporary constitutional courts) attest it is dead — the constitution is a closed legal order whose authority derives from its own validity, not an external demos. The contest is structural: no external arbiter can resolve it because the dispute is about what counts as an arbiter.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint does not continuously extract — it creates a latent extraction potential that activates during constitutional crises. Suppression is moderate-low (0.35) because the constraint's primary mechanism is authorization, not coercion; coercion appears only when institutions resist popular mobilization. Theater ratio is elevated (0.42) because institutional actors perform deference to popular sovereignty while structuring the rules of mobilization (amendment thresholds, convention rules, protest law) to manage the threat. Accessibility collapse is moderate (0.48) because alternative constitutional theories (judicial supremacy, legislative sovereignty) remain live and practiced. Resistance is moderate-high (0.55) because institutional actors actively resist ceding final authority, and the constraint's own beneficiaries (the demos) are often ambivalent about exercising constituent power.
 *
 * PERSPECTIVAL GAP:
 *   From the democratic participant seat, the constraint is a rope — genuine coordination enabling self-governance. From the institutional stability seat, it is a snare — unpredictable extraction of authority and continuity. From the constitutional expertise seat, it is a tangled_rope — coordination with asymmetric extraction. The engine computes these per-seat classifications from the structural data; the claimed_type reflects the constraint's dominant structural character across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Democratic participants and social movements are beneficiaries (d near 0.0) — the constraint subsidizes their interpretive authority. Constituent power subjects are identity-locked beneficiaries (d ~0.2) — nominally benefited but structurally excluded from effective exercise. Institutional stability, constitutional expertise, legislative continuity, and judicial independence are payers (d near 1.0) — they bear the costs of democratic override. The directionality derivation follows from beneficiary/victim declarations: the constraint's coordination function benefits the demos; its extraction function costs institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (authorization regress) remains live — no institutional settlement has resolved it. The constraint has not atrophied into a piton because the demos periodically mobilizes (amendments, conventions, revolutions) and the constraint's activation is real, not theatrical. However, the rising theater_ratio suggests institutional actors are learning to perform deference while hollowing out the constraint's operative force — a mandatrophy risk if democratic mobilization becomes purely ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel rather than a standalone constraint?',
    'Structural comparison with sibling readings judicial_supremacy_reading and legislative_sovereignty_reading: if they share the same constitutional text referent but instantiate different beneficiary/victim structures and different ε, they are readings of the kernel constitutional_text.',
    'Confirms this is a kernel-reading story; the engine''s per-seat classifications will diverge from the sibling readings'' classifications because directionality derivations differ per reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint instantiates the popular_sovereignty_reading of the constitutional_text kernel; sibling readings are separate constraint stories linked by network.affects_constraints.').

omega_variable(
    institutional_vs_democratic_extraction_boundary,
    'Does the extraction experienced by institutional actors (courts, legislature) represent asymmetric extraction from a coordination function, or is institutional stability a genuine coordination beneficiary of the popular sovereignty constraint?',
    'Historical analysis of constitutional crises: when popular mobilization overrides institutional judgment, do institutional actors experience net loss of coordination benefit, or do they gain stability from the democratic reset? Compare pre- and post-crisis institutional capacity.',
    'If institutional stability is a net beneficiary, the constraint may be a rope for institutions despite extracting from them in crises; if net victim, the constraint is a tangled_rope where popular sovereignty coordinates democratic participation at the cost of institutional autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_democratic_extraction_boundary, empirical, 'Whether institutional actors are coordinated or extracted-from by the popular sovereignty constraint.').

omega_variable(
    revolution_amendment_boundary,
    'Where does the constraint''s coordination function end and pure extraction begin: at formal amendment, convention, or revolutionary overthrow?',
    'Case study of constitutional transitions: measure extractiveness and suppression at each mechanism (amendment, convention, revolution). If suppression and extraction spike only at revolutionary threshold, the constraint is coordination with a high-stakes exit; if elevated across all three, the constraint extracts continuously.',
    'Determines whether the constraint''s high theater_ratio and moderate extractiveness reflect a genuine coordination core with a dangerous exit option, or a structure that extracts across its entire operational range.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revolution_amendment_boundary, conceptual, 'Whether the constraint''s three realization mechanisms form a unified coordination function or a gradient from coordination to extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__popular_sovereignty_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement_basis(cons_tr_t50, observed).
narrative_ontology:measurement(cons_tr_t100, constitutional_text__popular_sovereignty_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement_basis(cons_tr_t100, observed).
narrative_ontology:measurement(cons_tr_t150, constitutional_text__popular_sovereignty_reading, theater_ratio, 150, 0.4).
narrative_ontology:measurement_basis(cons_tr_t150, observed).
narrative_ontology:measurement(cons_tr_t200, constitutional_text__popular_sovereignty_reading, theater_ratio, 200, 0.42).
narrative_ontology:measurement_basis(cons_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_text__popular_sovereignty_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(cons_be_t50, observed).
narrative_ontology:measurement(cons_be_t100, constitutional_text__popular_sovereignty_reading, base_extractiveness, 100, 0.41).
narrative_ontology:measurement_basis(cons_be_t100, observed).
narrative_ontology:measurement(cons_be_t150, constitutional_text__popular_sovereignty_reading, base_extractiveness, 150, 0.43).
narrative_ontology:measurement_basis(cons_be_t150, observed).
narrative_ontology:measurement(cons_be_t200, constitutional_text__popular_sovereignty_reading, base_extractiveness, 200, 0.45).
narrative_ontology:measurement_basis(cons_be_t200, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t50, constitutional_text__popular_sovereignty_reading, suppression_requirement, 50, 0.3).
narrative_ontology:measurement_basis(cons_su_t50, observed).
narrative_ontology:measurement(cons_su_t100, constitutional_text__popular_sovereignty_reading, suppression_requirement, 100, 0.33).
narrative_ontology:measurement_basis(cons_su_t100, observed).
narrative_ontology:measurement(cons_su_t150, constitutional_text__popular_sovereignty_reading, suppression_requirement, 150, 0.35).
narrative_ontology:measurement_basis(cons_su_t150, observed).
narrative_ontology:measurement(cons_su_t200, constitutional_text__popular_sovereignty_reading, suppression_requirement, 200, 0.35).
narrative_ontology:measurement_basis(cons_su_t200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(constitutional_text__popular_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint, judicial_supremacy_reading, and legislative_sovereignty_reading form a constraint family decomposing the 'constitutional text' label into three structurally distinct claims with different ε values, different beneficiary/victim structures, and different computed types. The ε-invariance principle requires separate stories: judicial supremacy has low ε for courts but high for demos; legislative sovereignty has low ε for legislature but high for courts; popular sovereignty has moderate ε for all institutional actors but coordinates democratic participation. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text__popular_sovereignty_reading, institutional, 0.85).
constraint_indexing:directionality_override(constitutional_text__popular_sovereignty_reading, organized, 0.15).
constraint_indexing:directionality_override(constitutional_text__popular_sovereignty_reading, powerless, 0.25).
constraint_indexing:directionality_override(constitutional_text__popular_sovereignty_reading, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
