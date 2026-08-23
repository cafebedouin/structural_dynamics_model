% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy Reading of Constitutional Authority
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the parliamentary_primacy_reading of
 *   the constitutional_authority_boundary kernel. The reading asserts that
 *   constitutional text is subordinate to parliamentary sovereignty: the
 *   elected legislature retains final authority to define constitutional
 *   meaning through ordinary or entrenched legislation. The judiciary
 *   exercises only advisory or easily-overridden review. The arrangement is
 *   claimed as a genuine coordination mechanism (rope) solving the democratic
 *   legitimacy problem, with low extraction (ε≈0.20) on the judiciary's
 *   constrained role. The claim/metric independence is maintained: the
 *   reading claims rope; the metrics describe low extraction, moderate
 *   suppression (foreclosure of judicial review), and minimal theater.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.25).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '9775b002-711d-4f6a-9b07-76f105c7c1f5').
narrative_ontology:cs_kernel_codification('9775b002-711d-4f6a-9b07-76f105c7c1f5', fixed_text).
narrative_ontology:cs_authority_grounding('9775b002-711d-4f6a-9b07-76f105c7c1f5', lineage).
narrative_ontology:cs_reading_relation('9775b002-711d-4f6a-9b07-76f105c7c1f5', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('9775b002-711d-4f6a-9b07-76f105c7c1f5', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('9775b002-711d-4f6a-9b07-76f105c7c1f5', foundational, parliamentary_sovereignty_as_constitutional_foundation).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_as_constitutional_foundation, holdable).
narrative_ontology:cs_axiom_grounding('9775b002-711d-4f6a-9b07-76f105c7c1f5', parliamentary_sovereignty_as_constitutional_foundation, conventional).
narrative_ontology:cs_axiom('9775b002-711d-4f6a-9b07-76f105c7c1f5', secondary, judicial_review_subordinate_to_legislative_will).
narrative_ontology:cs_axiom_status(judicial_review_subordinate_to_legislative_will, holdable).
narrative_ontology:cs_axiom_grounding('9775b002-711d-4f6a-9b07-76f105c7c1f5', judicial_review_subordinate_to_legislative_will, conventional).
narrative_ontology:cs_reference_frame('9775b002-711d-4f6a-9b07-76f105c7c1f5', parliamentary_sovereignty_framework).
narrative_ontology:cs_drift_state('9775b002-711d-4f6a-9b07-76f105c7c1f5', post_human_rights_act_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9775b002-711d-4f6a-9b07-76f105c7c1f5', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, parliament).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, electorate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_legitimacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to define constitutional meaning through ordinary or entrenched legislation. Sets the constitutional agenda and can override judicial interpretations. Benefits from concentrated interpretive authority and democratic legitimacy. No effective exit — the institution is constituted by this authority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains democratic control over constitutional meaning through elected representatives. Constitutional changes reflect electoral outcomes rather than judicial precedent. Exit is constrained — citizens cannot easily opt out of the constitutional order, but the arrangement is justified as their collective self-governance.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, electorate, beneficiary,
    organized, biographical, constrained, national).

% Exercises advisory or easily-overridden review only. Cannot invalidate legislation; may issue declarations of incompatibility that parliament can disregard. Professional identity is fused to legal interpretation, making exit from the constrained role nearly impossible without abandoning judicial office. This reading views the constrained role as proper constitutional design, not extraction.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary, payer,
    institutional, generational, identity_locked, national).

% Analyze and debate the legitimacy of parliamentary primacy versus rival readings. Their work informs public discourse and judicial reasoning but they hold no formal authority. Exit is analytical — they can shift frameworks without material cost.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% Lack structural protection against legislative majorities. Rights depend on parliamentary forbearance rather than judicial enforcement. Would object to the absence of entrenched judicial review but are not institutional participants in the constitutional settlement.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, minority_groups, excluded,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of final interpretive authority in a democratic constitution by vesting it in the elected legislature, ensuring constitutional meaning tracks democratic will and avoiding counter-majoritarian difficulty.
% TRANSFER_FUNCTION: Moves final constitutional interpretive authority from the judiciary to the legislature. The judiciary surrenders strong-form review power; the legislature gains unchallengeable definitional authority. The electorate gains democratic control over constitutional evolution.
% ABSENT_VOICES: Minority groups and rights-advocacy organizations are structurally excluded from the constitutional settlement — they would demand entrenched judicial review as a shield against legislative majorities but have no formal role in the parliamentary primacy framework.
% DISAPPEARANCE_RATIONALE: If parliamentary primacy vanished overnight, courts would assume final interpretive authority (judicial supremacy) or a coordinate construction would emerge. Constitutional meaning would decouple from electoral cycles. Rights protections would shift from political to legal guarantees. The democratic constitution would reorganize around a different authority boundary.
% FOUNDING_PROBLEM: The founding problem was legitimate constitutional authority in a democratic polity: how to ensure the constitution serves the people's will rather than entrenching judicial preferences. Parliamentary primacy was built to make constitutional change responsive to electoral majorities and prevent unelected judges from overriding democratic choices.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (parliamentary sovereignty theorists, democratic theorists like Bellamy, Waldron) attest the problem remains live — judicial activism continues to threaten democratic legitimacy. Critics (judicial supremacy advocates, coordinate construction theorists) attest the problem is misstated — the founding problem was protecting minority rights from majoritarian abuse, which parliamentary primacy fails to solve. Corroboration from outside beneficiaries: political scientists documenting democratic backsliding where parliamentary primacy enabled rights erosion; comparative constitutional scholars noting most modern constitutions adopt some form of judicial review.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).
:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.20) because the judiciary's constrained role is the coordination design itself, not rent extraction — the arrangement solves who has final say. Suppression (0.25) reflects foreclosure of strong-form judicial review as an alternative; this is structural (constitutional design), not active coercion. Theater is minimal (0.10) — the arrangement functions as designed. Accessibility collapse (0.40) is moderate: coordinate construction and judicial supremacy remain live intellectual alternatives but are foreclosed institutionally. Resistance (0.30) comes from judicial supremacy advocates and rights theorists. The measurement series shows extractiveness and suppression rising mid-century as judicial review expanded globally, then stabilizing as parliamentary primacy systems (UK, NZ) codified weak-form review (HRA, NZBORA) without surrendering legislative finality.
 *
 * PERSPECTIVAL GAP:
 *   From parliament's seat, the arrangement is pure coordination (rope) — it solves the legitimacy problem efficiently. From the judiciary's seat, the same structure operates as constrained authority — professional identity is fused to a role the reading defines as subordinate. From minority groups' seat, it operates as a snare — no exit, no protection, extraction of rights security. The engine computes this divergence; the authored claim (rope) reflects the agenda-setter's perspective, not a synthesis.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament is the structural beneficiary (d→0.0): it collects interpretive authority, controls constitutional meaning, and faces no effective exit. The electorate is a diffuse beneficiary (d≈0.2): gains democratic control but bears risk of majoritarian abuse. The judiciary is a constrained payer (d→0.8): surrenders strong-form review, identity-locked into the constrained role. Minority groups are excluded (d→1.0): trapped, no voice, bear costs of unchecked legislative power. Constitutional scholars are analytical observers (d=0.5). The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (democratic control of constitutional meaning) remains contested — judicial activism and rights revolutions challenge whether parliamentary primacy still solves it or has become a vehicle for majoritarian overreach. The mandate has not atrophied (parliamentary primacy remains the operating principle in UK, NZ, Finland) but its justification is actively disputed. No single party captures extraction gains — the legislature's authority is the coordination function itself, not a rent. This distinguishes it from a piton (where function atrophies but form persists theatrically) or snare (where coordination is cover for extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the parliamentary_primacy_reading a distinct constraint with its own ε, or merely a perspective on a single constraint?',
    'Apply ε-invariance test: if measuring the constraint from judicial_supremacy_reading yields ε≈0.8 (strong extraction from legislature) while this reading yields ε≈0.2, they are different constraints. The kernel label ''constitutional authority boundary'' conflates them.',
    'If distinct, each reading gets its own constraint story with independent classification. The engine computes per-reading types. If not distinct, the framework must model observable-dependent ε — which DP-001 forbids.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether kernel readings instantiate separate constraints per ε-invariance principle.').

omega_variable(
    sibling_reading_delta,
    'What structural elements do the sibling readings change relative to this reading?',
    'Compare stakeholder roles, beneficiary/victim sets, and ε referents across the three readings. Judicial supremacy flips beneficiary (judiciary) and victim (legislature). Coordinate construction distributes authority across three branches with no final arbiter.',
    'If sibling readings produce different beneficiary/victim structures and ε values, they confirm separate constraints. If they only differ in evaluation of the same structure, they are perspectives on one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta, conceptual, 'Structural delta between parliamentary primacy and its sibling readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the foreclosure of strong-form judicial review structural (constitutional design) or internalized (judiciary accepts subordination)?',
    'Examine judicial behavior in parliamentary primacy systems: do courts self-limit because of constitutional doctrine (internalized) or because of institutional consequences (structural)? Post-HRA UK jurisprudence shows courts pushing boundaries — suggesting suppression is partly structural, not fully internalized.',
    'If internalized, suppression metric understates effective constraint on judiciary — they carry the suppression internally. If structural, suppression is accurately measured at 0.25. Affects classification: higher effective suppression could push toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of judicial review in parliamentary primacy systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cab_ppr_tr_t1900, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(cab_ppr_tr_t1950, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(cab_ppr_tr_t1970, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(cab_ppr_tr_t1990, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(cab_ppr_tr_t2000, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(cab_ppr_tr_t2010, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(cab_ppr_tr_t2024, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(cab_ppr_be_t1900, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(cab_ppr_be_t1950, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(cab_ppr_be_t1970, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(cab_ppr_be_t1990, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(cab_ppr_be_t2000, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(cab_ppr_be_t2010, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 2010, 0.2).
narrative_ontology:measurement(cab_ppr_be_t2024, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(cab_ppr_su_t1900, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(cab_ppr_su_t1950, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1950, 0.18).
narrative_ontology:measurement(cab_ppr_su_t1970, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(cab_ppr_su_t1990, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(cab_ppr_su_t2000, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(cab_ppr_su_t2010, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(cab_ppr_su_t2024, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__parliamentary_primacy_reading, 0.1).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the constitutional_authority_boundary constraint family. Each reading instantiates a different constraint with different ε, beneficiaries, and classification. Parliamentary primacy (this story) claims rope with legislature as beneficiary. Judicial supremacy claims rope/snare with judiciary as beneficiary. Coordinate construction claims rope/tangled_rope with distributed authority. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__parliamentary_primacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_authority_boundary__parliamentary_primacy_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
