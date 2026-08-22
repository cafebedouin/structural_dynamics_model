% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership as Conditional Treaty (Sovereignty Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty reading' of the
 *   contested federation_membership kernel. It frames federation membership
 *   as a conditional treaty among sovereign states, where national authority
 *   retains legitimacy over border control and free movement is a negotiable
 *   policy concession rather than a constitutional right. The constraint
 *   extracts mobility rights from mobile citizens, cross-border workers, and
 *   asylum seekers to subsidize national labor market protection and
 *   governmental border discretion. The coordination function is genuine —
 *   sovereign states need a framework to cooperate without merging — but it
 *   is hybridized with asymmetric extraction: the same treaty structure that
 *   enables trade coordination also empowers national vetoes over mobility.
 *   This reading coexists with the integration_reading (which treats free
 *   movement as constitutional right) but creates structural pressure on it
 *   by legitimating national restriction as treaty-compliant.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.68).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.72).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '5b2cee21-a4ab-49d2-bc79-329dd5431f88').
narrative_ontology:cs_kernel_codification('5b2cee21-a4ab-49d2-bc79-329dd5431f88', formalized).
narrative_ontology:cs_authority_grounding('5b2cee21-a4ab-49d2-bc79-329dd5431f88', lineage).
narrative_ontology:cs_interpretation_layer_present('5b2cee21-a4ab-49d2-bc79-329dd5431f88').
narrative_ontology:cs_reading_relation('5b2cee21-a4ab-49d2-bc79-329dd5431f88', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('5b2cee21-a4ab-49d2-bc79-329dd5431f88', foundational, national_border_sovereignty_inviolable).
narrative_ontology:cs_axiom_status(national_border_sovereignty_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('5b2cee21-a4ab-49d2-bc79-329dd5431f88', national_border_sovereignty_inviolable, conventional).
narrative_ontology:cs_axiom('5b2cee21-a4ab-49d2-bc79-329dd5431f88', foundational, free_movement_as_policy_concession).
narrative_ontology:cs_axiom_status(free_movement_as_policy_concession, holdable).
narrative_ontology:cs_axiom_grounding('5b2cee21-a4ab-49d2-bc79-329dd5431f88', free_movement_as_policy_concession, conventional).
narrative_ontology:cs_reference_frame('5b2cee21-a4ab-49d2-bc79-329dd5431f88', post_war_treaty_framework).
narrative_ontology:cs_drift_state('5b2cee21-a4ab-49d2-bc79-329dd5431f88', post_maastricht_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5b2cee21-a4ab-49d2-bc79-329dd5431f88', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, federal_governments).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, domestic_union_members).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, cross_border_workers).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, asylum_seekers).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, national_border_sovereignty).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, conditional_federation_treaty).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, labor_market_protectionism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Domestic workers and unions benefit from restricted labor competition due to mobility barriers. Their wage floors and bargaining positions are protected by national border controls that limit inflows of cross-border workers. They can organize politically but cannot individually exit the national labor market without emigrating.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_labor_markets, beneficiary,
    organized, biographical, constrained, national).

% National governments set and enforce migration policy within the federation framework. They retain authority over border admissions, work permits, and residency rights. They benefit from the treaty flexibility to restrict movement when politically expedient, while still accessing federation trade and coordination benefits. They can arbitrate between federation and national commitments.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, federal_governments, agenda_setter,
    institutional, generational, arbitrage, continental).

% Unionized workers in protected sectors gain wage protection and job security from mobility restrictions. They are organized through collective bargaining and political representation. Their exit options are limited to sectoral or geographic mobility within the nation.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, domestic_union_members, beneficiary,
    organized, biographical, constrained, national).

% Citizens who would exercise free movement rights — for work, study, family, or retirement — face bureaucratic barriers, permit regimes, and discretionary denials. Their identity as federation citizens is fused with the promise of mobility; exit from this identity is psychologically and politically costly. They bear the cost of restricted life chances across borders.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    moderate, biographical, identity_locked, continental).

% Workers who live and work across national borders within the federation face daily permit checks, quotas, and arbitrary revocation. Their livelihoods depend on mobility that the sovereignty reading treats as discretionary. They have no viable exit — their homes, families, and jobs are split across borders.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, cross_border_workers, payer,
    powerless, immediate, trapped, regional).

% People fleeing persecution who arrive at federation borders face national asylum procedures that the sovereignty reading treats as fully national competence. They are excluded from supranational protection standards and bear the full cost of restrictive border enforcement. No exit from the constraint except through dangerous irregular routes.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, asylum_seekers, payer,
    powerless, immediate, trapped, continental).

% Political parties and civil society actors advocating for deeper integration and free movement as constitutional right. They are structurally excluded from the sovereignty reading's framing, which treats their position as illegitimate overreach. They can organize at federation level but are blocked by national veto points.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, integrationist_parties, excluded,
    organized, generational, mobile, continental).

% Scholars and institutions analyzing the federation's constitutional structure from outside the contest. They observe the tension between treaty flexibility and integration commitment, and the asymmetric extraction of mobility restriction on mobile populations.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, federation_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for sovereign states to cooperate on trade, security, and regulatory alignment while retaining the national veto over who enters their territory — solving the coordination problem of inter-state cooperation without requiring a supranational citizenship regime.
% TRANSFER_FUNCTION: Moves the cost of border enforcement, labor market protection, and political risk of open borders from national governments and domestic labor markets onto mobile citizens, cross-border workers, and asylum seekers — who lose life chances, livelihood security, and protection in exchange for national governments retaining border discretion.
% ABSENT_VOICES: Mobile citizens who would exercise free movement, cross-border workers whose daily lives are disrupted by permit regimes, and asylum seekers denied supranational protection standards are structurally excluded from the sovereignty reading's framing. They are not seated at the treaty negotiation table; their interests are represented only obliquely through integrationist parties that lack veto power.
% DISAPPEARANCE_RATIONALE: If the sovereignty reading vanished overnight — i.e., if free movement became a non-negotiable constitutional right — national labor markets would face immediate competitive pressure, federal governments would lose a primary lever of migration control, domestic unions would demand new protections, and mobile populations would gain enforceable rights. The federation's political economy would reorganize around supranational citizenship.
% FOUNDING_PROBLEM: Post-war European states needed a framework for economic cooperation and peace that did not require surrendering the national monopoly on border control — the defining attribute of sovereign statehood after centuries of war and displacement.
% FOUNDING_PROBLEM_CORROBORATION: National governments and domestic labor organizations attest the founding problem remains live: border control is still essential to democratic accountability and labor market stability. Integrationist parties, mobile citizen advocates, and supranational courts attest the founding problem is substantially solved — peace and cooperation are established, and the sovereignty reading now functions as rent extraction by incumbent national elites. No consensus outside the beneficiary set.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because mobility restriction transfers substantial life chances and economic opportunity from mobile populations to national labor markets and governments. Suppression is high (0.72) because the constraint's persistence depends on active enforcement: border controls, permit regimes, detention, and deportation machinery. Theater ratio is moderate-low (0.25) — the coordination function (trade, security cooperation) is real and not purely performative, but a growing share of enforcement activity serves extraction (mobility restriction) rather than coordination. Accessibility collapse is moderate (0.65) — alternative arrangements (supranational citizenship, open borders) are conceptually available but politically blocked by national veto points. Resistance is moderate (0.55) — mobile populations and integrationist actors resist but face high structural barriers.
 *
 * PERSPECTIVAL GAP:
 *   From the national government seat, the constraint is genuine coordination: a treaty framework that solves the cooperation problem while preserving the democratic legitimacy of border control. From the mobile citizen seat, the same structure is enforced extraction: a promise of mobility converted into a discretionary privilege. From the cross-border worker seat, it is a daily suppression mechanism. The engine computes this divergence from the declared power/exit/role structure — the claimed_type (tangled_rope) reflects the structural hybridity that no single seat experiences as pure coordination or pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments (agenda_setter, institutional power) sit near the beneficiary end (d ~ 0.15) — they set the rules and collect the political rents of border control. National labor markets and domestic unions (beneficiary, organized power) sit at low d (~0.2) — they gain protection without bearing enforcement costs. Mobile citizens (payer, identity_locked exit) sit at high d (~0.85) — their identity as federation citizens is fused with the mobility promise, making exit from the extraction psychologically and politically costly. Cross-border workers and asylum seekers (payer, trapped) sit at d ~ 0.95 — no viable exit, full exposure to extraction. Integrationist parties (excluded, mobile exit) sit at d ~ 0.6 — they are structurally excluded but can organize at federation level. Federation analysts (observer, analytical) sit at d = 0.5 — symmetric analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war cooperation without surrendering border sovereignty) was live in 1950s–1990s. By the 2000s, peace and deep economic integration were established — the coordination function no longer requires mobility restriction as a necessary condition. Yet the constraint persists and extraction has increased (measurements show rising base_extractiveness from 0.45 to 0.68). This is mandatrophy: the mandate (cooperation framework) has outlived its function, but the constraint remains because national governments and domestic labor markets benefit from the extraction, and the sovereignty reading's framing prevents the integration_reading from displacing it. The treaty's conditional structure means no sunset clause exists — the extraction is built into the constitutional design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mobility_restriction_necessity,
    'Is mobility restriction structurally necessary for the federation''s trade and security coordination function, or is it a separable policy choice that the sovereignty reading treats as treaty-required?',
    'Natural experiment from federation enlargements: if new member states gain trade/security benefits before free movement rights are fully implemented (transitional arrangements), the functions are separable. Counterfactual: would the federation collapse if free movement became non-negotiable?',
    'If mobility restriction is necessary for coordination, part of the measured extraction is the price of the treaty itself (rope component). If separable, the restriction is pure extraction riding on a real coordination function (snare component within tangled_rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mobility_restriction_necessity, conceptual, 'Whether the coordination and extraction components of the sovereignty reading are structurally separable.').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds mobile citizens to the federation mobility promise such that exit from the extraction is identity-locked rather than merely constrained?',
    'Longitudinal survey of mobile citizen populations: measure identity attachment to ''European citizen'' vs. national identity, and correlate with willingness to accept mobility restriction vs. demand for rights enforcement. Compare with cross-border workers (trapped) and asylum seekers (trapped) to isolate identity component.',
    'If identity_locked is confirmed, the sovereignty reading''s extraction is amplified by the victims'' own identity investment — they cannot ''just leave'' the constraint without abandoning a constitutive self-concept. This makes the constraint more extractive than a comparable constraint with merely constrained exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'The psychological/institutional mechanism that fuses mobile citizens'' identity to the mobility promise, making exit identity-locked.').

omega_variable(
    sovereignty_reading_foreclosure,
    'Does the sovereignty reading''s core premise (national border legitimacy as treaty-conditional) logically foreclose the integration reading''s core premise (free movement as constitutional right) within any single legal framework, or do they merely coexist as competing positions?',
    'Constitutional court jurisprudence analysis: when a national court invokes treaty conditionality to restrict mobility, does it treat the integration reading''s constitutional claim as legally incoherent (foreclosed) or as a competing interpretation that loses on balance (coexists)? Track citation patterns in CJEU vs. national constitutional court dialogues.',
    'If forecloses, the two readings cannot coexist in one framework — the federation must choose one constitutional logic. If coexists_with, the tension is permanent and the constraint family exhibits stable structural conflict. The reading_relation declaration (coexists_with) reflects the latter assessment, but this omega documents the irreducible ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_reading_foreclosure, conceptual, 'Whether the sovereignty and integration readings are logically incompatible within a single framework or merely competing live positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(federation_membership__sovereignty_reading_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(federation_membership__sovereignty_reading_tr_t6, federation_membership__sovereignty_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(federation_membership__sovereignty_reading_tr_t12, federation_membership__sovereignty_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(federation_membership__sovereignty_reading_tr_t18, federation_membership__sovereignty_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement(federation_membership__sovereignty_reading_tr_t24, federation_membership__sovereignty_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(federation_membership__sovereignty_reading_tr_t30, federation_membership__sovereignty_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(federation_membership__sovereignty_reading_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(federation_membership__sovereignty_reading_be_t6, federation_membership__sovereignty_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(federation_membership__sovereignty_reading_be_t12, federation_membership__sovereignty_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(federation_membership__sovereignty_reading_be_t18, federation_membership__sovereignty_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(federation_membership__sovereignty_reading_be_t24, federation_membership__sovereignty_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(federation_membership__sovereignty_reading_be_t30, federation_membership__sovereignty_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(federation_membership__sovereignty_reading_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(federation_membership__sovereignty_reading_su_t6, federation_membership__sovereignty_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(federation_membership__sovereignty_reading_su_t12, federation_membership__sovereignty_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(federation_membership__sovereignty_reading_su_t18, federation_membership__sovereignty_reading, suppression_requirement, 18, 0.69).
narrative_ontology:measurement(federation_membership__sovereignty_reading_su_t24, federation_membership__sovereignty_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(federation_membership__sovereignty_reading_su_t30, federation_membership__sovereignty_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership__sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% This constraint and federation_membership__integration_reading form a constraint family decomposing the federation_membership kernel. The sovereignty reading claims treaty conditionality and national border legitimacy (high ε from mobility restriction). The integration reading claims irreversible integration and free movement as constitutional right (lower ε, different beneficiary/victim structure). They are linked via affects_constraints. The ε values differ substantially: this reading's ε = 0.68 reflects extraction from mobile populations; the integration reading's ε would reflect extraction from national governments losing border control. Different referents, different structures, same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership__sovereignty_reading, organized, 0.15).
constraint_indexing:directionality_override(federation_membership__sovereignty_reading, institutional, 0.1).
constraint_indexing:directionality_override(federation_membership__sovereignty_reading, moderate, 0.85).
constraint_indexing:directionality_override(federation_membership__sovereignty_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
