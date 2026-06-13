% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Gelassenheit Separation Principle: Functional Isolation Reading
 *   domain: religious/technological/institutional
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested Gelassenheit
 *   separation kernel. The PRINCIPLE READING interprets separation as
 *   avoidance of structural entanglement in worldly systems (economic
 *   dependency, informational integration, institutional hierarchy),
 *   permitting technologies that are functionally isolated—solar power
 *   off-grid, pneumatic tools without connection to external networks—while
 *   forbidding technologies that create structural entanglement regardless of
 *   isolation possibility (internet, insurance, banking). This reading is
 *   distinct from the ARTIFACT READING (separation means visible material
 *   distinctiveness from English society) and the CONSEQUENCE READING
 *   (separation means preserving community practices like visiting and mutual
 *   aid). The principle reading makes a specific claim about what separation
 *   IS—functional independence—that differs structurally from what the
 *   artifact and consequence readings claim. Each reading produces its own
 *   constraint with its own ε, its own victim set, and its own
 *   classification. This document generates ONLY the principle reading.
 *
 * KEY AGENTS:
 *   - community_theological_authority: Institutional agenda-setter with interpretive authority; derives legitimacy from lineage but must adjudicate ambiguous functional-isolation cases
 *   - technology_adopters: Moderate-power payers with identity-locked exit; bear the cost of petitioning and uncertainty; include both individuals and household cooperatives
 *   - boundary_ambiguity_bearers: Powerless payers carrying the cost of the principle reading's core ambiguity—the distinction between functional and structural entanglement is contested even within the doctrine
 *   - historical_separation_tradition: Non-agent beneficiary (a vindicated proposition, not an actor); the principle reading upholds a specific theological claim about how separation can be maintained
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.45).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.52).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Gelassenheit Separation Principle: Functional Isolation Reading").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious/technological/institutional").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, '724e5724-4858-45ad-98a6-867304eaf06d').
narrative_ontology:cs_kernel_codification('724e5724-4858-45ad-98a6-867304eaf06d', distributed).
narrative_ontology:cs_authority_grounding('724e5724-4858-45ad-98a6-867304eaf06d', lineage).
narrative_ontology:cs_interpretation_layer_present('724e5724-4858-45ad-98a6-867304eaf06d').
narrative_ontology:cs_reading_relation('724e5724-4858-45ad-98a6-867304eaf06d', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('724e5724-4858-45ad-98a6-867304eaf06d', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('724e5724-4858-45ad-98a6-867304eaf06d', foundational, functional_isolation_sufficiency).
narrative_ontology:cs_axiom_status(functional_isolation_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('724e5724-4858-45ad-98a6-867304eaf06d', functional_isolation_sufficiency, deontological).
narrative_ontology:cs_axiom('724e5724-4858-45ad-98a6-867304eaf06d', foundational, structural_entanglement_prohibited).
narrative_ontology:cs_axiom_status(structural_entanglement_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('724e5724-4858-45ad-98a6-867304eaf06d', structural_entanglement_prohibited, deontological).
narrative_ontology:cs_reference_frame('724e5724-4858-45ad-98a6-867304eaf06d', functional_independence_separateness).
narrative_ontology:cs_drift_state('724e5724-4858-45ad-98a6-867304eaf06d', digital_communication_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('724e5724-4858-45ad-98a6-867304eaf06d', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, community_theological_authority).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, technology_adopters).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, boundary_ambiguity_bearers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).
:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is MODERATE (0.45 at interval end), not high. The principle reading creates coordination value—it solves the genuine dilemma of how to permit beneficial technology while maintaining separation identity—and it does not require the high suppression a snare would. However, it is still extractive because (1) interpretive authority is centralized and boundary-ambiguous cases create asymmetric costs; (2) the constraint forbids technologies (internet, insurance) whose functional isolation is technically possible but definitionally excluded by structural-entanglement reasoning; (3) the uncertainty cost falls hardest on technology adopters and powerless boundary-ambiguity bearers. Suppression is moderate (0.52): the constraint is actively maintained through petition review, education of younger members, and exclusion of competing readings from authority. Theater is low-moderate (0.28): the principle reading is genuinely functional (it does permit some technologies and genuinely solves a dilemma), but as stakes_inflation rises over the interval (digital connectivity's expanding importance), more of the enforcement activity becomes performative—the authority must work harder to defend functional-isolation boundaries that feel increasingly arbitrary to younger adopters. Accessibility_collapse is high (0.72): once a technology is classified as structurally entangled, alternatives effectively disappear for identity-locked members (exit costs include severing kinship, shared economy, spiritual belonging). The measurement series track the period from 1950 (farm mechanization era) through 2026 (digital communication era). Extractiveness and suppression_requirement show monotonic rise: as technology stakes increase (internet, smartphones, digital agriculture), the authority's work to maintain boundaries intensifies and the cost to petitioners rises. Theater_ratio shows early rise (as electrification and communication technology proliferate) then plateaus (the authority has achieved consistent enforcement by 2010). The shared time grid enables all three metrics to be read together and shows that theater stabilizes while extractiveness and suppression continue rising—a sign that the constraint is settling into a more stable, performatively-maintained equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The authority and technology adopters should compute different types. From the authority's seat, the principle reading is genuine coordination (it permits technology adoption within a maintained separation identity; without it, the community faces binary choices). From the adopter's seat, especially for identity-locked individuals, the constraint computes as extractive (interpretive power is centralized, boundary cases are uncertain and costly, and technologies technically isolated are forbidden anyway). From the boundary-ambiguity bearer's seat, who has no voice in interpretation, the constraint computes as a snare (pure suppression). The engine will compute per-seat types from the structural data; the authored claim does not adjudicate which is 'correct.' The divergence is what the apparatus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Community_theological_authority (d near 0.1, beneficiary): sets rules, collects no direct rent but preserves interpretive authority; moderately mobile (could adopt other readings). Technology_adopters (d near 0.75, mixed): bear petition costs and uncertainty; identity_locked exit means high d even on individual calculations, but they also benefit from access to some technologies. Boundary_ambiguity_bearers (d near 0.85, victims): powerless, identity_locked, bear the cost of the doctrine's internal contradiction without voice in resolution. The principle reading's structural data generates a directionality gradient that should reflect these differences; no overrides are needed because the derivation chain (beneficiary + victim + exit) captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The principle reading exhibits early-stage mandatrophy signals. The founding problem—how to permit beneficial technology while maintaining separation—is contested in status (the authority attests it is live; artifact and consequence advocates attest the principle reading MISIDENTIFIES separation). The theater_ratio is rising as technology stakes increase, suggesting the authority spends more effort on performative boundary maintenance. However, the constraint has not yet fully atrophied: it genuinely permits some technologies (solar, pneumatic tools) that a pure artifact reading would forbid, and it has generated a coherent interpretive tradition. Full mandatrophy would require the founding problem to be dead AND the constraint to persist as pure performance. Currently, the principle reading remains active coordination with extractive overlay, not yet degraded inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_isolation_ambiguity,
    'What counts as structural entanglement versus functional isolation? The principle reading claims a solar system off-grid is functionally isolated but internet access is structurally entangling. But is a solar system truly isolated if it uses supply-chain components from global manufacturing? Is internet access truly entangling if used only for information retrieval without economic dependency?',
    'Systematic case-law development within the community: trace which technologies the authority permits and forbids, and identify the pattern. Compare against the community''s actual structural dependencies (supply chains, information flows, financial entanglement) and measure whether the authority''s classifications track those dependencies or rest on definitional boundaries.',
    'If the classifications rest on actual dependency patterns, the principle reading is coherent; if they rest on definitional boundaries that do not track actual integration into worldly systems, the reading is internally inconsistent and the constraint is more extractive than authored (the authority is defending boundaries, not genuine separation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_isolation_ambiguity, empirical, 'Whether the functional isolation criterion tracks actual structural dependency or is a definitional boundary the authority maintains').

omega_variable(
    interpretation_authority_consolidation,
    'Is the centralization of interpretive authority a necessary feature of the principle reading, or is it an institutional artifact that could be distributed? Could the principle reading—functional isolation as the separation criterion—be maintained with distributed authority (household-level decision-making, consensus discernment) rather than institutional adjudication?',
    'Comparison with communities that claim to hold the principle reading but use different authority structures; analysis of whether household-level interpretation produces more or less consistency in technology adoption; ethnographic study of whether centralized authority produces more defensible boundaries or just more uniform boundaries.',
    'If distributed authority could maintain functional-isolation reasoning consistently, the constraint''s extractiveness is a feature of institutional concentration rather than the principle reading itself. If only centralized authority can maintain consistency, the extractiveness is inherent to this reading as instantiated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_authority_consolidation, conceptual, 'Whether the principle reading''s extractiveness is inherent or institutional').

omega_variable(
    sibling_reading_boundary_permeability,
    'To what degree can the principle reading coexist within a single community with the artifact or consequence readings? Or does each reading require institutional consolidation around a single evaluative framework to function?',
    'Study of communities that explicitly permit multiple readings (e.g., ''some households use functional-isolation reasoning, others use artifact-based reasoning, others use consequence-based reasoning''). Document whether the community experiences this as productive pluralism or as boundary dissolution.',
    'If the readings can coexist without convergence, the principle reading''s extractiveness is lower (less authority consolidation needed) and its classification might shift. If they require institutional choice and exclusion, the principle reading''s extractiveness is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary_permeability, conceptual, 'Whether the sibling readings are genuinely coexistable or mutually exclusive within operational community practice').

omega_variable(
    identity_lock_internalization,
    'Is the measured suppression structural (the authority has genuine enforcement capacity—exclusion, shunning, economic sanctions) or internalized (technology adopters believe they SHOULD want separation even when exit is technically available)? Or both, and in what proportion?',
    'Post-exit trajectory study: follow individuals who leave the community and measure whether suppression (resistance to technology adoption, separation-oriented worldview) persists after the authority''s enforcement mechanisms are removed. If it persists substantially, suppression is internalized; if it decays, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and the constraint approaches snare-level suppression at the individual seat. If structural, the constraint is more vulnerable to authority decay and less stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether suppression is structural or internalized in the principle reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 1950, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t1950, gelassenheit_separation__principle_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(gela_tr_t1970, gelassenheit_separation__principle_reading, theater_ratio, 1970, 0.16).
narrative_ontology:measurement(gela_tr_t1990, gelassenheit_separation__principle_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(gela_tr_t2010, gelassenheit_separation__principle_reading, theater_ratio, 2010, 0.27).
narrative_ontology:measurement(gela_tr_t2020, gelassenheit_separation__principle_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(gela_tr_t2026, gelassenheit_separation__principle_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(gela_be_t1950, gelassenheit_separation__principle_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(gela_be_t1970, gelassenheit_separation__principle_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement(gela_be_t1990, gelassenheit_separation__principle_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(gela_be_t2010, gelassenheit_separation__principle_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(gela_be_t2020, gelassenheit_separation__principle_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(gela_be_t2026, gelassenheit_separation__principle_reading, base_extractiveness, 2026, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t1950, gelassenheit_separation__principle_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(gela_su_t1970, gelassenheit_separation__principle_reading, suppression_requirement, 1970, 0.42).
narrative_ontology:measurement(gela_su_t1990, gelassenheit_separation__principle_reading, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement(gela_su_t2010, gelassenheit_separation__principle_reading, suppression_requirement, 2010, 0.51).
narrative_ontology:measurement(gela_su_t2020, gelassenheit_separation__principle_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(gela_su_t2026, gelassenheit_separation__principle_reading, suppression_requirement, 2026, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__principle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__principle_reading, 0.12).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the gelassenheit_separation kernel. All three readings emerge from a single community commitment ('separation') but instantiate different evaluative frameworks: PRINCIPLE_READING uses functional isolation (this file); ARTIFACT_READING uses visible material distinctiveness; CONSEQUENCE_READING uses community-relational effects. Each reading has a different epsilon, different victim set, and different classification because each claims a different structural criterion for separation. They are not perspectives on the same constraint—they are three distinct constraints linked by kernel, not by alternative observation. The principle reading influences both siblings: it constrains their authority space by establishing functional isolation as an alternative evaluative framework, which creates pressure on artifact-based advocates (why forbid solar if it is functionally isolated?) and on consequence-based advocates (why forbid internet if community practices are preserved?). Both sibling readings coexist as live positions held by different community factions; the principle reading does not foreclose them but changes their epistemic burden.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
