% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__council_communist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Council Communist Revolutionary Method
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint instantiates the council communist reading of the
 *   revolutionary method kernel: the claim that workers' councils (soviets)
 *   must replace both the capitalist state and the vanguard party as the
 *   direct democratic organs of working-class power. Power is held by
 *   federated workplace assemblies with recallable delegates. The constraint
 *   is structurally contested by two sibling readings: the vanguard rupture
 *   reading (party seizure of state power) and the democratic gradualism
 *   reading (electoral reform). This reading treats the council system as a
 *   low-extraction coordination mechanism that nevertheless actively
 *   disenfranchises the old state and party apparatus. The authored metrics
 *   (Îµ=0.25, high resistance) and claimed type (tangled_rope) are
 *   independent: the engine may compute a different per-seat classification
 *   based on structural position.
 *
 * KEY AGENTS:
 *   - autonomous_worker_collectives: Primary beneficiary and agenda-setter (organized/constrained) â hold direct democratic power through federated assemblies.
 *   - state_bureaucrats: Primary payer/victim (institutional/trapped) â lose administrative monopoly and material privilege.
 *   - party_officials: Secondary payer/victim (organized/trapped) â lose vanguard authority and structural role.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.7).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Council Communist Revolutionary Method").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__council_communist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, 'abd63372-f32b-4bb5-902e-6956a3eed71f').
narrative_ontology:cs_kernel_codification('abd63372-f32b-4bb5-902e-6956a3eed71f', distributed).
narrative_ontology:cs_authority_grounding('abd63372-f32b-4bb5-902e-6956a3eed71f', distributed).
narrative_ontology:cs_reading_relation('abd63372-f32b-4bb5-902e-6956a3eed71f', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('abd63372-f32b-4bb5-902e-6956a3eed71f', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('abd63372-f32b-4bb5-902e-6956a3eed71f', foundational, direct_council_sovereignty).
narrative_ontology:cs_axiom_status(direct_council_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('abd63372-f32b-4bb5-902e-6956a3eed71f', direct_council_sovereignty, deontological).
narrative_ontology:cs_axiom('abd63372-f32b-4bb5-902e-6956a3eed71f', foundational, party_anti_sovereignty).
narrative_ontology:cs_axiom_status(party_anti_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('abd63372-f32b-4bb5-902e-6956a3eed71f', party_anti_sovereignty, deontological).
narrative_ontology:cs_reference_frame('abd63372-f32b-4bb5-902e-6956a3eed71f', direct_democratic_self_management).
narrative_ontology:cs_drift_state('abd63372-f32b-4bb5-902e-6956a3eed71f', post_vanguard_consolidation_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('abd63372-f32b-4bb5-902e-6956a3eed71f', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, party_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers organized into workplace and community assemblies that hold direct legislative and executive power through recallable delegates. They coordinate production, distribution, and administration without mediation by a separate state or party apparatus. Exit from the council system means leaving the self-governing workplace or community, which severs both livelihood and political voice.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, agenda_setter).

% Career administrators of the displaced capitalist or transitional state apparatus. They lose their monopoly on administrative expertise, budgetary control, and hierarchical authority as council committees absorb state functions. Their structural position is abolished rather than reformed; many face material dispossession and political exclusion.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats, payer,
    institutional, biographical, trapped, national).

% Functionaries of vanguard or parliamentary parties who see their organizational role dissolve as the councils become the exclusive site of political decision-making. They lose career trajectories, ideological authority, and structural leverage as the party form is superseded by direct assembly power.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, party_officials, payer,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective self-governance of the working class through federated, recallable workplace assemblies, replacing separate political and economic administration with direct democratic control over production and state functions.
% TRANSFER_FUNCTION: Transfers political power and administrative authority from professional state bureaucrats and party officials to autonomous workplace collectives and their recallable delegates.
% ABSENT_VOICES: Liberal democratic theorists, social democratic parliamentarians, and technocratic managerial strata who would argue for representative institutions, gradual reform, or expert administration are structurally excluded from the councils' legitimating framework; their voices appear only as external opposition.
% DISAPPEARANCE_RATIONALE: If the council system vanished, workplace assemblies would lose their governing authority, state and party structures would reassert administrative monopoly, and the pattern of direct democratic self-management would collapse back into representative or vanguardist hierarchies.
% FOUNDING_PROBLEM: The capitalist state and vanguard party apparatus constitute alienated power structures that reproduce class domination; working-class self-emancipation requires organs of direct democracy that fuse economic and political power.
% FOUNDING_PROBLEM_CORROBORATION: Council communist theorists such as Pannekoek and RÃ¼hle attest the founding problem from within the tradition. Mainstream social democratic, liberal, and vanguardist historians largely deny the problem, attributing council failures to internal incoherence rather than external suppression. Explicit corroboration from outside the beneficiary set is sparse and politically polarized; most non-beneficiary accounts treat the founding problem as an ideological construction rather than an empirical diagnosis.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.25, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__council_communist_reading_tests).
:- end_tests(manifesto_revolutionary_method__council_communist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.25 because the council system is designed as decentralized self-management with minimal surplus extraction; costs are primarily participatory (attendance, vigilance) rather than transferred surplus. Suppression is high (0.70) because the constraint's realization requires actively dismantling the competing state and party apparatus and preventing their restoration. Theater is low (0.18) because council proceedings are meant to be substantive decision-making rather than ritual. Resistance is very high (0.85) because the displaced state and party strata actively resist dismantling, and rival readings (vanguardism, gradualism) structurally oppose council supremacy. Accessibility_collapse is moderate (0.40): alternatives (parliamentary democracy, vanguard state) remain conceptually available and are actively championed by rival readings.
 *
 * PERSPECTIVAL GAP:
 *   The autonomous worker collectives experience the constraint as self-government and coordination; the engine will compute a low directionality and likely classify their seat as rope or scaffold. State bureaucrats and party officials experience the constraint as dispossession and disenfranchisement; the engine will compute high directionality and likely classify their seat as snare. The divergence is structural: the same arrangement is simultaneously self-management for one class and expropriation for another.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the autonomous worker collectives, who receive political power and coordination benefits. Victim declarations map to state bureaucrats and party officials, who bear the cost of lost authority and privilege. The low extractiveness reflects that the transfer is primarily political (authority) rather than economic (surplus), though material privilege is also affected. No override is needed: the structural derivation (beneficiary + organized + constrained exit vs. victim + institutional/organized + trapped exit) correctly assigns directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the council system as pure coordination (rope) â which would ignore the active disenfranchisement of the old state/party strata â or as pure extraction (snare) â which would ignore the genuine democratic coordination function for workers. It also distinguishes the constraint from a scaffold: while council communists often view the council phase as transitional to full communism, the constraint itself lacks a formal sunset clause and its justification is the steady state of workers' self-management, not merely a transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    council_stability_under_siege,
    'Does the council system inherently tend toward higher extractiveness and bureaucratization when under external military and economic siege, or can it maintain its low-extraction coordination function?',
    'Comparative historical analysis of council regimes in Germany (1918-1921), Russia (1917-1918), and Hungary (1956) to measure internal extraction trajectory under external pressure.',
    'If extraction inevitably rises under pressure, the low Îµ authored here reflects a transient pre-bureaucratic phase, and the constraint is better classified as scaffold or piton in practice; if stable, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_stability_under_siege, empirical, 'Whether council communism''s low extraction is structurally stable or context-dependent.').

omega_variable(
    extraction_vs_expropriation_boundary,
    'Is the displacement of state bureaucrats and party officials by workers'' councils ongoing extraction or a one-time revolutionary expropriation?',
    'Analysis of material flows after council seizure of power: do former bureaucrats suffer continuous extraction (e.g., reduced rations, forced labor) or a discrete loss of privilege?',
    'Ongoing extraction supports tangled_rope classification; discrete expropriation supports a rope classification with a revolutionary rupture event.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_expropriation_boundary, conceptual, 'Whether victim status is continuous extraction or one-time dispossession.').

omega_variable(
    committer_kernel_ambiguity,
    'This constraint is one reading of the contested kernel manifesto_revolutionary_method. Does the council communist reading''s logical incompatibility with vanguard party sovereignty represent a genuine foreclosure or a strategic disagreement resolvable by synthesis (e.g., Luxemburgist dual legitimation)?',
    'Textual and historical analysis of attempts to reconcile council sovereignty with party leadership (Kautsky, Luxemburg, early Bolshevism) to determine logical versus strategic contradiction.',
    'If resolvable, the relation to vanguard_rupture_reading should shift from forecloses to influences, altering the kernel''s contamination propagation model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_ambiguity, conceptual, 'Foreclosure versus strategic tension between council and vanguard readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mani_tr_t6, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(mani_tr_t12, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(mani_tr_t18, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 18, 0.16).
narrative_ontology:measurement(mani_tr_t24, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 24, 0.18).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(mani_be_t6, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 6, 0.2).
narrative_ontology:measurement(mani_be_t12, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(mani_be_t18, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 18, 0.24).
narrative_ontology:measurement(mani_be_t24, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 24, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(mani_su_t6, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 6, 0.63).
narrative_ontology:measurement(mani_su_t12, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(mani_su_t18, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 18, 0.69).
narrative_ontology:measurement(mani_su_t24, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
