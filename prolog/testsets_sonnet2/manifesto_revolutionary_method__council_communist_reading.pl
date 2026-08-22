% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Workers' Council (Soviet) Direct Democracy — Council Communist Reading
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This story authors the council-communist reading of the
 *   revolutionary-method kernel: the claim that workers' councils —
 *   federated, mandate-bound workplace assemblies with instantly recallable
 *   delegates — constitute the legitimate organ of proletarian power,
 *   replacing both the capitalist state apparatus and the centralized
 *   vanguard party. Internally, the arrangement this reading describes is
 *   close to pure coordination: assemblies solve real production and
 *   distribution problems with minimal coercive overhead, and no internal
 *   party captures rents from council operation (ε_low, ~0.25). What makes
 *   this reading structurally interesting is the asymmetry between its
 *   internal extraction profile and the suppression it meets from outside:
 *   rival readings (vanguardist and gradualist) have historically suppressed
 *   actual council formations by force (Kronstadt 1921, Barcelona 1937,
 *   Hungary 1956) rather than out-arguing them, so the story's suppression
 *   metric is high not because the council form suppresses its own
 *   participants but because it has never been permitted to persist against
 *   external state and party coercion long enough to be tested on its own
 *   terms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.62).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Workers' Council (Soviet) Direct Democracy — Council Communist Reading").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, 'db358958-845f-4940-b5dd-19da5b913204').
narrative_ontology:cs_kernel_codification('db358958-845f-4940-b5dd-19da5b913204', fixed_text).
narrative_ontology:cs_authority_grounding('db358958-845f-4940-b5dd-19da5b913204', distributed).
narrative_ontology:cs_reading_relation('db358958-845f-4940-b5dd-19da5b913204', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('db358958-845f-4940-b5dd-19da5b913204', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('db358958-845f-4940-b5dd-19da5b913204', foundational, power_must_be_held_at_point_of_production_not_delegated_upward).
narrative_ontology:cs_axiom_status(power_must_be_held_at_point_of_production_not_delegated_upward, holdable).
narrative_ontology:cs_axiom_grounding('db358958-845f-4940-b5dd-19da5b913204', power_must_be_held_at_point_of_production_not_delegated_upward, deontological).
narrative_ontology:cs_axiom('db358958-845f-4940-b5dd-19da5b913204', foundational, vanguard_party_leadership_reproduces_state_coercion).
narrative_ontology:cs_axiom_status(vanguard_party_leadership_reproduces_state_coercion, holdable).
narrative_ontology:cs_axiom_grounding('db358958-845f-4940-b5dd-19da5b913204', vanguard_party_leadership_reproduces_state_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('db358958-845f-4940-b5dd-19da5b913204', id_1917_1918_soviet_and_council_formations_pre_consolidation).
narrative_ontology:cs_drift_state('db358958-845f-4940-b5dd-19da5b913204', post_20th_century_state_socialist_collapse, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('db358958-845f-4940-b5dd-19da5b913204', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, workplace_assembly_members).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, federated_council_delegates).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, capitalist_property_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Meet in open workplace assemblies where they debate and vote directly on production, distribution, and delegate mandates. They hold immediately recallable delegates rather than fixed representatives; their exit from any given council is real because they can reconstitute assemblies elsewhere without needing party sanction.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, workplace_assembly_members, agenda_setter,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, workplace_assembly_members, beneficiary).

% Federate horizontally with other collectives for coordination of supply chains and mutual defense, without ceding standing authority to any central committee. They gain self-management and collective control over the means of production they operate, at the cost of needing to build coordination capacity from scratch under contested conditions.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, mobile, regional).

% Carry revocable, mandated positions between federated councils, executing decisions taken at the assembly level rather than exercising independent authority. Their power is instantly recallable, which limits their own exit — deviation from mandate ends their delegation immediately.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, federated_council_delegates, agenda_setter,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, federated_council_delegates, beneficiary).

% Staff the standing administrative apparatus of the capitalist state — tax collection, policing, regulatory enforcement, permanent civil service. Under the council-communist reading this entire apparatus is to be dissolved and replaced by federated assemblies, dissolving their institutional function and position rather than transferring it to a new hierarchy they could occupy.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats, payer,
    institutional, biographical, trapped, national).

% Hold cadre positions in a centralized revolutionary party premised on leading the working class through a disciplined organizational hierarchy toward eventual state power. The council-communist reading treats this leadership function itself as illegitimate — mandates should flow from assemblies, not from a party center — displacing their organizational role even in a successful revolutionary scenario.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials, payer,
    organized, biographical, constrained, national).

% Hold formal ownership and control over enterprises that workplace assemblies would expropriate and place under direct worker management. Their exit is not organizational reassignment but outright dispossession of the property claim itself.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, capitalist_property_owners, payer,
    institutional, generational, trapped, global).

% Vanguardist and social-democratic currents argue council communism is organizationally naive — too slow to coordinate at scale, too vulnerable to counter-revolutionary suppression without disciplined leadership or gradual institutional capture. Historically, these currents have suppressed council formations (Kronstadt, Barcelona 1937) more often than council communists have been permitted to contest the argument on equal terms within the movement's own institutions.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, rival_socialist_currents, excluded,
    organized, generational, constrained, global).

% Study the empirical record of council formations (1905 and 1917 Russia, 1918-19 Germany, 1936 Spain, 1956 Hungary) to assess whether federated worker democracy has ever been permitted to stabilize before being crushed by either capitalist or vanguardist state power, or whether its structural instability is intrinsic.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, historical_materialist_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__council_communist_reading, diffuse).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__council_communist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of coordinating production and distribution among workers without recreating a standing coercive apparatus over them: mandated, recallable delegation lets assemblies coordinate at scale while power remains materially exercised at the point of production.
% TRANSFER_FUNCTION: Moves formal control over enterprises and administrative functions from capitalist owners and state bureaucrats to workplace assemblies, and moves organizational authority from vanguard party cadres to directly elected, recallable council delegates.
% ABSENT_VOICES: Rival socialist currents (vanguardist and social-democratic) would object that councils cannot defend a revolution against organized counter-revolutionary force without centralized command — a claim that has been settled historically by suppression (Kronstadt 1921, Barcelona May Days 1937) rather than by argument on equal footing.
% DISAPPEARANCE_RATIONALE: If workplace-assembly federation as a mode of holding power vanished, worker self-management would collapse back into either capitalist firm hierarchy or party-directed state planning — the entire structural claim of this reading is that a third form of holding power (neither capital nor vanguard party) is possible; its disappearance simply means that third form never existed as a stable state.
% FOUNDING_PROBLEM: The 1905 and 1917 Russian soviets and the German and Spanish council movements arose to solve a concrete problem: workers needed to coordinate seizure and running of production during revolutionary rupture, without either restoring capitalist management or subordinating themselves to a party apparatus that might reproduce state coercion in new form.
% FOUNDING_PROBLEM_CORROBORATION: Council communists (Pannekoek, Mattick, the Dutch-German left) attest the problem remains live because every vanguardist revolution to date (Russia, China, Cuba) reproduced a coercive party-state rather than dissolving it. Independent historians of the Russian Revolution and the Spanish Civil War (outside both council-communist and vanguardist traditions) corroborate that soviet and collective structures were suppressed by Bolshevik and Republican-aligned Communist forces respectively — evidence that the councils were never permitted to fail or succeed on their own terms, which is itself contested by vanguardist historiography holding centralization was a wartime necessity, not a substitution of the founding problem.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.25, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.25 at interval end) because within a functioning council structure, the coordinating delegates hold no standing surplus over the assemblies that mandate and can instantly recall them — there is no rent-collecting seat internal to the arrangement. Suppression is authored high (0.62) and rising sharply early in the interval because the metric captures the coercive response councils have met from both capitalist state forces and vanguard party apparatuses at the moment of formation, not coercion the council form itself applies to its members. Theater ratio stays low and flat (0.20) because delegate mandate-and-recall is a functioning mechanism in the historical record (1917 soviets prior to Bolshevik consolidation, early Spanish collectives), not empty ritual.
 *
 * PERSPECTIVAL GAP:
 *   From the workplace-assembly seat, federation is genuine low-coercion coordination — the engine should compute this seat close to rope. From the state-bureaucrat and vanguard-party seats, the same arrangement is experienced as elimination of their institutional position, which the engine may compute closer to snare or tangled_rope depending on how it weighs their institutional power against the low internal ε. This divergence is exactly the seat-computation the framework exists to surface: the coordination story is real for the assemblies and simultaneously extractive-by-elimination for the displaced apparatuses.
 *
 * DIRECTIONALITY LOGIC:
 *   Workplace assembly members and autonomous worker collectives are declared beneficiaries: they gain direct control over production and distribution and bear no imposed hierarchy, so directionality sits near the full-beneficiary end. State bureaucrats and vanguard party officials are declared victims: the reading's success dissolves their institutional function and organizational role respectively, placing them near the full-target end regardless of their considerable external power, because within THIS reading's kernel they have no seat to occupy. Capitalist property owners are payers in the strongest sense — dispossession, not reassignment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating revolutionary production without recreating coercive centralized power — remains contested as live: council communists hold it live because every historical vanguardist success reproduced a party-state; vanguardists and gradualists hold the council form obsolete or naive because it has never survived counter-revolutionary pressure. This story does not resolve that contest; it authors only the council-communist reading's own internal structure, which is the discipline Rule 1 requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    council_scalability_ambiguity,
    'Can federated workplace assemblies coordinate production and self-defense at the scale of a modern industrial economy under active counter-revolutionary pressure, or does the council form require a scale/security tradeoff that historically forces recentralization?',
    'Comparative institutional analysis of the surviving duration and coordination capacity of historical council formations (1917-18 Russia pre-Bolshevik consolidation, 1918-19 Germany, 1936-37 Spain, 1956 Hungary) against contemporaneous centralized alternatives operating under comparable external pressure.',
    'If councils are shown to require recentralization under military pressure as an empirical regularity rather than a contingent historical accident, the council-communist reading''s low internal ε would need re-examination for a hidden centralization cost currently unmeasured; if councils can be shown to have been crushed by external force rather than internal scalability failure, the reading''s claim stands unweakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_scalability_ambiguity, empirical, 'Whether council federation is scalable/defensible or intrinsically unstable under pressure.').

omega_variable(
    kernel_reading_incommensurability,
    'Do the council-communist, vanguard-rupture, and democratic-gradualist readings of the revolutionary-method kernel share enough common ground (a shared founding text, a shared diagnosis of capitalist class rule) to be considered readings of ONE kernel, or have they diverged into structurally distinct political traditions with only a common ancestor text?',
    'Textual and doctrinal-history analysis of whether adherents of each reading recognize the others as legitimate interpretive contests over the same commitments, versus treating each other as having abandoned the shared commitment altogether.',
    'If the readings are genuinely incommensurable rather than contesting readings, the kernel_id itself may need splitting into separate lineages rather than three readings of one kernel — this would not change this story''s own ε or classification but would affect how the reading_relations to siblings should be authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three readings genuinely share one kernel or have split into separate traditions.').

omega_variable(
    internal_ep_vs_external_suppression_attribution,
    'How much of the measured high suppression (0.62) is properly attributable to this constraint''s own operation versus to the operation of the rival readings that suppress it externally?',
    'Decompose suppression events historically by actor: catalog whether coercive incidents (Kronstadt, Barcelona) were internal to council governance or externally imposed by vanguardist/state forces, and re-derive a suppression measure attributable strictly to the council form''s own internal operation.',
    'If nearly all measured suppression is externally imposed, the council-communist reading''s TRUE internal suppression may be near-zero (closer to rope than the authored 0.62 suggests), with the authored figure conflating the reading''s own structure with the hostile environment it operates in — this would sharpen the seat divergence analysis rather than change beneficiary/victim declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_ep_vs_external_suppression_attribution, empirical, 'Whether authored suppression reflects this reading''s own operation or the coercion of rival readings against it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mani_tr_t4, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(mani_tr_t8, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(mani_tr_t12, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(mani_tr_t16, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(mani_tr_t24, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 24, 0.2).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(mani_be_t4, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(mani_be_t8, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(mani_be_t12, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(mani_be_t16, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(mani_be_t24, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 24, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mani_su_t4, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(mani_su_t8, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(mani_su_t12, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(mani_su_t16, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(mani_su_t24, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__council_communist_reading, 0.1).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'the Manifesto's revolutionary method' per the epsilon-invariance principle. council_communist_reading (this file) authors ε_low (~0.25) internally with high external suppression from rival readings; vanguard_rupture_reading and democratic_gradualism_reading are separate files with their own ε values, beneficiary/victim sets, and claimed types. All three link to each other via affects_constraints because each reading's historical practice materially shaped the legitimacy conditions and resource availability of the others (e.g., Bolshevik suppression of soviets directly shaped the council-communist tradition's later theoretical positions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
