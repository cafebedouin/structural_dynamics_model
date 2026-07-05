% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Council Communist Reading: Federated Workplace Assemblies Replacing State and Party
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This story instantiates the council communist reading of the manifesto's
 *   revolutionary-method kernel: power held directly by federated,
 *   mandate-bound workplace assemblies, with no standing state apparatus and
 *   no vanguard party mediating between class and power. Internally, this
 *   arrangement is close to a genuine coordination mechanism — low
 *   extraction, because delegate recall prevents rent accumulation, and
 *   coordination is horizontal rather than hierarchically captured. What
 *   rises sharply over the measured interval is not internal extraction but
 *   external suppression: the suppression_requirement series tracks the
 *   historically observed pattern in which council formations (Petrograd
 *   1917, Barcelona 1936-37, Hungary 1956) faced escalating coercive pressure
 *   from both counter-revolutionary state powers and rival revolutionary
 *   factions (vanguard parties) seeking to recapture the coordination
 *   function the councils had displaced. The theater_ratio stays low because
 *   council institutions in this reading are not performing coordination
 *   while doing something else — the low ratio reflects genuine functional
 *   operation, not absence of drift.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.68).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Council Communist Reading: Federated Workplace Assemblies Replacing State and Party").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, 'd627a76f-7c3d-4a32-9e87-f49851748fa2').
narrative_ontology:cs_kernel_codification('d627a76f-7c3d-4a32-9e87-f49851748fa2', distributed).
narrative_ontology:cs_authority_grounding('d627a76f-7c3d-4a32-9e87-f49851748fa2', distributed).
narrative_ontology:cs_reading_relation('d627a76f-7c3d-4a32-9e87-f49851748fa2', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('d627a76f-7c3d-4a32-9e87-f49851748fa2', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('d627a76f-7c3d-4a32-9e87-f49851748fa2', foundational, no_permanent_directing_organ_above_the_class).
narrative_ontology:cs_axiom_status(no_permanent_directing_organ_above_the_class, holdable).
narrative_ontology:cs_axiom_grounding('d627a76f-7c3d-4a32-9e87-f49851748fa2', no_permanent_directing_organ_above_the_class, deontological).
narrative_ontology:cs_axiom('d627a76f-7c3d-4a32-9e87-f49851748fa2', secondary, recallable_mandate_prevents_power_accumulation).
narrative_ontology:cs_axiom_status(recallable_mandate_prevents_power_accumulation, holdable).
narrative_ontology:cs_axiom_grounding('d627a76f-7c3d-4a32-9e87-f49851748fa2', recallable_mandate_prevents_power_accumulation, instrumental).
narrative_ontology:cs_reference_frame('d627a76f-7c3d-4a32-9e87-f49851748fa2', pre_party_soviet_dual_power_1917).
narrative_ontology:cs_drift_state('d627a76f-7c3d-4a32-9e87-f49851748fa2', post_bolshevik_consolidation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('d627a76f-7c3d-4a32-9e87-f49851748fa2', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, council_delegates).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, rank_and_file_workers).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, displaced_state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, displaced_party_officials).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, council_delegates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers organized at the point of production form assemblies that directly deliberate and vote on production and distribution decisions without delegating standing authority to a permanent state apparatus or a party hierarchy. They hold recallable mandates over their delegates and can reconstitute the federation's coordinating bodies at will. Their exit from any single council structure is real — they can recompose federations, split, or merge — because no external body holds coercive power over them.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, agenda_setter).

% Elected and immediately recallable representatives who carry the mandates of their sending assemblies into federated coordinating congresses. They administer coordination between councils but hold no independent power base; their tenure is contingent on constant reaffirmation. They bear reputational and material cost if they drift from their base's mandate, since recall is swift and unceremonious.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, council_delegates, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, council_delegates, payer).

% Career administrators whose function — managing coercive apparatus, taxation, and centralized law — has no place in a council system organized around federated, mandate-bound worker assemblies. Their institutional position, professional identity, and material security are extinguished by the abolition of the standing state; they cannot simply relocate their function, since council coordination bodies are structurally designed to prevent a permanent administrative caste from re-forming.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, displaced_state_bureaucrats, payer,
    institutional, biographical, trapped, national).

% Vanguard party cadres whose claim to lead the working class through a disciplined, centralized organization is explicitly ruled out by this reading, which treats party-directed seizure of state power as itself a reproduction of hierarchical rule. Their organizational capital — cadre discipline, ideological authority, hierarchical command structure — becomes a liability rather than an asset once councils federate directly without party mediation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, displaced_party_officials, payer,
    organized, biographical, trapped, national).

% Adherents of the vanguard-rupture and democratic-gradualism readings contest the council communist claim in movement debates, historical retrospectives (Kronstadt, factory committees in 1917-18, German council movement 1918-19), and organizational competition for the same working-class constituency. They are structurally present in the broader kernel contest but are not seated within this constraint's own internal decision structure — this reading's internal governance gives them no vote.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, rival_reading_adherents, excluded,
    organized, generational, constrained, continental).

% Surrounding capitalist states and rival socialist state formations have historically moved militarily and economically against council formations lacking a centralized apparatus capable of waging war or negotiating diplomatically — this is the primary external source of suppression this reading faces, distinct from the low internal extractiveness of the council structure itself.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, external_state_powers, excluded,
    institutional, generational, arbitrage, national).

% Historians and political theorists who examine the record of actual council formations (Petrograd Soviet before Bolshevik consolidation, Barcelona 1936-37, Hungary 1956 workers' councils) to assess whether federated council power proved viable absent either state or party mediation, and what caused its recurring defeat or absorption.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, historical_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__council_communist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates production and distribution decisions across workplaces through recallable, mandate-bound delegate structures, solving the collective-action problem of large-scale economic coordination without reconstituting a standing coercive apparatus or a permanent directing organization above the working class itself.
% TRANSFER_FUNCTION: Moves decision-making authority away from both the capitalist state's administrative apparatus and the vanguard party's centralized leadership, and distributes it horizontally among workplace assemblies federated through recallable delegation; no rent or surplus is extracted upward to a permanent coordinating body.
% ABSENT_VOICES: Displaced bureaucrats and party cadres would object that coordination at scale (defense, large infrastructure, foreign relations) requires more durable executive capacity than mandate-recall structures provide; they are excluded from this reading's own governance by design, since the reading's core premise is that such permanent capacity IS the problem, not the solution.
% DISAPPEARANCE_RATIONALE: If federated council power were to disappear as an organizing principle, the vacated coordination space would be filled either by a reconstituted state administration or by a vanguard party apparatus — both alternatives this reading exists specifically to foreclose. Historically, when actual councils were suppressed or absorbed (Kronstadt 1921, Barcelona 1937), the coordination function did not vanish; it was recaptured by exactly the apparatuses the councils displaced.
% FOUNDING_PROBLEM: The problem of how workers exercise power directly, at the point of production, without that power being alienated either into a bureaucratic state machine (which reproduces class domination in new form) or into a disciplined party apparatus claiming to act on the class's behalf (which substitutes party rule for proletarian self-rule).
% FOUNDING_PROBLEM_CORROBORATION: Council communist theorists (Pannekoek, Mattick) and participant memoirs from Barcelona's CNT-FAI collectives attest the problem remains live wherever centralized parties or states claim to act 'for' the working class. Historians outside the council communist tradition (e.g. accounts of the Kronstadt suppression and the Bolshevik dissolution of factory committees) corroborate that the tension between direct workers' power and centralized revolutionary authority was real and unresolved, not merely a theoretical construct of council communists themselves — but they diverge on whether the founding problem was ever actually solvable at scale, which is the substance of the contest with the sibling readings.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.25 at interval end) reflecting the reading's own structural claim: recallable delegation and horizontal federation prevent a coordinating body from accumulating standing extraction from the base it coordinates. Suppression is authored high and rising (0.40 to 0.68) because this reading's persistence depends overwhelmingly on resisting external suppression — both from surviving state apparatuses and from vanguard-party rivals contesting the same working-class constituency for organizational leadership. Accessibility collapse is moderate (0.35): alternatives to council federation (state capture, party leadership) remain visible and are actively organized by rival factions, so collapse is far from complete — this is not a mountain-like foreclosure of alternatives. Resistance is authored high (0.72) because the reading's own claim is inherently contested by two well-organized rival readings with material stakes in defeating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomous worker collectives and their recallable delegates sit near the beneficiary end: they exercise direct control and bear no extraction from a standing coordinating apparatus above them. Displaced bureaucrats and party officials sit near the full-target end: their institutional function and professional identity are structurally abolished by the reading's core claim, and their exit options are trapped — there is no alternative career path for a coercive-apparatus administrator or vanguard cadre inside a system explicitly designed to prevent the re-formation of such roles. Rival reading adherents are excluded rather than coordinated or extracted from — they are outside this reading's own decision structure by construction, contesting it from without.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — direct workers' power without alienation into state or party — is authored as contested rather than resolved: the reading's own adherents hold it live, while historical outcomes (repeated suppression or absorption of actual councils) leave open whether the arrangement was ever viable at scale for the durations claimed. This prevents mislabeling the reading as either a fully vindicated coordination success or a pure ideological cover story; the high external suppression and moderate accessibility collapse show real contest rather than either uncontested function or uncontested extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    council_scale_viability,
    'Can federated workplace assemblies coordinate large-scale functions (defense, heavy industry, interregional logistics) durably without reconstituting either a standing bureaucratic apparatus or a centralized party leadership — or does scale itself force a choice between the sibling readings?',
    'Comparative historical analysis of the duration and functional scope achieved by actual council formations (Petrograd 1917 pre-Bolshevik-consolidation, Barcelona 1936-37, Hungary 1956) against the scale and duration of the coordination tasks they faced, particularly under external military pressure.',
    'If council federations reliably fail at defense-scale coordination without reintroducing centralized command, this reading''s core claim collapses toward the vanguard_rupture_reading under pressure; if council federations can sustain such coordination, the sibling readings lose their strongest structural objection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_scale_viability, empirical, 'Whether horizontal council federation is structurally viable at the scale required for defense and heavy coordination, or whether scale forces convergence toward centralized alternatives.').

omega_variable(
    which_reading_is_the_kernel_committer,
    'Is the manifesto''s revolutionary-method kernel genuinely under-determined between these three readings, or does the source text itself privilege one reading (e.g. through emphasis on ''winning the battle of democracy'' vs. ''smashing the state machine'') such that the other readings are later interpretive impositions rather than co-equal readings?',
    'Close textual and historical-contextual analysis of the founding texts against the immediate post-1917 interpretive disputes (council communist critiques of Bolshevik practice vs. Leninist state theory vs. Second International gradualism) to establish whether the kernel is genuinely ambiguous or whether one reading has claimed under-determination it does not structurally possess.',
    'If the kernel privileges one reading, the other two readings (including this one) would need to be re-authored as contested-legitimacy constructions rather than co-equal committer readings — this would not change this reading''s internal ε but would change how its authority_grounding should be characterized relative to the siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_kernel_committer, conceptual, 'Whether the three sibling readings are genuinely co-equal committer readings of an ambiguous kernel, or whether the source text structurally favors one over the others.').

omega_variable(
    internal_extraction_under_prolonged_siege,
    'Does sustained external suppression force internal centralization within council federations that would raise their internal extractiveness above the authored low baseline — i.e., does defending against the vanguard_rupture_reading''s rivals and against surviving state power push councils toward reproducing the very centralized apparatus this reading rejects?',
    'Track internal governance changes within historical council formations under prolonged external military or political pressure (e.g., militia command centralization in Barcelona under the Spanish Civil War) and compare pre-siege and under-siege internal decision structures.',
    'If prolonged siege reliably raises internal extractiveness, the low ε (0.25) authored here should be understood as characteristic of the reading''s ideal form only, not of its sustained historical instances under pressure — this would motivate a separate constraint story for the under-siege variant rather than revising this story''s ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_extraction_under_prolonged_siege, empirical, 'Whether prolonged external suppression forces internal centralization that would change this reading''s own internal extraction profile over time.').


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
narrative_ontology:measurement(mani_tr_t8, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(mani_tr_t12, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(mani_tr_t16, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(mani_tr_t24, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 24, 0.2).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mani_be_t4, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 4, 0.18).
narrative_ontology:measurement(mani_be_t8, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(mani_be_t12, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(mani_be_t16, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(mani_be_t24, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 24, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mani_su_t4, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(mani_su_t8, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(mani_su_t12, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(mani_su_t16, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(mani_su_t24, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__council_communist_reading, 0.1).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the manifesto_revolutionary_method kernel. council_communist_reading, vanguard_rupture_reading, and democratic_gradualism_reading each instantiate a structurally distinct claim about how revolutionary transformation should proceed and where power should ultimately sit; each carries its own ε, beneficiary/victim structure, and classification. This file's low internal ε (0.25) contrasts with the expected higher internal extractiveness of vanguard_rupture_reading (where party leadership itself becomes a standing extractive position over the class it claims to represent) and with democratic_gradualism_reading's distinct suppression profile (electoral-institutional capture rather than direct coercive contest). All three should be read as co-equal, mutually contesting readings of the same underlying kernel, linked here for contamination and coupling analysis, not merged into one averaged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
