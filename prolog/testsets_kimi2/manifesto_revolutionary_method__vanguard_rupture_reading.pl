% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__vanguard_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__vanguard_rupture_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__vanguard_rupture_reading
 *   human_readable: Vanguard Party Seizure of State Power as Revolutionary Method
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the vanguard_rupture_reading of the
 *   manifesto_revolutionary_method kernel: the claim that revolutionary
 *   transformation requires organized seizure of state power by a vanguard
 *   party, establishing a party-guided dictatorship of the proletariat as the
 *   necessary transitional form. The reading treats centralized party-state
 *   authority as the indispensable instrument for defeating bourgeois
 *   reaction and directing social transformation. Structurally, the
 *   arrangement concentrates sovereign decision-making in the party cadre and
 *   state-planning apparatus, while suppressing political pluralism and
 *   autonomous worker self-organization as threats to unity or bourgeois
 *   infiltration. The authored metrics describe a constraint that carries a
 *   genuine coordination function (organizing defense against
 *   counter-revolution, coordinating social transformation) alongside high
 *   asymmetric extraction (monopoly of political power, bureaucratic resource
 *   capture). The claim/metric independence is maintained: the reading is
 *   claimed as tangled_rope while the metrics honestly report substantial
 *   extractiveness and suppression.
 *
 * KEY AGENTS:
 *   - vanguard_party_cadres: Primary agenda-setter (institutional/identity_locked) â sets and enforces the revolutionary method, captures state power and ideological monopoly.
 *   - state_planning_apparatus: Primary beneficiary (institutional/constrained) â benefits from centralized resource allocation authority and bureaucratic expansion.
 *   - political_pluralists: Primary target (powerless/trapped) â bears suppression of plural political organization and civil liberties.
 *   - autonomous_worker_organizations: Secondary target (organized/trapped) â bears subordination of direct worker control to party-state channels.
 *   - revolutionary_theoreticians: Analytical observer (analytical/analytical) â external assessor of the method's historical trajectory and terminal conditions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.72).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.85).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Seizure of State Power as Revolutionary Method").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, 'ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98').
narrative_ontology:cs_kernel_codification('ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98', formalized).
narrative_ontology:cs_authority_grounding('ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98', lineage).
narrative_ontology:cs_interpretation_layer_present('ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98').
narrative_ontology:cs_reading_relation('ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_reading_relation('ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_axiom('ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98', foundational, vanguard_party_as_necessary_consciousness_bearer).
narrative_ontology:cs_axiom_status(vanguard_party_as_necessary_consciousness_bearer, holdable).
narrative_ontology:cs_axiom_grounding('ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98', vanguard_party_as_necessary_consciousness_bearer, empirically_contingent).
narrative_ontology:cs_axiom('ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98', foundational, bourgeois_state_must_be_smashed_not_captured).
narrative_ontology:cs_axiom_status(bourgeois_state_must_be_smashed_not_captured, holdable).
narrative_ontology:cs_axiom_grounding('ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98', bourgeois_state_must_be_smashed_not_captured, instrumental).
narrative_ontology:cs_reference_frame('ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98', vanguard_party_hegemony).
narrative_ontology:cs_drift_state('ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98', post_revolutionary_bureaucratization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ce1f6ae3-6cf2-4fe6-a64a-8d40e50e3a98', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the revolutionary party-state apparatus through democratic centralism; sets ideological line and personnel policy. Cadre advancement and material security depend on fidelity to the vanguard role. Exit means abandoning a lifetime-accumulated political identity and organizational network.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres, beneficiary).

% Administrative elite implementing centralized economic and political plans. Benefits from expanded bureaucratic scope, resource allocation authority, and career ladders under party guidance. Exit to non-state roles carries significant status and income loss.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, beneficiary,
    institutional, generational, constrained, national).

% Includes social democrats, anarchists, liberal socialists, and dissenting factions. Their parties and presses are banned or absorbed; they bear the cost of one-party monopoly through exclusion from political life, imprisonment, or exile. No legal pathway to organize alternatives.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    powerless, biographical, trapped, national).

% Factory committees, independent trade unions, and worker councils seeking self-management. Their autonomous action is suppressed as economism, syndicalism, or factionalism. Subordinated to party-state channels; strikes and independent bargaining are treated as counter-revolutionary.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations, payer,
    organized, biographical, trapped, national).

% Analytical seat assessing whether the vanguard structure is a transitional instrument or a permanent bureaucratic formation. External to immediate power but provides the ideological framing that justifies or criticizes the method.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, revolutionary_theoreticians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__vanguard_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the dispersed and heterogeneous working class into a unified military-political force capable of seizing and holding state power against organized bourgeois counter-revolution.
% TRANSFER_FUNCTION: Transfers political sovereignty and organizational autonomy from federated worker assemblies and plural political parties into the centralized party-state apparatus.
% ABSENT_VOICES: Council communists and democratic gradualists are structurally excluded; they would argue for direct workers' council supremacy or parliamentary transition but are delegitimized as reformist or bourgeois-influenced.
% DISAPPEARANCE_RATIONALE: If the vanguard monopoly vanished, competing revolutionary strategies (council democracy, electoral socialism) would resurface; the centralized state form would likely fragment into plural working-class organizations or yield to bourgeois democratic restoration.
% FOUNDING_PROBLEM: The working class is spontaneously fragmented, politically heterogeneous, and militarily inferior to the organized capitalist state; without centralized leadership, revolutionary movements are defeated by counter-revolutionary violence.
% FOUNDING_PROBLEM_CORROBORATION: Party historiography and Leninist theory assert the problem remains live. Council communists, dissident socialist historians, and autonomous worker organizations attest from outside the beneficiary set that the problem was resolved by the initial revolutionary seizure and the arrangement now serves bureaucratic consolidation; no non-beneficiary corroboration supports the indefinite continuation of vanguard monopoly.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the party-state monopolizes political power and economic planning authority in ways that decouple benefit from the coordinated masses. Suppression is higher (0.85) because the constraint's persistence depends on actively banning alternative political parties, independent unions, and council self-management. Theater ratio is moderate (0.45): the counter-revolutionary threat was genuine at inception, but a growing share of enforcement activity serves the performative maintenance of revolutionary purity that masks bureaucratic consolidation. Accessibility collapse is substantial (0.70) because once the vanguard framework is institutionalized, alternative revolutionary pathways (councils, gradualism) are not merely disadvantaged but structurally extinguished. Resistance is moderate (0.60) because suppressed groups persist in clandestine or marginalized opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the cadre seat, the constraint appears as necessary coordination against counter-revolution and class fragmentation; from the autonomous worker seat, it appears as the suppression of self-emancipation. The engine computes this divergence from the structural role and exit data: identity-locked agenda-setters with generational time horizons experience a coordination device, while trapped organized payers with biographical horizons experience extraction. The authored claim does not adjudicate the divergence; it names the structure that produces it.
 *
 * DIRECTIONALITY LOGIC:
 *   Party cadres are near the full-beneficiary end (low d): the constraint subsidizes their monopoly on political power, personnel control, and ideological authority. State planners are beneficiaries (low d): they collect expanded bureaucratic rents. Political pluralists are near the full-target end (high d): the constraint extracts their political existence and organizational capacity. Autonomous worker organizations are high-target (high d): their self-activity is extracted into party-state channels. The theoretician seat is neutral (analytical d). The divergence between the cadre seat and the worker organization seat is structural, not perspectival illusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (defeating bourgeois reaction) was live during the revolutionary crisis. Over the interval, the party-state apparatus reproduced the emergency justification while the objective threat diminished, shifting the constraint from genuine coordination toward extraction. The absence of a sunset clause or credible terminal condition means the 'transitional' dictatorship has no structural expiration. This prevents mislabeling the early phase as pure extraction or the late phase as pure coordination: the same structure carries both functions, producing the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transitional_permanence_ambiguity,
    'Is the party-guided dictatorship of the proletariat a genuinely transitional form with a terminal condition, or has it become a permanent bureaucratic class structure?',
    'Historical assessment of post-revolutionary regimes for terminal withering-away; if no terminal condition is ever activated, the ''transitional'' framing functions as ideological cover.',
    'If permanent, the constraint''s coordination story loses its transitional justification and shifts toward snare; if genuinely transitional with credible sunset, scaffold classification becomes plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_permanence_ambiguity, empirical, 'Whether the dictatorship form is transitional or permanent.').

omega_variable(
    kernel_reading_alternative_structures,
    'What structural configuration would emerge if the council_communist or democratic_gradualist reading were adopted instead of the vanguard rupture reading?',
    'Comparative historical analysis of council regimes (e.g., 1918-21 soviets before party subordination) and gradualist social-democratic consolidation.',
    'Would reveal whether the high extraction and suppression observed are necessary features of revolutionary transition or artifacts of the vanguard form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_alternative_structures, conceptual, 'Structural delta between sibling readings of the revolutionary method kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative revolutionary pathways achieved primarily through structural state coercion, or through internalized ideological commitment to vanguardism among the working class?',
    'Analysis of resistance patterns: if autonomous worker organizations revive immediately when state coercion weakens, suppression is structural; if they persist in subordination, internalization is significant.',
    'Internalized suppression would raise effective extraction beyond the structural measure, strengthening snare-like characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of alternative pathways.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mani_tr_t4, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(mani_tr_t8, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(mani_tr_t12, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(mani_tr_t16, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(mani_tr_t24, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(mani_be_t4, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(mani_be_t8, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(mani_be_t12, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(mani_be_t16, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(mani_be_t24, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 24, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(mani_su_t4, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 4, 0.82).
narrative_ontology:measurement(mani_su_t8, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 8, 0.84).
narrative_ontology:measurement(mani_su_t12, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 12, 0.85).
narrative_ontology:measurement(mani_su_t16, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 16, 0.86).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(mani_su_t24, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 24, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, democratic_gradualism_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, council_communist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the manifesto_revolutionary_method kernel, decomposed from the natural-language concept 'revolutionary method' into structurally distinct claims per the epsilon-invariance principle. The vanguard_rupture_reading, democratic_gradualism_reading, and council_communist_reading are separate constraints with different epsilon values, beneficiary/victim structures, and stakeholder seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
