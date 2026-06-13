% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Workers' Councils as Direct Democratic Revolutionary Authority (Council Communist Reading)
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   The council-communist reading interprets the revolutionary transformation
 *   as the seizure of productive power by workers acting through their own
 *   democratic assemblies — not through a vanguard party claiming to
 *   represent them, and not through the existing capitalist state apparatus.
 *   Councils are federated structures where delegates carry explicit mandates
 *   from their constituent assemblies and remain immediately recallable. The
 *   founding texts (Marx, Lenin's early *State and Revolution*,
 *   council-communist theorists) present this as the realization of workers'
 *   control; implementation (Bolshevik October, Hungarian Councils, Paris
 *   Commune) shows recurring tension between the assembly-democratic logic
 *   and the centralizing pressure of coordinating large economies and
 *   defending against counter-revolution. This reading is in direct
 *   structural conflict with the vanguard-rupture reading (party authority
 *   supersedes assembly authority) and in tension (not logical conflict) with
 *   democratic-gradualism (electoral authority supersedes assembly
 *   authority). The authored metrics describe the constraint AS IT OPERATES
 *   WITHIN A COUNCIL SYSTEM — low internal extractiveness (0.25) because
 *   democratic accountability and mandate discipline limit rent-seeking, but
 *   high external suppression (0.72) because the system must defend itself
 *   against state/party/capitalist counter-claims to authority and against
 *   reconcentration pressures.
 *
 * KEY AGENTS:
 *   - Autonomous worker collectives: direct producers, assembly democracy, agenda-setters in their own workplaces
 *   - Federated workplace assemblies: coordinate across industries, delegates carry mandates, council structure replaces party/state
 *   - Industrial working class: the power base that sustains the system through participation, only constituency councils require
 *   - State bureaucratic apparatus: displaced by council coordination, loses legitimacy and enforcement capacity
 *   - Professional party officials: lose institutional seat under assembly supremacy, identity-locked to party authority
 *   - Capitalist class: ownership abolition, exit is capital flight or counter-revolutionary violence
 *   - Competing revolutionary readings: vanguardism and gradualism both contest council authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.72).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Workers' Councils as Direct Democratic Revolutionary Authority (Council Communist Reading)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, '9697b24e-8880-4248-8f3e-97cbd30814a7').
narrative_ontology:cs_kernel_codification('9697b24e-8880-4248-8f3e-97cbd30814a7', fixed_text).
narrative_ontology:cs_authority_grounding('9697b24e-8880-4248-8f3e-97cbd30814a7', lineage).
narrative_ontology:cs_interpretation_layer_present('9697b24e-8880-4248-8f3e-97cbd30814a7').
narrative_ontology:cs_reading_relation('9697b24e-8880-4248-8f3e-97cbd30814a7', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('9697b24e-8880-4248-8f3e-97cbd30814a7', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('9697b24e-8880-4248-8f3e-97cbd30814a7', foundational, workers_councils_sole_sovereign_authority).
narrative_ontology:cs_axiom_status(workers_councils_sole_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('9697b24e-8880-4248-8f3e-97cbd30814a7', workers_councils_sole_sovereign_authority, deontological).
narrative_ontology:cs_axiom('9697b24e-8880-4248-8f3e-97cbd30814a7', foundational, immediate_recall_delegate_accountability).
narrative_ontology:cs_axiom_status(immediate_recall_delegate_accountability, holdable).
narrative_ontology:cs_axiom_grounding('9697b24e-8880-4248-8f3e-97cbd30814a7', immediate_recall_delegate_accountability, conventional).
narrative_ontology:cs_axiom('9697b24e-8880-4248-8f3e-97cbd30814a7', secondary, party_subordination_to_council_mandate).
narrative_ontology:cs_axiom_status(party_subordination_to_council_mandate, holdable).
narrative_ontology:cs_axiom_grounding('9697b24e-8880-4248-8f3e-97cbd30814a7', party_subordination_to_council_mandate, deontological).
narrative_ontology:cs_reference_frame('9697b24e-8880-4248-8f3e-97cbd30814a7', workers_council_supremacy).
narrative_ontology:cs_drift_state('9697b24e-8880-4248-8f3e-97cbd30814a7', post_october_consolidation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('9697b24e-8880-4248-8f3e-97cbd30814a7', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, federated_workplace_assemblies).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, industrial_working_class).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucratic_apparatus).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, professional_party_officials).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, capitalist_class).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).

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
 *   Extractiveness is low (0.25) WITHIN councils because the constraint-logic is democratic accountability with immediate recall, transparent mandate discipline, and no concentrated position that captures extraction (no leader, no cadre, no proprietary mechanism). Theater ratio is also low (0.18) because the reading's legitimacy rests on genuine participation — performance without substance would dissolve authority quickly. Suppression is high (0.72) because the constraint persists against external pressure: state apparatus, parties claiming vanguard authority, capitalist counter-attacks, and competing revolutionary readings all contest council legitimacy. The measurement series shows a slow rise in extractiveness and theater over the interval (0.12→0.28 and 0.08→0.26) reflecting the documented historical pattern: councils initially high-participation but facing pressure to professionalize, delegate activity becoming more specialized, decision-making gradually shifting from assemblies toward council organs, and management roles reproducing quasi-bureaucratic hierarchy. This drift is NOT an argument the reading fails — it is evidence of the structural tension the reading inherits: can participatory democracy scale, or does federation inevitably reconcentrate power? The suppression requirement rises (0.55→0.80) as counter-revolutionary forces mobilize — state/party/capital organized resistance to the council system intensifies, requiring councils to defend themselves more actively. At t0, the reading is in-power but contested; by t50, if the projection holds, the reading is under severe external pressure while internal mechanisms face gradual concentration.
 *
 * PERSPECTIVAL GAP:
 *   From the autonomous worker collectives' position (agenda-setter + beneficiary seat), the reading offers genuine democracy and control. From the state bureaucratic apparatus' position, it is usurpation and chaos. From party-official seats, it is betrayal of centralized organization. The engine should compute these divergences from the structural positions — the same constraint reads as rope (genuine coordination for workers) from worker seats and as snare-with-external-defense (extraction defended by violence) from displaced authority seats. The analytical observer seat should recognize the tension between the reading's internal logic (democracy via assemblies) and the measurement drift (councils→bureaucracy), which maps onto the historical question: is this tension fatal to the reading, or a side effect of external pressure?
 *
 * DIRECTIONALITY LOGIC:
 *   Worker collectives and federated assemblies are BENEFICIARIES and AGENDA-SETTERS: they collect authority, set production priorities, distribute the product. Their directionality is low (0.0–0.2: beneficiary end). The industrial working class benefits from control and eliminated wage hierarchy; also low directionality. The state apparatus, party officials, and capitalists are PAYERS (lose authority, property, institutional seats). Their directionality is high (0.8–1.0: target end). Competing readings occupy an EXCLUDED position — they claim authority the council system rejects. A vanguardist or gradualist analyst in a council system is an excluded voice arguing for a different authority structure. The suppression value reflects how hard the system must work to maintain council supremacy against these competing claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (workers' lack of control) is LIVE in the reading: councils exist to solve it. The constraint claimed type is ROPE (coordination + mild internal transfer for collective provisioning). The risk of mandatrophy appears in the measurement series as theater ratio rising (assembly ritual without decision-making power) and extractiveness drifting upward (delegates accumulating power, councils becoming hierarchy). The tension is between the reading's own logic (immediate recall, transparent mandate, assembly supremacy) and the structural pressure to reproduce hierarchy when coordinating large-scale production. This is not proof the reading fails — it is evidence the constraint-type is stable (rope with internal distribution cost, not snare) IF AND ONLY IF the participatory mechanisms remain functional. If theater rises above 0.4 and extractiveness crosses 0.5, the boundary condition fails and the constraint would transition from rope to tangled_rope (coordination cover for reconcentrated power). Currently, the measurement trajectory suggests this boundary is not yet crossed, but the projection shows approach. The commentary does not claim the reading prevails; it models the structural conditions of its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    participatory_depth_vs_federation_scale,
    'Can workplace assemblies maintain direct democratic participation while coordinating across large regional and national federations, or does federation necessarily reconcentrate decision-making power in delegate and council hands?',
    'Historical examination of council systems (Paris Commune duration, Hungarian Councils 1956, Yugoslav self-management scalability, Rojava communes scaling). Empirical test: does theater_ratio continue rising as federation scope expands, and does delegate recall remain functional or become nominal?',
    'If federation requires delegation that reconcentrates power, the constraint transitions toward tangled_rope (coordination cover for hidden hierarchy). If assemblies can sustain participation at scale (perhaps through digital coordination, modified decision procedures), the rope classification holds and extractiveness plateaus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(participatory_depth_vs_federation_scale, empirical, 'Whether council democracy preserves participatory legitimacy at federation scale or reproduces bureaucratic hierarchy under democratic forms.').

omega_variable(
    reading_versus_vanguard_outcome,
    'Is the council-communist reading a stable equilibrium, or is it necessarily transient to one of the competing readings (vanguard consolidation or democratic regression to electoral system)?',
    'Examine sustained council systems (Yugoslavia 50+ years; Rojava decadal trajectory); test whether suppression_requirement remains stable or whether it either drops (councils stabilize into routine, external threat diminishes) or rises sharply (revolutionary pressure forces centralization).',
    'If council systems converge toward vanguard control (suppression → party discipline, extractiveness → party rent), the reading is a transition-state, not a stable constraint. If they converge toward electoral democracy (mandate → voting), the reading is a defeated variant. If suppression stabilizes, the reading may be functionally viable as ongoing revolutionary form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_versus_vanguard_outcome, conceptual, 'Whether the council-communist reading describes a stable structural form or a transient phase in the revolutionary process that necessarily yields to other readings.').

omega_variable(
    internal_extraction_vs_collective_provisioning,
    'Is the measured extractiveness (0.25) drawn from councils'' legitimate collective needs (defense, infrastructure, administration) or from actual hierarchy and rent-seeking reproducing class division within the working class?',
    'Transparent accounting of where councils allocate surplus: if distribution remains egalitarian and proportional to need, extraction is provisioning cost; if managerial/specialized workers accumulate larger shares, extraction is reproducing hierarchy. Measure wage ratios (ratio of highest to lowest council-authorized compensation) and track over time.',
    'If extraction is purely provisioning, the rope classification is stable and theater_ratio is valid measure of ritual vs. substance. If extraction becomes rent-seeking (higher pay for delegates/managers), the system is drifting toward tangled_rope or snare-within-rope (hierarchy hidden by democratic forms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_extraction_vs_collective_provisioning, empirical, 'Whether the constraint''s internal transfer is legitimate collective cost or emergent class re-stratification within the working class.').

omega_variable(
    committer_reading_contest_outcome,
    'Which reading of the manifesto_revolutionary_method kernel actually describes the outcome of a revolutionary process claiming to institute councils? Does the vanguard reading''s centralization overcome council democracy, or does the council reading sustain itself?',
    'Historical outcome: October Revolution (vanguard reading won; councils were subordinated); Hungarian 1956 (council reading attempted; suppressed externally); Yugoslav (council-communist modified by party guidance hybrid; neither pure reading); Rojava (council reading operative; under military/diplomatic pressure). Future experiment: any revolutionary process that explicitly institutes councils and tests maintenance of assembly supremacy against external and internal pressures.',
    'Outcome determines whether this reading is empirically viable or normatively appealing but structurally defeated. If the vanguard reading always wins in practice, the council reading becomes a utopian ideal with no stable instantiation. If councils can persist (as in Rojava, albeit modified), the reading has structural feasibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_contest_outcome, empirical, 'The final arbiter: does the council-communist reading describe a workable revolutionary form, or does history show it is necessarily overcome by competing readings?').

omega_variable(
    reading_foreclosure_status,
    'Does the vanguard-rupture reading''s core premise (party dictatorship as necessary form of revolutionary transformation) logically foreclose the council-communist reading''s premise (workers'' councils as sole sovereign authority), or do they coexist as competing live options that historical actors can choose between?',
    'Examine whether a party can logically subordinate councils to its direction while claiming to represent workers'' interests, or whether subordination requires denying workers'' councils genuine sovereignty. If councils can be both sovereign AND party-guided (party acts as coordinator within assembly authorization), readings coexist; if not, vanguardism forecloses councils.',
    'If readings logically conflict (foreclose), the kernel is irresolvable and both readings cannot be true in the same revolutionary process. If they coexist (different parties hold different readings), the contest is political, not logical. This affects how cs_structure.reading_relations are classified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_status, conceptual, 'Whether the vanguard and council readings logically exclude each other or remain live competing options within revolutionary Marxism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(mani_tr_t10, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(mani_tr_t30, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(mani_tr_t50, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 50, 0.26).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(mani_be_t10, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(mani_be_t30, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement(mani_be_t50, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 50, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mani_su_t10, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(mani_su_t30, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(mani_su_t50, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__council_communist_reading, 0.18).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'manifesto_revolutionary_method'. Sibling readings (vanguard_rupture_reading, democratic_gradualism_reading) interpret the same founding texts (Marx, Lenin, revolutionary practice) but instantiate structurally different constraints with different beneficiary/victim structures, different ε values, and different authority claims. The readings are linked through the kernel and through omega variables documenting the interpretive contest. Each reading should be generated as a separate constraint story with its own cs_structure.reading_relations and cs_structure.axioms blocks. Cross-reading analysis should examine whether readings foreclose each other or coexist as live political options, and whether historical revolutionary processes can instantiate multiple readings or only one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
