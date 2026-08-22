% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__hybrid_reading, []).

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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Preparedness as Layered Memorial-Competence System
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   Preparedness regimes in democratic states persist as hybrid systems where
 *   memorial elements (commemorative exercises, naming ceremonies, annual
 *   drills framed as remembrance) stabilize political and budgetary
 *   commitment, while competence elements (operational exercises,
 *   after-action reviews, equipment maintenance cycles) maintain actual
 *   response capacity. The constraint is the standing preparedness
 *   arrangement — the requirement that jurisdictions maintain both layers
 *   simultaneously. The memorial layer generates legitimacy and budget
 *   protection; the competence layer absorbs the operational risk. Tension
 *   arises because memorial activities are cheaper, more visible, and
 *   politically rewarded, while competence activities are costly, invisible
 *   until failure, and politically punished when they reveal gaps. Over the
 *   interval, the theater ratio rises as memorial elements expand to fill the
 *   commitment gap left by competence underinvestment, and extractiveness
 *   rises as frontline responders and municipal budgets bear the cost of
 *   maintaining readiness that is increasingly performative. This reading
 *   instantiates the kernel's 'hybrid' claim: both layers are structurally
 *   necessary, but their coexistence creates a maintenance cost that neither
 *   layer alone would incur.
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: agenda_setter (institutional, generational, arbitrage, global) — administers the hybrid system, controls exercise design and budget allocation
 *   - public_safety_political_leadership: beneficiary (powerful, biographical, mobile, national) — gains political credit from memorial visibility while deferring competence costs
 *   - institutional_memory_keepers: beneficiary (organized, generational, identity_locked, regional) — professional identity fused to the memorial layer; exit means abandoning their self-concept as guardians of the lesson
 *   - frontline_responders: payer (moderate, biographical, constrained, local) — bear the operational risk when competence atrophies; constrained exit (career specialization, geographic tie)
 *   - municipal_budgets: payer (organized, generational, constrained, regional) — absorb the maintenance cost of dual-layer system; constrained exit (tax base, intergovernmental mandates)
 *   - vulnerable_populations: excluded (powerless, immediate, trapped, local) — would object to performative preparedness that fails them; structurally excluded from exercise design and after-action accountability
 *   - after_action_analysts: observer (analytical, civilizational, analytical, universal) — sees the full structure; no stake in the arrangement's persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.42).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.31).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Preparedness as Layered Memorial-Competence System").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, 'd403ccec-2a7e-491d-a66d-fdc5ecd0d605').
narrative_ontology:cs_kernel_codification('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', distributed).
narrative_ontology:cs_authority_grounding('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', practice).
narrative_ontology:cs_interpretation_layer_present('d403ccec-2a7e-491d-a66d-fdc5ecd0d605').
narrative_ontology:cs_reading_relation('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_axiom('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', foundational, preparedness_requires_dual_layer).
narrative_ontology:cs_axiom_status(preparedness_requires_dual_layer, holdable).
narrative_ontology:cs_axiom_grounding('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', preparedness_requires_dual_layer, instrumental).
narrative_ontology:cs_axiom('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', foundational, memorial_stabilizes_competence_commitment).
narrative_ontology:cs_axiom_status(memorial_stabilizes_competence_commitment, holdable).
narrative_ontology:cs_axiom_grounding('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', memorial_stabilizes_competence_commitment, empirically_contingent).
narrative_ontology:cs_axiom('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', secondary, tension_creates_maintenance_cost).
narrative_ontology:cs_axiom_status(tension_creates_maintenance_cost, holdable).
narrative_ontology:cs_axiom_grounding('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', tension_creates_maintenance_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', post_cold_war_civil_defense_transition).
narrative_ontology:cs_drift_state('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', post_911_homeland_security_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d403ccec-2a7e-491d-a66d-fdc5ecd0d605', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, public_safety_political_leadership).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, institutional_memory_keepers).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, municipal_budgets).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, vulnerable_populations).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, preparedness_requires_both_commitment_and_competence).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, memorial_elements_prevent_abandonment).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, competence_elements_prevent_catastrophic_failure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the hybrid preparedness system: designs exercises, allocates budget between memorial and competence activities, reports to political leadership and intergovernmental bodies. Can shift emphasis between layers but cannot eliminate either without losing mandate. Exit is arbitrage-grade — they can move between jurisdictions, consult internationally, or transition to private sector resilience roles.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Gains political credit from memorial events (anniversaries, naming ceremonies, high-visibility drills) which are low-cost and media-friendly. Defers competence investments (equipment modernization, staffing, unglamorous maintenance) to future budgets. Mobile exit — can shift portfolio, blame predecessors, or claim credit for 'lessons learned' without bearing operational consequences.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, public_safety_political_leadership, beneficiary,
    powerful, biographical, mobile, national).

% Professional identity is fused to the memorial layer — they are the 'keepers of the lesson,' the designers of commemorative exercises, the authors of after-action narratives. Their career progression, professional recognition, and self-concept depend on the memorial layer's centrality. Exit would mean abandoning their identity as guardians of collective memory; they are identity_locked, not merely constrained.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, institutional_memory_keepers, beneficiary,
    organized, generational, identity_locked, regional).

% Bear the operational risk when competence atrophies: they respond with outdated equipment, insufficient staffing, and training scenarios designed for memorial visibility rather than threat realism. Constrained exit — specialized skills, geographic ties to community, pension vesting, and professional identity as responders make lateral moves difficult. They see the gap between memorial exercises and real incidents most clearly but lack agenda-setting power.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_responders, payer,
    moderate, biographical, constrained, local).

% Absorb the maintenance cost of the dual-layer system: memorial events, competence training, equipment cycles, and the overhead of managing both. Constrained exit — tax base is fixed, intergovernmental mandates require preparedness spending, and cutting either layer triggers political or regulatory penalties. The budget is the seat where the tension between layers materializes as fiscal trade-offs.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, municipal_budgets, payer,
    organized, generational, constrained, regional).

% Experience the constraint's failure mode most directly — when memorial performance substitutes for competence, they are the first to die in disasters. Structurally excluded from exercise design, after-action reviews, and budget decisions. Would object to performative preparedness that fails them, but have no venue to be heard. Trapped exit — geographic, economic, and political immobility.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, vulnerable_populations, excluded,
    powerless, immediate, trapped, local).

% See the full structure across jurisdictions and decades. No stake in the arrangement's persistence — their professional reward comes from accurate diagnosis, not from the hybrid system's legitimacy. Analytical exit — they can change frameworks, jurisdictions, or disciplinary lenses without personal cost.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, after_action_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__hybrid_reading, emergency_management_agencies).
narrative_ontology:fixing_cost_class(preparedness_commitment__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains societal commitment to preparedness across electoral cycles and quiet decades by binding memorial visibility (which generates political legitimacy and budget protection) to competence maintenance (which prevents catastrophic failure). Neither layer alone achieves both: memorial-only loses operational capacity; competence-only loses political support.
% TRANSFER_FUNCTION: Moves operational risk and fiscal burden from political leadership and institutional memory-keepers to frontline responders and municipal budgets. The memorial layer extracts political attention and budget share; the competence layer extracts operational capacity from responders who must compensate for memorial-layer crowding.
% ABSENT_VOICES: Vulnerable populations (excluded stakeholders) would object to performative preparedness that fails them in actual disasters. Community organizations representing high-risk neighborhoods are not invited to exercise design or after-action accountability. Their absence is structural — the hybrid system's legitimacy depends on their silence.
% DISAPPEARANCE_RATIONALE: If the hybrid preparedness constraint vanished overnight, jurisdictions would not simply abandon preparedness. Some would shift to competence-only frameworks (investing in operational capacity, dropping memorial events), some would shift to memorial-only (cheap visibility, no operational readiness), and most would drift into ad-hoc improvisation. The world rearranges: budget allocations shift, exercise designs change, political credit structures realign, and vulnerable populations bear the variance in outcomes.
% FOUNDING_PROBLEM: After each major disaster, political will for preparedness surges but decays exponentially during quiet decades. The memorial layer was designed to make preparedness visible and politically durable across the decay cycle; the competence layer was designed to maintain the operational capacity that memorial visibility claims to protect. The hybrid arrangement was built to solve the temporal mismatch between political attention spans and threat recurrence intervals.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management agencies and institutional memory-keepers attest the problem is live — threat recurrence intervals exceed political cycles, and memorial elements are the only proven mechanism to sustain commitment. Budget officers and reform-oriented legislators attest the problem is dead — modern risk modeling, insurance incentives, and intergovernmental mandates can sustain preparedness without memorial theater. Independent scholars of civil defense history (e.g., Tierney 2019, Boin & McConnell 2007) corroborate that memorial elements have historically been necessary for budget survival but also document cases where competence-only regimes persisted (Swedish civil defense 1990-2010, Finnish comprehensive security model).
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).
:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) reflects the maintenance cost of running two layers: the memorial layer extracts political attention and budget share from the competence layer; the competence layer extracts operational capacity from frontline responders who must compensate for memorial-layer crowding. Suppression (0.31) is moderate — the constraint does not actively ban alternatives (competence-only or memorial-only regimes exist in some jurisdictions) but the hybrid form is mandated by intergovernmental frameworks and professional standards, creating structural pressure to maintain both. Theater ratio (0.38) captures the growing share of exercise activity that is memorial-framed rather than competence-testing. Accessibility collapse (0.58) reflects that once the hybrid logic is accepted — 'we need both remembrance and readiness' — alternatives (pure competence, pure memorial, or no preparedness) appear irresponsible or impossible. Resistance (0.47) comes from frontline responders who see the gap, budget officers who see the cost, and reform advocates who argue for competence-first redesign. The claimed type is tangled_rope because the constraint genuinely coordinates (maintains societal commitment to preparedness across electoral cycles) AND extracts asymmetrically (political leadership and memory-keepers benefit; responders and budgets pay).
 *
 * PERSPECTIVAL GAP:
 *   From the emergency_management_agency seat, the hybrid system is necessary coordination — without memorial elements, preparedness funding collapses after each quiet decade; without competence elements, the agency loses its operational mandate. From the frontline_responder seat, the same system is extractive — they train for scenarios the memorial layer designs for visibility, not for the threats the competence layer would prioritize. From the institutional_memory_keeper seat, the memorial layer IS the constraint — their professional identity is constituted through it (identity_locked). From the political_leadership seat, the memorial layer is a low-cost legitimacy generator (mobile — they can shift framing). The engine computes these divergences from the declared power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda_setters (emergency_management_agencies) sit near symmetric d ≈ 0.5: they administer the constraint and bear some cost (budget defense, exercise design) but also control its shape. Beneficiaries (political_leadership, institutional_memory_keepers) have low d: political_leadership extracts legitimacy with mobile exit; memory_keepers have identity_locked exit but are net beneficiaries (the constraint validates their professional existence). Payers (frontline_responders, municipal_budgets) have high d: responders are constrained (specialized career, local ties) and bear operational risk; budgets are constrained (mandated, tax-base limited). Excluded (vulnerable_populations) have trapped exit and would be high d if included. Observer (after_action_analysts) has analytical exit and d ≈ 0.5 by default. The beneficiary/victim declarations map directly to these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — maintaining societal preparedness across generations without a standing threat — is contested (live for memory_keepers and agencies; dead for budget officers who see no recent catastrophic failure; contested for political_leadership who need the memorial layer for legitimacy). The hybrid reading prevents mislabeling: a pure coordination reading (rope) would miss the asymmetric extraction of competence by memorial; a pure extraction reading (snare) would miss the genuine coordination function that prevents total preparedness collapse. The mandatrophy is partially resolved: the memorial layer's original function (preventing abandonment) is live; the competence layer's original function (preventing failure) is live but underfunded; the tension between them is the extractive core. The constraint persists because neither layer can be removed without triggering the failure mode the other prevents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memorial_competence_boundary,
    'Where is the structural boundary between memorial elements that stabilize commitment and memorial elements that displace competence?',
    'Longitudinal analysis of exercise design: when after-action reviews shift from identifying capability gaps to validating narrative compliance, the boundary is crossed.',
    'If the boundary has been crossed in most jurisdictions, the hybrid reading collapses toward husk_reading — the memorial layer no longer stabilizes commitment to competence but substitutes for it. The constraint''s claimed_type would shift from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_competence_boundary, empirical, 'Whether memorial elements still serve their coordination function or have become pure extraction').

omega_variable(
    commitment_without_memorial_counterfactual,
    'Would preparedness commitment actually collapse without memorial elements, or is that a self-justifying claim by memory-keepers?',
    'Natural experiment: jurisdictions that have shifted to competence-only frameworks (e.g., some Nordic civil defense models) — track budget stability and public support over 20+ years.',
    'If commitment persists without memorial layer, the hybrid reading''s coordination claim is false — the memorial layer is pure extraction. The constraint reclassifies toward snare. If commitment collapses, the coordination function is validated and tangled_rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commitment_without_memorial_counterfactual, empirical, 'Whether the memorial layer''s coordination function is genuine or a cover story').

omega_variable(
    kernel_reading_framing,
    'Is the ''hybrid'' framing itself a distinct structural claim, or a diplomatic synthesis that obscures the real contest between competence_reading and husk_reading?',
    'Analyze whether any jurisdiction actually operates the hybrid as a designed system with explicit resource allocation to both layers, versus jurisdictions that drift into hybrid by default while claiming one or the other.',
    'If hybrid is only a retrospective label for drift, not a designed arrangement, the constraint story describes an emergent pattern, not an instituted constraint — ε and claimed_type must be reassessed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the hybrid reading describes an instituted system or an emergent drift pattern').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(preparedness_hybrid_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(preparedness_hybrid_tr_t5, preparedness_commitment__hybrid_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(preparedness_hybrid_tr_t10, preparedness_commitment__hybrid_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(preparedness_hybrid_tr_t15, preparedness_commitment__hybrid_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(preparedness_hybrid_tr_t20, preparedness_commitment__hybrid_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(preparedness_hybrid_tr_t25, preparedness_commitment__hybrid_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(preparedness_hybrid_tr_t30, preparedness_commitment__hybrid_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(preparedness_hybrid_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(preparedness_hybrid_be_t5, preparedness_commitment__hybrid_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(preparedness_hybrid_be_t10, preparedness_commitment__hybrid_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(preparedness_hybrid_be_t15, preparedness_commitment__hybrid_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(preparedness_hybrid_be_t20, preparedness_commitment__hybrid_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(preparedness_hybrid_be_t25, preparedness_commitment__hybrid_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(preparedness_hybrid_be_t30, preparedness_commitment__hybrid_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(preparedness_hybrid_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(preparedness_hybrid_su_t5, preparedness_commitment__hybrid_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement(preparedness_hybrid_su_t10, preparedness_commitment__hybrid_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(preparedness_hybrid_su_t15, preparedness_commitment__hybrid_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(preparedness_hybrid_su_t20, preparedness_commitment__hybrid_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(preparedness_hybrid_su_t25, preparedness_commitment__hybrid_reading, suppression_requirement, 25, 0.31).
narrative_ontology:measurement(preparedness_hybrid_su_t30, preparedness_commitment__hybrid_reading, suppression_requirement, 30, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, intergovernmental_preparedness_mandates).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, emergency_management_funding_formulas).

% DUAL FORMULATION NOTE:
% This is the hybrid_reading of the preparedness_commitment kernel. The competence_reading (preparedness as live exercised knowledge) and husk_reading (preparedness as memorial performance) are sibling constraints with different ε and different beneficiary/victim structures. This reading asserts both layers are structurally necessary but their coexistence creates maintenance cost. The competence_reading forecloses the memorial layer; the husk_reading forecloses the competence layer; this reading coexists_with both as a third position held by institutional practitioners who manage the tension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__hybrid_reading, organized, 0.35).
constraint_indexing:directionality_override(preparedness_commitment__hybrid_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
