% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Preparedness as Layered Commitment System
 *   domain: institutional/governance/disaster_preparedness
 *
 * SUMMARY:
 *   This constraint is ONE READING of a contested kernel about preparedness
 *   across institutional generations. The kernel is the standing commitment
 *   to maintain disaster preparedness through peacetime. Three readings
 *   decompose this kernel: competence_reading emphasizes that preparedness IS
 *   live exercised knowledge and that memorial layer is decorative
 *   distraction; husk_reading emphasizes that preparedness becomes memorial
 *   performance drained of operational competence; hybrid_reading (this
 *   story) asserts that both layers are structurally necessary and their
 *   tension creates an asymmetric extraction mechanism. This reading
 *   instantiates the constraint as a tangled_rope: genuine coordination
 *   function (both layers maintain preparedness) bundled with asymmetric
 *   extraction (constrained jurisdictions and technical specialists bear
 *   dual-layer maintenance cost while institutional continuity and political
 *   leadership capture the legitimacy benefit). The theater ratio rises over
 *   the interval (0.35 → 0.48) indicating growing performative element in
 *   memorial maintenance, but remains below 0.5, suggesting the competence
 *   layer still commands real investment.
 *
 * KEY AGENTS:
 *   - institutional_continuity_beneficiaries: institutional actors that benefit from unbroken preparedness legacy across generations (low d, stable beneficiary)
 *   - operational_preparedness_staff: emergency management professionals who administer both layers and enforce the constraint (moderate power, constrained exit)
 *   - resource_constrained_jurisdictions: smaller systems that pay dual-layer costs on limited budgets (moderate power, trapped-to-constrained exit, high d)
 *   - political_leadership: executive authority that mandates both layers and extracts legitimacy from each (powerful, mobile exit, moderate d with override candidates)
 *   - technical_competence_advocates: specialists arguing competence layer deserves priority; bear opportunity cost of memorial investment (moderate power, constrained exit, moderate d)
 *   - memorial_custodians: archivists and historians maintaining institutional memory; identity-locked to the memorial function (moderate power, identity-locked exit, high d despite moderate power)
 *   - affected_disaster_survivors: communities whose experience grounds both layers; excluded from agenda-setting (powerless, trapped exit, identity through memorial but no voice in competence decisions)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.58).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.34).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Preparedness as Layered Commitment System").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "institutional/governance/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, 'ccdfb500-d973-42e9-b426-3e8b00a6e5d1').
narrative_ontology:cs_kernel_codification('ccdfb500-d973-42e9-b426-3e8b00a6e5d1', formalized).
narrative_ontology:cs_authority_grounding('ccdfb500-d973-42e9-b426-3e8b00a6e5d1', lineage).
narrative_ontology:cs_interpretation_layer_present('ccdfb500-d973-42e9-b426-3e8b00a6e5d1').
narrative_ontology:cs_reading_relation('ccdfb500-d973-42e9-b426-3e8b00a6e5d1', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccdfb500-d973-42e9-b426-3e8b00a6e5d1', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_axiom('ccdfb500-d973-42e9-b426-3e8b00a6e5d1', foundational, dual_layer_structural_necessity).
narrative_ontology:cs_axiom_status(dual_layer_structural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('ccdfb500-d973-42e9-b426-3e8b00a6e5d1', dual_layer_structural_necessity, empirically_contingent).
narrative_ontology:cs_axiom('ccdfb500-d973-42e9-b426-3e8b00a6e5d1', secondary, memorial_prevents_recursive_forgetting).
narrative_ontology:cs_axiom_status(memorial_prevents_recursive_forgetting, holdable).
narrative_ontology:cs_axiom_grounding('ccdfb500-d973-42e9-b426-3e8b00a6e5d1', memorial_prevents_recursive_forgetting, empirically_contingent).
narrative_ontology:cs_reference_frame('ccdfb500-d973-42e9-b426-3e8b00a6e5d1', institutional_preparedness_dual_layer_maintenance).
narrative_ontology:cs_drift_state('ccdfb500-d973-42e9-b426-3e8b00a6e5d1', contemporary_generational_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ccdfb500-d973-42e9-b426-3e8b00a6e5d1', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, institutional_continuity_beneficiaries).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, resource_constrained_jurisdictions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, political_leadership).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, technical_competence_advocates).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, memorial_custodians).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, institutional_memory_irreplaceability).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, dual_layer_maintenance_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations, leadership, and governance structures that benefit from unbroken institutional memory across personnel turnover and generational cycles. They collect the coordination function—continuous preparedness—without running the memorial layer maintenance themselves. Benefit from the legitimacy and social trust that accrues to institutions that visibly remember and honor their crises and learning.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, institutional_continuity_beneficiaries, beneficiary,
    institutional, generational, analytical, national).

% Emergency management professionals, disaster preparedness coordinators, and operational competence maintainers. They administer both the memorial layer (commemoration ceremonies, archival maintenance, institutional narratives) and the competence layer (training cycles, equipment maintenance, simulation drills). They enforce the constraint by ensuring both layers persist. Their authority derives from their technical expertise and their role as keepers of institutional learning.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, operational_preparedness_staff, agenda_setter,
    organized, biographical, constrained, national).

% Smaller municipalities, developing regions, and resource-limited emergency management systems must maintain both memorial and competence layers on constrained budgets. They pay through diverted funding from other services, exhausted staff time maintaining dual-layer infrastructure, and opportunity cost of not specializing in one layer alone. Their exit is constrained because abandoning preparedness entirely carries unacceptable disaster risk; accepting one layer alone (pure memorial or pure competence) produces legitimacy deficits or operational failures.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, resource_constrained_jurisdictions, payer,
    moderate, biographical, constrained, regional).

% Elected officials and executive leadership direct institutional preparedness policy and funding allocation. They set the mandate for both layers (memorial ceremonies maintain public confidence; competence drills prevent catastrophes) and extract political legitimacy from both—appearing as custodians of memory and as competent guardians against disaster. They also bear the cost when either layer fails visibly (a forgotten anniversary signals institutional negligence; a bungled response signals incompetence). Their exit option is to deprioritize preparedness entirely, shifting resources to more politically salient domains.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, political_leadership, agenda_setter,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, political_leadership, payer).

% Emergency operations specialists, engineers, and technical experts who argue that operational competence (scenario planning, equipment certification, skill currency) should dominate resource allocation. They bear the cost of the memorial layer as a distraction from competence maintenance; they argue resources spent on memorial ceremonies are resources not spent on updated protocols. Their constraints lie in the institutional mandate for both layers—they cannot exit the memorial requirement without losing organizational legitimacy.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, technical_competence_advocates, payer,
    moderate, biographical, constrained, national).

% Historians, survivors' advocates, archivists, and cultural institutional stewards who maintain the memorial layer—the narratives, ceremonies, and institutional memory of past crises. They bear the cost of dual-layer maintenance through fragmented attention and resource competition with technical competence specialists. Their identity is fused with the role of remembering; they experience leaving the constraint as a betrayal of the dead and a violation of their professional identity.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, memorial_custodians, payer,
    moderate, generational, identity_locked, national).

% Communities that experienced past disasters. Their interests are partly served by both layers (competence reduces future disaster severity; memorial honors their loss) but they are excluded from agenda-setting. They cannot opt out of preparedness but also cannot directly influence how the two layers are balanced. Their voice appears in memorial narratives but not in technical competence decisions.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, affected_disaster_survivors, excluded,
    powerless, biographical, trapped, regional).

% External researchers, auditors, and comparative governance analysts who evaluate the hybrid layer system. They take no direct stake in the constraint but document the institutional dynamics, measure maintenance burden, and compare hybrid systems to single-layer alternatives. They serve as a corrective voice for narrative analysis and as a repository for institutional learning across jurisdictions.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__hybrid_reading, institutional_continuity_beneficiaries).
narrative_ontology:fixing_cost_class(preparedness_commitment__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains institutional preparedness across generational and personnel turnover by embedding preparedness in two reinforcing layers: the memorial layer (institutional narrative, commemoration, acknowledgment of past crises) creates legitimacy and cultural commitment to remembering why preparedness matters; the competence layer (training cycles, equipment maintenance, scenario planning, skill currency) maintains operational capacity. Together they solve the problem of why preparedness persists when no immediate crisis justifies the expense.
% TRANSFER_FUNCTION: Extracts ongoing resource commitment (funding, staff time, political attention) from constrained jurisdictions and technical specialists and directs it toward dual-layer maintenance. The constraint moves resources from competing priorities (immediate service delivery, political projects) into the dual infrastructure of memorial ceremonies and competence systems. The beneficiary is the institutional continuity function itself—the constraint ensures that organizations survive generational cycles with preparedness intact, rather than abandoning it when the founding crisis fades from memory.
% ABSENT_VOICES: Communities directly affected by past disasters are memorialized but excluded from agenda-setting decisions about how to balance memorial burden against competence investment. Technical specialists who oppose memorial layer investment are heard in policy debates but are overruled by political leadership's institutional legitimacy requirements. Alternative preparedness models (single-layer technical-only, or memorial-only with atrophied competence) are structurally excluded because the hybrid constraint enforces both layers simultaneously.
% DISAPPEARANCE_RATIONALE: If the dual-layer constraint disappeared, institutions would face immediate pressure to choose: invest in competence to prevent disasters, or invest in memorial to maintain legitimacy and social trust. Without the constraint holding both simultaneously, many jurisdictions would abandon the expensive memorial layer, leading to institutional amnesia and vulnerability to recursive crises (repeated mistakes for lack of transmitted learning). Others would abandon competence maintenance (equipment aging, staff skill decay), preserving memorial ceremonies while losing operational capacity—producing the husk_reading outcome. The constraint's absence would force disaggregation into single-layer systems across different institutional contexts.
% FOUNDING_PROBLEM: Institutions lose preparedness during peacetime because the original crisis fades from living memory. New generations ask why resources are devoted to preparedness for something that has not happened in decades. Simultaneously, institutions that remember crises but neglect technical competence maintain memorial ceremonies while their operational capacity atrophies. The founding problem was diagnosed as: how do we keep preparedness alive when the motivating crisis recedes from immediate experience, AND how do we ensure the remembered crisis actually translates into operational competence rather than theatrical performance?
% FOUNDING_PROBLEM_CORROBORATION: Emergency management professionals and institutional historians agree that memory loss during generational turnover remains an active challenge; comparative disaster-response data from jurisdictions with weakened preparedness systems corroborates the finding that competence degrades when memorial investment drops. Public inquiries into disaster responses have identified both failures of institutional memory (repeated mistakes despite prior learning) and failures of technical competence (updated protocols not practiced). The founding problem remains live across multiple institutional contexts.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the constraint enforces dual-layer maintenance on actors with limited alternatives—the cost of preserving both institutional memory AND operational competence is substantial and not fully justified by the private benefits to resource-constrained jurisdictions. Suppression is low-to-moderate (0.34) because the constraint is legitimized by genuine institutional continuity logic and visible disaster risk; actors accept dual-layer maintenance as reasonable rather than coercive, though they resist the resource burden. Theater rises from 0.35 to 0.48 over the interval, suggesting that as memory fades and the original crisis recedes, memorial ceremonies increasingly serve legitimacy maintenance rather than genuine preparedness cultivation—but the rise is gradual, not accelerating, indicating the competence layer retains real investment. Accessibility_collapse is moderate (0.62): institutions can theoretically exit preparedness entirely or adopt single-layer models, but the cultural and legal mandates for institutional continuity make these exits practically unavailable. Resistance is high (0.71) from technical specialists who argue competence should dominate, from resource-constrained jurisdictions resisting dual-layer burden, and from memorial custodians defending institutional memory—the constraint holds despite real contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional-continuity beneficiary seat, the constraint is genuine coordination—both layers are necessary to maintain preparedness across generational cycles; abandoning either layer risks institutional failure and crisis repetition. From the resource-constrained-jurisdiction seat, the constraint is extractive dual-layer burden imposed by wealthier systems that can afford both without stress; they argue for single-layer competence and view memorial maintenance as performative luxury they cannot afford. From the political-leadership seat, the constraint is legitimate institutional maintenance that supplies democratic legitimacy (we remember our crises and learn from them) and disaster prevention (we are prepared). From the technical-competence-advocate seat, the constraint is inefficient: memorial ceremonies are noise in the competence signal; resources devoted to memorial ceremonies are not available for updated equipment and protocols. The hybrid reading asserts that ALL these perspectives are structurally correct within their seats: the constraint genuinely coordinates institutional continuity while genuinely extracting from constrained actors while genuinely legitimizing political leadership while genuinely suppressing technical prioritization. The engine computes the divergence across seats from the declared beneficiary/victim/role structure and the directionality derivation chain.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional continuity beneficiaries sit at beneficiary end (d near 0): they collect the coordination function (stable preparedness) without bearing proportionate cost; the constraint subsidizes them. Operational preparedness_staff (agenda-setter) sit at symmetric-to-target (d ~0.4-0.6): they enforce the constraint and derive authority from its dual-layer logic, but they also bear substantial implementation burden and field the resistance from specialists and constrained jurisdictions. Resource_constrained_jurisdictions sit at target end (d near 1.0): they pay dual-layer maintenance cost disproportionate to their capture of the coordination benefit; the burden falls on them precisely because they have fewest alternatives. Political leadership sits at moderate-to-beneficiary (d ~0.3-0.5 with override): they benefit from legitimacy flows from both layers but also bear political cost when either layer fails visibly; they have the most mobile exit (deprioritizing preparedness entirely) but face electoral consequences if they exercise it. Memorial_custodians sit at high-extraction end (d near 0.8) despite moderate power because their identity is fused with the memorial function—they cannot exit without existential identity loss; the constraint exploits identity-lock to secure memorial layer maintenance at lower cost than paying external contractors would require. Technical_competence_advocates sit at moderate-target (d ~0.6): they bear opportunity cost of diverted competence investment and must suppress their arguments that competence should dominate; the constraint subordinates technical prioritization to institutional legitimacy logic. Directionality overrides: political_leadership's d overrides downward from structural derivation (~0.55) to 0.35 to account for their ability to deprioritize preparedness entirely and their capture of legitimacy flows; memorial_custodians' d overrides upward from power-atom baseline (moderate → ~0.6) to 0.75 to account for identity-lock mechanism that substitutes identity fusion for material exit constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutions lose preparedness during peacetime) remains live across jurisdictions; the constraint has not outlived its founding mandate. However, the theater_ratio rise from 0.35 to 0.48 is a mandatrophy signal: it suggests that as living memory of the original crisis fades, the memorial layer increasingly serves institutional theater (we appear to remember and care) rather than genuine preparedness cultivation (we actually exercise the learning). If the theater ratio crosses 0.5 and continues rising while extractiveness remains elevated, the constraint will transition from tangled_rope (genuine coordination bundled with asymmetric extraction) to piton (atrophied coordination persisting through theatrical maintenance and institutional inertia). The current classification holds because the competence layer still drives measurable investment in preparation and the memorial layer still generates real institutional learning, not just ceremony. Mandatrophy is not yet resolved but is a live omega variable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theater_ratio_accumulation_path,
    'Will the rising theater_ratio (0.35 → 0.48) eventually saturate at a piton-level ceiling (>0.65) where memorial ceremonies become pure institutional performance, or will it stabilize in the tangled_rope range (0.45-0.55) as a sustainable tension between layers?',
    'Longitudinal measurement of drama-investment (ceremonial elaboration) vs. competence-investment (equipment spend, training hours) over 50-year horizon. If drama-investment continues rising while competence-investment plateaus or declines, theater_ratio will cross 0.5 and continue; if both scale proportionally, theater_ratio stabilizes near current level.',
    'If theater_ratio crosses 0.65, reclassify to piton (atrophied coordination maintained theatrically). If it stabilizes below 0.55, the constraint remains tangled_rope with stable dual-layer tension. The terminal classification depends on this trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_accumulation_path, empirical, 'Whether the hybrid system''s theater burden accumulates to piton-level or remains as sustainable tangled_rope tension.').

omega_variable(
    identity_lock_durability_for_memorial_custodians,
    'Memorial custodians report identity-lock (unable to exit memorial layer maintenance without existential identity loss). Will this lock persist as generational replacement occurs, or will younger archivists and historians develop instrumental rather than identity-fused relationships to preparedness memory?',
    'Cohort analysis of professional identity narratives in archival and historical fields over 15-20 year period. Measure the proportion of new-cohort practitioners who report calling as identity-constituted vs. instrumental professional role.',
    'If identity-lock persists across generational replacement, memorial_custodians remain high-d targets and the constraint maintains its extraction mechanism through identity exploitation. If identity-lock erodes and younger cohorts adopt instrumental-professional framing, memorial_custodians'' d drops significantly (from ~0.75 to ~0.4-0.5) and the constraint loses a key extraction lever—requiring either higher explicit compensation or constraint degradation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_durability_for_memorial_custodians, empirical, 'Whether memorial layer maintenance relies on persistent identity-lock or on generationally renewable instrumental professional commitment.').

omega_variable(
    competence_layer_actual_vs_ceremonial_investment,
    'How much of measured competence-layer investment (training budgets, equipment maintenance, scenario planning) is directed at actual operational readiness vs. at demonstrating preparedness to political oversight and public assurance?',
    'Audit of competence-layer spending aligned to real skill currency (post-training competence testing), equipment readiness (functional testing under stress), and scenario-planning utility (was the plan actually used in recent incidents). Measure competence-output per dollar spent.',
    'If competence-layer investment is largely theatrical (spending looks like preparation but produces minimal operational readiness), then both layers are primarily extractive and the constraint should reclassify to snare. If competence-layer investment produces real operational capacity, the constraint remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_layer_actual_vs_ceremonial_investment, empirical, 'Whether the competence layer represents genuine operational maintenance or is itself mostly theatrical alongside the memorial layer.').

omega_variable(
    kernel_reading_boundaries_contested,
    'Can the three readings (competence, husk, hybrid) coexist as live institutional positions, or does one reading logically foreclose the others?',
    'Analysis of which institutional actors hold which reading and whether any reading explicitly rejects the others'' core premises. Do technical competence advocates explicitly reject the hybrid reading''s claim that both layers are necessary, or do they merely disagree about resource priority within a shared framework?',
    'If readings are coexists_with positions (different actors, shared framework, resource-priority dispute), the kernel remains contested and multiple readings will be authored. If competence_reading''s core premise (memorial is eliminable) logically forecloses husk_reading (memorial persists but becomes drained), then competence and husk do not coexist and the kernel structure collapses. This impacts which sibling readings are valid peers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundaries_contested, conceptual, 'Whether the three preparedness readings occupy coexistent positions or whether logical foreclosure exists between them.').

omega_variable(
    resource_constrained_jurisdictions_coalition_potential,
    'Will resource-constrained jurisdictions organize collectively to demand single-layer competence-only preparedness policy, or will they remain individually trapped by local disaster risk perception?',
    'Track formation of multi-jurisdiction advocacy coalitions; measure proportion of constrained jurisdictions signaling willingness to exit dual-layer mandate if political permission existed.',
    'Coalition formation would increase resistance (current 0.71 → 0.85+) and create pressure for constraint reform or bifurcation. Individual trap maintenance preserves the current resistance level and the extraction mechanism. High coordination among constrained jurisdictions could produce political shift to single-layer mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_constrained_jurisdictions_coalition_potential, empirical, 'Whether resource-constrained jurisdictions can overcome collective-action barriers to demand preparedness policy restructuring.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t8, preparedness_commitment__hybrid_reading, theater_ratio, 8, 0.39).
narrative_ontology:measurement_basis(prep_tr_t8, observed).
narrative_ontology:measurement(prep_tr_t16, preparedness_commitment__hybrid_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement_basis(prep_tr_t16, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__hybrid_reading, theater_ratio, 24, 0.47).
narrative_ontology:measurement_basis(prep_tr_t24, observed).
narrative_ontology:measurement(prep_tr_t32, preparedness_commitment__hybrid_reading, theater_ratio, 32, 0.49).
narrative_ontology:measurement_basis(prep_tr_t32, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__hybrid_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t8, preparedness_commitment__hybrid_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(prep_be_t8, observed).
narrative_ontology:measurement(prep_be_t16, preparedness_commitment__hybrid_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement_basis(prep_be_t16, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__hybrid_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement_basis(prep_be_t24, observed).
narrative_ontology:measurement(prep_be_t32, preparedness_commitment__hybrid_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement_basis(prep_be_t32, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__hybrid_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t8, preparedness_commitment__hybrid_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement_basis(prep_su_t8, observed).
narrative_ontology:measurement(prep_su_t16, preparedness_commitment__hybrid_reading, suppression_requirement, 16, 0.32).
narrative_ontology:measurement_basis(prep_su_t16, observed).
narrative_ontology:measurement(prep_su_t24, preparedness_commitment__hybrid_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement_basis(prep_su_t24, observed).
narrative_ontology:measurement(prep_su_t32, preparedness_commitment__hybrid_reading, suppression_requirement, 32, 0.34).
narrative_ontology:measurement_basis(prep_su_t32, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__hybrid_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement_basis(prep_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__hybrid_reading, 0.18).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel decomposes into three structurally distinct constraints per the ε-invariance principle. Each reading instantiates a different ε and different beneficiary/victim structure: competence_reading focuses on operational readiness (low ε, beneficiary=institutional continuity, victim=none, treats memorial as noise); husk_reading focuses on memorial performance drained of competence (high ε, victim=operational preparedness, beneficiary=institutional theater); hybrid_reading (this story) treats both layers as structurally coupled with asymmetric extraction (moderate-high ε, beneficiary=institutional continuity, victim=constrained jurisdictions). All three readings are authored as separate constraint stories linked via network.affects_constraints. The kernel remains contested because different institutional actors hold different readings with different core premises about whether memorial layer is necessary, decorative, or extractive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__hybrid_reading, powerful, 0.35).
constraint_indexing:directionality_override(preparedness_commitment__hybrid_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
