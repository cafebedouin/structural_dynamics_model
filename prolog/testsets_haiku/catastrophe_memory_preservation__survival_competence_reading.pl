% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Catastrophe Memory Preservation via Mandatory Ritual Participation (Survival Competence Reading)
 *   domain: religious/social/disaster_preparedness
 *
 * SUMMARY:
 *   A community bearing historical memory of a catastrophic event (flood,
 *   famine, invasion, plague) maintains mandatory ritual reenactment to
 *   preserve threat-recognition protocols across generations. The
 *   survival-competence reading asserts that this ritual encodes operational
 *   survival knowledge (how to recognize warning signs, execute cascade
 *   arrest, manage scarce resources under threat) into embodied practice and
 *   emotional memory in a way that cannot be reliably transmitted through
 *   writing or classroom instruction. Participants are required to reenact
 *   the catastrophe annually or cyclically, absorbing emotional labor, time
 *   cost, and constraint on autonomy. This reading instantiates the
 *   constraint as tangled_rope: it coordinates a real knowledge-transfer
 *   function (future generations inherit embodied competence) while
 *   extracting mandatory participation from the present generation. The claim
 *   and metrics are intentionally divergent: the constraint is claimed as
 *   tangled_rope (mixed coordination + extraction); the authored metrics
 *   describe high extractiveness, moderate suppression, low theater. The
 *   engine computes per-seat type; the divergence is the measurement.
 *
 * KEY AGENTS:
 *   - ritual_authority_elders: Custodians of the ritual and the historical record; maintain participation as non-negotiable for cultural competence; extract authority from knowledge-stewardship (organized power, identity-locked exit)
 *   - present_generation_participants: Absorb mandatory participation cost; receive threat-recognition knowledge but lose autonomy during ritual seasons; cannot exit without loss of identity and community status (moderate power, identity-locked exit)
 *   - future_generations: Non-agent beneficiary cohort; will inherit embodied competence; have no voice in present participation decision (powerless, trapped exit, civilizational horizon)
 *   - competing_modernization_narratives: Excluded from legitimacy; argue for alternative (written, classroom, simulation) knowledge-transfer mechanisms; would reshape the constraint if admitted to the decision structure (moderate power, constrained exit)
 *   - disaster_response_specialists: Observers; empirically assess whether ritual produces superior threat-recognition relative to alternatives (institutional power, analytical exit)
 *   - cultural_continuity_advocates: Defend the ritual as core to identity and collective memory, independent of survival-competence claims; support mandatory participation as justified by social continuity function (organized power, identity-locked exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.45).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Catastrophe Memory Preservation via Mandatory Ritual Participation (Survival Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious/social/disaster_preparedness").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, 'cdd5d526-db13-40f0-b9c7-c564b3841d2f').
narrative_ontology:cs_kernel_codification('cdd5d526-db13-40f0-b9c7-c564b3841d2f', fixed_text).
narrative_ontology:cs_authority_grounding('cdd5d526-db13-40f0-b9c7-c564b3841d2f', lineage).
narrative_ontology:cs_interpretation_layer_present('cdd5d526-db13-40f0-b9c7-c564b3841d2f').
narrative_ontology:cs_reading_relation('cdd5d526-db13-40f0-b9c7-c564b3841d2f', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('cdd5d526-db13-40f0-b9c7-c564b3841d2f', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('cdd5d526-db13-40f0-b9c7-c564b3841d2f', foundational, embodied_protocol_transfer_empirically_necessary).
narrative_ontology:cs_axiom_status(embodied_protocol_transfer_empirically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('cdd5d526-db13-40f0-b9c7-c564b3841d2f', embodied_protocol_transfer_empirically_necessary, empirically_contingent).
narrative_ontology:cs_axiom('cdd5d526-db13-40f0-b9c7-c564b3841d2f', secondary, ritual_exclusion_of_alternative_mechanisms).
narrative_ontology:cs_axiom_status(ritual_exclusion_of_alternative_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('cdd5d526-db13-40f0-b9c7-c564b3841d2f', ritual_exclusion_of_alternative_mechanisms, conventional).
narrative_ontology:cs_reference_frame('cdd5d526-db13-40f0-b9c7-c564b3841d2f', embodied_threat_recognition_necessity).
narrative_ontology:cs_drift_state('cdd5d526-db13-40f0-b9c7-c564b3841d2f', contemporary_modernization_pressure, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cdd5d526-db13-40f0-b9c7-c564b3841d2f', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, cultural_continuity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and enforce the mandatory ritual cycle: reenactment of historical catastrophe, transmission of threat-recognition protocols, assertion that participation is non-negotiable for competent adulthood. Their authority derives from custodianship of the historical record and the claim that embodied practice is the only reliable knowledge transfer mechanism. They enforce participation through community sanctions and status withholding.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_authority_elders, agenda_setter,
    organized, generational, identity_locked, local).

% Required to participate in annual/cyclical ritual reenactment: time investment, emotional labor (reliving catastrophe), constraint on alternative life paths during ritual seasons. They absorb the cost of maintaining intergenerational knowledge transfer while also receiving the benefit of learning threat-recognition protocols that may save their own lives. Exit means community expulsion or loss of cultural identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, beneficiary).

% Will inherit embodied threat-recognition capacity and historical memory that prevents catastrophic unpreparedness. They have no voice in the present participation decision and no alternative means of acquiring this knowledge if the ritual atrophies or is abandoned. Non-agent: future persons are not present to negotiate; the constraint's beneficiary function is indexed to a generational cohort not yet in the conversation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__survival_competence_reading, future_generations).

% Argue that written records, education systems, and disaster-preparedness training are superior knowledge-transfer mechanisms, making mandatory ritual participation obsolete or even harmful (imposing trauma on children, preventing youth mobility, consuming resources). They are systematically excluded from the ritual authority's decision structure about what counts as legitimate knowledge transfer.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, competing_modernization_narratives, excluded,
    moderate, biographical, constrained, regional).

% Study whether embodied ritual reenactment produces demonstrably superior threat-recognition and survival decision-making relative to classroom training, simulation, or written protocols. They provide empirical assessment of whether the constraint's coordination function (knowledge transfer) justifies the extraction (mandatory participation cost).
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, disaster_response_specialists, observer,
    institutional, biographical, analytical, regional).

% Defend mandatory ritual participation as the core mechanism for maintaining cultural identity and collective memory, even independent of direct survival-competence outcomes. They argue that the ritual's extraction cost is justified by the coordination it provides for group coherence and historical consciousness.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, cultural_continuity_advocates, beneficiary,
    organized, generational, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__survival_competence_reading, ritual_authority_elders).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transfers operational threat-recognition protocols (recognition of warning signs, cascade-arrest decision trees, resource-management under constraint) from elder knowledge to younger cohorts through embodied reenactment rather than classroom instruction. The claim is that visceral, repeated practice embeds threat-recognition in muscle memory and emotional association in a way text-based instruction cannot achieve.
% TRANSFER_FUNCTION: Moves mandatory participation time, emotional labor, and constrained autonomy from the present-generation participants to the ritual authority (as administrators/custodians) and to future generations (as knowledge holders who will inherit the embodied competence without having to rebuild it from scratch).
% ABSENT_VOICES: Modernization advocates and alternative-education proponents are structurally excluded from setting knowledge-transfer policy. They would argue for measured adoption of written, video, or simulation-based training alongside or instead of mandatory embodied ritual. Their exclusion is maintained by the ritual authority's control over legitimacy-definition: if you question the ritual's necessity, you are defined as outside the community of competent cultural agents.
% DISAPPEARANCE_RATIONALE: If mandatory catastrophe-memory ritual disappeared overnight, the community would reorganize around alternative knowledge-transfer mechanisms (written protocols, formal education, professional disaster training). Threat-recognition capacity would remain transmissible but via different channels; the present generation would lose forced participation obligations but future generations would lack the embodied competence the elders claim is irreplaceable. The community's identity and preparedness posture would restructure.
% FOUNDING_PROBLEM: A community experienced a catastrophe that killed many and resulted from failure to recognize warning signs and manage cascading failures. Survivors observed that those with embodied memory of the threat pattern (who had lived through it) recognized the cascade and protected themselves or their households; those who learned the history only verbally or didactically did not. The ritual was established to encode threat-recognition protocols into embodied practice so that future generations would recognize the pattern and act decisively.
% FOUNDING_PROBLEM_CORROBORATION: Ritual elders attest the founding problem is live: threat patterns recur and embodied recognition is still necessary. Disaster-response specialists and historians from outside the community attest that the historical catastrophe was real and that some survivors did respond better — but they dispute whether embodied ritual is the only or superior mechanism for maintaining that competence. Educational modernizers argue the founding problem (failure to transmit threat-recognition) is solvable through alternative means and no longer requires this specific extraction. No corroboration from future generations (they do not yet exist to testify). The dispute centers on whether the founding problem's solution requires mandatory embodied ritual or whether it can be solved through alternative mechanisms that impose lower extraction cost.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) because mandatory participation is costly (time, emotional labor, autonomy constraint) and is sustained by institutional authority that excludes alternatives from legitimacy consideration. It is not maximal because the coordination function is genuine and acknowledged even by skeptics — the question is whether the extraction cost is proportionate to the transfer benefit. Suppression is moderate (0.45) because the constraint operates primarily through identity-lock (exit means cultural expulsion) rather than coercive force; participants internalize the obligation as self-conceptually constitutive. Theater ratio is low-moderate (0.28): the ritual remains functionally oriented to threat-recognition drill rather than purely symbolic mourning, but rising theater over the interval suggests increasing proportion of performative activity that defends the ritual's necessity rather than actually transferring operational knowledge. Accessibility collapse is moderate (0.62): alternatives exist (classroom, written, simulation) but have been suppressed from legitimacy by ritual authority's framing of embodied practice as uniquely efficacious. Resistance is moderate-high (0.58): modernization advocates and younger participants contest mandatory participation, but resistance is constrained by identity-lock and community sanctions. The measurement series shows extractiveness rising and plateauing, theater ratio rising with decreasing slope (suggesting the ritual is becoming more performative over time, defending its institutional position rather than improving knowledge transfer), and suppression stabilizing (identity-lock remains constant across the interval). This trajectory is consistent with a constraint that coordinates a real function but exhibits rising institutional overhead relative to functional benefit.
 *
 * PERSPECTIVAL GAP:
 *   From the elder-authority seat, this constraint is essential coordination: it preserves knowledge that will save future lives and maintains cultural continuity. The extraction (mandatory participation) is justified as the price of that transmission. From the present-generation-participant seat, it is costly extraction (time, labor, autonomy) justified by an uncertain future benefit (will the threat recur? will the knowledge actually help?). From the future-generation seat (non-agent, but analytically), it is a benefit imposed without consent — they inherit competence but never chose to accept the extraction cost imposed on their parents. The ritual authority computes the constraint as justified tangled rope; modernizers compute it as camouflaged snare using survival-competence mythology to defend institutional authority; specialists await empirical verdict. The engine computes per-seat directionality from beneficiary/victim declarations and exit options: elders sit near the beneficiary end (they extract authority and maintain institutional position); present participants sit near the target end (mandatory participation cost, identity-locked exit making exit prohibitive); future cohorts sit as non-agent beneficiaries (receive competence but never participate in the decision). This asymmetry is structural to the constraint and should manifest in different type computations across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual elders (agenda_setter): d ≈ 0.2–0.3 (beneficiary seat: maintain institutional authority, extract custodian status, but also absorb enforcement costs and historical responsibility for knowledge transmission). Present-generation participants (payer): d ≈ 0.7–0.8 (target seat: mandatory participation cost, internalized obligation through identity-lock, constrained exit through community sanctions, no arbitrage option). Future generations (beneficiary, non-agent): d = 0.0 (no directionality computed for non-agents; they are the referent of benefit but not a structural seat). Competing modernizers (excluded): d ≈ 0.5–0.6 (symmetric-to-target: they lose legitimacy status and institutional voice but are not mandated to participate in the ritual itself; their extraction is institutional exclusion rather than direct participation cost). Disaster specialists (observer): d = analytical (analytical seat; compute only the mismatch between ritual authority's claims and empirical outcomes). The divergence between elder-seat computation and payer-seat computation should be sharp: elders compute the constraint as functional coordination with acceptable enforcement cost; payers compute it as asymmetric extraction defended by institutional authority. This is the per-seat classification divergence the engine measures; it is not a defect in the authored structure but the point of the measurement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (recognizing threat patterns and managing cascades to prevent catastrophe) is live in the ritual authority's framing but contested in modernizers' framing. The disappearance verdict is world_rearranges: if mandatory ritual vanished, the community would reorganize around alternative knowledge-transfer mechanisms; the world does not stay the same without the constraint. The founding_problem_status is contested: elders attest the threat patterns are still live; modernizers attest the founding problem is solvable through alternative means and no longer requires embodied ritual. The mismatch is (contested founding_problem_status, world_rearranges disappearance_verdict): the constraint's mandate is philosophically and empirically disputed. This is the entry condition for mandatrophy analysis. If rising theater_ratio indicates the ritual is increasingly performative (defending its institutional position rather than transferring operational knowledge), and if future empirical research shows alternatives are equivalent or superior, the constraint will accumulate evidence of mandate obsolescence while the institutional apparatus persists. This is the mandatrophy signature: (1) founding problem contested or dead, (2) world rearranges without constraint (arrangement is not natural law), (3) rising theater ratio (institutional activity defending the constraint's necessity rather than functional operation), (4) suppression stabilizing or rising (institutional effort to maintain participation against growing resistance). Currently the constraint exhibits partial mandatrophy indicators: rising theater, contested founding problem, institutional suppression of alternatives. The resolution will depend on empirical outcome of the embodied-vs-symbolic-transfer omega: if ritual proves superior, mandate is live; if alternatives are equivalent, mandate is dead and the constraint transitions to piton (institutional inertia maintaining a constraint whose function has atrophied).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    embodied_vs_symbolic_transfer,
    'Does embodied ritual practice demonstrably produce superior threat-recognition and survival decision-making relative to written protocol, classroom instruction, or simulation training?',
    'Comparative study of threat-recognition accuracy and decision speed across cohorts trained via ritual, classroom, written protocol, and simulation under controlled conditions. Longitudinal follow-up during actual threat events (or high-fidelity simulation of cascading disasters).',
    'If ritual shows measurable superiority, the extraction (mandatory participation cost) is justified as the price of superior coordination. If alternatives are equivalent or superior, the extraction appears to persist as institutional inertia rather than functional necessity, shifting the classification toward snare. If ritual shows inferiority, the constraint becomes mandated obsolescence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(embodied_vs_symbolic_transfer, empirical, 'Whether the constraint''s claimed coordination function (embodied threat-recognition transfer) is empirically superior to alternatives.').

omega_variable(
    modernization_foreclosure_mechanism,
    'Is the constraint''s persistence maintained by the functional claim (embodied transfer is necessary) or by institutional authority that excludes alternative knowledge-transfer frameworks from legitimacy?',
    'Policy experiment: introduce alternative training pathways (classroom, written, simulation) as optional supplements, then measure (1) participation uptake, (2) threat-recognition outcomes for each pathway, (3) whether ritual authority retains legitimacy or loses participation. Track whether exclusion of alternatives is structural (embodied practice objectively necessary) or institutional (alternatives are suppressed from consideration).',
    'If alternatives are voluntarily abandoned by participants who try them, the constraint''s persistence is functional. If alternatives are excluded from even being considered legitimate, the constraint entangles coordination (real transfer need) with institutional domination (exclusion of competitors). This determines whether classification remains tangled_rope or shifts to snare-with-coordination-cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_foreclosure_mechanism, empirical, 'Whether the constraint''s suppression is structural (alternatives genuinely inferior) or institutional (competitors systematically excluded from legitimacy).').

omega_variable(
    intergenerational_consent_boundary,
    'Can present-generation extraction on behalf of future generations be justified through the ritual framework, or is it a fundamental asymmetry the constraint cannot resolve?',
    'Philosophical/institutional analysis: does the community recognize any mechanism by which future generations could retroactively consent to or reject the extraction imposed on their behalf? Or is the constraint fundamentally paternalistic (present authority decides what future persons need, without their voice)?',
    'If future-generation consent is treated as impossible-in-principle (they do not exist to consent), the constraint is fundamentally asymmetric extraction. If the community recognizes mechanisms by which adult future generations can assess whether the ritual was worthwhile and either continue or abandon it voluntarily, the extraction is constrained by an intergenerational consent gate. This affects whether the constraint sustains legitimacy or accumulates mandatrophy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_consent_boundary, conceptual, 'Whether the constraint''s intergenerational extraction structure can be resolved through consent mechanisms or remains fundamentally paternalistic.').

omega_variable(
    kernel_reading_boundary,
    'Is this constraint a reading of the catastrophe-memory-preservation kernel that emphasizes operational survival competence, or is it simply one historical implementation that has been superseded by alternative readings?',
    'Textual/historical analysis of ritual texts, elder testimony, and community disputes about the ritual''s purpose. Map whether the survival-competence reading is explicitly articulated in authoritative ritual sources, or whether it is an inferred reading imposed by modern analysts. Track whether the community itself contests which reading (survival-competence vs. mourning-practice vs. hybrid-atrophy) is the correct understanding of the kernel.',
    'If the survival-competence reading is an explicit, coherent articulation from the ritual tradition''s own sources, it is a primary reading. If it is an analytical imposition by external observers, it is a secondary reading less grounded in the committing authority. This affects the reading''s claim to represent authentic intergenerational knowledge, not just modern projection onto historical practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the survival-competence reading is an authentic articulation from the ritual tradition or an external analytical imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 25, 0.45).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__survival_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe-memory-preservation kernel. The survival-competence reading emphasizes operational knowledge transfer and threat-recognition as the primary coordination function; ritual is the necessary mechanism for embodying protocols. Sibling readings emphasize symbolic mourning (mourning_practice_reading) or institutional atrophy (hybrid_atrophy_reading) as the actual primary function. Each reading has its own constraint story with distinct ε values, beneficiary structures, and type classifications. The readings coexist as live positions held by different parties (elders vs. modernizers vs. institutional analysts); neither forecloses the others at the kernel level, though each reading's policy implications would constrain or reshape the others' operational freedom if adopted. Link these three stories via network.affects_constraints to model the family structure and enable family-level analysis of how contest resolution in one reading would propagate to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
