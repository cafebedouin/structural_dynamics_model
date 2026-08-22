% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_embedded_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Ritual-Embedded Transmission of Catastrophe Survival Competence (Hybrid/Co-constitutive Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This story instantiates the hybrid/embedded reading of the
 *   catastrophe-memory-transmission kernel: ritual form and survival
 *   competence are treated as co-constitutive, not as two separable layers
 *   where one could in principle be extracted from the other and delivered by
 *   another medium. On this reading, asking 'what is the ritual really for,
 *   the symbolism or the survival skill' is a category error, because the
 *   skill has no existence apart from the symbolic enactment that carries it.
 *   This differs sharply from the sibling operational_competence_reading,
 *   which treats the ritual as an efficient-but-replaceable delivery vehicle
 *   for extractable procedural knowledge (implying the knowledge COULD in
 *   principle be taught via manual or drill without the symbolic apparatus),
 *   and from the symbol_continuity_reading, which treats the operational
 *   payload as incidental to the ritual's real function of preserving
 *   communal identity and mourning practice. The three readings share the
 *   same standing arrangement — a community's cyclical ritual practice tied
 *   to a historical catastrophe — but author different mechanisms for what
 *   actually gets transmitted and why fidelity matters, which is exactly the
 *   ε-invariance principle at work: same kernel, three constraints.
 *
 * KEY AGENTS:
 *   - ritual_specialists: administer and embody the form; identity-locked into fidelity, not extracting a surplus
 *   - practicing_descendant_community: beneficiaries of both symbolic membership and embedded operational capacity, inseparably
 *   - future_generations_facing_recurrence: powerless, trapped beneficiaries who inherit whatever fidelity or degradation occurred before them
 *   - reformist_youth_faction: excluded voice arguing for separability the hybrid reading denies is possible
 *   - outside_disaster_researchers: analytical observers testing the inseparability claim empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Ritual-Embedded Transmission of Catastrophe Survival Competence (Hybrid/Co-constitutive Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, '37aeaff7-a110-49f7-8cd7-2559ec922a5d').
narrative_ontology:cs_kernel_codification('37aeaff7-a110-49f7-8cd7-2559ec922a5d', implicit).
narrative_ontology:cs_authority_grounding('37aeaff7-a110-49f7-8cd7-2559ec922a5d', practice).
narrative_ontology:cs_interpretation_layer_present('37aeaff7-a110-49f7-8cd7-2559ec922a5d').
narrative_ontology:cs_reading_relation('37aeaff7-a110-49f7-8cd7-2559ec922a5d', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('37aeaff7-a110-49f7-8cd7-2559ec922a5d', catastrophe_memory_transmission__operational_competence_reading, influences).
narrative_ontology:cs_axiom('37aeaff7-a110-49f7-8cd7-2559ec922a5d', foundational, form_function_coconstitution).
narrative_ontology:cs_axiom_status(form_function_coconstitution, holdable).
narrative_ontology:cs_axiom_grounding('37aeaff7-a110-49f7-8cd7-2559ec922a5d', form_function_coconstitution, empirically_contingent).
narrative_ontology:cs_axiom('37aeaff7-a110-49f7-8cd7-2559ec922a5d', secondary, non_propositional_knowledge_requires_embodied_transmission).
narrative_ontology:cs_axiom_status(non_propositional_knowledge_requires_embodied_transmission, holdable).
narrative_ontology:cs_axiom_grounding('37aeaff7-a110-49f7-8cd7-2559ec922a5d', non_propositional_knowledge_requires_embodied_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('37aeaff7-a110-49f7-8cd7-2559ec922a5d', embodied_practice_as_original_transmission_medium).
narrative_ontology:cs_drift_state('37aeaff7-a110-49f7-8cd7-2559ec922a5d', contemporary_reformist_pressure_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('37aeaff7-a110-49f7-8cd7-2559ec922a5d', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_descendant_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_specialists).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, future_generations_facing_recurrence).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, embodied_cognition_thesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, form_function_coconstitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and enact the precise sequence of gestures, chants, timing, and material preparation that constitutes the ritual. They cannot separate their professional identity from correct performance; their status and self-understanding are constituted by fidelity to the form. They administer the transmission but do not extract a surplus from it — their compensation, where it exists, is proportional to maintenance of the practice, not accumulation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_specialists, agenda_setter,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_specialists, beneficiary).

% Participate in the ritual cycle and thereby absorb, through repeated embodied rehearsal rather than explicit instruction, the behavioral competencies (evacuation routes, resource caching, seasonal threat signs, coordination roles) that the ritual choreography encodes. Exit from the practice is possible but costs the operational knowledge along with the symbolic membership — the two cannot be cleanly separated.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_descendant_community, beneficiary,
    organized, generational, constrained, regional).

% Not yet born or not yet old enough to have chosen participation; they inherit whatever operational capacity the current generation's ritual fidelity has preserved. If the catastrophe recurs before they have absorbed the embedded knowledge, they bear the cost of any degradation in transmission. They have no voice in how faithfully the form is kept.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, future_generations_facing_recurrence, beneficiary,
    powerless, civilizational, trapped, local).

% Some younger community members want to modernize or abbreviate the ritual (shorter ceremonies, translated language, secularized framing) for reasons of relevance or practicality. Their proposals are rarely adopted because elders and specialists insist that altering the form risks silently deleting the embedded operational content — but the reformists are not systematically consulted on which elements are load-bearing versus which are historically incidental.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, reformist_youth_faction, excluded,
    moderate, biographical, constrained, local).

% Anthropologists and hazard scientists study whether the community's post-catastrophe survival outcomes correlate with ritual fidelity, attempting to distinguish which choreographic elements carry operational information from which are purely commemorative. They have no stake in the ritual's continuation but their findings could validate or undermine the specialists' claim that form and function cannot be separated.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, outside_disaster_researchers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__hybrid_embedded_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__hybrid_embedded_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual cycle coordinates the transmission of non-propositional survival knowledge (evacuation sequencing, resource pre-positioning, role assignment under crisis, environmental threat cues) across generations who have not personally experienced the catastrophe, by embedding that knowledge in choreographed, repeatable, memorable practice rather than in texts or lectures that would require literacy, abstraction, or deliberate study to retain under stress.
% TRANSFER_FUNCTION: The arrangement does not primarily move resources from one party to another; it moves competence forward in time — from the generation that survived (and their specialist descendants) to generations that have not yet faced the catastrophe. What flows is procedural capacity, embedded in symbolic form, and it flows outward to everyone who participates faithfully rather than being captured by any single seat.
% ABSENT_VOICES: The reformist youth faction would argue for separating essential operational content from historically contingent ornamentation so the ritual could be shortened or modernized without losing its function; they are structurally present in the community but rarely given authority over which elements to preserve, because the specialists' claim of inseparability forecloses that negotiation before it starts.
% DISAPPEARANCE_RATIONALE: If the ritual cycle vanished overnight, the embedded procedural knowledge it carries would not simply persist in some other accessible form, because on this reading the knowledge has no existence independent of the enacted practice. Evacuation sequencing, resource-caching timing, and threat-recognition cues encoded in gesture and chant would have to be reconstructed from scratch, likely at the cost of lives in the next recurrence, before the catastrophe recurs and the gap is discovered the hard way.
% FOUNDING_PROBLEM: A past catastrophic event (flood, eruption, famine, or comparable disaster) exceeded the community's capacity to survive without coordinated, rehearsed response; the ritual was built by survivors to ensure their descendants would retain the coordinated response even after living memory of the original event faded.
% FOUNDING_PROBLEM_CORROBORATION: Ritual specialists and the practicing community attest the founding problem remains live because the underlying hazard (seasonal or cyclical) has not disappeared and periodic close calls are cited as confirming evidence. Outside disaster researchers, examining historical hazard records and comparing ritual-observing versus non-observing analogous communities, offer only partial corroboration: some choreographic elements correlate with measurable preparedness outcomes, but researchers cannot confirm that every retained element still carries operational content rather than having become purely commemorative residue — corroboration exists but is incomplete and comes with an explicit caveat from outside the benefiting parties.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).
:- end_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) and rising only slightly over the century-scale interval, reflecting genuine, low-overhead coordination rather than rent extraction — no party is positioned to capture a surplus from the transmission function itself. Suppression is low-moderate (0.22): fidelity is enforced more by internalized identity-lock among specialists and by the structural fact of inseparability (deviation degrades function, not by external coercive machinery) than by active punishment of alternatives. Theater ratio is low (0.15) and only creeps upward, consistent with a practice whose performative and functional content remain substantially fused rather than one where performance has detached from function — a genuine rope substrate, not yet drifting piton-ward. Accessibility collapse is authored comparatively high (0.68) for a rope, reflecting the hybrid reading's core claim: once you accept that form and function are co-constitutive, alternative transmission media (manuals, secular drills) are not live options without reconstructing the knowledge from nothing, which meaningfully closes off the alternative that the operational_competence_reading treats as available.
 *
 * DIRECTIONALITY LOGIC:
 *   Every named beneficiary (current community, specialists, future generations) is positioned as receiving the transmitted capacity rather than paying for someone else's capture of it — there is no declared victim group on this reading, consistent with the expected structural delta, because the only party who could be harmed is discontinued practice itself, which harms the community diffusely rather than transferring resource to an extractor. Ritual specialists sit closest to agenda-setting but their exit option is identity-locked rather than arbitrage-grade, which keeps their derived directionality from reading as a captured beneficiary position; they administer the form because their identity is constituted by it, not because they profit from withholding it.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resists mislabeling this arrangement as pure extraction precisely because it denies the separability that extraction narratives require: there is no identifiable party skimming a surplus by keeping form and function bundled, because on this reading they cannot be unbundled even in principle. Conversely it resists mislabeling the arrangement as pure natural law (mountain) despite an embodied-knowledge substrate, because the ritual is still a human choice maintained by specific practitioners who could, in principle, let fidelity lapse — the founding problem's contested status (recurring hazard vs. faded urgency) is exactly the fact pattern that would trigger reclassification if the operational function were shown to have fully atrophied while the symbolic form persisted by inertia alone (the piton the symbol_continuity_reading would more comfortably accommodate).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_function_separability,
    'Is the claimed inseparability of ritual form and survival competence a genuine structural fact about embodied cognition, or a doctrine maintained by ritual specialists because it protects their gatekeeping authority over the transmission process?',
    'Comparative field study: identify communities that have abbreviated or secularized equivalent rituals and measure whether measurable operational competence (evacuation time, resource pre-positioning accuracy, threat recognition) degrades, holds steady, or is unaffected relative to communities that preserved full ritual fidelity.',
    'If competence holds steady after abbreviation, the hybrid reading collapses toward the operational_competence_reading (form is separable, specialists'' inseparability claim was gatekeeping cover, and the constraint reclassifies toward tangled_rope with specialists as beneficiaries and reformists as payers of an unnecessary cost). If competence degrades measurably, the hybrid reading is vindicated and the rope/mountain-substrate classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_function_separability, empirical, 'Whether form-function inseparability is a real embodied-cognition constraint or a specialist-protective doctrine.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three sibling readings of the catastrophe_memory_transmission kernel locate their disagreement — is it about WHAT is transmitted (competence vs. identity), or about WHETHER the transmission medium (ritual form) is separable from its payload?',
    'This is a conceptual/framing question rather than an empirical one: it can be partially clarified by asking practitioners and researchers to identify specific ritual elements and classify each as load-bearing-for-competence, load-bearing-for-identity, or genuinely fused, then checking whether any element resists that classification (which would support the hybrid reading specifically).',
    'If most elements can be cleanly sorted into competence-only or identity-only bins, the hybrid reading is largely dissolved into the other two and this constraint should be understood as a weighted composite rather than a distinct structural claim. If a substantial residue of elements resists sorting, the hybrid reading captures something the other two miss and stands as structurally distinct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Whether the hybrid reading is a genuinely distinct structural claim or a blend of the two sibling readings.').

omega_variable(
    future_generation_no_voice_ambiguity,
    'Given that future_generations_facing_recurrence cannot consent to or contest the fidelity choices made on their behalf, does their powerless/trapped position constitute a form of latent victimhood that the hybrid reading''s ''no clear victim'' framing obscures?',
    'Track whether any documented instance exists of a community discovering, after a recurrence, that ritual drift had silently deleted operationally critical content before the event exposed the gap — this would be direct evidence of realized (not merely latent) harm to an unconsented party.',
    'If such instances are documented, future generations should be treated as a de facto victim group despite the coordination framing, which would push the classification toward tangled_rope (coordination function for the living, asymmetric risk borne by the unborn) rather than a clean rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generation_no_voice_ambiguity, preference, 'Whether unconsenting future beneficiaries with no voice constitute a victim class under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 80, 0.17).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 100, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__hybrid_embedded_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__hybrid_embedded_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'the ritual transmits catastrophe survival knowledge' per the ε-invariance principle. All three share the same standing arrangement (a community's cyclical catastrophe-commemoration ritual) but author distinct mechanisms and consequently distinct ε trajectories: this hybrid_embedded_reading holds extractiveness low and stable (co-constitution implies no separable surplus to extract); operational_competence_reading is expected to test extractiveness against measurable competence outcomes and may show extraction if specialists gatekeep a separable, teachable core; symbol_continuity_reading is expected to deprioritize operational metrics entirely and evaluate the arrangement primarily against identity/mourning preservation, likely landing closer to rope-with-different-beneficiary-structure or piton if operational content has genuinely atrophied. Network edges link all three so contamination/coupling analysis can trace how empirical resolution of the form-function separability question (see omega form_function_separability) would ripple across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
