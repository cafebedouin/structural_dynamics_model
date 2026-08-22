% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Catastrophe Memory Transmission via Ritual Form-Function Unity
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   A community that has survived periodic catastrophes maintains a ritual
 *   practice that encodes survival competence—threat recognition, resource
 *   coordination, psychological resilience—through embodied,
 *   non-propositional knowledge transmission. The ritual's form (specific
 *   gestures, vocalizations, spatial sequences, object handling) and its
 *   function (transmitting competence that works) are claimed to be
 *   inseparable: altering the form degrades the function, but the function
 *   only exists through enacted form. This constraint is the
 *   hybrid_embedded_reading of the catastrophe_memory_transmission kernel—a
 *   reading that emphasizes form-function co-constitution, distinct from
 *   operational_competence_reading (which treats form as the vehicle for
 *   function) and symbol_continuity_reading (which treats identity
 *   preservation as the intrinsic good). The constraint is claimed as rope
 *   (coordination through shared practice), with a mountain substrate
 *   (embodied knowledge as physical constraint on what can be transmitted).
 *
 * KEY AGENTS:
 *   - ritual_community: Enacts and maintains the constraint; identity-locked to the practice; receives coordination benefit from shared competence.
 *   - next_generation_initiates: Learn through embodied participation; wholly dependent on ritual form for competence acquisition; experience form-change as competence loss.
 *   - external_documentation_advocates: Institutional seat; argue for explicit extraction; powerful but excluded from ritual deliberation; experience the constraint as obscurantism.
 *   - practicing_catastrophe_survivors: Attest to ritual's functional necessity; competence-dependent; use as evidence for constraint's coordination function.
 *   - academic_observers: Analytical seat; measure ritual fidelity, competence outcomes, form-degradation effects; frame the constraint's type.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Catastrophe Memory Transmission via Ritual Form-Function Unity").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, 'b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2').
narrative_ontology:cs_kernel_codification('b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2', fixed_text).
narrative_ontology:cs_authority_grounding('b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2', practice).
narrative_ontology:cs_interpretation_layer_present('b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2').
narrative_ontology:cs_reading_relation('b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2', foundational, form_function_inseparability).
narrative_ontology:cs_axiom_status(form_function_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2', form_function_inseparability, empirically_contingent).
narrative_ontology:cs_axiom('b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2', foundational, embodied_knowledge_non_propositional).
narrative_ontology:cs_axiom_status(embodied_knowledge_non_propositional, holdable).
narrative_ontology:cs_axiom_grounding('b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2', embodied_knowledge_non_propositional, empirically_contingent).
narrative_ontology:cs_reference_frame('b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2', embodied_competence_transmission_framework).
narrative_ontology:cs_drift_state('b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2', modernization_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b9a2b18c-c957-4ee6-87d9-c7a6aa3b0ee2', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, next_generation_initiates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_catastrophe_survivors).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__hybrid_embedded_reading, external_documentation_advocates).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, embodied_knowledge_non_propositional).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, form_function_inseparability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and maintains the ritual sequence that encodes survival competence from prior catastrophe. The community transmits knowledge through embodied repetition—gesture, vocalization, spatial movement, object handling—that cannot be fully captured in propositional statement. Participation constitutes membership; discontinuing the practice would be experienced as cultural death even if propositional knowledge were preserved in writing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_community, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_community, beneficiary).

% Learn survival competence through embodied participation in the ritual: threat recognition, resource location, social coordination patterns during crisis, psychological resilience. The knowledge is acquired through apprenticeship and repetition, not through instruction manuals. Altering the ritual form degrades what they can learn—a changed gesture loses the pattern-recognition affordance it carried, a skipped phrase removes a mnemonic anchor.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, next_generation_initiates, beneficiary,
    moderate, biographical, identity_locked, local).

% Argue that survival competence should be captured in writing, video, or explicit instruction to 'democratize' it and insure against community discontinuity. They bear the cost of being told this approach fails—that propositional extraction loses the embodied component essential to actual survival competence. From their seat, the ritual's refusal to 'say what it knows' looks like obscurantism.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, external_documentation_advocates, payer,
    powerful, biographical, arbitrage, national).

% Depend on the embodied competence transmitted through ritual to recognize threat patterns and coordinate response during actual crisis. Their survival during recurring catastrophe attests to the constraint's functionality. They experience the ritual not as theater but as practical necessity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_catastrophe_survivors, beneficiary,
    moderate, immediate, trapped, local).

% Study how knowledge persists outside propositional form. They measure ritual fidelity, outcomes during catastrophe, and the degradation that occurs when form changes. Their framing affects whether the constraint is read as coordination (rope) or as natural law (mountain substrate).
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, academic_observers, observer,
    analytical, generational, analytical, global).

% Would argue that embodied, locally-embedded knowledge should be replaced by standardized, portable, propositionally-explicit competence training. They are excluded from the ritual community's deliberations but shape broader policy around education and disaster response. Their absence is structural—inclusion would require ceding the premise that embodied non-propositional knowledge is valuable.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, cultural_assimilationists, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__hybrid_embedded_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__hybrid_embedded_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes survival competence—threat recognition, resource location, social coordination, psychological resilience—through embodied ritual practice that transmits non-propositional knowledge across generations. The ritual solves the coordination problem of keeping a community's survival capacity intact across catastrophes that interrupt lived experience.
% TRANSFER_FUNCTION: Moves competence (threat-recognition patterns, resource-location heuristics, crisis-response choreography, emotional regulation anchors) from elder practitioners to initiates through repetitive embodied participation. The transfer occurs through mimesis, muscle memory, vocalization patterns, and spatial orientation—not through instruction or documentation.
% ABSENT_VOICES: Cultural assimilationists and modernization advocates argue for extracting the 'actual knowledge' into explicit form (written manuals, video instruction, classroom curricula). They would say the ritual is an inefficient wrapper around propositional facts that could be transmitted more efficiently. They are absent because inclusion would require acceptance that embodied, non-propositional knowledge is irreplaceable.
% DISAPPEARANCE_RATIONALE: If the ritual were discontinued, the community would retain propositional memory of the catastrophe (historical accounts, documented lessons) but would lose the embodied competence to recognize threat patterns in novel contexts, coordinate resource-gathering under stress, and regulate group psychology during crisis. Survivors' capacity would degrade measurably in the next catastrophe; the community's response coordination would suffer. The social world would reorganize around degraded survival capacity and higher catastrophe casualties.
% FOUNDING_PROBLEM: A catastrophic event (war, plague, natural disaster, famine) killed many and nearly destroyed the community. Survivors needed to encode the threat-recognition and resource-coordination patterns that kept them alive—patterns learned through embodied practice under extreme stress. Propositional memory alone (accounts of what happened, facts about where resources are) proved insufficient to transmit the pattern-recognition and coordinated response that actual survival required.
% FOUNDING_PROBLEM_CORROBORATION: Survivors and current practitioners attest that the ritual transmits competence that written documentation and explicit instruction do not. Academic observers studying communities that have lost the ritual (through assimilation, forced discontinuity, or modernization) document measurable degradation in threat-recognition speed, resource-coordination efficiency, and crisis-response outcomes compared to communities maintaining ritual practice. Anthropological and neuroscientific literature on embodied cognition and non-propositional knowledge supports the structural claim (sources outside the practicing community).
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness at 0.38 is moderate-low because the constraint benefits the entire community (not concentrated rents), the transmission is genuine coordination (all parties participate voluntarily, though exit is identity-locked), and the form-function unity is a material constraint rather than enforced extraction. Suppression at 0.22 is low because there is no external coercion—the community maintains the practice willingly, though external institutional pressure (state education, modernization) creates suppression directed at the constraint's persistence. Theater ratio at 0.18 is low because the ritual's primary function is actual competence transmission verified in catastrophe outcomes; theatrical maintenance is minimal (though some ritual elements serve mnemonic function that blurs function/theater). Accessibility collapse at 0.72 is high because discontinuing the ritual is experienced as identity-dissolution, making alternatives psychologically unavailable even if propositional documentation existed. Resistance at 0.31 is moderate—external advocates resist the constraint's claim to irreplaceability, but the community's resistance to changing the form is substantial. The measurement series runs t0 to t40 (one shared grid) tracking: early period (t0-10, low extractiveness as traditional practice), middle period (t10-30, rising extractiveness as external modernization pressure increases and some community members start experiencing the constraint as burden), late projection (t30-40, stabilization as the community either integrates modernization or reaffirms traditional practice). Theater rises modestly as defensive documentation and explicit justification replace tacit practice (Goodhart drift: justifying the ritual propositionally shifts energy from transmission to explanation).
 *
 * PERSPECTIVAL GAP:
 *   From the ritual community's seat and the initiates' seat, the constraint is genuine rope—coordination that solves a real problem (transmitting competence to survive catastrophe). From the external documentation advocates' seat, the constraint is snare or piton (obstruction of more efficient transmission, maintained by identity-lock and institutional conservatism). From the academic observers' seat, the constraint is either rope (if embodied knowledge is genuinely irreplaceable) or false_summit_mountain (if the constraint is claimed as natural law about form-function inseparability but is actually constructed institutional practice). The engine will compute these per-seat from the structural data: ritual community members have low directionality (beneficiaries, identity-locked but voluntary participation); external advocates have high directionality (targets of the constraint's claim that explicit extraction fails, trapped in the position of losing the argument); next-generation initiates sit near 0.5 (genuine coordination benefit but identity-locked suppression).
 *
 * DIRECTIONALITY LOGIC:
 *   The ritual community (agenda_setter + beneficiary) and initiates (beneficiary, identity_locked) are the structural beneficiaries: the constraint solves a coordination problem and they receive the benefit of transmitted competence. Their low directionality (near 0.0) reflects that they benefit from the arrangement and participate voluntarily (though identity-lock modulates their exit_options). External documentation advocates are structural targets: they argue the knowledge should be extracted and made portable, but the constraint's persistence (and the reading's claim that extraction fails) denies them this. Their high directionality (near 1.0) reflects that they are excluded from setting the constraint and bear the cost of being told their approach is inferior. Catastrophe survivors are beneficiaries but with immediate time_horizon and trapped exit—their directionality is moderate (genuine benefit, high suppression from circumstance). Academic observers are analytical, neutral in directionality. The one directionality override needed: external_documentation_advocates should be d≈0.75 (not full target, because they retain institutional power and exit options; institutional power atoms sitting near full target usually have some arbitrage). Beneficiaries list includes ritual_community and next_generation_initiates; no victims list (the constraint has no clear victim unless one reads it as suppressing external documentation advocates, which would be an alternative reading—the operational_competence_reading or symbol_continuity_reading might construct victims differently).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophe survival competence) remains live: catastrophes continue and communities still need the embodied knowledge the ritual transmits. The disappearance verdict (world_rearranges) reflects that discontinuing the ritual would degrade actual survival capacity. The constraint avoids mandatrophy (the founding problem has not outlived the constraint's function). However, there is a secondary mandatrophy risk: if external modernization pressure succeeds in replacing embodied transmission with explicit documentation, and IF that replacement actually works as well, then the founding problem becomes obsolete and the constraint becomes purely identity-driven (piton). The omega on form_function_decomposability is the probe for this risk—it tracks whether the constraint's functional claim is true or a cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_function_decomposability,
    'Can the survival competence encoded in ritual form be fully extracted into propositional documentation, video instruction, or explicit curriculum, or is something irreducibly lost in translation?',
    'Longitudinal comparison of threat-recognition accuracy, resource-coordination speed, and crisis-response outcomes between communities maintaining ritual practice and communities using only documented/explicit instruction. Compare baseline vs. post-catastrophe performance.',
    'If competence CAN be fully extracted, the constraint is better read as snare (the ritual''s persistence is theater and identity-lock, not functional necessity). If extraction inevitably loses critical components, the constraint is genuinely rope (form-function unity is structural). This determines whether the reading''s core claim stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(form_function_decomposability, empirical, 'Whether embodied, non-propositional competence is inseparably bound to ritual form.').

omega_variable(
    identity_lock_vs_functional_necessity,
    'Is the constraint maintained because initiates and elders are identity-locked to the ritual (discontinuity would feel like death even if competence were preserved), or because the ritual is genuinely the only effective transmission mechanism for embodied knowledge?',
    'Study communities that have abandoned the ritual under modernization pressure but retained competence through alternative (explicit/documented) methods. Measure psychological distress (identity-loss effects) separately from competence metrics (does threat-recognition actually degrade). Separate the identity-lock suppression from the functional requirement.',
    'High identity-lock suppression with low functional necessity would suggest the constraint is better classified as snare or piton (sustained by identity fusion, not coordination function). High functional necessity would support rope classification even if identity-lock is also present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_functional_necessity, empirical, 'Whether the constraint persists from functional necessity or from identity-fusion suppression.').

omega_variable(
    sibling_reading_distinguishability,
    'This reading claims form and function are inseparable. The operational_competence_reading emphasizes threat-pattern encoding and coordination, treating form as the vehicle; the symbol_continuity_reading emphasizes identity and mourning as intrinsic goods, treating function as secondary. Do these readings coexist as live alternatives, or does the evidence support one over the others?',
    'Ethnographic documentation of community deliberation about why the ritual persists: do practitioners justify it as ''this is the only way to learn survival skills'' (operational reading) or as ''this keeps us as a people'' (symbol_continuity reading) or as ''form and function are one thing'' (hybrid reading)? Interview practitioners across age/experience levels.',
    'If practitioners clearly favor one justification, that reading''s ε is higher (more grounded in actual maintenance drivers); if multiple justifications coexist, all three readings are live coexisting positions. This affects which sibling relationship applies (coexists_with vs. influences).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_distinguishability, conceptual, 'Whether the hybrid_embedded_reading, operational_competence_reading, and symbol_continuity_reading represent distinct live positions or compete such that evidence favors one.').

omega_variable(
    modernization_pressure_asymmetry,
    'Does external pressure to extract the knowledge into explicit form come from genuine functional improvement (documentation works better) or from institutional imperatives (state education systems, professional licensing, standardization regimes) that prioritize portability over efficacy?',
    'Compare threat-recognition outcomes in communities maintaining ritual vs. communities using state-mandated disaster-response training. Control for other variables (recent catastrophe experience, resource availability, social cohesion). Separate performance metrics from institutional-adoption metrics.',
    'If extraction into explicit form produces comparable or better survival outcomes, the constraint''s extraction (0.38) might be justified as coordination cost rather than exploitation—rope classification holds but the suppression (0.22) becomes the critical axis. If explicit form produces worse outcomes, the suppression is read as institutional coercion to abandon working practice, shifting the constraint toward tangled_rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernization_pressure_asymmetry, empirical, 'Whether pressure to extract knowledge into explicit form serves functional improvement or institutional standardization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmt_hybrid_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cmt_hybrid_tr_t0, observed).
narrative_ontology:measurement(cmt_hybrid_tr_t5, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(cmt_hybrid_tr_t5, observed).
narrative_ontology:measurement(cmt_hybrid_tr_t10, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(cmt_hybrid_tr_t10, observed).
narrative_ontology:measurement(cmt_hybrid_tr_t20, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(cmt_hybrid_tr_t20, observed).
narrative_ontology:measurement(cmt_hybrid_tr_t30, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement_basis(cmt_hybrid_tr_t30, projected).
narrative_ontology:measurement(cmt_hybrid_tr_t40, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(cmt_hybrid_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(cmt_hybrid_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(cmt_hybrid_be_t0, observed).
narrative_ontology:measurement(cmt_hybrid_be_t5, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement_basis(cmt_hybrid_be_t5, observed).
narrative_ontology:measurement(cmt_hybrid_be_t10, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(cmt_hybrid_be_t10, observed).
narrative_ontology:measurement(cmt_hybrid_be_t20, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(cmt_hybrid_be_t20, observed).
narrative_ontology:measurement(cmt_hybrid_be_t30, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement_basis(cmt_hybrid_be_t30, projected).
narrative_ontology:measurement(cmt_hybrid_be_t40, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(cmt_hybrid_be_t40, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__hybrid_embedded_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__hybrid_embedded_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_transmission kernel decomposes into three structurally distinct readings, each instantiating a different constraint with different ε values and beneficiary/victim structures. The hybrid_embedded_reading (this constraint) claims form-function inseparability; the operational_competence_reading emphasizes threat-pattern encoding with form as vehicle; the symbol_continuity_reading emphasizes identity preservation as intrinsic good. All three readings coexist in community and academic discourse. They share the referent (how does a community transmit catastrophe-survival knowledge across generations) but differ in what mechanism is primary. Family network: all three constraints should link via affects_constraints; the hybrid reading influences the operational reading (if form and function are inseparable, patterns cannot be separated from their execution) and coexists with the symbol reading (both preserve something intrinsic, though different things).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__hybrid_embedded_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
