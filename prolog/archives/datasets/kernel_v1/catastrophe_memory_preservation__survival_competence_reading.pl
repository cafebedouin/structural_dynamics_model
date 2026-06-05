% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Catastrophe Memory Preservation via Ritual (Survival-Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Catastrophe-memory rituals — disaster commemorations, reenactments,
 *   survival-preparation ceremonies, collective danger rehearsals — occupy a
 *   contested structural position. From the survival-competence reading,
 *   ritual functions as an embodied knowledge-transfer mechanism: the lived,
 *   visceral, emotionally-saturated rehearsal of threat and response teaches
 *   participants (especially younger generations) patterns of recognition and
 *   coordination that abstract instruction alone cannot convey. The
 *   constraint extracts substantial cost from present participants (time,
 *   emotional labor, opportunity cost, sometimes physical ordeal) to encode
 *   survival-relevant knowledge in future generations. From an analytical
 *   distance, this constraint shows all six classifications across different
 *   structural positions: the present-generation participant experiencing
 *   maximum extraction (snare); the knowledge-holding institution
 *   experiencing coordination (rope); the community experiencing mixed
 *   extraction-coordination (tangled_rope); the modernized institution
 *   experiencing degraded ritual (piton); and the analytical observer risking
 *   naturalization as immutable law (mountain). The constraint's high
 *   extractiveness (0.58) and significant suppression (0.62) reflect that
 *   participation is mandatory (enforced via social/identity pressure),
 *   demanding (time and emotional cost), and asymmetric (present generation
 *   bears cost, future generation receives benefit). Theater ratio (0.48)
 *   indicates that the ritual retains genuine operational function — it is
 *   not merely performative — but that function may be degrading as threats
 *   become historical rather than active.
 *
 * KEY AGENTS:
 *   - Present-generation ritual participants (powerless/identity_locked): Bear full cost of participation; identity fused with ritual role; structurally mobile but cannot exercise exit without identity dissolution.
 *   - Future generations (beneficiaries): Receive encoded threat-recognition competence; optionality and actual benefit contingent on whether threat remains active and whether alternative transmission methods emerge.
 *   - Knowledge-holding institution (elders, religious authorities, organized/arbitrage): Maintain authority status through ritual structure; benefit from institutional preservation; experienced constraint as primarily coordinative.
 *   - Community observing ritual (moderate/constrained): Subject to social pressure; both constrained by and benefiting from ritual's coordination function; heterogeneous experience depending on kin relationship to original catastrophe.
 *   - Secularized/modernized institution (institutional/arbitrage): Maintains ritual through inertia or cultural identity rather than active threat-recognition transmission; sees own function as degraded.
 *   - Analytical observer (analytical/analytical): Risks naturalizing contingent institutional practice as immutable law; FSM candidate due to beneficiary declaration on what appears to be natural constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Catastrophe Memory Preservation via Ritual (Survival-Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, 'bc890809-c90c-4775-87bf-be4f6370cd8a').
narrative_ontology:cs_kernel_codification('bc890809-c90c-4775-87bf-be4f6370cd8a', distributed).
narrative_ontology:cs_authority_grounding('bc890809-c90c-4775-87bf-be4f6370cd8a', practice).
narrative_ontology:cs_interpretation_layer_present('bc890809-c90c-4775-87bf-be4f6370cd8a').
narrative_ontology:cs_reading_relation('bc890809-c90c-4775-87bf-be4f6370cd8a', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc890809-c90c-4775-87bf-be4f6370cd8a', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('bc890809-c90c-4775-87bf-be4f6370cd8a', foundational, embodied_threat_recognition_irreducible).
narrative_ontology:cs_axiom_status(embodied_threat_recognition_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('bc890809-c90c-4775-87bf-be4f6370cd8a', embodied_threat_recognition_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('bc890809-c90c-4775-87bf-be4f6370cd8a', foundational, intergenerational_survival_obligation).
narrative_ontology:cs_axiom_status(intergenerational_survival_obligation, holdable).
narrative_ontology:cs_axiom_grounding('bc890809-c90c-4775-87bf-be4f6370cd8a', intergenerational_survival_obligation, deontological).
narrative_ontology:cs_reference_frame('bc890809-c90c-4775-87bf-be4f6370cd8a', active_embodied_transmission).
narrative_ontology:cs_drift_state('bc890809-c90c-4775-87bf-be4f6370cd8a', contemporary_modernized_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bc890809-c90c-4775-87bf-be4f6370cd8a', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESENT-GENERATION PARTICIPANT (SNARE) — Identity fused with ritual obligation ('keeper of the tradition,' 'guardian of the knowledge'). Structurally mobile (could refuse participation) but identity-locked: exiting would mean abandoning the identity constituted through the role. Bears full burden of time, emotional labor, and opportunity cost of ritual participation. No meaningful benefit flows to them — the benefit flows to future generations they may never meet. Maximum experienced extraction from a biographical horizon.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__survival_competence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: COMMUNITY OBSERVING RITUAL (TANGLED ROPE) — Constrained by social pressure, normative expectations, and genuine coordination benefit (shared threat-recognition, collective memory rehearsal). The ritual both coordinates the community's survival capacity AND extracts from them the obligation to participate, to invest emotional labor, to structure family life around commemorative cycles. Some agents benefit (elders who see their knowledge integrated; younger learners acquiring competence), while others bear costs (those without kin in the remembered catastrophe; those whose life plans conflict with ritual calendar). Exit is possible but socially costly.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__survival_competence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: KNOWLEDGE-HOLDING INSTITUTION (ROPE) — Institutional authority (elders, religious authorities) benefits from the ritual's coordination function: they maintain status as knowledge-keepers, their authority is regularly reaffirmed through the ritual's structure, and their expertise becomes indispensable to each generation. The ritual solves the collective action problem of transmitting survival-critical knowledge. From this perspective, the constraint is primarily coordinative — the extraction mechanism (authority maintenance) is inseparable from the coordination function (threat-recognition rehearsal), but the net effect is stable rather than ratcheting.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__survival_competence_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, the constraint appears as an immutable feature of how human communities preserve survival knowledge: embodied ritual is the only mechanism by which visceral threat-recognition (fear responses, sensory memory, intuitive pattern recognition) can be transmitted across generations. Propositional knowledge alone ('here is how floods happen') does not encode the embodied competence ('here is what it feels like to be caught in one'). This perspective naturalizes the ritual as a law of collective survival. However, the structural data (extractiveness, beneficiaries, victims, enforcement requirements) will reveal this as a false summit — the constraint is contingent institutional choice, not natural law.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__survival_competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: SECULARIZED / MODERNIZED INSTITUTION (PITON) — In contexts where the ritual's original threat (flood, earthquake, famine, genocide) is historically distant or has been technologically mitigated, the ritual persists as memorial/cultural practice with degraded operational function. Theater ratio rises as actual survival competence transfer falls. The ritual is maintained through institutional inertia, aesthetic value, or identity maintenance rather than active threat-recognition transmission. The institution sees itself as performing a vestigial role — 'preserving tradition' rather than 'preserving survival capacity.' This is the degraded form of the survival-competence reading.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__survival_competence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_preservation__survival_competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_preservation__survival_competence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.58): High-moderate. The survival-competence reading posits that ritual encodes genuinely valuable knowledge (threat recognition patterns, coordinated response protocols, embodied fear responses calibrated to actual dangers) that future generations need. The extraction is substantial — present participants bear high costs (time, emotional/physical ordeal, life-planning constraints around ritual calendar). However, extractiveness is not as high as a pure snare (0.72+) because the constraint has genuine coordination function: it solves the collective action problem of knowledge transmission. The extraction is not hidden (participants know they are learning/teaching), and some agents (elders, younger learners) do benefit within the present generation. The rising trajectory (0.42→0.58 over 50 intervals) reflects that as threat becomes historical rather than immediate, the extraction cost is increasingly questioned while the coordination function is reframed as cultural identity rather than survival. SUPPRESSION (0.62): Moderate-high. Participation is enforced through social/identity mechanisms (identity_locked exit option), kinship obligations, social ostracism for non-participation, and enculturation from childhood. These are real barriers, though not as absolute as physical confinement. The rising trajectory (0.55→0.62) reflects that as communities modernize and formal schooling/careers provide alternative identity sources, maintaining ritual participation requires stronger enforcement mechanisms — social shaming intensifies as structural incentives weaken. THEATER RATIO (0.48): Moderate. The ritual is not purely performative — it accomplishes actual threat-recognition transmission and community coordination. But theater is substantial because much of the ritual's 'work' is symbolic, aesthetic, and emotionally-valorized beyond its strictly functional content. As the threat becomes historical, theater rises (0.28→0.48) while the operational transfer function stabilizes or declines. The trajectory indicates gradual shift toward the hybrid_atrophy or piton reading.
 *
 * PERSPECTIVAL GAP:
 *   The present-generation participant sees snare (maximum extraction, identity-locked exit); the knowledge-holding institution sees rope (coordination function, authority maintenance); the community sees tangled_rope (mixed extraction and coordination); the modernized institution sees piton (degraded ritual maintained through inertia); the analytical observer risks seeing mountain (natural law of how knowledge transmits). The gap reveals that the constraint's classification is observer-relative: a single structural phenomenon (the ritual) produces radically different experiential classifications depending on the agent's structural position. The perspectival gap is not a measurement problem — it is the essence of the constraint's operation. For the target (present participant), the constraint extracts. For the beneficiary (knowledge-holding institution), it coordinates. The extraction is not hidden; it is justified through reference to future-generation benefit — a future that the present participant may not directly experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position: (1) Present participant: victim (bears cost) + identity_locked exit → high d (0.85+) → high χ → snare. (2) Knowledge institution: beneficiary (maintains authority status) + arbitrage exit → low d (0.15) → low χ → rope. (3) Community: both + constrained exit → moderate d (0.55) → moderate χ → tangled_rope. (4) Analytical: analytical power/exit → canonical d (0.72) → standard f(d) → mountain (but FSM candidate due to beneficiary declaration). The directionality gap between victim and beneficiary (d: 0.85 vs 0.15) is the structural signature of extraction — the same constraint flows toward one agent and away from another. No override needed; the structural data is coherent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint illustrates the mandatrophy through the false-summit risk: the analytical observer naturalizes the constraint as mountain (immutable law of how embodied knowledge transmits across generations) when in fact it is contingent institutional choice (tangled_rope with significant extraction). The resolution depends on empirical resolution of the omegas: if alternative transmission mechanisms achieve comparable outcomes, the ritual's extraction is unjustified (reclassifies toward snare). If embodied transmission is genuinely superior, the extraction is justified as coordination cost (tangled_rope confirmed). If the threat has become historical/mitigated, the extraction is no longer justified by survival benefit (reclassifies toward piton or pure snare). The mandatrophy is resolved not by choosing a type a priori but by testing the structural assumptions: does the constraint actually preserve survival-relevant knowledge? Is that knowledge actually needed? Are there alternatives? The answers determine the true classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    embodied_vs_propositional_knowledge_transfer,
    'Does embodied ritual transmission of threat-recognition (visceral fear, sensory memory, intuitive pattern detection) actually transfer survival-critical knowledge that propositional/classroom instruction cannot?',
    'Comparative analysis: disaster response outcomes for communities with active ritual traditions vs those relying on formal training/instruction; ethnographic evidence of knowledge retention and application in actual threats; neuroscience of embodied memory vs abstract learning under stress.',
    'If embodied transfer is superior: ritual extraction is justified as coordination cost for survival benefit (strengthens tangled_rope). If propositional+training achieves comparable outcomes: ritual is luxury (extraction without survival justification, shifts toward snare). If hybrid (embodied needed for some competences, propositional sufficient for others): constraint decomposes into separate stories with different epsilon values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_vs_propositional_knowledge_transfer, empirical, 'Whether embodied ritual transmission transfers survival knowledge better than propositional instruction').

omega_variable(
    intergenerational_knowledge_decay,
    'How much threat-recognition competence (pattern recognition, response protocols, community coordination) is actually retained by generation N+1 if generation N does not actively perform ritual? What is the decay rate?',
    'Longitudinal ethnographic/historical tracking of communities that discontinued ritual practice: measure actual threat-response competence 1, 2, 3 generations later; compare to communities maintaining active ritual. Distinguish between conscious knowledge loss vs degradation in embodied competence.',
    'High decay (>50% competence loss per generation without ritual): ritual''s extraction is justified by survival benefit (strong tangled_rope). Minimal decay: ritual is optional aesthetic/cultural practice (extraction without benefit, snare). The decay rate determines whether the ritual''s suppression requirement is structural (needed to prevent knowledge loss) or imposed (enforced for non-survival reasons).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_knowledge_decay, empirical, 'Rate of threat-recognition knowledge decay across generations without active ritual').

omega_variable(
    reading_vs_hybrid_atrophy_distinction,
    'Is this constraint currently instantiating the survival-competence reading (active threat-recognition transmission) or the hybrid-atrophy reading (once-survival-competent ritual degraded to memorial under modernity)?',
    'Direct ethnographic observation: Are community members explicitly acquiring, rehearsing, and preparing to apply threat-recognition in actual disasters? Or is the ritual functioning primarily as historical commemoration and cultural identity maintenance? Interview participants about the ritual''s stated purpose and their understanding of its transmission role.',
    'If survival-competence reading: extractiveness justified by future-generation benefit (tangled_rope). If hybrid-atrophy reading: ritual is degraded piton or theatrical snare (high theater, low function). The readings are not alternative framings of the same constraint — they represent genuinely different structural states of the same ritual, and the actual structural evidence (what is being transmitted, how it is being used, whether it affects real threat response) determines which reading applies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_hybrid_atrophy_distinction, empirical, 'Whether the constraint instantiates survival-competence or hybrid-atrophy reading').

omega_variable(
    identity_lock_vs_constrained_exit_ambiguity,
    'For ritual participants classified as identity_locked, what proportion of their participation barrier is genuinely cognitive/identity-fusion vs externally coercive (social ostracism, economic penalty, exclusion from kin networks)?',
    'Exit-cost ethnography: interview participants who have partially or fully disengaged from ritual; measure emotional/psychological response vs practical costs; distinguish between ''I cannot imagine myself outside this'' (identity lock) and ''I can imagine it but the social cost is prohibitive'' (constrained).',
    'High identity-lock proportion: suppression mechanism is internalized; the constraint persists even if external barriers are removed (psychological capture, deep enculturation). High external-coercion proportion: barrier is structural isolation/economic dependency; exit becomes possible if external barriers are addressed (deprogramming, economic alternatives). This distinction determines whether suppression remains stable post-exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit_ambiguity, empirical, 'Proportion of identity-lock vs externally-coercive exit barriers for ritual participants').

omega_variable(
    future_generation_optionality,
    'Do beneficiaries (future generations) actually exercise choice in ritual participation, or does the constraint''s extraction automatically transfer to them, replicating the cycle?',
    'Longitudinal study: track cohorts of young people raised within ritual traditions; measure rates of voluntary continuation vs abandonment; measure whether those who abandon suffer threat-recognition knowledge loss or whether formal/alternative training substitutes.',
    'If future generations freely choose participation (high optionality): beneficiary relationship is genuine (they genuinely benefit from the competence transfer). If participation transfers the extraction cycle (low optionality, replication): the constraint is extractive from both present and future generations; the ''beneficiary'' framing masks a perpetual commitment cycle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generation_optionality, empirical, 'Whether future generations exercise genuine choice in ritual participation or inherit the extraction cycle').

omega_variable(
    alternative_transmission_mechanisms,
    'Are there alternative mechanisms for transmitting threat-recognition competence (formal training, simulation, deliberate practice, storytelling without embodied ritual) that achieve comparable or superior outcomes without the extraction cost?',
    'Comparative study: communities using ritual vs non-ritual transmission methods; measure competence retention, response quality in actual threats, participant well-being/autonomy, and intergenerational continuity. Controlled comparison of embodied vs non-embodied instruction outcomes.',
    'If alternatives are comparable or superior: the ritual''s extraction is unjustified; constraint reclassifies from tangled_rope toward snare (high extraction, minimal coordination benefit). If ritual is superior: extraction is justified by survival benefit. If hybrid (ritual superior for some competence dimensions, alternatives sufficient for others): constraint decomposes into separate stories with different ε and type profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_transmission_mechanisms, empirical, 'Whether non-ritual transmission mechanisms can substitute for ritual without knowledge loss').

omega_variable(
    survival_threat_actual_vs_memorial,
    'Is the primary threat that the ritual preservves competence against currently active (flood-prone region, seismic zone, recurring famine), historically distant (genocide, historical catastrophe), or absent (mitigated by technology/infrastructure)?',
    'Hazard assessment: document current threat environment; distinguish between ''we practice this because floods still happen here'' vs ''we practice this to remember the flood that happened 200 years ago'' vs ''we practice this despite having dams and early warning systems.''',
    'Active threat: extraction cost is justified by genuine survival benefit (tangled_rope is appropriate). Historical/distant threat: extraction is partially justified (intergenerational knowledge preservation) but benefit is speculative (future generations may never face the threat). Mitigated threat: ritual''s extraction is theatrical (shifted toward piton/snare) — the original justification no longer applies, but the constraint persists through inertia.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(survival_threat_actual_vs_memorial, empirical, 'Whether the threat the ritual preserves competence against is currently active, historical, or technologically mitigated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmp_surv_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cmp_surv_tr_t25, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(cmp_surv_tr_t50, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(cmp_surv_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cmp_surv_be_t25, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 25, 0.54).
narrative_ontology:measurement(cmp_surv_be_t50, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cmp_surv_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cmp_surv_su_t25, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(cmp_surv_su_t50, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, attachment_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_preservation kernel decomposes into three constraint stories with distinct ε values and structural signatures. The survival-competence reading (this story) models ritual as embodied knowledge transfer with genuine coordination function but high extraction cost. The mourning_practice_reading models the same ritual as symbolic continuity without operational transfer. The hybrid_atrophy_reading models the degradation trajectory from survival-competent to memorial. These are not alternative measurements of the same constraint — they represent structurally distinct claims about what the ritual does and what it preserves. Different communities and traditions hold different readings active. The ε values differ significantly, driven by different assumptions about knowledge transfer mechanisms and actual threat environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
