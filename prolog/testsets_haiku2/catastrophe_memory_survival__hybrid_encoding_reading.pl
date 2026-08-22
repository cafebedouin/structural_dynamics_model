% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Dual-Register Ritual Encoding: Symbolic Boundary + Practical Knowledge
 *   domain: religious/social/cognitive
 *
 * SUMMARY:
 *   This constraint describes ritual as a dual-register system: symbolic
 *   boundary-maintenance (identity, sacred narrative, collective meaning)
 *   operating inseparably from embedded practical knowledge (resource timing,
 *   environmental adaptation, family coordination). The constraint is the
 *   social/cognitive coherence that holds both registers together despite
 *   analytical pressure to separate them. The reading instantiated here
 *   treats the dual encoding as a unified functional whole, neither reducible
 *   to symbol nor to competence alone. The referent for extractiveness is the
 *   standing arrangement (the dual-register ritual structure itself, as
 *   practiced), assessed from this hybrid reading's own lights: low ε because
 *   the community genuinely coordinates both functions through the practice
 *   and neither register extracts from the other—both are constitutive. The
 *   external analyst, by contrast, experiences high pressure to
 *   binary-classify, which is the source of measured suppression and theater:
 *   the analyst must defend the unresolved duality against theoretical
 *   reduction.
 *
 * KEY AGENTS:
 *   - ritual_practicing_communities: beneficiary, identity-locked participants in the dual-register practice
 *   - external_analysts: payer, scholars and credentialing bodies that force binary classification
 *   - knowledge_transmission_bearers: beneficiary and agent, elders maintaining intact pedagogical structure
 *   - younger_generation_learners: beneficiary and payer, acquiring both registers through participation
 *   - credentialing_institutions: excluded, structurally unable to recognize dual-register encoding
 *   - ritual_practice_itself: observer (non-agent), the symbolic-plus-practical coherence the constraint preserves
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.29).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Dual-Register Ritual Encoding: Symbolic Boundary + Practical Knowledge").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious/social/cognitive").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '502b278d-d546-4dc9-881b-7b7ccbe34869').
narrative_ontology:cs_kernel_codification('502b278d-d546-4dc9-881b-7b7ccbe34869', distributed).
narrative_ontology:cs_authority_grounding('502b278d-d546-4dc9-881b-7b7ccbe34869', practice).
narrative_ontology:cs_interpretation_layer_present('502b278d-d546-4dc9-881b-7b7ccbe34869').
narrative_ontology:cs_reading_relation('502b278d-d546-4dc9-881b-7b7ccbe34869', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('502b278d-d546-4dc9-881b-7b7ccbe34869', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_axiom('502b278d-d546-4dc9-881b-7b7ccbe34869', foundational, dual_registers_inseparable_for_survival).
narrative_ontology:cs_axiom_status(dual_registers_inseparable_for_survival, holdable).
narrative_ontology:cs_axiom_grounding('502b278d-d546-4dc9-881b-7b7ccbe34869', dual_registers_inseparable_for_survival, empirically_contingent).
narrative_ontology:cs_axiom('502b278d-d546-4dc9-881b-7b7ccbe34869', secondary, analytical_reduction_destructive).
narrative_ontology:cs_axiom_status(analytical_reduction_destructive, holdable).
narrative_ontology:cs_axiom_grounding('502b278d-d546-4dc9-881b-7b7ccbe34869', analytical_reduction_destructive, instrumental).
narrative_ontology:cs_reference_frame('502b278d-d546-4dc9-881b-7b7ccbe34869', ritual_dual_functionality_baseline).
narrative_ontology:cs_drift_state('502b278d-d546-4dc9-881b-7b7ccbe34869', contemporary_academic_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('502b278d-d546-4dc9-881b-7b7ccbe34869', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practicing_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, knowledge_transmission_bearers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, younger_generation_learners).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, external_analysts).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, younger_generation_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and maintain ritual practices that encode both symbolic identity-boundary work (what makes us 'us') and embedded practical knowledge (timing calendars, resource-management protocols, family coordination systems, environmental adaptation strategies). The practice itself holds both registers inseparably; the community does not theoretically resolve which register the ritual 'really' serves—both functions persist through participation. Exit would fragment both knowledge transmission and collective identity simultaneously.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practicing_communities, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practicing_communities, agenda_setter).

% Scholars, anthropologists, policy-makers, and credentialing bodies who force binary classification: 'Is this ritual primarily symbolic or primarily functional?' The classification pressure (separate the registers, choose one, justify why the other is epiphenomenal) imposes analytical cost on the community to defend the dual encoding without fracturing it. The analyst's framework treats the unresolved duality as a problem to be solved rather than a structural feature of the practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, external_analysts, payer,
    powerful, biographical, mobile, global).

% Elders, ceremonial specialists, and knowledge-keepers who maintain and transmit the ritual across generations. They hold both the symbolic instruction (what the ritual means, the boundary it marks, the sacred narrative it enacts) and the practical instruction (the timing, the resource coordination, the environmental reading embedded in the ritual's structure) as a unified pedagogical transmission. Separation of these registers at the transmission level corrupts both.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, knowledge_transmission_bearers, beneficiary,
    moderate, biographical, identity_locked, local).

% Acquire both registers through participation: the symbolic meaning deepens through repetition and narrative instruction; the practical knowledge (when to plant, how resources cycle, family obligations) becomes embodied through enactment. They depend on the dual-register structure remaining intact; if analysts fragment it into 'real' vs. 'symbolic' components, the transmission pathway breaks and neither register survives intact.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, younger_generation_learners, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, younger_generation_learners, payer).

% Universities, grant-making bodies, and professional disciplines require published taxonomies of knowledge. Ritual knowledge must be categorized as either 'folklore' (symbolic, non-functional, preserved for cultural heritage) or 'traditional ecological knowledge' (functional, instrumentally valuable, legitimated through environmental science). The dual-register encoding sits between these categories and cannot be credentialed without collapsing the duality. Their classification demand is structural pressure exerted on the community's practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, credentialing_institutions, excluded,
    institutional, generational, mobile, global).

% The non-agent entity: the constraint's referent is the dual-register arrangement itself, the symbolic-plus-practical coherence that the constraint preserves. The constraint is the social/cognitive pressure to maintain this unresolved duality even as external analysis pressure demands separation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practice_itself, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practice_itself).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual coordinates symbolic identity-maintenance (boundary-marking, collective meaning, sacred narrative continuity) WITH practical knowledge-transmission (environmental timing, resource cycles, family protocols, adaptation strategies) in a single enactment structure. Participants do not distinguish the registers; both are accomplished in one performed coherence.
% TRANSFER_FUNCTION: Transfers authority from knowledge-keepers to learners: symbolic authority (I understand what we are and why we do this) and practical authority (I can read the season, manage the resource, execute the coordination) flow together through the ritual's enactment. The constraint moves these authorities intact or not at all.
% ABSENT_VOICES: Analysts who would separate the registers are present (external_analysts, credentialing_institutions) and their classification pressure is inside the story. Absent are younger generations who have been trained to read ritual through the scholar's binary lens (symbol vs. function) and who consequently lose access to the dual-register coherence—they are structurally excluded from perceiving what the practice encodes because the analytical framework fragments the transmission.
% DISAPPEARANCE_RATIONALE: If the dual-register constraint vanished and the ritual froze into a single register (pure symbol, or pure practical function), the community would lose either collective identity-continuity (if symbolic register collapses) or practical adaptive capacity (if functional register collapses). The world rearranges through knowledge loss and potential social fragmentation.
% FOUNDING_PROBLEM: A community faces a catastrophe (environmental change, forced migration, persecution, material crisis) that threatens both physical survival and identity-continuity. Ritual emerges as a structure that keeps both problems in view: the symbolic work (naming who we are, why we persist, what is sacred about our persistence) cannot solve resource scarcity or environmental timing, and the practical knowledge (when to plant, how to shelter, resource allocation) cannot solve meaning or collective coherence. Ritual solves both simultaneously by encoding both registers inseparably.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists studying post-disaster communities (Tsing on forest ecologies, Blaser on indigenous ontologies, ethnographers of ritual practice in economically precarious communities) attest that ritual does carry both registers and that separation degrades both. Communities themselves attest the dual functioning through their resistance to analytical binary classification. Academic observers external to the benefiting communities (comparative religionists, cognitive scientists studying ritual) confirm that the unresolved duality is structurally present, though some dispute whether it is intentional or coincidental.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).
:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.38 at interval end) because the dual-register encoding generates genuine coordination benefit for the community: identity maintenance and knowledge transmission are inseparably accomplished. The constraint does not pit one register against the other; both are constitutive. However, extractiveness rises gradually (0.22 to 0.38 over the interval) as analytical pressure intensifies and the community must expend energy defending the unresolved duality. Theater is moderate-rising (0.28 to 0.42) because the community increasingly performs 'both registers matter equally' in response to analyst pressure, and the performance itself becomes strategic self-defense rather than transparent practice. Suppression is moderate (0.29) because the analytical framework does not prevent ritual practice, but it does demand theoretical justification and categorization, which is a form of cognitive/institutional suppression. Accessibility_collapse is moderate-high (0.64) because once the dual-register structure is theoretically attacked, the community cannot simply exit—the practice is identity-constituting—but alternatives (fragmented symbol-only or function-only ritual, or abandonment) are all more costly than defending the unresolved duality. Resistance is moderate (0.51) because the community actively resists binary classification but does not mobilize large-scale institutional counter-pressure; the resistance is embedded in continued practice and scholarly pushback. The measurement series tracks increasing analytical pressure (theater rising, extractiveness rising) that does not yet threaten the practice's survival but creates measurable strain. All metrics are authored on a single shared time grid (every metric at every time point).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (ritual_practicing_communities, knowledge_transmission_bearers, younger_generation_learners) experience the constraint as enabling genuine coordination—both registers are necessary, neither extracts from the other. From their perspective the constraint is a rope: it solves a real coordination problem (how to transmit both identity AND knowledge through a single enactment) with minimal coercive overhead. The external analyst seats (credentialing_institutions, powerful analysts) experience the same constraint as suppressive: the unresolved duality prevents them from credentialing the knowledge, taxonomizing the ritual, or fitting it into institutional categories. From their perspective it is a tangled_rope or snare: they are forced to choose a register to legitimate, and the community's refusal to choose (or their insistence that both registers are equally real) blocks analytical progress. The engine computes these divergences from structural data: beneficiary seats get low d (low extraction), analyst seats get high d (extraction from their analytical authority). This divergence is structural to the kernel contest—it is why the same arrangement instantiates different readings for different parties.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations are minimal and precise. ritual_practicing_communities is declared beneficiary because the dual-register structure is constitutive of their identity and knowledge transmission—they depend on it. external_analysts is NOT declared victim (they are powerful seats with mobile exit and global scope; they can leave the analysis unchanged). Instead, the analytical pressure is modeled as high suppression (0.29) and theater (analysts must defend against binary reduction) because the constraint's very existence (the community's refusal to binary-classify) suppresses the analyst's classificatory authority. Suppression here is not coercion but institutional/epistemic blocking. The directionality derivation chain: ritual_practicing_communities holds identity_locked exit (cannot leave without fracturing identity) + moderate power + generational horizon + is a declared beneficiary → d near 0.2-0.3 (beneficiary end). external_analysts holds powerful power + mobile exit + global scope + is NOT a declared beneficiary → baseline high d, but the specific structural role (they are forcing classification, not bearing extraction) routes through suppression rather than victimhood. The engagement is asymmetric authority, not asymmetric extraction in the victim/payer sense.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophe → need dual registers for survival) is live and unresolved. The constraint persists not through mandatrophy but through active community practice and genuine coordinating function. The founding_problem_status:live + disappearance_verdict:world_rearranges mismatch does NOT fire mandatrophy because the founding problem actually remains live (catastrophe and resource scarcity have not been solved; ritual remains necessary for survival). Mandatrophy would trigger only if the founding problem were dead (status:dead) but the constraint persisted through theater/inertia. This constraint avoids mandatrophy by maintaining genuine function; the rising theater_ratio reflects analytical pressure, not functional decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_register_intentionality,
    'Is the dual-register encoding of ritual an intentional design feature (the community consciously maintains both registers as necessary coordination) or an emergent byproduct of adaptive practice (both registers happen to fit together in ways the community does not theoretically articulate)?',
    'Ethnographic analysis of community''s own meta-discourse about ritual: do knowledge-keepers explicitly teach ''both registers matter equally,'' or do they simply teach the ritual and both registers implicitly transmit? Does the community resist binary classification because it is theoretically committed to hybridity, or because separation practically breaks transmission?',
    'If intentional, the constraint is a designed coordination mechanism and sustains rope classification across all seats. If emergent, the constraint is an accident of effective practice that analysts are now threatening to destroy; the question becomes whether the community can maintain dual-register practice once the duality is made explicit and theorized. This affects whether the community can defend the hybrid reading against analytical pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_register_intentionality, empirical, 'Whether the dual-register structure is consciously maintained or implicitly held.').

omega_variable(
    register_separation_possibility,
    'Could the symbolic register and practical knowledge register be separated, transmitted independently, and both survive intact?',
    'Natural experiments from communities that have attempted to extract practical knowledge into academic/credentialed form while maintaining ritual: does the stripped knowledge function equivalently? Does the ritual survive as pure symbol? Do younger learners who receive academic knowledge + symbolic ritual (instead of unified dual-register transmission) acquire both functions?',
    'If separation is possible, the constraint is a tradition-specific choice to avoid separation, not a structural necessity. External analysts would have legitimacy to demand binary-classification and the community would face genuine choice between maintaining hybrid incoherence and accepting functional decomposition. If separation destroys both functions, the constraint reflects irreducible structural coupling and the hybrid reading''s claim to structural necessity is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(register_separation_possibility, empirical, 'Whether the dual registers are structurally inseparable or only traditionally unified.').

omega_variable(
    analytical_pressure_mechanism,
    'Is the extractiveness/theater rise over the interval driven by increased analytical pressure (scholars, credentialing demand, institutional categorization) or by internal community factors (younger learners losing fluency in the dual-register, elders aging out, resource scarcity increasing)?',
    'Temporal analysis: correlate the rise in theater and extractiveness with documented increases in academic attention to the ritual, credentialing institutions'' classification demand, and policy/heritage-designation frameworks. Control for internal factors (demographic change, resource availability) to isolate analytical pressure''s contribution.',
    'If analytical pressure is the driver, the constraint''s rising strain is entirely remediable by shifting analyst frameworks—the community''s practice could return to lower theater if external demand for binary classification ceased. If internal factors are primary, the strain is endemic and reflects genuine threat to ritual transmission from within. This affects whether the hybrid reading is truly viable or whether it is sustainable only under conditions of low external scrutiny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analytical_pressure_mechanism, empirical, 'What drives increasing theater and extractiveness: external analytical pressure or internal functional degradation.').

omega_variable(
    committer_framing_ambiguity,
    'Is the hybrid_encoding_reading a reading the community explicitly holds, or is it a reading that analysts have reconstructed from community practice? Does the community theorize its own ritual as hybrid-register, or is that theory imposed by external analysts seeking to explain apparent ''irrationality'' (why preserve practical knowledge through symbolic ritual)?',
    'Ethnographic content analysis: direct questions to knowledge-keepers about whether they understand ritual as dual-register, or whether they understand it as fundamentally ritual (symbol/meaning) that happens to embed practical knowledge, or as fundamentally practical knowledge (survival) that happens to be expressed symbolically. The community''s own framing determines which reading it instantiates.',
    'If the community explicitly theorizes hybrid-register, this reading is the community''s own frame and the constraint enforces that frame against external pressure to reduce. If hybrid is analyst-imposed, the community is being forced into this reading by scholars, and the constraint is actually enforcing analyst categories, not community coherence. This affects whether the beneficiary (ritual_practicing_communities) is genuinely benefiting from the constraint or being conscripted into a frame external to their own understanding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_ambiguity, conceptual, 'Whether the hybrid-register reading is the community''s own explicit theory or an analyst-imposed reconstruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_survival__hybrid_encoding_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__hybrid_encoding_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).

% DUAL FORMULATION NOTE:
% Part of the catastrophe_memory_survival constraint family. This reading (hybrid_encoding) claims the ritual operates on inseparable symbolic and functional registers. Sibling readings decompose along different register priorities: competence_transmission_reading reduces symbol to epiphenomenon (function primary), symbol_survival_reading reduces practical knowledge to coincidental (identity primary). The three readings have the same empirical referent (ritual practice) but different structural ε values and different beneficiary/victim configurations. They coexist as live scholarly/community positions; this reading influences both siblings by asserting the incoherence of both reductions, but does not foreclose them (communities can and do adopt single-register framing). All three readings share the founding problem (catastrophe survival); they diverge on what aspect of ritual solves it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
