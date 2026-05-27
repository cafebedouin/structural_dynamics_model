% ============================================================================
% CONSTRAINT STORY: mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mourning_practice_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mourning_practice_reading
 *   human_readable: Catastrophe Memory Mourning Practice Reading: Ritual Identity Preservation through Grief Containment
 *   domain: religious_studies/cultural_anthropology/memory_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the MOURNING_PRACTICE_READING of the
 *   catastrophe_memory_transmission kernel. The mourning practice reading
 *   premises survival on ritual containment: the community survives
 *   catastrophe's memory by establishing boundaries between mourning-days
 *   (when collective grief is processed, ancestors are invoked, trauma is
 *   witnessed) and living-days (when ordinary life resumes, future
 *   orientation is possible, the community regenerates). This reading teaches
 *   that the BOUNDARY ITSELF is the survival mechanism — without it, grief
 *   would be consuming; with it, grief becomes manageable, transmissible, and
 *   structurally connected to identity. The constraint exhibits Tangled Rope
 *   structure because it simultaneously provides genuine coordination
 *   function (maintains community continuity, transmits trauma memory,
 *   creates collective identity) and extracts significant cost (emotional
 *   labor of annual grief-processing, identity-locked participation,
 *   restricted autonomy, psychological burden of inheriting unresolved
 *   trauma). The extractiveness value (0.58) reflects that the emotional
 *   labor is substantial and concentrated on present-day mourners while the
 *   benefits (cultural preservation, intergenerational continuity,
 *   institutional authority for ritual keepers) are asymmetrically
 *   distributed. The reading is NOT a claim about whether mourning practices
 *   are good or bad — it is a structural claim about what the constraint is
 *   and how it operates. Sibling readings (survival_competence_reading,
 *   hybrid_pedagogical_reading) offer alternative premises about what
 *   mechanisms enable post-catastrophe survival, but this reading focuses
 *   specifically on how ritual boundary-maintenance and
 *   grief-compartmentalization preserve collective identity through inherited
 *   trauma.
 *
 * KEY AGENTS:
 *   - Present-Day Mourning Community: Primary victim (powerless/identity_locked) — bears emotional labor of annual grief-processing; identity constituted through participation; cannot exit without severing connection to ancestors and community
 *   - Ritual Authority Structures: Primary beneficiary (institutional/arbitrage) — elders, clergy, memory keepers whose authority and legitimacy depend on stewardship of proper mourning practice; experience constraint as pure coordination
 *   - Younger Generation: Secondary victim/partial beneficiary (moderate/constrained) — face genuine coordination problem (maintain community continuity) alongside real extraction (emotional labor, restricted autonomy, identity commitments); can exit at high cost
 *   - Diaspora Coordination Networks: Organized victim/beneficiary (organized/constrained) — coordinate mourning practice across geography; bear costs of maintaining ritual form across diaspora while benefiting from cohesion it produces
 *   - Trauma-Informed Transformation Movement: Organized alternative-seeker (organized/mobile) — see mourning practice as temporary problem with therapeutic alternatives; visible exit path through institutional change
 *   - Analytical Observer: Sees immutable cultural requirement (analytical/analytical) — risks naturalizing contingent institutional practice as inherent to cultural survival
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mourning_practice_reading, 0.58).
domain_priors:suppression_score(mourning_practice_reading, 0.68).
domain_priors:theater_ratio(mourning_practice_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mourning_practice_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(mourning_practice_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(mourning_practice_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mourning_practice_reading, tangled_rope).
narrative_ontology:human_readable(mourning_practice_reading, "Catastrophe Memory Mourning Practice Reading: Ritual Identity Preservation through Grief Containment").
narrative_ontology:topic_domain(mourning_practice_reading, "religious_studies/cultural_anthropology/memory_studies").

domain_priors:requires_active_enforcement(mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mourning_practice_reading, '0dc05eac-657a-4ec9-9dd6-32328bf34ae0').
narrative_ontology:cs_created_at('0dc05eac-657a-4ec9-9dd6-32328bf34ae0', '').
narrative_ontology:cs_kernel_codification('0dc05eac-657a-4ec9-9dd6-32328bf34ae0', implicit).
narrative_ontology:cs_authority_grounding('0dc05eac-657a-4ec9-9dd6-32328bf34ae0', lineage).
narrative_ontology:cs_interpretation_layer_present('0dc05eac-657a-4ec9-9dd6-32328bf34ae0').
narrative_ontology:cs_kernel_id(mourning_practice_reading, catastrophe_memory_transmission).
narrative_ontology:cs_reading_relation('0dc05eac-657a-4ec9-9dd6-32328bf34ae0', catastrophe_memory_survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('0dc05eac-657a-4ec9-9dd6-32328bf34ae0', catastrophe_memory_hybrid_pedagogical_reading, influences).
narrative_ontology:cs_axiom('0dc05eac-657a-4ec9-9dd6-32328bf34ae0', foundational, bounded_ritual_containment_enables_survival).
narrative_ontology:cs_axiom_status(bounded_ritual_containment_enables_survival, holdable).
narrative_ontology:cs_axiom_grounding('0dc05eac-657a-4ec9-9dd6-32328bf34ae0', bounded_ritual_containment_enables_survival, empirically_contingent).
narrative_ontology:cs_axiom('0dc05eac-657a-4ec9-9dd6-32328bf34ae0', foundational, identity_continuity_through_inherited_mourning).
narrative_ontology:cs_axiom_status(identity_continuity_through_inherited_mourning, holdable).
narrative_ontology:cs_axiom_grounding('0dc05eac-657a-4ec9-9dd6-32328bf34ae0', identity_continuity_through_inherited_mourning, deontological).
narrative_ontology:cs_reference_frame('0dc05eac-657a-4ec9-9dd6-32328bf34ae0', ritual_boundary_maintenance_paradigm).
narrative_ontology:cs_drift_state('0dc05eac-657a-4ec9-9dd6-32328bf34ae0', contemporary_therapeutic_culture_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mourning_practice_reading, ritual_authority_structures).
narrative_ontology:constraint_beneficiary(mourning_practice_reading, intergenerational_continuity_maintenance).
narrative_ontology:constraint_victim(mourning_practice_reading, present_day_mourning_community).
narrative_ontology:constraint_victim(mourning_practice_reading, emotional_labor_bearers).
narrative_ontology:constraint_victim(mourning_practice_reading, trauma_integration_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOURNER BOUND BY INHERITED TRAUMA (SNARE) — Identity constituted through participation in catastrophe memory. Cannot imagine themselves outside the mourning cycle without severing connection to community and ancestors. Structurally mobile (could skip the ritual) but identity-fused with the practice — exit would require abandoning not just the ritual but the identity it transmits. Bears maximum emotional labor without compensation or relief mechanism.
constraint_indexing:constraint_classification(mourning_practice_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: YOUNGER GENERATION NEGOTIATING TRANSMISSION (TANGLED ROPE) — Faces genuine coordination problem (maintain community continuity and trauma memory) alongside real extraction (expected to bear emotional labor, restricted autonomy on memorial dates, identity commitments limiting other choices). Can exit at high cost (cultural alienation, family rupture) but not without cost. Experiences both the benefit of collective identity and the extraction of annual grief-processing obligation.
constraint_indexing:constraint_classification(mourning_practice_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RITUAL AUTHORITY STRUCTURE (ROPE) — Institutional bodies (elders, clergy, memory keepers) benefit from the constraint: their authority, legitimacy, and social role depend on being the stewards of proper mourning practice. Experience the constraint as pure coordination — defining the ritual boundaries, teaching the forms, maintaining the container. Extraction runs toward them through prestige and institutional continuity. Can exit (transfer authority) but at the cost of losing institutional role; this is arbitrage-level exit, not mobility.
constraint_indexing:constraint_classification(mourning_practice_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: DIASPORA COMMUNITY AT DISTANCE (TANGLED ROPE) — Organized groups (diaspora networks, diaspora religious institutions, memory preservation organizations) face coordination challenge across geography and generations — how to maintain ritual form when dispersed. Also face extraction: emotional labor of maintaining collective identity, resource costs of gathering, pressure to transmit against assimilative forces. High coordination function (diaspora cohesion depends on shared memory practice) alongside high extraction (costs borne by those maintaining the practice while beneficiaries include those who benefit from diaspora cohesion without participating).
constraint_indexing:constraint_classification(mourning_practice_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRAUMA-INFORMED TRANSFORMATION (SCAFFOLD) — Organized movement (trauma therapists, historical redress commissions, therapeutic culture advocates) sees the traditional mourning practice as a temporary problem with a sunset: evolving toward trauma-informed therapeutic practice that processes grief more continuously rather than through bounded annual ritual. Sees the ritual as providing coordination function but with high emotional cost that newer methodologies might reduce. Exit path is visible and achievable through institutional change; extraction is temporary because the practice is being supplanted by alternative mechanisms.
constraint_indexing:constraint_classification(mourning_practice_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — MEMORY AS NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, traumatic memory must be preserved and transmitted to prevent intergenerational rupture and cultural annihilation. The act of mourning is constitutive of survivor identity and community continuity — inseparable from what it means to persist after catastrophe. This perspective risks naturalizing a contingent institutional practice (bounded annual ritual) as an immutable requirement of cultural survival. The engine's false summit detector may identify this as a false mountain if beneficiary structures are present.
constraint_indexing:constraint_classification(mourning_practice_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mourning_practice_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mourning_practice_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mourning_practice_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mourning_practice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The measurement trajectory shows extractiveness increasing from 0.42 at the constraint's emergence (when ritual was flexible, adaptive to local circumstances) to 0.58 in the contemporary period (when ritual has become more formalized, obligatory, and disconnected from actual trauma integration). This rise reflects institutional ossification: the rhythm that once emerged organically from grief cycles is now enforced as schedule. Present-day mourners inherit not just the trauma but also the specific ritualized form, with less agency to adapt it. Suppression (0.68): High. Multiple barriers prevent exit: legal status of cultural/religious practice (social/legal barriers), identity fusion making exit feel like death (psychological barriers), family and community consequences of non-participation (social barriers), lack of alternative frameworks for trauma integration (institutional barriers). For identity_locked agents, the suppression persists internally — even if legal barriers were removed, the identity frame makes exit unthinkable. Theater ratio (0.52): Moderate-high and rising. Early ritual practice (time 0) was more functionally integrated with grief processing — people mourned because they needed to. Contemporary practice shows increased theater: prescribed forms (how to mourn correctly), temporal markers (when to mourn), prescribed expressions (what to say, how to move). The rise from 0.38 to 0.52 reflects professionalization of the ritual — specialist knowledge required, performative elements increasing, functional integration with actual emotion-processing declining. Yet theater remains below 0.70 (piton threshold) because the ritual continues to serve genuine emotional and community functions, not purely performative ones. Claimed type (Tangled Rope): Required because the constraint has both coordination function (maintains intergenerational memory, enables collective identity, transmits trauma knowledge) AND asymmetric extraction (emotional labor concentrated on mourners, benefits concentrated on authority structures and intergenerational continuity projects). The beneficiary set (ritual authority structures, intergenerational continuity) and victim set (mourning community, emotional labor bearers) are structurally distinct, requiring active enforcement of participation norms.
 *
 * PERSPECTIVAL GAP:
 *   The mourning practice reading generates six distinct classifications from the same structural constraint. The mourner bound by identity-lock sees a Snare (pure extraction, no exit option imaginable) because their identity is constituted through the practice. The younger generation sees Tangled Rope (genuine coordination problem but also real extraction) because they have constrained options and partially benefit. The ritual authority structures see Rope (pure coordination, no extraction from their position) because they experience the constraint as the legitimate exercise of their institutional role. The diaspora networks see Tangled Rope (organizing around the practice while bearing its costs) from an organized perspective. The trauma-informed transformation movement sees Scaffold (temporary problem with visible sunset through institutional change) because they have agency and see alternative pathways. The analytical observer at civilizational scale risks seeing Mountain (immutable requirement of cultural survival), which the engine's false summit detector will flag as a naturalization of a contingent institutional practice. The perspectival gap reveals that the mourning practice reading is NOT a description of objective reality but a specific institutional reading with specific beneficiaries and victims — which is precisely the point of the kernel decomposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is derived from power level, exit options, and structural position relative to the extraction flow. Identity_locked mourners with powerless status and no real exit option (even though structurally mobile, they cannot imagine exit) produce high d → high f(d) → high experienced extractiveness. Constrained agents with moderate power get moderate d depending on whether they are net beneficiaries or net targets. Institutional arbitrage actors (ritual authorities) get low d because they are net beneficiaries with exit options (they can transfer authority). Organized agents vary by whether their organizational power translates to exit capacity (diaspora networks are constrained by coordination requirements; trauma reformers are mobile toward alternatives). The analytical observer derives d from standard canonical value (0.73 for analytical power), producing the mountain classification that the false summit detector interrogates. The ascending extractiveness trajectory (0.42→0.58) suggests that d is increasing over time — the constraint is becoming more asymmetric as ritual becomes more formalized, benefiting authority structures more clearly while extracting more from mourners.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing coordination function from extraction mechanism. The mourning practice reading ACKNOWLEDGES both: ritual genuinely coordinates intergenerational memory transmission (Tangled Rope gate requirement: beneficiaries present, victims present, enforcement required). This prevents misclassification as pure extraction (Snare) by showing the coordination is real. But it also prevents misclassification as pure coordination (Rope) by showing the extraction is real and asymmetrically distributed. The false summit detection (mountain classification from analytical observer) is particularly important here because cultural survival narratives are among the most common cover stories for extractive practices. By declaring beneficiaries (ritual authority structures, intergenerational continuity maintenance) on what could otherwise be read as a natural law (we must transmit catastrophe memory or our culture dies), the story forces the engine to interrogate whether the constraint is truly immutable or whether specific institutional beneficiaries have incentives to present it as such.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_core_premise,
    'Is the mourning practice reading''s core premise that ritual containment (bounded mourning days vs. living days) is the SURVIVAL mechanism for both individuals and communities? Or is it that the ritual primarily EXTRACTS emotional labor while claiming survival function?',
    'Longitudinal study comparing communities with bounded ritual structures vs. communities with continuous trauma integration modalities: measure intergenerational transmission stability, individual psychological outcomes, community cohesion over 2+ generations',
    'If survival mechanism: the extraction (emotional labor) is legitimate coordination cost, and extractiveness drops to 0.35–0.45 (Rope or weak Tangled Rope). If primarily extraction: the survival framing is cover story, extractiveness remains 0.58–0.70 (Snare or strong Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_core_premise, empirical, 'Whether bounded mourning ritual is survival mechanism or extraction mechanism masked as such').

omega_variable(
    identity_lock_mechanism,
    'What portion of the experienced extraction for identity_locked mourners is structural (legal/social barriers to exit) vs. internalized (identity fusion that persists even if barriers were removed)?',
    'Post-diasporic assimilation analysis: study individuals who exited mourning practice; measure whether perceived extraction ''travels'' with them after exit (internalized suppression) or decreases (structural suppression). Track identity coherence and trauma integration in exited vs. continuing populations.',
    'If structural dominates: removing legal/social barriers could reduce experienced extraction. If internalized dominates: extraction persists because identity frame makes exit unthinkable regardless of barriers; requires identity transformation, not institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Degree to which suppression is structural vs. internalized cognitive capture').

omega_variable(
    sibling_reading_foreclosure,
    'Does this reading''s core premise (survival through bounded grief-containment and boundary maintenance) logically foreclose the survival_competence_reading (which premises survival on everyday adaptive practices and trauma-informed healing), or do they coexist as different parties'' legitimate frameworks?',
    'Textual and community analysis of actual practitioners: can a single individual or community hold both readings simultaneously? Are they held by different factions with zero overlap? Does adoption of one reading require rejection of the other?',
    'If forecloses: only one reading survives in any coherent framework; the other is eliminated. If coexists: both readings are live options held by different communities or traditions; the kernel admits multiple valid readings simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether mourning practice reading forecloses or coexists with survival competence reading').

omega_variable(
    boundary_maintenance_cost,
    'What is the true cost (psychological, social, economic) of maintaining the ritual boundary between mourning-days and living-days? Is this cost compensated or extracted?',
    'Cost-benefit analysis: measure time, emotional labor, economic resources required to maintain boundary; identify who bears costs vs. who benefits from the boundary''s maintenance; compare against alternative integration modalities',
    'If costs are distributed and compensated: extractiveness drops (Rope or weak Tangled Rope). If costs concentrate on mourners while benefits accrue to authority structures: extractiveness remains high (Tangled Rope or Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_maintenance_cost, empirical, 'Cost-benefit structure of ritual boundary maintenance').

omega_variable(
    cultural_preservation_necessity,
    'Is the mourning practice necessary for cultural preservation in this specific cultural context, or is it one possible mechanism among alternatives?',
    'Comparative study of catastrophe-survivor communities: identify which cultural preservation mechanisms are effective (language transmission, historical narrative, ritual practice, institutional continuity, artistic expression) and whether mourning ritual is essential or supplementary',
    'If essential and irreplaceable: extractiveness is cost of cultural survival (0.40–0.55, Tangled Rope). If supplementary or replaceable: the constraint is more purely extractive (0.55–0.75, Snare or strong Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_preservation_necessity, empirical, 'Whether mourning ritual is necessary or replaceable for cultural preservation').

omega_variable(
    intergenerational_autonomy_gap,
    'At what generational distance does inherited trauma''s binding force on mourning participation weaken? Is there a generational threshold after which the identity-lock mechanism breaks?',
    'Multi-generational longitudinal study: track participation, identity fusion, felt obligation, and experienced extraction across 4+ generations post-catastrophe. Identify point of inflection where ritual becomes optional rather than constitutive.',
    'If threshold < 2 generations: identity lock is strong and persistent; extractiveness remains high. If threshold 2–4 generations: extraction weakens with distance; younger cohorts may reclassify from Snare to Tangled Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_autonomy_gap, empirical, 'Generational persistence of identity-lock in mourning obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mourning_practice_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mour_tr_t0, mourning_practice_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mour_tr_t1, mourning_practice_reading, theater_ratio, 1, 0.44).
narrative_ontology:measurement(mour_tr_t2, mourning_practice_reading, theater_ratio, 2, 0.5).
narrative_ontology:measurement(mour_tr_t3, mourning_practice_reading, theater_ratio, 3, 0.52).

% Extraction over time
narrative_ontology:measurement(mour_be_t0, mourning_practice_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mour_be_t1, mourning_practice_reading, base_extractiveness, 1, 0.48).
narrative_ontology:measurement(mour_be_t2, mourning_practice_reading, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(mour_be_t3, mourning_practice_reading, base_extractiveness, 3, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mourning_practice_reading, attachment_coordination).
narrative_ontology:affects_constraint(mourning_practice_reading, catastrophe_memory_survival_competence_reading).
narrative_ontology:affects_constraint(mourning_practice_reading, catastrophe_memory_hybrid_pedagogical_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_transmission kernel decomposes into three constraint stories, each instantiating a different reading of how post-catastrophe communities transmit memory and preserve identity. The mourning_practice_reading (this story) focuses on ritual containment as survival mechanism (ε≈0.58, Tangled Rope). The survival_competence_reading focuses on adaptive everyday practices and continuous trauma integration (expected ε≈0.30–0.40, Rope or Scaffold). The hybrid_pedagogical_reading focuses on explicit historical narrative transmission combined with flexible rituals (expected ε≈0.45–0.55, Tangled Rope). Each reading has its own beneficiary/victim structure, extractiveness trajectory, and measurement profile. They are linked via network edges to show that they are alternative framings of the same kernel — sibling readings, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mourning_practice_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
