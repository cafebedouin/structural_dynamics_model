% ============================================================================
% CONSTRAINT STORY: hybrid_pedagogical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_pedagogical_reading, []).

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
 *   constraint_id: hybrid_pedagogical_reading
 *   human_readable: Catastrophe-Memory Preservation Through Mourning-as-Vigilance (Hybrid Pedagogical Reading)
 *   domain: religious_studies/cultural_anthropology/memory_studies
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'catastrophe_memory_transmission'. The hybrid-pedagogical reading holds
 *   that mourning rituals preserve community identity AND encode functional
 *   survival-competence through the emotional work of grief-processing: the
 *   practice of sitting with collective loss teaches both emotional
 *   resilience (enabling future adaptation) and threat-recognition patterns
 *   (enabling early-warning to future vulnerable populations). This reading
 *   couples grief-work and vigilance as a single pedagogical mechanism.
 *   However, this reading is one of three structurally distinct
 *   interpretations of how catastrophe-memory preserves communities. The
 *   sibling readings—mourning_practice_reading (grief's primary function is
 *   emotional/identity coherence; survival knowledge is secondary or
 *   vestigial) and survival_competence_reading (mourning is primarily a
 *   knowledge-transmission system for threat-recognition; emotional work is
 *   instrumentally valuable only insofar as it enables attention to danger
 *   patterns)—occupy the same contested kernel but make different claims
 *   about what the ritual's fundamental purpose is and what extractive costs
 *   it justifies. This constraint story analyzes the hybrid-pedagogical
 *   reading as a coherent, internally consistent interpretation. The sibling
 *   readings are other constraints with their own ε values, perspectives, and
 *   structural data. The kernel itself—what catastrophe-memory transmission
 *   actually does—remains under-theorized in the original domain; the three
 *   readings represent three different committer positions on what the kernel
 *   means.
 *
 * KEY AGENTS:
 *   - Ritual Authority (institutional/arbitrage): Institutional custodians of mourning practice — benefit from control over catastrophe narrative, legitimacy as knowledge-keepers, and ability to modulate ritual content. See constraint as pure coordination of emotional and threat-knowledge transmission.
 *   - Intergenerational Knowledge Bearers (moderate/identity_locked): Members whose identity is constituted through their role as keepers of catastrophe-memory and survival-competence transmission. Structurally mobile but identity-fused with transmission function. Bear disproportionate labor burden while experiencing emotional authenticity suppression.
 *   - Participating Mourners (moderate/constrained): Community members who experience genuine grief-processing and acquire threat-recognition competence through ritual, but face suppression in acceptable grief forms and timing. Benefit from community cohesion and resilience-building but constrained by ritual format.
 *   - Future Vulnerable Populations (powerless/trapped): Inherit mourning rituals without capacity to evaluate whether embedded survival knowledge remains functional or has degraded into pure theater. Bear maximum cost if vigilance-competence has atrophied while ritual performance persists.
 *   - Anthropological Archivists (powerful/mobile): External observers who document rituals increasingly as historical artifacts and symbolic systems rather than functional survival mechanisms. See ritual persistence (maintained through archival preservation and academic study) but experience theater-ratio as high — original threat context has receded.
 *   - Analytical Observer (analytical/analytical): Civilizational/universal perspective that risks naturalizing contingent institutional arrangements as immutable features of human social reproduction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_pedagogical_reading, 0.48).
domain_priors:suppression_score(hybrid_pedagogical_reading, 0.62).
domain_priors:theater_ratio(hybrid_pedagogical_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_pedagogical_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hybrid_pedagogical_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hybrid_pedagogical_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_pedagogical_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_pedagogical_reading, "Catastrophe-Memory Preservation Through Mourning-as-Vigilance (Hybrid Pedagogical Reading)").
narrative_ontology:topic_domain(hybrid_pedagogical_reading, "religious_studies/cultural_anthropology/memory_studies").

domain_priors:requires_active_enforcement(hybrid_pedagogical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(hybrid_pedagogical_reading, distributed).
narrative_ontology:cs_authority_grounding(hybrid_pedagogical_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(hybrid_pedagogical_reading).
narrative_ontology:cs_kernel_id(hybrid_pedagogical_reading, catastrophe_memory_transmission).
narrative_ontology:cs_reading_relation(hybrid_pedagogical_reading, mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation(hybrid_pedagogical_reading, survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom(hybrid_pedagogical_reading, foundational, grief_enables_threat_attention).
narrative_ontology:cs_axiom_status(grief_enables_threat_attention, holdable).
narrative_ontology:cs_axiom_grounding(hybrid_pedagogical_reading, grief_enables_threat_attention, empirically_contingent).
narrative_ontology:cs_axiom(hybrid_pedagogical_reading, foundational, survival_knowledge_is_embodied_in_ritual_form).
narrative_ontology:cs_axiom_status(survival_knowledge_is_embodied_in_ritual_form, holdable).
narrative_ontology:cs_axiom_grounding(hybrid_pedagogical_reading, survival_knowledge_is_embodied_in_ritual_form, empirically_contingent).
narrative_ontology:cs_reference_frame(hybrid_pedagogical_reading, grief_as_adaptive_pedagogical_practice).
narrative_ontology:cs_drift_state(hybrid_pedagogical_reading, contemporary_professionalized_early_warning, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_pedagogical_reading, ritual_authority).
narrative_ontology:constraint_beneficiary(hybrid_pedagogical_reading, intergenerational_knowledge_transmission).
narrative_ontology:constraint_victim(hybrid_pedagogical_reading, future_vulnerable_populations).
narrative_ontology:constraint_victim(hybrid_pedagogical_reading, grief_processing_authenticity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE VULNERABLE POPULATION (SNARE) — Inherits mourning rituals but cannot evaluate whether embedded survival knowledge remains intact or has degraded into pure theater. No capacity to exit the vulnerability created by atrophied warning-competence. Bears maximum cost if vigilance-mechanism has decayed while ritual performance persists.
constraint_indexing:constraint_classification(hybrid_pedagogical_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PARTICIPATING MOURNERS (TANGLED ROPE) — Experience genuine emotional processing and acquire some threat-recognition competence through ritual participation, but also bear suppression: ritual format constrains grief expression (timing, acceptable emotions, narrative boundaries), and much emotional labor goes to ritual reproduction rather than authentic processing. Some genuine coordination function (emotional resilience, community cohesion) alongside significant extraction (emotional work required to fit grief into ritual form).
constraint_indexing:constraint_classification(hybrid_pedagogical_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RITUAL AUTHORITY (ROPE) — Institutional guardians of mourning practice benefit from control over catastrophe narrative and knowledge transmission. Experience the constraint as pure coordination: ritual authority solves the problem of transmitting both emotional coping and threat-recognition across generations. Can modulate ritual content to serve contemporary needs. Net beneficiary during ritual performance periods.
constraint_indexing:constraint_classification(hybrid_pedagogical_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERGENERATIONAL KNOWLEDGE BEARERS (TANGLED ROPE with identity_locked exit) — Members whose identity is constituted through their role as keepers of catastrophe-memory and survival-competence transmission. Structurally mobile (could abandon the knowledge-keeper role) but identity-fused with transmission function. Experience the constraint as both genuine knowledge coordination and asymmetric labor burden: they bear responsibility for accuracy of embedded survival knowledge while others participate less intensively. Identity-lock prevents exit even when transmission burden becomes unsustainable.
constraint_indexing:constraint_classification(hybrid_pedagogical_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 5: ANTHROPOLOGICAL ARCHIVISTS (PITON) — Powerful external observers (researchers, cultural institutions) who document and preserve mourning practices see them increasingly as historical artifacts and symbolic systems rather than functional survival mechanisms. Theater ratio is high: ritual continues and is documented, but the original threat context has receded (either catastrophe is historicized or prevention infrastructure makes the embedded warning system seemingly obsolete). Piton classification reflects that the constraint persists through institutional maintenance (archival preservation, academic study) even as functional vigilance has atrophied.
constraint_indexing:constraint_classification(hybrid_pedagogical_reading, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Universal/civilizational perspective that frames catastrophe-memory preservation as an immutable feature of human social reproduction: all societies must encode survival knowledge and emotional resilience through ritual; the coupling of grief and vigilance is inherent to how collective trauma becomes collective competence. However, the structural data contradicts this mountain classification — ritual authority benefits, future populations' vulnerability is contingent on knowledge preservation choices, and extraction flows are measurable. Engine's false-summit detector will identify this as naturalization.
constraint_indexing:constraint_classification(hybrid_pedagogical_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_pedagogical_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_pedagogical_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_pedagogical_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hybrid_pedagogical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_pedagogical_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_pedagogical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts through asymmetric labor distribution (knowledge-bearers bear disproportionate transmission burden), institutional control over catastrophe-narrative (ritual authority can modulate content), and suppression of grief authenticity (ritual form constrains acceptable emotions and timing). However, extractiveness is not severe because genuine coordination functions exist: the ritual does provide emotional resilience and some threat-recognition competence to participants. The trajectory from 0.32 → 0.48 over 50 time-units reflects increasing extraction as the original catastrophe context recedes and ritual authority increasingly manages narrative rather than transmitting functional knowledge. Suppression (0.62): Moderate-high. Significant suppression mechanisms include: ritual format constraints on grief expression (duration, acceptable emotions, narrative boundaries), institutional gatekeeping of catastrophe-narrative, knowledge-bearer identity-lock preventing exit from transmission role, and cultural prohibition against questioning ritual utility. Suppression is not total because participants retain some agency in how they process grief and adapt threat-recognition to contemporary context. Theater ratio (0.58): Moderate. Initial theater (0.42) reflects genuine dual function: emotional processing is authentic and threat-recognition competence is being actively practiced. Drift toward higher theater (0.58) reflects increasing performance of ritual continuity as original threat context becomes historical; the ritual persists but increasing portion is maintenance-as-practice rather than functional threat-monitoring.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps reveal why this hybrid-pedagogical reading is contested. Ritual authority sees pure Rope (coordination of emotional resilience and knowledge transmission with minimal extraction). Participating mourners experience Tangled Rope (genuine emotional work and competence-building alongside suppression of authentic grief and labor asymmetry). Knowledge-bearers with identity-lock experience Tangled Rope with trapped exit (same mixed experience as mourners but unable to exit the knowledge-keeper role). Future vulnerable populations perceive Snare (inherit ritual without capacity to evaluate whether warning-system functions; bear cost of false vigilance if competence has degraded). Anthropological archivists perceive Piton (ritual persists through institutional maintenance, but functional vigilance has atrophied as threat context became historical). The analytical observer risks Mountain (naturalizes grief-vigilance coupling as immutable feature of human social reproduction). The gaps show that whether this reading is empirically adequate depends on whether mourning rituals actually preserve functional threat-recognition competence or primarily provide emotional validation with vestigial knowledge content.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position relative to extraction flow. Ritual authority (beneficiary + arbitrage exit): low d → low/negative χ. They control narrative and can exit the constraint at will (shift to professional early-warning systems or abandon catastrophe-narrative entirely). Intergenerational knowledge-bearers (victim + identity_locked exit): high d → high f(d) → high χ. They cannot exit because their identity is constituted through transmission role; they bear asymmetric labor while ritual authority benefits from their knowledge-keeping. Participating mourners (mixed + constrained exit): moderate d → moderate χ. They benefit from emotional resilience and competence-building but bear suppression cost and cannot easily exit community participation without social penalty. Future vulnerable populations (victim + trapped exit): very high d → highest f(d) → maximum χ. They have no voice in whether rituals continue and bear catastrophic cost if vigilance-competence has degraded. The directionality chain produces the observed perspectival gap: beneficiaries (authority, some mourners) perceive Rope; constrained agents perceive Tangled Rope; identity-locked agents perceive high extraction within Tangled Rope; trapped future populations perceive Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through reading-specificity: the hybrid-pedagogical reading avoids false dichotomy between 'pure emotional coordination' and 'pure knowledge extraction' by holding that the mechanisms are structurally coupled. Grief-work enables threat-attention; threat-attention frames grief-resilience as adaptive. The constraint is genuinely Tangled Rope because both functions exist and create real beneficiaries (ritual authority, emotional resilience for participants) alongside real extraction (asymmetric transmission burden, narrative control, suppression of authentic grief). However, mandatrophy persists at a deeper level: if the sibling readings turn out to be empirically adequate (if mourning in tradition A is primarily emotional-coordination with vestigial knowledge, while mourning in tradition B is primarily knowledge-transmission with incidental emotion-work), then the hybrid-pedagogical reading is an over-general categorization that should decompose into separate stories per tradition. The true mandatrophy resolution requires cross-traditional empirical work to establish whether the coupling is universal or tradition-specific.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    embedded_survival_knowledge_retention,
    'Does the mourning ritual actually preserve functional survival-competence across generations, or has it become primarily an emotional-processing and identity-affirming practice with vestigial threat-recognition content?',
    'Comparative analysis: test knowledge-bearer competence in threat-recognition/adaptive response against non-ritual-educated populations; measure recall accuracy of embedded survival knowledge by ritual-generation participants; assess whether threat-recognition content is actively invoked in contemporary crisis response or remains dormant/symbolic',
    'If survival knowledge is retained and functional: constraint is genuine Tangled Rope (mixed coordination and extraction). If knowledge is degraded or vestigial: constraint is Piton (ritualized maintenance of knowledge that no longer functions as warning-system). If knowledge is completely absent but ritual persists: constraint becomes pure Snare (extraction via false promise of preparedness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embedded_survival_knowledge_retention, empirical, 'Whether mourning rituals actually preserve functional survival-competence or primarily emotional processing').

omega_variable(
    vigilance_authenticity_versus_performance,
    'Is the ''vigilance'' encoded in mourning-ritual authentic threat-monitoring competence, or performative enactment of awareness that provides psychological reassurance without actual early-warning capacity?',
    'Outcome tracking: compare communities that maintain vigilance-practice against those that have institutionalized professional early-warning systems; measure false-positive vs false-negative rates in community threat-detection; assess whether ritual mourning generates actionable intelligence or primarily affirms group identity',
    'If authentic vigilance: extraction is justified as coordination cost of maintaining high-alert competence. If performative: constraint becomes more extractive — community bears suppression cost (grief ritual labor) without functional benefit, and ritual authority benefits from false promise of preparedness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vigilance_authenticity_versus_performance, empirical, 'Whether vigilance is functional threat-monitoring or performative awareness').

omega_variable(
    reading_specificity_hybrid_pedagogical,
    'Is this reading coherent across all major religious/cultural traditions that employ mourning-as-pedagogy, or are the mechanisms of grief-to-competence translation tradition-specific?',
    'Cross-cultural comparison of how grief-work encodes survival knowledge in Jewish mourning practices, Islamic funerary traditions, Andean ayni systems, Indigenous Australian Law transmission, Christian martyrdom narratives, etc.; identification of universal cognitive/emotional mechanisms vs tradition-specific epistemic architecture',
    'If universal: this reading applies broadly, and ε values can be calibrated across traditions with high confidence. If tradition-specific: each tradition instantiates different ε (some may be closer to pure Rope, others closer to Snare), and separate constraint stories should be generated for major traditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specificity_hybrid_pedagogical, conceptual, 'Whether hybrid-pedagogical mechanism is universal or tradition-specific').

omega_variable(
    kernel_reading_ambiguity,
    'Does this reading (hybrid-pedagogical: grief encodes both emotional resilience AND threat-recognition) represent a distinct kernel interpretation, or is it an attempt to unify two separate kernel readings (mourning-as-emotion-work vs mourning-as-survival-knowledge-transmission)?',
    'Examine whether the two mechanisms (grief-processing and threat-recognition) are structurally coupled (affect-regulation enables attention to threat patterns) or merely historically co-instantiated; test whether communities can maintain one mechanism without the other; assess whether ritual authority treats them as unified or separable',
    'If truly hybrid: this constraint story correctly instantiates the reading. If separable: the constraint should decompose into two stories (mourning_practice_reading and survival_competence_reading are indeed separate kernel readings), and the hybrid-pedagogical reading may be a false fusion. The kernel itself may be under-theorized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether grief-processing and threat-recognition are structurally coupled or separable mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_pedagogical_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hyped_tr_t0, hybrid_pedagogical_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hyped_tr_t25, hybrid_pedagogical_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(hyped_tr_t50, hybrid_pedagogical_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(hyped_be_t0, hybrid_pedagogical_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hyped_be_t25, hybrid_pedagogical_reading, base_extractiveness, 25, 0.41).
narrative_ontology:measurement(hyped_be_t50, hybrid_pedagogical_reading, base_extractiveness, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_pedagogical_reading, attachment_coordination).
narrative_ontology:affects_constraint(hybrid_pedagogical_reading, mourning_practice_reading).
narrative_ontology:affects_constraint(hybrid_pedagogical_reading, survival_competence_reading).

% DUAL FORMULATION NOTE:
% The three readings (hybrid-pedagogical, mourning-practice, survival-competence) are distinct constraint stories instantiating different interpretations of the same kernel. They are linked as siblings in catastrophe_memory_transmission, not as stages of a single constraint. Each has its own ε (expected to range 0.35–0.55), its own perspectives, and its own structural data. The hybrid-pedagogical reading claims structural coupling of grief-work and threat-recognition; the sibling readings claim one mechanism is primary and the other secondary. These are genuinely different constraints with different extractiveness profiles, not observables of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_pedagogical_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
