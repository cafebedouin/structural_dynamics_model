% ============================================================================
% CONSTRAINT STORY: adaptive_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adaptive_competence_reading, []).

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
 *   constraint_id: adaptive_competence_reading
 *   human_readable: Ritual as Adaptive Competence Transmission Through Catastrophe Rehearsal
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint models ritual practice as a structurally dual mechanism:
 *   authentic catastrophe-rehearsal vehicle for transmitting survival
 *   competence AND a gatekeeping structure that concentrates competence
 *   access. The adaptive-competence reading emphasizes the first function
 *   (ritual encodes survival knowledge through embodied performance,
 *   mourning-practice, and threat-recognition training) while acknowledging
 *   asymmetric benefits. The constraint is extracted primarily from
 *   communities that maintain ritual without accessing the interpretive frame
 *   that converts rehearsal into operational knowledge, and benefits those
 *   (religious institutions, knowledge-keeper elites, organized communities)
 *   that can decode and extract competence. Over the interval, secularization
 *   and institutional absorption increase theater ratio (ritual form persists
 *   in museums/archives as performance while living transmission attenuates)
 *   and suppression (displacement, assimilation pressure, institutional
 *   suppression of traditional practice creates barriers to competence
 *   maintenance). The reading is one of three sibling readings of a contested
 *   kernel: catastrophe-memory-transmission can be read as
 *   adaptive-competence transmission (this reading), symbolic-continuity
 *   preservation (sibling), or hybrid-resilience coordination (sibling). Each
 *   reading has different beneficiary/victim structures and claims different
 *   primary functions.
 *
 * KEY AGENTS:
 *   - Ritual Maintainers Without Competence Access (powerless/trapped) — Bear repetitive labor of ritual maintenance but lack interpretive frame converting rehearsal to operational knowledge; victims of the constraint's asymmetry
 *   - Displaced Practitioners (moderate/constrained) — Communities experiencing cultural disruption; ritual practice severed by diaspora or institutional suppression; face both emotional weight and competence transmission loss
 *   - Religious Institutions / Knowledge-Keeper Elites (institutional/arbitrage) — Priesthoods, elder councils, oral tradition custodians that control the interpretive layer; net beneficiaries by gatekeeping competence extraction and accumulating cultural authority
 *   - Community Organizing Networks (organized/constrained) — Mutual aid collectives, disaster-preparedness groups decoupling competence transmission from formal ritual; benefit from extracted lessons but constrained by resistance and institutional suppression
 *   - Secularized Institutional Memory (institutional/arbitrage) — Universities, archives, public health agencies absorbing ritual content into secular frames; benefit from appropriation of competence knowledge; maintain atrophied ritual forms theatrically
 *   - Analytical Observer (analytical/analytical) — Civilizational perspective risking naturalization of contingent institutional arrangements as immutable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adaptive_competence_reading, 0.38).
domain_priors:suppression_score(adaptive_competence_reading, 0.42).
domain_priors:theater_ratio(adaptive_competence_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adaptive_competence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(adaptive_competence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(adaptive_competence_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adaptive_competence_reading, tangled_rope).
narrative_ontology:human_readable(adaptive_competence_reading, "Ritual as Adaptive Competence Transmission Through Catastrophe Rehearsal").
narrative_ontology:topic_domain(adaptive_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(adaptive_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(adaptive_competence_reading, '78cce421-5c7f-452b-b749-e090a1f0a87f').
narrative_ontology:cs_kernel_codification('78cce421-5c7f-452b-b749-e090a1f0a87f', distributed).
narrative_ontology:cs_authority_grounding('78cce421-5c7f-452b-b749-e090a1f0a87f', practice).
narrative_ontology:cs_interpretation_layer_present('78cce421-5c7f-452b-b749-e090a1f0a87f').
narrative_ontology:cs_reading_relation('78cce421-5c7f-452b-b749-e090a1f0a87f', adaptive_competence_reading__symbolic_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('78cce421-5c7f-452b-b749-e090a1f0a87f', adaptive_competence_reading__hybrid_resilience_reading, coexists_with).
narrative_ontology:cs_axiom('78cce421-5c7f-452b-b749-e090a1f0a87f', foundational, competence_transmission_primary_function).
narrative_ontology:cs_axiom_status(competence_transmission_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('78cce421-5c7f-452b-b749-e090a1f0a87f', competence_transmission_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('78cce421-5c7f-452b-b749-e090a1f0a87f', foundational, institutional_gatekeeping_extractive).
narrative_ontology:cs_axiom_status(institutional_gatekeeping_extractive, holdable).
narrative_ontology:cs_axiom_grounding('78cce421-5c7f-452b-b749-e090a1f0a87f', institutional_gatekeeping_extractive, empirically_contingent).
narrative_ontology:cs_reference_frame('78cce421-5c7f-452b-b749-e090a1f0a87f', pre_institutional_ritual_competence).
narrative_ontology:cs_drift_state('78cce421-5c7f-452b-b749-e090a1f0a87f', contemporary_secularization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('78cce421-5c7f-452b-b749-e090a1f0a87f', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(adaptive_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adaptive_competence_reading, communities_that_extract_survival_lessons).
narrative_ontology:constraint_victim(adaptive_competence_reading, ritual_maintainers_without_competence_conversion).
narrative_ontology:constraint_victim(adaptive_competence_reading, displaced_practitioners_losing_transmission_channels).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RITUAL MAINTAINER WITHOUT COMPETENCE CONVERSION (SNARE) — Obligated to perform catastrophe rehearsal (mourning rituals, threat-recognition ceremonies, survival drills embedded in religious practice) but lacks access to the interpretive frame that converts ritual content into operational knowledge. Bears the repetitive labor cost of ritual maintenance; extraction concentrated here because the competence transmission mechanism bypasses them. Trapped by community obligation and cultural identity; no legitimate exit from participation without losing standing.
constraint_indexing:constraint_classification(adaptive_competence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DISPLACED PRACTITIONER (TANGLED ROPE) — Communities experiencing geographical displacement, cultural disruption, or institutional suppression of ritual practice. Rituals functioned as dual vehicles: mourning-practice AND competence transmission. When displacement severs the ritual chain (diaspora, forced assimilation, institutional prohibition), the transmission of survival lessons collapses while the emotional weight of displacement increases. Constrained exit: maintaining ritual in exile carries material costs (time, resources, social marginalizing) and provides diminished competence transmission. Mixed: the ritual's emotional/symbolic function persists even when its competence function degrades.
constraint_indexing:constraint_classification(adaptive_competence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS INSTITUTION / KNOWLEDGE-KEEPER ELITE (ROPE) — Priesthoods, elder councils, oral tradition custodians that maintain and transmit the interpretive frame converting ritual rehearsal into survival competence. Net beneficiary: they accumulate cultural authority and practical influence from being the gatekeepers of competence extraction. For them, ritual is coordination mechanism — coordinating community response to catastrophic risk through rehearsal. They experience the constraint as enabling, not extractive. Arbitrage options: they can adapt the ritual to new threats, reinterpret it for changed circumstances, or transmit selectively to trusted pupils.
constraint_indexing:constraint_classification(adaptive_competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: COMMUNITY ORGANIZING ADAPTIVE RESPONSE (TANGLED ROPE) — Organized groups (mutual aid networks, disaster-preparedness collectives, indigenous knowledge councils) that have decoupled competence transmission from formal ritual structures. They benefit from the ritual's encoded survival lessons (they can extract and repurpose the competence) while bearing costs of maintaining alternative transmission channels and resisting institutional suppression of adaptive practice. Constrained: they must operate partially underground or in parallel to official ritual structures; they benefit from the constraint by proving competence transmission is possible, but also bear costs of system resistance.
constraint_indexing:constraint_classification(adaptive_competence_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: SECULARIZED INSTITUTIONAL MEMORY / ARCHIVE SYSTEM (PITON) — Formal institutional repositories (universities, museums, archival systems, disaster-preparedness agencies) that have absorbed ritual content into secular frames (disaster sociology, anthropological records, public health protocols). The ritual's competence transmission has been extracted and institutionalized; the ritual form itself persists in museum/archive but mostly as performance or historical artifact. Theater ratio high: institutions maintain 'cultural heritage' while the living transmission mechanism has atrophied. The piton classification derives from the gap between archive function (preserving records) and original function (transmitting embodied competence through catastrophe rehearsal).
constraint_indexing:constraint_classification(adaptive_competence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, ritual transmission of survival competence might appear as an inevitable feature of how communities encode dangerous knowledge: any system that faces recurring catastrophic threats will develop rehearsal mechanisms, and these will necessarily embed in cultural/religious forms because that is the substrate available for long-term memory transmission in low-literacy or pre-literate contexts. The constraint could appear immutable — structurally necessary. However, the presence of beneficiaries (knowledge-keeper elites, institutions that extract competence) and the heterogeneity of extraction burden suggests this is a false summit: the 'inevitable' framing naturalizes a specific institutional arrangement where competence transmission is controlled and asymmetric.
constraint_indexing:constraint_classification(adaptive_competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adaptive_competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(adaptive_competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adaptive_competence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(adaptive_competence_reading, TR),
    TR >= 0.70.

:- end_tests(adaptive_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The competence transmission is genuine — ritual rehearsal does encode survival knowledge — but extraction is asymmetric: knowledge-keeper institutions concentrate access and interpretation. The value reflects that competence is created through the constraint (not pure rent), but distribution is controlled. Suppression (0.42): Moderate. Barriers to competence access include institutional gatekeeping (formal initiation requirements, esoteric interpretation), cultural gatekeeping (women/outsiders excluded from certain rituals), displacement/diaspora effects (severed transmission chains), and modern institutional suppression (assimilation pressure, secularization). But suppression is not absolute — informal transmission channels exist (kitchen-table teaching, embodied memory), and communities maintain practice despite barriers. Theater ratio (0.35 → 0.62): Rising trajectory indicates increasing performative-to-functional gap. Early in the interval, ritual practice maintains strong living function — rehearsal is conducted for genuine threat-response. Over time, secularization and institutional absorption increase theater: formal disaster-preparedness agencies extract competence into protocols, ritual forms move to museums/heritage sites, and embodied transmission attenuates. The rise in theater ratio corresponds to the piton classification in late-stage observation — the ritual persists as performance while its live function degrades.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives is structured by access to the competence-extraction mechanism. Knowledge-keeper elites see Rope — the constraint coordinates community threat-response and they benefit from gatekeeping. Organized communities see Tangled Rope — they extract competence but bear costs of resistance. Ritual maintainers see Snare — they bear the labor cost without access to the competence. Secularized institutions see Piton — they have appropriated the competence; the ritual persists theatrically. Displaced communities experience catastrophic Tangled Rope — both the emotional weight and the competence transmission are lost. The analytical observer risks Mountain — treating competence transmission as natural/inevitable — but the beneficiary structure reveals this as false summit: competence transmission could be egalitarian and non-extractive if institutional gatekeeping were removed.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by their access to the competence-extraction mechanism and their structural position relative to ritual maintenance labor. Knowledge-keeper elites have high beneficiary status (they control interpretation) and arbitrage-level exit (they can adapt, selectively transmit, reinterpret) — d ≈ 0.2 (full beneficiary). Ritual maintainers without competence access have victim status (they bear the labor cost) and trapped-level exit (cultural obligation, no legitimate exit) — d ≈ 0.9 (full target). Organized communities have moderate beneficiary status (they can extract competence) and constrained exit (institutional resistance) — d ≈ 0.5 (symmetric). The engine derives d from beneficiary/victim declarations + exit options; the commentary maps these to the structural mechanism: institutional gatekeeping.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by disaggregating ritual's dual function: catastrophe-rehearsal (genuine coordination problem) and competence-transmission (benefit distribution vehicle). Mandatrophy would arise if the constraint's original mandate (surviving catastrophic threat through rehearsal) had outlived its function — but the reading shows the mandate is LIVE and the constraint is structurally necessary for threat-response coordination. However, the institutional gatekeeping mechanism (who has access to competence interpretation) is where extraction occurs and is NOT mandated by the survival problem itself — that is a secondary institutional layer. The reading avoids mandatrophy by claiming the primary coordination function is real, while identifying the secondary extraction mechanism as contingent institutional arrangement rather than survival necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_extraction_mechanism,
    'Is the competence transmission truly a function of the ritual structure itself, or does competence extraction require an additional institutional layer (priesthood, elder council, secret society) that selectively interprets and teaches?',
    'Comparative analysis: communities with formal knowledge-keeper institutions vs. communities with distributed/egalitarian ritual practice. If distributed communities show equal or higher competence transmission, the extraction mechanism is institutional gatekeeping, not ritual structure. If competence transmission requires formal interpretation layer, the constraint is inherently extractive.',
    'If distributed: reclassify from Tangled Rope to Rope (competence transmission is coordination, extraction is institutional overlay). If institutional: confirm Tangled Rope — the competence is real but access is gatekept.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_extraction_mechanism, empirical, 'Whether competence extraction requires institutional gatekeeping or emerges from ritual structure itself').

omega_variable(
    displacement_competence_collapse,
    'When ritual practice is severed by displacement/diaspora, does competence transmission collapse immediately, or does embodied memory sustain transmission through informal channels?',
    'Historical analysis of displaced communities: measurement of disaster-response competence in first, second, third generations post-displacement; documentation of informal competence transmission (kitchen-table teaching, implicit modeling, performance memory) independent of formal ritual.',
    'If immediate collapse: displacement is catastrophic data loss (victim set = displaced communities). If sustained: informal channels preserve competence; formal ritual was gatekeeping mechanism, not sole transmission vehicle (reclassify victim set).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_competence_collapse, empirical, 'Whether competence transmission survives displacement or collapses with ritual structure').

omega_variable(
    ritual_reading_kernel_ambiguity,
    'Is this constraint properly understood as a reading of the catastrophe-memory-transmission kernel, or is it instantiating a different constraint altogether (competence-transmission-as-social-control)?',
    'Kernel decomposition: (1) if the core claim is ''ritual transmits survival competence,'' then this is ONE reading of how catastrophe memory works; (2) if the core claim is ''institutional actors use competence transmission as a mechanism for social control,'' then a separate constraint (institutional_competence_gatekeeping) should be authored as a sibling or upstream constraint. Diagnostic: does the story''s extracted beneficiary (competence-extraction institutions) define the constraint''s type, or is competence transmission the primary function and institutional gatekeeping secondary?',
    'If institutional control is primary: this reading (adaptive_competence_reading) should reclassify to pure_coordination (Rope) and a new constraint (institutional_competence_gatekeeping) should be authored as upstream. If competence transmission is primary: this reading stands as Tangled Rope, with institutional gatekeeping as a secondary extraction layer. The kernel reading remains adaptive_competence_reading if the navigational problem (''how do communities encode survival knowledge?'') drives the story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_reading_kernel_ambiguity, conceptual, 'Whether this reading of the kernel is structurally accurate or conflates two distinct constraints').

omega_variable(
    authenticity_drag_in_modernization,
    'As secular institutions (disaster-preparedness agencies, public health systems) extract competence from ritual forms, does the ritual lose adaptive capacity (becomes museum artifact, piton) or does it gain by being freed from institutional gatekeeping?',
    'Longitudinal case analysis: communities that have secularized ritual content into formal disaster-preparedness vs. communities maintaining ritual as formal practice. Measurement: speed of adaptation to new threats, community participation in exercises, competence retention across generations.',
    'If secularization improves adaptation: the extraction by modern institutions is net beneficial; piton classification is over-stated (reclassify toward Rope). If secularization degrades adaptation (loss of embodied memory, community alienation): piton classification confirmed; extraction mechanism revealed as institutional appropriation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_drag_in_modernization, empirical, 'Whether modernization/secularization improves or degrades competence transmission').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adaptive_competence_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adapt_comp_theater_t0, adaptive_competence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(adapt_comp_theater_t100, adaptive_competence_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(adapt_comp_theater_t200, adaptive_competence_reading, theater_ratio, 200, 0.62).

% Extraction over time
narrative_ontology:measurement(adapt_comp_extract_t0, adaptive_competence_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(adapt_comp_extract_t100, adaptive_competence_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(adapt_comp_extract_t200, adaptive_competence_reading, base_extractiveness, 200, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(adapt_comp_suppress_t0, adaptive_competence_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(adapt_comp_suppress_t100, adaptive_competence_reading, suppression_requirement, 100, 0.42).
narrative_ontology:measurement(adapt_comp_suppress_t200, adaptive_competence_reading, suppression_requirement, 200, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adaptive_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(adaptive_competence_reading, 0.1).
narrative_ontology:affects_constraint(adaptive_competence_reading, symbolic_continuity_reading).
narrative_ontology:affects_constraint(adaptive_competence_reading, hybrid_resilience_reading).
narrative_ontology:affects_constraint(adaptive_competence_reading, institutional_competence_gatekeeping).
narrative_ontology:affects_constraint(adaptive_competence_reading, diaspora_transmission_loss).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_transmission kernel. The sibling readings (symbolic_continuity_reading, hybrid_resilience_reading) are structurally distinct constraints with different ε values and beneficiary/victim structures. All three are linked via network.affects_constraints to reflect kernel kinship. The upstream constraint institutional_competence_gatekeeping models the extraction mechanism in isolation; the downstream constraint diaspora_transmission_loss models the specific failure mode when displacement severs ritual chains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
