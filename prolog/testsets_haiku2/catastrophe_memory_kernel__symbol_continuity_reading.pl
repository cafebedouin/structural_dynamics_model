% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Ritual Preservation of Symbolic Continuity and Collective Identity (Catastrophe Memory Kernel: Symbol Continuity Reading)
 *   domain: religious/social/cultural
 *
 * SUMMARY:
 *   Ritual, in this reading, is the mechanism by which a diaspora community
 *   preserves symbolic continuity and collective identity across time —
 *   generations of separation, persecution, acculturation pressure, and
 *   technological change. The rituals (prayer services, memorial
 *   commemorations, lifecycle rites, holiday observances) carry the
 *   community's historical memory, theological commitments, and mutual
 *   recognition codes across centuries. The kernel is contested: other
 *   readings emphasize that ritual encodes survival competence
 *   (trauma_encoding_reading), enforces group boundaries
 *   (boundary_maintenance_reading), or transmits practical adaptive capacity
 *   (survival_competence_reading). This reading — symbol_continuity_reading —
 *   foregrounds symbolic transmission and identity coherence as the primary
 *   function. The constraint is authored as low-extractiveness rope: it
 *   coordinates collective memory without demanding survival sacrifice, and
 *   while it does constrain adaptive modification, that constraint is
 *   experienced as the necessary price of coherence, not as oppression.
 *   Theater_ratio is high (0.68) and rising, indicating that the ritual's
 *   practical utility has declined while its performative/symbolic function
 *   has intensified — a signal of piton drift. The claim/metric independence
 *   rule: the constraint is CLAIMED as rope (genuine coordination of symbolic
 *   continuity) while the high theater_ratio documents that the operational
 *   survival function has atrophied; the engine computes the type from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - tradition_continuity_institutional: the persistence of the symbolic tradition itself (non-agent beneficiary)
 *   - ritual_practitioners: organized community members who enact and transmit ritual; identity-locked, receive symbolic coherence but pay the cost of rigidity
 *   - younger_generation_pragmatists: constrained by family/community pressure to comply; want adaptive modification but face exclusion
 *   - diaspora_theological_authorities: agenda-setters who interpret tradition and enforce compliance; maintain that symbolic content is the kernel
 *   - external_majority_culture: excluded from ritual's interior but shapes conditions of observance
 *   - ethnographic_observer: analytical seat documenting whether the constraint operates as claimed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.15).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Ritual Preservation of Symbolic Continuity and Collective Identity (Catastrophe Memory Kernel: Symbol Continuity Reading)").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious/social/cultural").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, 'ff5c89e6-da40-48e6-b23a-d0b8bc57f184').
narrative_ontology:cs_kernel_codification('ff5c89e6-da40-48e6-b23a-d0b8bc57f184', distributed).
narrative_ontology:cs_authority_grounding('ff5c89e6-da40-48e6-b23a-d0b8bc57f184', lineage).
narrative_ontology:cs_interpretation_layer_present('ff5c89e6-da40-48e6-b23a-d0b8bc57f184').
narrative_ontology:cs_reading_relation('ff5c89e6-da40-48e6-b23a-d0b8bc57f184', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff5c89e6-da40-48e6-b23a-d0b8bc57f184', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff5c89e6-da40-48e6-b23a-d0b8bc57f184', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('ff5c89e6-da40-48e6-b23a-d0b8bc57f184', foundational, symbolic_coherence_as_primary_function).
narrative_ontology:cs_axiom_status(symbolic_coherence_as_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('ff5c89e6-da40-48e6-b23a-d0b8bc57f184', symbolic_coherence_as_primary_function, deontological).
narrative_ontology:cs_axiom('ff5c89e6-da40-48e6-b23a-d0b8bc57f184', foundational, identity_constitution_through_ritual_enactment).
narrative_ontology:cs_axiom_status(identity_constitution_through_ritual_enactment, holdable).
narrative_ontology:cs_axiom_grounding('ff5c89e6-da40-48e6-b23a-d0b8bc57f184', identity_constitution_through_ritual_enactment, conventional).
narrative_ontology:cs_reference_frame('ff5c89e6-da40-48e6-b23a-d0b8bc57f184', diaspora_identity_through_symbolic_transmission).
narrative_ontology:cs_drift_state('ff5c89e6-da40-48e6-b23a-d0b8bc57f184', contemporary_digital_communication_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff5c89e6-da40-48e6-b23a-d0b8bc57f184', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_institutional).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_impulses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, ritual_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, ritual_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, younger_generation_pragmatists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional memory and symbolic transmission structures of the diaspora community — the scribal traditions, liturgical calendars, oral teaching lineages, and memorial practices that constitute 'us' as a coherent identity across generations. Benefits from ritual's capacity to transfer symbolic meaning, collective memory, and group boundaries across time without requiring operational justification or adaptive modification. The 'beneficiary' here is not an individual agent but the persistence of tradition-as-such.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_institutional, beneficiary,
    organized, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_institutional).

% Community members who perform and transmit rituals: cantors, liturgists, teachers, and the broader practicing community. They experience ritual as the primary vehicle for their own identity — their self-understanding as members of the tradition is constituted through enacted ritual. They receive the symbolic coherence and cultural continuity the ritual preserves, but also bear the cost of enacting practices that may no longer directly serve survival or adaptation. Their exit option is severely constrained because leaving the ritual is experienced as identity dissolution, not just behavioral change.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_practitioners, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, ritual_practitioners, payer).

% Community members (often younger, more acculturated, or demographically mobile) who perceive ritual observance as incompatible with contemporary life, professional demands, or assimilationist strategy. They bear the social cost of non-compliance (exclusion from community events, family conflict, loss of insider status) and the cognitive cost of maintaining dual cultural identities. Their alternatives are constrained: full compliance with ancestral practice, partial observance that triggers disapproval, or exit that means severing kinship and community ties.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, younger_generation_pragmatists, payer,
    moderate, biographical, constrained, global).

% The collected practical pressures — changed habitats, occupational mobility, demographic mixing, medical advances, technological availability, economic reorganization — that would otherwise reshape ritual practice. These are not agents with voices but structural forces that ritual's symbolic continuity conservatism resists. The 'victim' here is the adaptive potential locked out by ritual's stabilization function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_impulses, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_impulses).

% The broader society in which the minority tradition practices — it is structurally excluded from the ritual's interior meaning, though it shapes the conditions (permitting or suppressing observance, determining accessibility of ritual time/space) in which the ritual operates. It would have a voice in whether the constraint exists at all (tolerance vs. suppression) but no voice in the ritual's content or transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, external_majority_culture, excluded,
    powerful, generational, analytical, national).

% Religious scholars, rabbinical councils, liturgical guardians, and institutional memory-keepers who interpret and formally authorize ritual practice. They assert the authority to determine which practices count as 'traditional' and which as 'deviation,' and they exercise enforcement through social recognition and religious standing. They maintain that the symbolic content — not its operational survival function — is the kernel that must be preserved.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, diaspora_theological_authorities, agenda_setter,
    institutional, civilizational, analytical, global).

% The analytical seat: researchers, historians, and external observers who document whether the constraint operates as claimed and measure its actual effects on community reproduction, transmission fidelity, adaptation capacity, and identity cohesion. They have no stake in the outcome but evidence that can challenge or confirm the reading.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ethnographic_observer, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual coordinates collective identity across time by providing a shared symbolic, behavioral, and embodied language that members recognize as 'us' — the same words, the same gestures, the same sacred calendar that connected them to ancestors and binds them to descendants. The coordination problem is: how does a minority tradition maintain coherence and distinction across diaspora, migration, persecution, and cultural pressure without continuous operational justification?
% TRANSFER_FUNCTION: Moves time and cognitive capacity from members' adaptive flexibility to the preservation of symbolic form — the ritual demands performance at prescribed times and with prescribed exactness, consuming hours and attention that might otherwise be deployed toward economic integration, occupational advancement, or cultural assimilation. The constraint transfers agency to the institutional authority (theologians, liturgists) who define what 'correct' practice is.
% ABSENT_VOICES: Voices of younger-generation pragmatists and those who assimilate are partially excluded — they attend rituals but do not author them, their pressure for modification is overridden by authority assertions of 'tradition,' and their alternative framings (adaptation as survival competence rather than symbolic betrayal) are marginalized. Voices of adaptive modification itself — the practical pressures that would reshape practice — are structurally unheard because adaptation is framed as threat rather than resource.
% DISAPPEARANCE_RATIONALE: If ritual preserving symbolic continuity vanished overnight, the tradition's self-understanding as a coherent historical entity would fragment. Younger generations would adopt assimilated practices or none at all; the liturgical calendar would drift; group boundaries would blur; the transmission of memory through enacted practice would halt. The diaspora would reorganize around pragmatic survival and assimilationist strategy rather than symbolic continuity. The world rearranges because collective identity is organized around the ritual; remove it and identity organization shifts to different axes.
% FOUNDING_PROBLEM: Diaspora communities face the problem of maintaining collective identity and continuity across time when they are geographically dispersed, culturally pressured, and physically separated from the ancestral homeland and institutions. Ritual was developed as a portable, embodied, collectively executable answer: no territory needed, only community memory and repeated enactment. The founding problem is real: diaspora dispersal genuinely disrupts institutional continuity.
% FOUNDING_PROBLEM_CORROBORATION: Religious authorities and community elders attest the founding problem is live — diaspora still threatens identity dispersal and acculturation remains a pressure. Historians and ethnographers outside the benefiting tradition corroborate that diaspora creates continuity problems. However, the same sources note that modern communication technology, international travel, and formal institutional organization (yeshivas, religious councils, diaspora organizations) now solve the geographical dispersal problem through means other than ritual — the founding problem is partly solved by infrastructure beyond ritual itself. The contested status reflects whether the ritual is necessary for the solution it was built to provide.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.22, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as low (0.22) because symbolic transmission, while cognitively demanding and temporally expensive, does not require sacrifice of material survival or inflict material harm. Younger-generation pragmatists bear the cost (social pressure, cognitive dissonance, occupational/lifestyle friction), but the cost is framed as the price of membership, not as exploitation. Suppression (0.15) is low because coercion is primarily internalized (identity-lock: leaving ritual is experienced as self-loss) rather than externally enforced. Theological authorities do not use police or economic sanction to compel compliance; they use social recognition and insider/outsider boundary maintenance. Theater_ratio (0.68, rising) is high because the ritual's symbolic/performative function has become primary relative to its operational function. The measurement series show theater_ratio rising from 0.52 to 0.70 over the interval (60+ years), indicating that ritual observance has shifted from 'we do this to transmit operative knowledge and capacity' to 'we do this to be ourselves' — the operative content has hollowed out while the performative/identity function has intensified. Accessibility_collapse (0.35) is moderate: alternatives to ritual observance exist (assimilation, secular identity, non-observant diaspora communities), but once someone commits to the tradition, the collapse is nearly complete (if you are in the tradition, ritual is the only path to insider status). Resistance (0.42) is moderate: some members resist (younger pragmatists, assimilationists), but resistance is not militant; it takes the form of non-compliance and partial observance rather than organized challenge to the institution.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the agenda-setter seat (theological authorities) and the payer seat (younger pragmatists) is structural. From the authority perspective, the constraint is genuine rope: it solves a real coordination problem (how to maintain collective identity across diaspora), and it produces goods that all parties benefit from (coherent tradition, insider community status, historical continuity). From the constrained payer perspective, the constraint is closer to snare: they are locked in by identity-fusion and social pressure; the goods (community belonging) are real but conditional on compliance; alternatives exist but are prohibitively costly (assimilation = identity loss, non-compliance = social exclusion). The ritual practitioners (identity-locked) occupy a middle position: they genuinely experience the ritual as identity-constituting and benefit from it without coercion, but they also bear the cost (temporal, cognitive, adaptability) of maintaining practices that no longer serve operational survival. The engine computes a per-seat type from these structural asymmetries: the authority seat sees rope; the constrained seat sees snare; the identity-locked practitioners see rope-that-is-also-part-of-self. These divergent readings are computational consequences of the power/exit/beneficiary-victim structure, not authoring choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures the agent's structural relationship to THIS constraint. Theological authorities: d near 0.0 (full beneficiary) — they maintain authority over symbolic interpretation and collect social recognition as 'keepers of tradition.' Ritual practitioners: d near 0.3–0.4 (partial beneficiary) — they receive identity coherence and community belonging, but also pay the rigidity cost; however, identity-lock makes exit prohibitive, so the cost is carried as part of the identity itself. Younger pragmatists: d near 0.65 (partial target) — they bear the compliance cost (family pressure, occupational friction, cognitive dissonance) without being major beneficiaries of the symbolic transmission (they prefer assimilation or secular identity). The tradition-as-institution (non-agent beneficiary): d = 0.0 (pure beneficiary) — the constraint's operation sustains its existence; the constraint is built to preserve it. The adaptive-modification impulses (non-agent victim): d = 1.0 (full target) — they are structurally suppressed by the constraint's rigidity function. These d values feed the engine's extraction computation (χ): beneficiaries get low/negative χ (subsidy rather than extraction), targets get high χ (amplified extraction), and symmetric positions get χ near the base ε.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is the condition where a constraint's mandate (the original problem it was built to solve) has outlived its function, but the constraint persists due to institutional inertia or theatrical maintenance. This constraint shows mandatrophy signs. The founding problem (diaspora dispersal disrupting collective identity and institutional continuity) was real and remains contested in its status, but the operative means have changed. Modern communication technology (telephone, internet, travel), formal institutional organization (yeshivas, religious councils, diaspora educational networks), and international academic publishing now solve the geographical continuity problem without ritual enactment. The ritual persists not because it solves the founding problem uniquely, but because (a) theological authority has invested in maintaining it, (b) community members' identities are fused with its practice, and (c) abandoning it is framed as cultural betrayal. The high theater_ratio (0.68, rising) is the diagnostic signal of mandatrophy: the ritual has shifted from 'operative transmission of adaptive knowledge' to 'performative enactment of identity.' A mandatrophy resolution would involve either (1) formally acknowledging the founding problem is solved and reconstituting the constraint as purely symbolic (lowering extractiveness claims), or (2) identifying a different survival-competence function the ritual actually serves and shifting the reading to trauma_encoding_reading (where the constraint would be justified by its operative warning-system function). Under the current symbol_continuity_reading, mandatrophy is not resolved — the constraint is authored as rope, but its theater_ratio suggests it has drifted toward piton (persistent performance with atrophied function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_operational_function_boundary,
    'Is the measured extractiveness (0.22) actually the cost of symbolic transmission, or is it the suppressed cost of adaptive rigidity that would emerge if communities tried to modify practice?',
    'Controlled comparison: communities that modify ritual practice for adaptation vs. communities that preserve strict continuity, measured on both identity-continuity and adaptive-capacity outcomes over a generational interval. If modified communities show equal identity continuity but higher adaptive capacity, extractiveness was underestimated; if modified communities show fragmented identity, the measured value is accurate.',
    'If extractiveness is accurate, this reading (symbol continuity as primary) holds; if extractiveness conceals suppressed adaptive cost, the constraint is more extractive than authored and the trauma_encoding_reading (adaptation-for-survival) becomes the better fit. The reclassification would move the constraint toward snare territory if hidden costs are discovered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_operational_function_boundary, empirical, 'Whether symbolic transmission cost is real or conceals suppressed adaptive cost.').

omega_variable(
    identity_lock_mechanism_internalization,
    'Is the identity-locking of ritual practitioners (exit_options: identity_locked) structural (external kinship/community sanctions for non-compliance) or internalized (the agent experiences ritual-loss as self-loss regardless of community pressure)?',
    'Post-exit trajectory study: practitioners who leave ritual practice and measure whether identity distress persists after community exclusion pressure is removed, and whether cognitive reframing (''ritual was cultural, not essential to me'') achieves resolution.',
    'If internalized, the suppression metric (0.15) understates the true suppression — the agent carries the constraint with them after exit. If structural, the measured suppression is accurate and reflects only social sanctions. This affects whether the constraint is sustainable (internalized = stable; structural = contestable if enforcement resources decline).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'Whether identity-lock in ritual practice is structural or internalized.').

omega_variable(
    kernel_reading_contest_foreclosure,
    'Do the symbol_continuity_reading (this constraint) and the survival_competence_reading logically foreclose each other, or can a single framework hold both — ritual simultaneously preserves symbolic continuity AND encodes adaptive capacity?',
    'Genealogical analysis: tracing whether historical authority structures that teach this ritual explicitly distinguish symbolic from operational levels, or treat them as inseparable. If historical teaching separates them, the readings coexist; if teaching fuses them, one reading historically foreclosed the other, and the contest is about reinstating what was suppressed.',
    'If they foreclose, the reading_relations should shift from coexists_with to forecloses; if they coexist, the current relation holds. This changes the constraint family''s configuration — a foreclosure relation implies one reading is a deliberate suppression of the other, not a difference in institutional perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_foreclosure, conceptual, 'Whether this reading''s axiom (symbolic continuity as primary) logically forecloses the survival-competence reading''s axiom (operative adaptation as primary).').

omega_variable(
    theater_ratio_meaning_high_performance,
    'Why is theater_ratio (0.68) so high — what proportion of observed ritual activity is performative maintenance (enacting the symbols without operational necessity) vs. functional (teaching, identity-bonding, transmission)?',
    'Activity audit: categorizing observed ritual enactments by function — how much time is spent on the central transmitted content (narrative, law, obligation transmission), how much on formal correctness verification (checking pronunciation, order, vestments), how much on audience/participant aesthetics (music, emotional resonance) that are not strictly necessary to transmission. If ≥70% falls into categories other than pure transmission, theater_ratio is accurate.',
    'If theater_ratio is accurate, it signals piton drift — the ritual is becoming increasingly performative and less functionally necessary. This supports the reading that symbolic continuity is the primary function; if theater_ratio were lower, operational transmission would be in evidence. A very high theater_ratio on a low-extractiveness constraint suggests the constraint persists through inertia and collective attachment rather than through material enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_meaning_high_performance, empirical, 'Proportion of ritual activity that is performative vs. functionally necessary to transmission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement_basis(cata_tr_t0, projected).
narrative_ontology:measurement(cata_tr_t14, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 14, 0.58).
narrative_ontology:measurement_basis(cata_tr_t14, observed).
narrative_ontology:measurement(cata_tr_t28, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 28, 0.61).
narrative_ontology:measurement_basis(cata_tr_t28, observed).
narrative_ontology:measurement(cata_tr_t42, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 42, 0.65).
narrative_ontology:measurement_basis(cata_tr_t42, observed).
narrative_ontology:measurement(cata_tr_t56, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 56, 0.68).
narrative_ontology:measurement_basis(cata_tr_t56, observed).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 70, 0.7).
narrative_ontology:measurement_basis(cata_tr_t70, observed).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 100, 0.68).
narrative_ontology:measurement_basis(cata_tr_t100, projected).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(cata_be_t0, projected).
narrative_ontology:measurement(cata_be_t14, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 14, 0.19).
narrative_ontology:measurement_basis(cata_be_t14, observed).
narrative_ontology:measurement(cata_be_t28, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 28, 0.21).
narrative_ontology:measurement_basis(cata_be_t28, observed).
narrative_ontology:measurement(cata_be_t42, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 42, 0.23).
narrative_ontology:measurement_basis(cata_be_t42, observed).
narrative_ontology:measurement(cata_be_t56, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 56, 0.22).
narrative_ontology:measurement_basis(cata_be_t56, observed).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 70, 0.24).
narrative_ontology:measurement_basis(cata_be_t70, observed).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement_basis(cata_be_t100, projected).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(cata_su_t0, projected).
narrative_ontology:measurement(cata_su_t14, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 14, 0.12).
narrative_ontology:measurement_basis(cata_su_t14, observed).
narrative_ontology:measurement(cata_su_t28, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 28, 0.14).
narrative_ontology:measurement_basis(cata_su_t28, observed).
narrative_ontology:measurement(cata_su_t42, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 42, 0.15).
narrative_ontology:measurement_basis(cata_su_t42, observed).
narrative_ontology:measurement(cata_su_t56, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 56, 0.16).
narrative_ontology:measurement_basis(cata_su_t56, observed).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 70, 0.15).
narrative_ontology:measurement_basis(cata_su_t70, observed).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 100, 0.15).
narrative_ontology:measurement_basis(cata_su_t100, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__symbol_continuity_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the catastrophe_memory_kernel. All four readings share a common referent (diaspora ritual practice and its role in collective memory), but each reading emphasizes a different function as PRIMARY: this reading emphasizes symbolic continuity; survival_competence_reading emphasizes operative survival transmission; trauma_encoding_reading emphasizes intergenerational trauma encoding; boundary_maintenance_reading emphasizes group boundary enforcement. Each reading authors a distinct ε because the PRIMARY function differs — what counts as extraction varies by reading. These are not perspectival variants of one constraint; they are structurally distinct claims about what the constraint is FOR. All four stories must be cross-linked via affects_constraints; the kernel_id and reading_id in the committer frame identify which reading each story instantiates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
