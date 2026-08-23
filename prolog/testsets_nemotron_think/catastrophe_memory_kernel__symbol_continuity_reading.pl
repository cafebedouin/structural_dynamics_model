% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Ritual as Symbolic Continuity Anchor
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story captures the symbol_continuity_reading of the
 *   catastrophe_memory_kernel: the claim that ritual's primary function is
 *   preserving symbolic continuity and collective identity across time,
 *   particularly through mourning-practices that serve as identity-markers.
 *   The reading posits low extractiveness — symbolic transmission without
 *   operational survival yield — but acknowledges ritual rigidity costs as a
 *   victim class (adaptive modification constrained). The beneficiary is
 *   framed as tradition-continuity itself (a vindicated proposition), though
 *   human traditionalist communities and identity-seeking descendants benefit
 *   from the continuity. Reformist practitioners and adaptive communities
 *   bear the rigidity costs. The constraint requires active enforcement
 *   (social sanctions for ritual deviation) and coordinates identity across
 *   generations (identity_coordination type).
 *
 * KEY AGENTS:
 *   - traditionalist_communities: Primary beneficiary (organized/constrained) — maintain ritual forms, collect identity-cohesion benefits
 *   - identity_seeking_descendants: Beneficiary (moderate/constrained) — receive symbolic continuity as identity anchor
 *   - reformist_practitioners: Primary victim/payer (moderate/constrained) — seek adaptive modification, face rigidity costs
 *   - adaptive_communities: Victim/payer (organized/constrained) — communities attempting ritual innovation, sanctioned by traditionalist enforcement
 *   - ritual_specialists: Agenda_setter (institutional/identity_locked) — authorize and transmit ritual forms, gatekeep adaptation
 *   - external_observers: Observer (analytical/analytical) — scholars of collective memory, ritual studies, religious studies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.35).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Ritual as Symbolic Continuity Anchor").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, 'ced0a20f-6fc4-43ea-bb95-847e13e844e7').
narrative_ontology:cs_kernel_codification('ced0a20f-6fc4-43ea-bb95-847e13e844e7', distributed).
narrative_ontology:cs_authority_grounding('ced0a20f-6fc4-43ea-bb95-847e13e844e7', practice).
narrative_ontology:cs_interpretation_layer_present('ced0a20f-6fc4-43ea-bb95-847e13e844e7').
narrative_ontology:cs_reading_relation('ced0a20f-6fc4-43ea-bb95-847e13e844e7', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ced0a20f-6fc4-43ea-bb95-847e13e844e7', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('ced0a20f-6fc4-43ea-bb95-847e13e844e7', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('ced0a20f-6fc4-43ea-bb95-847e13e844e7', foundational, ritual_preserves_symbolic_continuity).
narrative_ontology:cs_axiom_status(ritual_preserves_symbolic_continuity, holdable).
narrative_ontology:cs_axiom_grounding('ced0a20f-6fc4-43ea-bb95-847e13e844e7', ritual_preserves_symbolic_continuity, conventional).
narrative_ontology:cs_axiom('ced0a20f-6fc4-43ea-bb95-847e13e844e7', foundational, collective_identity_requires_embodied_transmission).
narrative_ontology:cs_axiom_status(collective_identity_requires_embodied_transmission, holdable).
narrative_ontology:cs_axiom_grounding('ced0a20f-6fc4-43ea-bb95-847e13e844e7', collective_identity_requires_embodied_transmission, conventional).
narrative_ontology:cs_reference_frame('ced0a20f-6fc4-43ea-bb95-847e13e844e7', symbolic_continuity_frame).
narrative_ontology:cs_drift_state('ced0a20f-6fc4-43ea-bb95-847e13e844e7', contemporary_secular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ced0a20f-6fc4-43ea-bb95-847e13e844e7', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, traditionalist_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, identity_seeking_descendants).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, reformist_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_communities).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_through_symbolic_transmission).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, collective_identity_requires_ritual_anchoring).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain established ritual forms as the primary vehicle of collective identity. They benefit from the cohesion and continuity the rituals provide, and their authority derives from being the recognized transmitters of tradition. Exit means abandoning the identity-framework that gives their communal role meaning.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, traditionalist_communities, beneficiary,
    organized, generational, constrained, national).

% Individuals (often second/third generation) who seek connection to collective history through ritual participation. They receive symbolic continuity as an identity anchor but inherit the rigidity constraints. Exit means losing the primary available symbolic language for their heritage.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, identity_seeking_descendants, beneficiary,
    moderate, biographical, constrained, national).

% Practitioners who seek to adapt ritual forms to contemporary contexts (gender-egalitarian language, abbreviated forms, trauma-informed modifications). They face social sanctions, exclusion from communal ritual roles, and legitimacy challenges. Their exit options are constrained by identity-investment and community ties.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, reformist_practitioners, payer,
    moderate, biographical, constrained, national).

% Whole communities (e.g., diaspora congregations, progressive denominations) attempting collective ritual innovation. They bear the institutional costs of schism, loss of recognition from traditionalist centers, and the effort of building alternative legitimacy. Exit means institutional isolation or reabsorption into traditionalist frameworks.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_communities, payer,
    organized, generational, constrained, regional).

% Clergy, cantors, ritual directors, and textual authorities who authorize ritual forms and police boundaries. Their professional identity and institutional authority are fused with the ritual tradition — they cannot exit without ceasing to be what they are. They set the agenda for what counts as authentic practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_specialists, agenda_setter,
    institutional, generational, identity_locked, national).

% Scholars of religious studies, collective memory, ritual theory, and anthropology who analyze the constraint from outside. They neither collect benefits nor pay costs; their role is structural description and comparative analysis across kernel readings.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, external_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual provides a stable symbolic framework that allows a community to maintain collective identity across catastrophic rupture and generational turnover. It solves the coordination problem of 'how do we remain the same people despite catastrophe and time?' by anchoring identity in repeatable, transmissible symbolic forms — particularly mourning-practices that mark the catastrophe as constitutive.
% TRANSFER_FUNCTION: Moves authority to define authentic practice from reformist/adaptive actors to traditionalist/ritual-specialist actors. Transfers the cost of rigidity (foregone adaptation, suppressed innovation) onto those who would modify the forms. Transfers identity-cohesion benefits to all participants, but the authority-rent accrues to the agenda-setters.
% ABSENT_VOICES: Those who have already exited — assimilated descendants, communities that abandoned the ritual framework entirely, and victims of ritual rigidity who left rather than contest. They are absent because the constraint's enforcement succeeded in making their departure the only viable adaptation. Also absent: future generations who will inherit the rigidity without having consented to it.
% DISAPPEARANCE_RATIONALE: If the ritual continuity constraint vanished overnight, communities would lose their primary symbolic language for collective identity-anchoring. New identity-forms would emerge (secular commemorations, digital archives, therapeutic frameworks), but the specific coordination function — transmitting catastrophe-memory as identity-constitutive through embodied ritual — would be gone. The world rearranges because the coordination problem (identity across rupture) remains but the solution disappears.
% FOUNDING_PROBLEM: After catastrophe (exile, genocide, displacement), how does a community preserve its collective identity when material continuity is destroyed? The arrangement was built to solve: identity-transmission without territory, institutions, or living memory — using only symbolic forms that can survive in bodies and texts.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist authorities attest the problem is live (new catastrophes, ongoing assimilation). Reformist practitioners and trauma scholars attest the founding problem has shifted — the catastrophe is past, the current problem is rigidity's cost on living communities. Independent historians of religion (e.g., scholars of diaspora Judaism, Armenian memory, Indigenous ceremony) corroborate that the founding problem was real but the solution's persistence now exceeds its adaptive function. No single outside authority corroborates the 'still live' claim unanimously — the contestation is the signal.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.22) because the constraint operates through symbolic transmission, not resource extraction — no material rents flow to a concentrated beneficiary. Suppression (0.35) reflects social enforcement of ritual norms (exclusion, shaming, loss of communal standing) rather than coercive state power. Theater_ratio (0.28) captures performative maintenance of ritual forms where the symbolic function is genuine but partly sustained by habit and identity-investment. Accessibility_collapse (0.42) indicates adapted rituals exist but face high legitimacy barriers. Resistance (0.38) comes from reformist practitioners and adaptive communities pushing for contextually relevant forms. The claimed_type tangled_rope reflects genuine coordination (identity continuity across generations) combined with asymmetric extraction (rigidity costs on adapters).
 *
 * PERSPECTIVAL GAP:
 *   From the traditionalist/ritual-specialist seat, the constraint appears as rope (genuine coordination preserving identity). From the reformist/adaptive seat, it appears as snare (rigidity enforced without survival yield). The engine computes this divergence from the structural data: same constraint, different directionalities, different effective extractions. The analytical observer sees the tangled_rope structure — coordination function real, but asymmetric costs real and enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditionalist communities and ritual specialists are structural beneficiaries (d ~ 0.2-0.3) — they set the agenda and gain identity-authority from continuity. Identity-seeking descendants are near-symmetric (d ~ 0.45) — they receive continuity benefits but also inherit rigidity constraints. Reformist practitioners and adaptive communities are targets (d ~ 0.7-0.8) — they bear adaptation costs, face enforcement, and have constrained exit (identity_locked for many). The vindicated propositions (tradition_continuity, collective_identity_anchoring) collect no rents; they are the coordination function's normative justification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving collective identity through catastrophe via symbolic continuity) remains live (contested status) — catastrophes recur, identity-anchoring remains needed. But the mandate has partially atrophied: the original survival-encoding function (per survival_competence_reading) has decayed, leaving symbolic continuity as the primary active function. The constraint persists not because the founding problem is dead, but because the coordination mechanism (ritual rigidity) has become partially self-justifying — identity_locked agents enforce it beyond its adaptive utility. This is not full mandatrophy (the problem is live) but a drifted mandate where enforcement exceeds functional need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (symbol_continuity_reading) of the contested catastrophe_memory_kernel. What structural elements do sibling readings (survival_competence_reading, trauma_encoding_reading, boundary_maintenance_reading) change, and where is the disagreement located?',
    'Comparative analysis of each reading''s stakeholder structure, extraction profile, and coordination function to map the kernel''s structural fracture lines.',
    'If sibling readings produce different constraint types or beneficiary/victim structures, the kernel is not a single constraint but a family — each reading must be authored separately with its own ε, linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment kernel decomposition: symbol_continuity_reading vs. sibling readings of catastrophe_memory_kernel').

omega_variable(
    beneficiary_abstract_vs_actor,
    'The declared beneficiary ''tradition-continuity itself'' is a proposition, not an actor. Does the constraint have human beneficiaries who collect rents, or is the coordination function genuinely non-extractive with only vindicated propositions?',
    'Trace whether any human group (traditionalist elites, ritual specialists, institutional gatekeepers) accrues material or status benefits from ritual rigidity beyond symbolic participation.',
    'If human beneficiaries exist, the constraint shifts toward tangled_rope or snare; if only propositions are vindicated, low extractiveness holds and the coordination function may be genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_abstract_vs_actor, empirical, 'Whether tradition-continuity as beneficiary masks human rent collection').

omega_variable(
    rigidity_cost_distribution,
    'Are ritual rigidity costs borne disproportionately by specific subgroups (women, youth, minorities, dissenters) or distributed evenly across the community?',
    'Ethnographic and historical analysis of who initiates ritual adaptation and who faces sanctions for deviation.',
    'If costs concentrate on structurally disadvantaged subgroups, suppression and extraction are higher than aggregate measures suggest; if diffuse, the tangled_rope classification with low ε stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rigidity_cost_distribution, empirical, 'Distributional incidence of ritual rigidity costs across community subgroups').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 60, 0.21).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 80, 0.22).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 100, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 60, 0.32).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 80, 0.34).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__symbol_continuity_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the catastrophe_memory_kernel family. The kernel decomposes into four readings with distinct structural profiles: symbol_continuity (this file, low ε, identity_coordination), survival_competence (operational adaptation, resource_allocation), trauma_encoding (warning system, information_standard), boundary_maintenance (group enforcement, enforcement_mechanism). Each reading has its own ε, stakeholders, and type. They are linked here via affects_constraints. The dual formulation is that the kernel's natural-language label ('catastrophe memory ritual') conceals four structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__symbol_continuity_reading, institutional, 0.25).
constraint_indexing:directionality_override(catastrophe_memory_kernel__symbol_continuity_reading, organized, 0.3).
constraint_indexing:directionality_override(catastrophe_memory_kernel__symbol_continuity_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
