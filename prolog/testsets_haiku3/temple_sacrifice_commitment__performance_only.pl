% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Temple Sacrifice Commitment—Performance-Only Reading
 *   domain: religious/legal/commitment-system
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   'temple_sacrifice_commitment.' The performance-only reading asserts that
 *   the sacrifice commitment requires material instantiation to be a genuine
 *   occupation of the law; study without performance is therefore archival
 *   preservation of a defunct practice, not performance of the living
 *   commitment. Under this reading, halakhic scholarship is low-extractive
 *   coordination (shared maintenance of an inherited textual apparatus) but
 *   the commitment itself is a dormant husk—no living obligation binds anyone
 *   to enact its material demands. The constraint described here is the
 *   dormancy structure that reading instantiates: a framework in which study
 *   legitimizes non-performance by redefining performance as impossible and
 *   study-only as the bound interpretation.
 *
 * KEY AGENTS:
 *   - halakhic_scholars: institutional maintainers of the legal framework; set boundaries between scholarship (permitted, low-extractive) and performance (forbidden, doctrinally ruled out)
 *   - liturgical_communities: inherit symbolic authority from the commitment; participate in reference-ritual without material performance; receive coordination benefit from shared textual inheritance
 *   - restoration_movement_advocates: excluded party that disputes the dormancy reading; would become victims if their reading gained authority (restoration would violate accumulated legal prohibitions)
 *   - contemporary_rabbinic_authority: both agenda-setter (legislates performance-prohibition) and observer (monitors boundary between permitted study and forbidden restoration)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.18).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.12).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.18).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, piton).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Commitment—Performance-Only Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious/legal/commitment-system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, 'db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d').
narrative_ontology:cs_kernel_codification('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d', fixed_text).
narrative_ontology:cs_authority_grounding('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d', lineage).
narrative_ontology:cs_interpretation_layer_present('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d').
narrative_ontology:cs_reading_relation('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d', temple_sacrifice_commitment__symbolic_transformation, forecloses).
narrative_ontology:cs_axiom('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d', foundational, material_instantiation_binding).
narrative_ontology:cs_axiom_status(material_instantiation_binding, holdable).
narrative_ontology:cs_axiom_grounding('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d', material_instantiation_binding, conventional).
narrative_ontology:cs_axiom('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d', secondary, commitment_dormancy_thesis).
narrative_ontology:cs_axiom_status(commitment_dormancy_thesis, holdable).
narrative_ontology:cs_axiom_grounding('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d', commitment_dormancy_thesis, conventional).
narrative_ontology:cs_reference_frame('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d', post_temple_destruction_dormancy).
narrative_ontology:cs_drift_state('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d', contemporary_rabbinic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('db1a6cc1-9f61-49ac-b8ca-fdf8624fd26d', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, halakhic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, liturgical_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the hermeneutic apparatus for sacrifice law within the tradition. They author interpretations, set boundaries between study and performance, and sustain the legal framework even though material sacrifice is forbidden by contemporary rabbinic authority and political circumstance. They argue that study without performance is archival preservation—scholarship about a dead law, not occupation of the living commitment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, halakhic_scholars, agenda_setter,
    institutional, generational, constrained, regional).

% Participate in prayer and ritual that reference sacrifice law without performing it. They inherit the commitment's language and structure through liturgy and receive the symbolic authority structure it confers. They do not bear direct cost for maintaining the legal framework; scholars bear that administrative burden.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, liturgical_communities, beneficiary,
    organized, generational, constrained, local).

% Argue that the commitment requires performance and that study-only is a diminishment of the tradition. They are excluded from mainstream halakhic discourse because their position is doctrinally rejected and politically untenable. If their reading gained authority, they would become victims: contemporary restoration would violate prohibitions (Roman law prevents actual sacrifice) and would require reversing decades of accumulated legal rulings.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, restoration_movement_advocates, excluded,
    moderate, biographical, trapped, regional).

% A non-agent entity: the interpretive practice of Talmudic and Mishnaic study-without-material-instantiation persists and thrives because the performance-only reading validates it as scholarship about a defunct practice rather than failed occupation of a living commitment. This framing allows indefinite intellectual engagement without demanding performance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, legal_exegetical_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__performance_only, legal_exegetical_tradition).

% Legislates the practical boundaries of the sacrifice commitment: forbids actual sacrifice, permits and encourages study, and maintains the legal fiction that the framework is 'dormant' rather than 'abandoned.' They administer both the performance-prohibition and the study-permission, which coexist paradoxically under the performance-only reading.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, contemporary_rabbinic_authority, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__performance_only, contemporary_rabbinic_authority, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The study of sacrifice law coordinates a shared textual and legal inheritance: communities maintain connection to the commitment through intellectual engagement, preserve the hermeneutic structure across generations, and maintain a common vocabulary for discussing what the law would demand if circumstances permitted it.
% TRANSFER_FUNCTION: Scholarly labor and administrative maintenance of the legal apparatus transfer from halakhic scholars and rabbinic authorities to the liturgical communities and tradition-bearers—scholars invest in the intellectual work of keeping the framework coherent; communities inherit the symbolic authority and framing that comes from standing within a known legal structure.
% ABSENT_VOICES: Restoration advocates and literalist-minded practitioners are structurally excluded from the discourse that defines what study-without-performance means; they would argue for material instantiation but are kept out by the contemporary consensus that such performance is forbidden and the reading-only framing is therefore the binding interpretation.
% DISAPPEARANCE_RATIONALE: If the performance-only reading and its framework disappeared, the commitment would either be restored to live performance (with all the attendant violations this would entail) or completely abandoned as a dead law. The tradition would lose the hermeneutic structure that permits simultaneous claim to the commitment and refusal of its material demands. Communities would either restore sacrifice (reversing centuries of prohibition) or explicitly adopt the abandoned-law reading, a major doctrinal shift.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) eliminated the material conditions for sacrifice performance. The commitment to sacrifice law persisted in the tradition through study and textual interpretation, creating the structural problem: how to claim the commitment remains binding while acknowledging that performance is impossible and, after the modern period, formally forbidden.
% FOUNDING_PROBLEM_CORROBORATION: Medieval and modern halakhic authorities attest the problem remains live: they continuously rule on the status of the commitment, forbid restoration attempts, and permit-encourage study as the legitimate form of engagement. Historical scholarship from outside the benefiting tradition corroborates that sacrifice performance was eliminated by political circumstance (Temple destruction, Roman prohibition) and later codified into law. Restoration advocates dispute whether the commitment is truly 'live' or merely 'dormant,' attesting the ambiguity.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).
:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The theater_ratio is high (0.62) and rises steadily over the interval because the constraint's primary activity is performative maintenance of its own dormancy: scholars produce interpretations that explain why performance is unnecessary, liturgy references sacrifice without enacting it, and the tradition's administrative apparatus maintains a legal structure for a dead law. The rising trajectory reflects increasing rationalization: as material restoration became doctrinally impossible, the performance-only reading required more elaborate explanation of how study-only could satisfy the commitment. Base extractiveness is low (0.18) because the beneficiary set (scholars and communities) is genuinely coordinated by the study requirement—they share investment in the textual apparatus and receive real benefits from that coordination. No party is trapped into bearing costs for the scholars' benefit; the asymmetry is minimal. Suppression is low because the dormancy structure does not depend on coercion: restoration is forbidden, but the prohibition emerges from law (not from coercive enforcement) and commands broad consensus. The measurement series shows stabilization after year 1000: theater_ratio plateaus once the dormancy reading becomes canonical, extractiveness drops slightly (the initial phase required more active legitimation work), and suppression stabilizes (the prohibition is now law, not defended against active resistance). This is characteristic piton pattern: a framework persisting past its functional life, animated by the administrative apparatus maintaining it rather than by living participants demanding it.
 *
 * PERSPECTIVAL GAP:
 *   Halakhic scholars and rabbinic authority see the performance-only reading as intellectually coherent and doctrinally binding: study of a law one cannot enact is still engagement with the law (legitimacy of the scholarly apparatus). Restoration advocates see the same reading as evasion: if the commitment is real, study alone is insufficient; the only coherent positions are either restore performance or explicitly abandon the law (rejection of the dormancy framing itself). The engine computes these divergent classifications from the structural data: the scholarly seat experiences low extraction (genuine coordination benefit) while the restoration-advocate seat, were it authorized, would face very high extraction (forced non-performance in service of a dead legal fiction). The performance-only reading itself is the mechanism that keeps these seats from forming a single framework—the reading forecloses the restoration reading's core premise (that commitment requires material instantiation).
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars benefit from the study-requirement: it validates their intellectual labor as performance and sustains the apparatus that gives them authority (d near the beneficiary end, ~0.25). Liturgical communities benefit from coordination (shared textual inheritance, symbolic authority structure) without bearing extraction cost (d symmetric, ~0.50). Restoration advocates, were they not excluded, would be high-target (forced non-performance through law), but exclusion itself prevents them from forming a stakeholder seat with directionality. The performance-only reading's core work is to exclude that seat and its directionality entirely—to make restoration-seeking invisible to the legal structure. This is identity-locked exit for restoration advocates: their commitment to material instantiation as binding doctrine prevents them from adopting the dormancy reading; they are locked into exclusion by the very commitment that motivates their objection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Temple sacrifice impossible) was live at t=0 (70 CE, destruction) and remains nominally live (the commitment persists), but the practical problem it solved is thoroughly solved by absence: no one performs sacrifice, so no one needs the law that forbids it. Yet the law persists. This is mandatrophy in motion: a commitment to maintain a dead legal structure because abandoning it would require doctrinal revision that the authority structure resists. The performance-only reading prevents mandatrophy declaration by maintaining that the commitment is 'dormant, not dead'—a juridical distinction that preserves the appearance of liveness while acknowledging material non-performance. The rising theater_ratio over the interval reflects increasing investment in this distinction: the less anyone actually demands performance, the more elaborate the explanation of why study-only is legitimate must become. A piton reading of a dead law, kept animated by scholars and communities who benefit from the apparatus's persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commitment_dormancy_vs_death,
    'Is the sacrifice commitment genuinely dormant (suspended, waiting restoration) or truly dead (abandoned but unacknowledged)?',
    'Natural history: if restoration attempts emerge and are suppressed by legal prohibition (not by consensus that the commitment is dead), dormancy is the correct reading; if restoration is rejected as incompatible with the tradition''s current self-understanding, the commitment is dead but unacknowledged.',
    'If dormant: the performance-only reading is correct and the commitment binds non-performance as law. If dead: the commitment has undergone unauthorized abandonment and study-only is rationalizing non-performance, not satisfying a binding obligation. Classification consequence: piton vs. zombie-snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_dormancy_vs_death, empirical, 'Whether the dormancy framing names a real suspended state or a cover story for unacknowledged death.').

omega_variable(
    identity_locked_restoration_advocates,
    'To what degree is restoration advocacy identity-locked (bound into the advocate''s identity as a commitment-keeper) versus structurally trapped (prevented by law and authority)?',
    'Post-authority-shift analysis: if the rabbinic prohibition were formally lifted and restoration advocates still declined to perform, identity-locking is the primary mechanism; if they immediately move toward performance, structural trapping was primary.',
    'If identity-locked: restoration advocates are not true victims (they are locked by their own commitment frame); if structurally trapped: they are victims of the prohibition and would become higher-extraction targets if restoration became possible. Affects victim-set definition and extraction computation under alternative readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_restoration_advocates, empirical, 'Mechanism binding restoration advocates to non-performance despite their doctrine.').

omega_variable(
    study_as_coordination_or_theater,
    'Is the scholarly study of sacrifice law genuine coordination (shared textual inheritance, communities maintaining connection) or primarily theater (rationalization of non-performance masquerading as engagement)?',
    'Functional analysis: if communities and scholars would spontaneously maintain the textual tradition absent the dormancy reading, study is coordination; if study persists only because the dormancy reading legitimizes it, study is theater in service of a dead commitment.',
    'If coordination: base_extractiveness remains low (~0.18) and the constraint classifies as piton (low-extraction institutional inertia). If theater: base_extractiveness should be higher (~0.35+) and the constraint becomes snare-like (scholars extracting authority through a dead-law maintenance apparatus). Changes the classification from piton to snare-variant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_coordination_or_theater, conceptual, 'Whether study serves genuine coordination or primarily rationalizes the commitment''s non-performance.').

omega_variable(
    performance_only_forecloses_study_as_exercise,
    'Does the performance-only reading logically foreclose the study-as-exercise reading (intellectual engagement IS performance), or do they represent irreducible competing interpretations of the kernel?',
    'Textual and doctrinal analysis of whether the kernel itself can be read to support both or whether the readings'' axioms directly contradict each other such that no single framework could hold both.',
    'If forecloses: the performance-only reading is structurally incompatible with study-as-exercise and the two readings cannot coexist in a single authority structure. If coexists: the readings represent different valid interpretive traditions and both remain live. Affects cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_only_forecloses_study_as_exercise, conceptual, 'Logical compatibility of performance-only axiom with study-as-exercise axiom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 1945).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.45).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__performance_only, theater_ratio, 500, 0.52).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__performance_only, theater_ratio, 1000, 0.58).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__performance_only, theater_ratio, 1500, 0.61).
narrative_ontology:measurement(temp_tr_t1800, temple_sacrifice_commitment__performance_only, theater_ratio, 1800, 0.62).
narrative_ontology:measurement(temp_tr_t1945, temple_sacrifice_commitment__performance_only, theater_ratio, 1945, 0.62).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__performance_only, base_extractiveness, 500, 0.2).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__performance_only, base_extractiveness, 1000, 0.18).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__performance_only, base_extractiveness, 1500, 0.17).
narrative_ontology:measurement(temp_be_t1800, temple_sacrifice_commitment__performance_only, base_extractiveness, 1800, 0.18).
narrative_ontology:measurement(temp_be_t1945, temple_sacrifice_commitment__performance_only, base_extractiveness, 1945, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__performance_only, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_commitment__performance_only, suppression_requirement, 500, 0.13).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_commitment__performance_only, suppression_requirement, 1000, 0.12).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_commitment__performance_only, suppression_requirement, 1500, 0.11).
narrative_ontology:measurement(temp_su_t1800, temple_sacrifice_commitment__performance_only, suppression_requirement, 1800, 0.12).
narrative_ontology:measurement(temp_su_t1945, temple_sacrifice_commitment__performance_only, suppression_requirement, 1945, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__performance_only, 0.12).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'temple_sacrifice_commitment.' All four readings (performance_only, study_as_exercise, hybrid_preparatory, symbolic_transformation) share the same kernel (the standing commitment to sacrifice law) but diverge in what that commitment binds and how it can be satisfied. Each reading produces a different constraint with a different ε, different beneficiary/victim structure, and different classification. The ε-invariance principle requires decomposition: the readings are not observables within a single constraint but rather separate constraints instantiated from the same kernel. This file instantiates the performance-only reading only; sibling readings are authored in separate constraint files, linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
