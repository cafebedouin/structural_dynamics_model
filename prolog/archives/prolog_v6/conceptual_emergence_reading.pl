% ============================================================================
% CONSTRAINT STORY: conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conceptual_emergence_reading, []).

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
 *   constraint_id: conceptual_emergence_reading
 *   human_readable: Statute of Anne as Conceptual Space Emergence: IP as Learning Tool, Not Perpetual Property
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is conventionally read as a pragmatic solution
 *   to a printing-industry coordination problem: authors needed incentives,
 *   the public needed access, and the Stationers' Company's perpetual
 *   common-law copyright had become untenable. This reading focuses on a
 *   different achievement: the statute creates a conceptual space within
 *   legal thought itself. By establishing copyright as a LIMITED right—not
 *   perpetual property—the statute instantiates a fundamental distinction
 *   that becomes foundational to all subsequent IP law. The distinction
 *   between temporary regulatory rights and perpetual property becomes
 *   thinkable as a separate category. This is not merely a policy choice
 *   about how long exclusivity should last; it is the establishment of a new
 *   conceptual boundary in legal space. Once this boundary exists—once
 *   'limited copyright' has been established as a legitimate legal form
 *   distinct from both perpetual monopoly and unregulated commons—it becomes
 *   the invariant framework within which all subsequent parties operate. You
 *   cannot coherently argue for perpetual copyright without first
 *   acknowledging and then rejecting the statutory distinction between
 *   limited and perpetual. The statute thus creates the very conceptual
 *   opposition that makes all future IP debates possible.
 *
 * KEY AGENTS:
 *   - Public Learning Commons: Primary beneficiary (organized/mobile) — gains access to derivative works and subsequent creation once the temporal boundary is established
 *   - Contemporary Authors: Beneficiary (powerful/arbitrage) — gain clear incentive structure and known exclusivity period rather than contested perpetual claims
 *   - Statute Framers: Institutional actors who effect the conceptual move — create the legal distinction itself
 *   - Stationers' Company: Institutional actor in degradation (institutional/constrained) — prior monopoly regime persists performatively but is structurally displaced
 *   - Subsequent IP Doctrine: Dependent constituency (analytical/analytical) — all post-1710 IP systems inherit the limited/perpetual distinction as foundational
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conceptual_emergence_reading, 0.18).
domain_priors:suppression_score(conceptual_emergence_reading, 0.12).
domain_priors:theater_ratio(conceptual_emergence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conceptual_emergence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(conceptual_emergence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(conceptual_emergence_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conceptual_emergence_reading, rope).
narrative_ontology:human_readable(conceptual_emergence_reading, "Statute of Anne as Conceptual Space Emergence: IP as Learning Tool, Not Perpetual Property").
narrative_ontology:topic_domain(conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(conceptual_emergence_reading, '72c8aa61-6909-40a1-83ef-a460510f3b82').
narrative_ontology:cs_created_at('72c8aa61-6909-40a1-83ef-a460510f3b82', '').
narrative_ontology:cs_kernel_codification('72c8aa61-6909-40a1-83ef-a460510f3b82', formalized).
narrative_ontology:cs_authority_grounding('72c8aa61-6909-40a1-83ef-a460510f3b82', lineage).
narrative_ontology:cs_interpretation_layer_present('72c8aa61-6909-40a1-83ef-a460510f3b82').
narrative_ontology:cs_kernel_id(conceptual_emergence_reading, statute_of_anne_ip_foundation).
narrative_ontology:cs_reading_relation('72c8aa61-6909-40a1-83ef-a460510f3b82', institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_reading_relation('72c8aa61-6909-40a1-83ef-a460510f3b82', entangled_event_reading, influences).
narrative_ontology:cs_axiom('72c8aa61-6909-40a1-83ef-a460510f3b82', foundational, limited_copyright_distinct_category).
narrative_ontology:cs_axiom_status(limited_copyright_distinct_category, holdable).
narrative_ontology:cs_axiom_grounding('72c8aa61-6909-40a1-83ef-a460510f3b82', limited_copyright_distinct_category, conventional).
narrative_ontology:cs_axiom('72c8aa61-6909-40a1-83ef-a460510f3b82', secondary, learning_as_countervailing_interest).
narrative_ontology:cs_axiom_status(learning_as_countervailing_interest, holdable).
narrative_ontology:cs_axiom_grounding('72c8aa61-6909-40a1-83ef-a460510f3b82', learning_as_countervailing_interest, deontological).
narrative_ontology:cs_reference_frame('72c8aa61-6909-40a1-83ef-a460510f3b82', statutory_common_law_distinction).
narrative_ontology:cs_drift_state('72c8aa61-6909-40a1-83ef-a460510f3b82', contemporary_extension_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conceptual_emergence_reading, public_learning_commons).
narrative_ontology:constraint_beneficiary(conceptual_emergence_reading, derivative_works_creators).
narrative_ontology:constraint_beneficiary(conceptual_emergence_reading, subsequent_authors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC LEARNING COMMONS (ROPE) — The statute creates a genuine coordination function: limited copyright enables derivative works, translations, and cumulative learning while providing author incentives. The commons benefits from the temporal boundary that prevents perpetual monopoly. Exit via common law (prior regime) was constrained; the statute creates mobile options by making the learning timeline knowable and finite. Low extraction experienced because the beneficiary gains both coordination access and temporal certainty.
constraint_indexing:constraint_classification(conceptual_emergence_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: CONTEMPORARY AUTHOR (ROPE) — Experiences the statute as pure coordination: incentives for creation, defined exclusivity period, known reversion to commons. The author benefits from the mechanism itself (clear property rights) rather than from extraction of others. Arbitrage exit available — author can choose other incentive structures (patronage, licensing, collaborative modes) but finds statute terms favorable. Low suppression because the terms are transparent and voluntary engagement is possible.
constraint_indexing:constraint_classification(conceptual_emergence_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / CONCEPTUAL EMERGENCE (MOUNTAIN) — From civilizational scope, the statute instantiates a fundamental conceptual distinction: separating temporary economic rights from perpetual property, fixing authorship as a distinct legal category, and making 'learning' a legitimate countervailing interest against monopoly. This reading treats the statute's core achievement as a conceptual innovation — the creation of a new legal category (limited copyright as a species distinct from both monopoly and commons) that becomes the foundation for all subsequent IP doctrine. The classification approaches mountain because the conceptual space, once carved out, becomes the invariant framework within which all parties operate: you cannot un-think the distinction between property and limited right once it has been established as a legitimate legal category. However, empirical instantiation requires enforcement, so perfect mountain status requires additional conditions (see omega variables).
constraint_indexing:constraint_classification(conceptual_emergence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: STATIONERS' COMPANY (PITON) — The prior regime (perpetual common-law copyright under guild control) has degraded but persists in the company's institutional claims. The statute formally displaces perpetual monopoly but the company continues to perform its regulatory function through custom and inertia. The constraint operates as piton: the old monopoly rhetoric ('protection of investment') persists as performative justification for enforcement practices that the statute has structurally constrained. Theater ratio reflects that the company must now justify each enforcement action under the statute's terms rather than invoking perpetual right. Exit from guild membership was highly constrained pre-statute; the statute creates new exit pathways (legitimate printing outside guild) but the guild persists through institutional inertia.
constraint_indexing:constraint_classification(conceptual_emergence_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conceptual_emergence_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(conceptual_emergence_reading, TR),
    TR >= 0.70.

:- end_tests(conceptual_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. This reading does not claim extraction is occurring; rather, it focuses on coordination function and conceptual innovation. The statute provides incentives (beneficial to authors) while creating access (beneficial to commons). Neither party exploits the other; both benefit from the clarity of the boundary. Low extractiveness reflects genuine coordination rather than zero-sum asymmetry. Suppression (0.12): Very low. The statute is transparent about its terms. Copyright is granted for fixed term then reverts; authors know the boundary in advance; the public knows when works enter the commons. No hidden mechanisms or alternative-closure suppress exit options. Theater ratio (0.25): Low. The statute's operation is straightforward: time passes, copyright expires, work enters public domain. Subsequent complications (extension legislation, term expansion) occur after this reading's focal interval. The statute itself has minimal performative content — it does what it says it does.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives reflects different temporal and categorical horizons. Authors and the commons see coordination (rope) because they operate within the statute's terms and benefit from its clarity. The stationers see degradation (piton) because their prior regime has been structurally displaced even if some institutional continuity persists. The analytical observer sees a conceptual achievement that approaches mountain status because the distinction between limited and perpetual has become an unthinkable-to-undo category boundary in legal thought. The key diagnostic gap: is the statute's core function coordination (rope, focused on incentive clarity) or conceptual innovation (mountain-approaching, focused on establishing a foundational legal category)? This reading argues that the conceptual innovation is the achievement, and the coordination function is secondary to the categorical achievement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for this reading reflects beneficiary relationship without victimization. The public learning commons is the beneficiary (d ≈ 0.05 — full beneficiary). Contemporary authors are beneficiaries of clear incentive structure (d ≈ 0.15 — moderate beneficiary). Neither group is extracted from by this mechanism; both experience low or negative f(d) → low or negative effective extraction chi. The Stationers' Company occupies a complex position: institutional power, constrained exit (perpetual rights are displaced but guild persists), so d ≈ 0.50 → moderate f(d). The piton classification reflects that the guild's prior monopoly rhetoric persists performatively even though structurally displaced — theater ratio drives the classification more than extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy through precise framing: it claims a rope-level coordination function (low extraction, genuine benefit to both parties) founded on a conceptual innovation (the statute creates the category 'limited copyright' as distinct from perpetual right). The claim is not that the statute is a natural law (which would be false summit) but that it is a legitimate coordination mechanism with a foundational conceptual achievement. The rope classification captures that coordination is real (authors gain incentives, commons gains access) while acknowledging that the statute is an enacted institutional mechanism, not an inexorable feature of knowledge production.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_distinction_materialization,
    'Does the conceptual distinction between limited right and perpetual property constitute a genuinely invariant logical division, or does it remain observer-dependent and contestable?',
    'Historical analysis of subsequent legal doctrines: do all post-1710 IP systems presuppose this distinction as foundational? Do attempts to revive perpetual copyright require denying or reworking the conceptual boundary? Can the distinction be coherently rejected within a single legal framework?',
    'If the distinction is genuinely invariant: the constraint approaches mountain status — it becomes the unthinkable-to-undo conceptual foundation. If contestable: it remains rope-level coordination (reversible, framework-dependent), and mountain classification was aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_distinction_materialization, conceptual, 'Whether limited/perpetual distinction is invariant or framework-dependent').

omega_variable(
    kernel_codification_vs_emergent_reading,
    'Is this reading describing the statute itself (the codified kernel) or an emergent conceptual consequence that goes beyond the statute''s explicit text?',
    'Textual analysis of the Statute of Anne (1710): Does the statute explicitly construct the limited/perpetual distinction as a conceptual boundary, or does the reading import that distinction from subsequent IP scholarship and doctrine? Are the statute''s operative clauses sufficient to establish the distinction without interpretive addition?',
    'If textual: the statute is kernel-codifying the conceptual emergence directly; this reading is reading-off-the-statute. If emergent: the reading is a construction that subsequent doctrine imposed onto the statute; the statute itself is more about pragmatic incentive design than conceptual innovation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_codification_vs_emergent_reading, conceptual, 'Whether conceptual emergence is textually explicit or doctrinally reconstructed').

omega_variable(
    learning_commons_materialization,
    'Does the temporal limit on copyright actually produce a functioning learning commons, or does it merely create a theoretical possibility that subsequent extension legislation has continuously deferred?',
    'Empirical history of copyright expiration: What proportion of works have actually entered the public domain and been subject to reuse/learning derivative creation? How many statutory extensions have reset the commons boundary since 1710? What is the actual availability of works for learning purposes during the copyright term vs. post-expiration?',
    'If commons is materialized: rope classification is structurally correct — coordination function is real. If continuously deferred: the statute is performative (piton-level theater) rather than functionally coordinating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(learning_commons_materialization, empirical, 'Whether temporal limit produces actual public domain access for learning').

omega_variable(
    sibling_reading_framing_question,
    'Which reading is correct: this one (conceptual emergence of limited/perpetual distinction as foundational category) or the institutional_reallocation_reading (statute as reallocation of monopoly from guild to nation-state) or the entangled_event_reading (statute as entanglement of multiple simultaneous structures with no privileged conceptual outcome)?',
    'This is routed to omega because it is a committer-frame uncertainty. The three readings occupy different frameworks (conceptual foundations vs. institutional power flows vs. entangled multiplicity). No single empirical test resolves which is ''correct'' because they are asking structurally different questions. See reading_relations and axioms in cs_structure for how they coexist/influence/foreclose one another.',
    'If this reading''s foundation (distinct legal category) is the core dynamic: other readings are downstream applications of the conceptual move. If institutional reallocation is core: the conceptual emergence is epiphenomenal to the power transfer. If entanglement is core: all three are simultaneous and no hierarchy holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_framing_question, conceptual, 'Which framing of the Statute of Anne is foundational among the three readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conceptual_emergence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emerge_tr_t0, conceptual_emergence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(emerge_tr_t50, conceptual_emergence_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(emerge_tr_t100, conceptual_emergence_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(emerge_be_t0, conceptual_emergence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(emerge_be_t50, conceptual_emergence_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(emerge_be_t100, conceptual_emergence_reading, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conceptual_emergence_reading, information_standard).
narrative_ontology:affects_constraint(conceptual_emergence_reading, institutional_reallocation_reading).
narrative_ontology:affects_constraint(conceptual_emergence_reading, entangled_event_reading).

% DUAL FORMULATION NOTE:
% The Statute of Anne kernel has three distinct readings, each with different ε values and beneficiary/victim structures. Conceptual emergence (this file) focuses on the categorical boundary creation and achieves low extraction (ε=0.18, rope). Institutional reallocation would focus on power transfer from guild to state and likely achieves higher extraction for the state actor. Entangled event treats all structures as simultaneous with no hierarchy. These are three constraints, not three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
