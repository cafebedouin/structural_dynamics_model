% ============================================================================
% CONSTRAINT STORY: constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_hybrid_reading, []).

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
 *   constraint_id: constitutional_hybrid_reading
 *   human_readable: Constitutional Hybrid Legitimacy: Symbolic Monarchy and Parliamentary Delegation
 *   domain: political_philosophy/constitutional_theory/legitimacy
 *
 * SUMMARY:
 *   The constitutional hybrid bifurcates sovereignty between symbolic
 *   continuity (grounded in hereditary monarchy) and operational legitimacy
 *   (grounded in parliamentary delegation). This constraint is ONE READING of
 *   the sovereign legitimacy kernel—the constitutional hybrid reading—which
 *   coexists with monarchical and republican readings that would classify the
 *   same institutional structure entirely differently. Under the hybrid
 *   reading, the Crown and Parliament coordinate rather than compete: the
 *   Crown maintains national identity, ceremonial authority, and continuity
 *   with historical tradition, while Parliament exercises policy authority,
 *   budgetary control, and electoral accountability. This reading achieves
 *   legitimacy by accepting that sovereignty need not be unified in a single
 *   source—it can be distributed across complementary institutions. The core
 *   beneficiaries are both institutions: the Crown gains legitimacy grounding
 *   from historical continuity and symbolic authority (without responsibility
 *   for unpopular policies), and Parliament gains legitimacy grounding from
 *   electoral accountability (without being held accountable for national
 *   ceremonial functions). The core victims are absolutist factions on both
 *   sides: monarchical traditionalists who believe the Crown should exercise
 *   operational authority, and republican purists who believe hereditary
 *   authority is categorically illegitimate. These factions experience the
 *   constraint as extraction because their foundational claims are suppressed
 *   in favor of the hybrid compromise. The constraint's theater ratio is high
 *   (0.62) because much of the constitutional apparatus consists of
 *   performative elements: ceremonial Crown-in-Parliament language,
 *   prerogative power frameworks that are exercised by Parliament de facto
 *   but held by the Crown de jure, and elaborate doctrinal scaffolding that
 *   legitimates what is fundamentally a pragmatic bifurcation rather than a
 *   principled legal doctrine.
 *
 * KEY AGENTS:
 *   - Crown Institution: Primary beneficiary (institutional/arbitrage) — maintains symbolic legitimacy, national continuity, and cultural authority without policy responsibility
 *   - Parliament Institution: Primary beneficiary (institutional/arbitrage) — exercises operational authority and electoral accountability without being held responsible for ceremonial/cultural continuity
 *   - Monarchical Absolutist Faction: Primary victim (powerless/identity_locked) — believes Crown should exercise sovereignty; suppressed by the hybrid arrangement
 *   - Republican Purist Faction: Primary victim (powerless/identity_locked) — believes hereditary authority is illegitimate; suppressed by the hybrid arrangement
 *   - The Electorate: Secondary agent (powerful/constrained) — benefits from institutional stability and symbolic continuity but bears cost of retaining hereditary constitutional powers
 *   - Constitutional Reform Movements: Organized agents (organized/mobile) — see the hybrid as temporary on path toward republicanism or crystallized constitutional form
 *   - Constitutional Scholarship: Institutional performer (institutional/arbitrage) — maintains theatrical legitimacy frameworks; sees hybrid as degraded compromise maintained by inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the hybrid as both genuine coordination achievement and as mechanism for suppressing absolutist claims on both sides
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_hybrid_reading, 0.24).
domain_priors:suppression_score(constitutional_hybrid_reading, 0.35).
domain_priors:theater_ratio(constitutional_hybrid_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_hybrid_reading, extractiveness, 0.24).
narrative_ontology:constraint_metric(constitutional_hybrid_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(constitutional_hybrid_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_hybrid_reading, "Constitutional Hybrid Legitimacy: Symbolic Monarchy and Parliamentary Delegation").
narrative_ontology:topic_domain(constitutional_hybrid_reading, "political_philosophy/constitutional_theory/legitimacy").

domain_priors:requires_active_enforcement(constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_hybrid_reading, 'da5a86ae-fa31-495e-85fc-14d72a6e9354').
narrative_ontology:cs_created_at('da5a86ae-fa31-495e-85fc-14d72a6e9354', '').
narrative_ontology:cs_kernel_codification('da5a86ae-fa31-495e-85fc-14d72a6e9354', formalized).
narrative_ontology:cs_authority_grounding('da5a86ae-fa31-495e-85fc-14d72a6e9354', lineage).
narrative_ontology:cs_interpretation_layer_present('da5a86ae-fa31-495e-85fc-14d72a6e9354').
narrative_ontology:cs_kernel_id(constitutional_hybrid_reading, sovereign_legitimacy).
narrative_ontology:cs_reading_relation('da5a86ae-fa31-495e-85fc-14d72a6e9354', monarchical_reading, coexists_with).
narrative_ontology:cs_reading_relation('da5a86ae-fa31-495e-85fc-14d72a6e9354', republican_reading, coexists_with).
narrative_ontology:cs_axiom('da5a86ae-fa31-495e-85fc-14d72a6e9354', foundational, dual_legitimation_coherent).
narrative_ontology:cs_axiom_status(dual_legitimation_coherent, holdable).
narrative_ontology:cs_axiom_grounding('da5a86ae-fa31-495e-85fc-14d72a6e9354', dual_legitimation_coherent, conventional).
narrative_ontology:cs_axiom('da5a86ae-fa31-495e-85fc-14d72a6e9354', foundational, sovereignty_divisible_across_institutions).
narrative_ontology:cs_axiom_status(sovereignty_divisible_across_institutions, holdable).
narrative_ontology:cs_axiom_grounding('da5a86ae-fa31-495e-85fc-14d72a6e9354', sovereignty_divisible_across_institutions, deontological).
narrative_ontology:cs_reference_frame('da5a86ae-fa31-495e-85fc-14d72a6e9354', dual_legitimation_principle).
narrative_ontology:cs_drift_state('da5a86ae-fa31-495e-85fc-14d72a6e9354', contemporary_democratic_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_hybrid_reading, crown_institution).
narrative_ontology:constraint_beneficiary(constitutional_hybrid_reading, parliament_institution).
narrative_ontology:constraint_victim(constitutional_hybrid_reading, monarchical_absolutism).
narrative_ontology:constraint_victim(constitutional_hybrid_reading, republican_purism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONALIST MONARCHICAL FACTION (SNARE) — Identity-fused with the belief that sovereignty inheres in the Crown alone. Parliamentary delegation is perceived as theft of authority. The faction is structurally trapped by its own identity commitment: exit requires abandoning the core claim that the monarch IS the state. Theater-heavy performative legitimacy (ceremonial, hereditary continuity rituals) with zero real policy authority. Maximum experienced extraction because the faction's voice is ritualized but not heeded.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: REPUBLICAN PURIST FACTION (SNARE) — Identity-fused with the belief that hereditary kingship is inherently illegitimate and incompatible with democratic authority. The constitutional hybrid is experienced as a defeat: monarchy persists structurally even if neutered functionally. Like the traditionalist faction, exit from this identity frame is unthinkable. The faction bears extraction because its core normative claim is systematically suppressed by the institutional reality of the hybrid (monarchy continues). Low material power, no exit.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CROWN INSTITUTION (ROPE) — Benefits from symbolic legitimacy and institutional continuity grounded in hereditary succession. The constraint is a coordination mechanism: the Crown coordinates national identity, ceremonial authority, and historical continuity without requiring policy expertise. Pure coordination with minimal extraction—the Crown has arbitrage options (can shift ceremonial focus, interpret symbolism, maintain cultural authority) and experiences the constraint as enabling rather than constraining. Theater-heavy (0.62) because ceremonial performance IS the crown's functional role.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARLIAMENT INSTITUTION (ROPE) — Benefits from operational legitimacy and policy authority grounded in parliamentary delegation. The constraint coordinates the division: Parliament governs policy; the Crown governs symbolic continuity. Both institutions have arbitrage options and experience the constraint as enabling rather than constraining. Parliament can evolve policy rapidly; the Crown maintains stable symbolic authority. The hybrid protects Parliament from being held accountable for all ceremonial/cultural functions and protects the Crown from being blamed for unpopular policies.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ELECTORATE (TANGLED ROPE) — Experiences both coordination benefit and asymmetric extraction. Coordination: hereditary succession removes electoral uncertainty about head-of-state selection, and ceremonial authority is delegated to a non-partisan actor. Extraction: the Crown retains constitutional powers (dissolution, assent, appointment) that Parliament controls de facto but the Crown technically holds. The electorate has powerful exit options (electoral change, constitutional reform) but faces suppression from institutional inertia. Mixed experience: benefits from institutional stability + symbolic continuity, but bears cost of retaining hereditary authority structure.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL REFORM COALITIONS (SCAFFOLD) — Organized agents (political parties, civil society) that view the hybrid as a temporary arrangement on a path toward either full republicanism or full constitutional clarity. The constraint is perceived as having a sunset: over generations, either the hybrid will stabilize as legitimate in its own right (new axiom: dual legitimation IS democratic), or electoral pressure will force a full transition to republicanism or pure monarchy. Reform movements have agency, organizational capacity, and visibility of exit paths. Theater persists but is expected to decline as constitutional legitimacy settles.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: CONSTITUTIONAL SCHOLARSHIP AND LEGAL COMMENTARY (PITON) — From the civilizational view, the hybrid is a degraded compromise maintained through institutional inertia and theatrical legal scholarship. The core coordination function (symbolic continuity + operational agility) has atrophied; what persists is the theater of constitutional fidelity and precedent. Scholars maintain elaborate doctrinal frameworks (Crown-in-Parliament, prerogative powers, reserve powers) that systematize what is fundamentally a structural ambiguity. Theater ratio is high (0.62) because scholarship performs legitimacy for an arrangement that is justified pragmatically ('it works') rather than principled.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER — LEGITIMACY PRESHEAF (TANGLED ROPE) — The civilizational analytical view sees the constraint as a genuine structural achievement: the hybrid distributes legitimacy sources across two institutions, reducing the risk of concentration and preventing any single actor from controlling both symbol and policy. But the constraint also carries asymmetric extraction: absolutist factions on both sides experience suppression of their core claims, and constitutional language is dense with theatrical performance of legitimacy. The analytical observer sees the hybrid as a coordination-with-extraction mechanism that has stabilized into a new constitutional form (not an intermediate state). Base extractiveness is low (0.24) because the coordination function is genuine and robust; suppression (0.35) reflects the genuine costs to purism on both sides.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_hybrid_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.24): Low-to-moderate. The hybrid achieves genuine coordination by distributing legitimacy sources and reducing concentration risk. Both beneficiary institutions experience net benefit with minimal extraction from each other. The extractiveness rises from the suppression of absolutist factions (monarchical and republican) whose core claims are systematically overridden by the hybrid compromise. Over the 100-year interval, extractiveness has increased slightly (0.18 → 0.24) as constitutional practice has crystallized the hybrid and reduced prospects for full transition to either monarchism or republicanism—absolutist factions face longer time horizons before any possible resolution. Suppression (0.35): Moderate. Structural barriers to exit for absolutist factions include: (1) the hybrid is entrenched in constitutional text and 300+ years of precedent; (2) transitioning to pure monarchy or pure republicanism requires either coup (monarchy) or constitutional reform (republic), both high-cost; (3) both factions have identity locks preventing them from accepting the hybrid as legitimate on principle. However, suppression is not extreme because democratic and reform mechanisms exist (electoral pressure, constitutional amendment, argumentative contestation). Theater ratio (0.62): Moderate-high. Constitutional frameworks invoke Crown-in-Parliament language, prerogative powers, reserve powers, and formal assent, all of which are performative—the Crown's constitutional role is largely ceremonial and symbolic. Parliament exercises these powers de facto while the Crown holds them de jure. This dual-track system requires dense theatrical apparatus to maintain: doctrinal literature explaining how the Crown's powers are really Parliament's, ceremonial protocols emphasizing Crown authority while deferring to parliamentary will, constitutional law courses teaching students that the sovereign is Crown-in-Parliament (a hybrid entity that unifies two sources). The theater supports the legitimacy of the bifurcation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival split across identity axes. Beneficiary institutions (Crown, Parliament) classify it as Rope—pure coordination with minimal extraction. Absolutist factions (both monarchical and republican) classify it as Snare—they are trapped by their identity commitments and suppressed by the arrangement. The electorate, with powerful exit options, classifies it as Tangled Rope—genuine coordination benefits (stability, symbolic continuity) paired with asymmetric extraction (retention of hereditary powers). Reform movements see it as Scaffold—a temporary arrangement on a path to eventual resolution (toward republicanism or crystallized hybrid legitimacy). The scholarly apparatus sees it as Piton—a degraded ritual maintained by inertia. The analytical observer sees it as Tangled Rope at the civilizational horizon—the arrangement is a genuine structural achievement (dual legitimation) that carries real costs for absolutist factions. The perspectival gap reveals that the constraint's classification is partially reading-dependent: whether one sees the hybrid as a legitimate coordinate system or as an illegitimate compromise depends on which reading of sovereignty (monarchical, republican, or hybrid) one accepts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d values for each perspective are derived from power level, exit options, and beneficiary/victim status. Beneficiary institutions (Crown, Parliament) with arbitrage options have low d values (≈0.05–0.15), producing negative or minimal f(d) and thus near-zero or negative χ—they experience minimal extraction because they benefit from the arrangement. Absolutist factions with identity locks and no structural exit have high d values (≈0.85–0.95), producing high f(d) and high χ—they experience maximum extraction because their core normative claims are suppressed and they cannot adopt alternative frames without abandoning identity. The electorate with powerful exit options but constrained by institutional inertia has moderate d (≈0.50–0.60), producing moderate f(d) and moderate χ. The analytical observer with analytical context has canonical d ≈0.73, producing f(d) ≈1.15 and proportional χ. Reform movements with organized power and mobile exit have lower d (≈0.35–0.45), producing lower f(d). The directionality profile shows asymmetric extraction flowing FROM absolutist factions TO beneficiary institutions, mediated by the institutional structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the reading structure itself. This constraint resolves the paradox (Mountain vs. Snare vs. Rope) by refusing to admit a unified classification: the constraint IS all three from different readings. Under the monarchical reading, it is a Snare (illegitimate delegation of royal authority). Under the republican reading, it is a Snare (illegitimate retention of hereditary authority). Under the hybrid reading, it is a Tangled Rope (legitimate coordination with costs borne by absolutist factions). The mandatrophy-resolver is the acceptance that legitimacy is reading-dependent, and that institutional arrangements can coordinate across legitimacy sources even when those sources remain formally contradictory. The constraint is stable not because it resolves the contradiction but because it institutionalizes it—making contradiction itself functional through the separation of symbolic and operational authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_legitimacy_convergence,
    'Do the two legitimacy sources (hereditary/symbolic and democratic/operational) converge toward a single integrated legitimacy system, or remain fundamentally distinct and potentially contradictory?',
    'Historical tracking of constitutional doctrine (whether courts, legislatures, and commentators increasingly treat the Crown-in-Parliament as a unified source or as two sources held in tension); analysis of whether contemporary political actors invoke both sources or default to one; measurement of public understanding of the locus of sovereignty',
    'Convergence would stabilize the hybrid and reduce theater ratio—the arrangement would become genuinely coherent. Divergence would increase suppression and extraction—the arrangement would persist as a compromise rather than a principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_legitimacy_convergence, empirical, 'Whether dual legitimation sources converge or remain distinct').

omega_variable(
    republican_pressure_trajectory,
    'Is the long-term trajectory of constitutional evolution toward republicanism (removal of the Crown as a constitutional actor), toward strengthened monarchy (reassertion of Crown prerogatives), or toward stable equilibrium at the current hybrid point?',
    'Comparative constitutional history across Westminster systems (Canada, Australia, New Zealand relative to UK and other monarchies); measurement of republican sentiment and reform activity; analysis of which institutional powers are actually exercised vs. held in abeyance',
    'Toward republicanism: the constraint is a scaffold with a genuine sunset toward republic. Toward monarchy: suppression of republican voices would increase, extractiveness would increase. Toward equilibrium: the hybrid achieves legitimacy in its own right and the scaffold classification is premature—reclassify as stable tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(republican_pressure_trajectory, empirical, 'Long-term trajectory toward republicanism, monarchism, or stable hybrid').

omega_variable(
    reading_committer_structure,
    'Is this constraint''s classification determined by the constitutional text (reading-invariant), or does it depend on which reading of constitutional legitimacy (monarchical, republican, hybrid) an actor endorses?',
    'The committer frame structure itself: this story instantiates one reading among siblings (monarchical, republican). The classification as tangled rope depends on accepting that dual legitimation IS coherent. A monarchical reading would classify the same constraint as a snare (illegitimate delegation of sovereign power). A republican reading would classify it as a snare (illegitimate retention of hereditary authority). The committer structure is the irreducible uncertainty.',
    'If reading-invariant (ε, suppression stable across readings): the constraint is structurally stable. If reading-dependent: the constraint''s classification floats across the six types depending on which sibling reading the observer accepts. This is not a practical weakness but a conceptual feature: legitimacy itself is reading-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_structure, conceptual, 'Kernel-reading ambiguity: whether classification depends on which legitimacy reading is adopted').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_hybrid_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(conhyb_tr_t0, constitutional_hybrid_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(conhyb_tr_t50, constitutional_hybrid_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(conhyb_tr_t100, constitutional_hybrid_reading, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(conhyb_be_t0, constitutional_hybrid_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(conhyb_be_t50, constitutional_hybrid_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(conhyb_be_t100, constitutional_hybrid_reading, base_extractiveness, 100, 0.24).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_hybrid_reading, monarchical_reading).
narrative_ontology:affects_constraint(constitutional_hybrid_reading, republican_reading).

% DUAL FORMULATION NOTE:
% The sovereign legitimacy kernel has three constraint readings: monarchical (pure monarchy authority), republican (pure democratic authority), and constitutional_hybrid (bifurcated authority). Each reading instantiates a distinct constraint with different ε values, beneficiary/victim structures, and classifications. This story (constitutional_hybrid_reading) links to its sibling readings via network edges, indicating conceptual dependence (all interpret the same kernel) and structural competition (adoption of one reading constrains adoption of others within a single unified political system, though different factions within the system can hold different readings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
