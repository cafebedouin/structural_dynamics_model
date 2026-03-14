% ============================================================================
% CONSTRAINT STORY: religious_organizational_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_religious_organizational_capture, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: religious_organizational_capture
 *   human_readable: Religious Organizational Capture
 *   domain: institutional/religious/governance
 *
 * SUMMARY:
 *   Religious organizational capture occurs when a religious institution's
 *   stated mission (spiritual practice, community care, doctrinal fidelity)
 *   becomes subordinated to institutional self-preservation, administrative
 *   priorities, and leadership wealth accumulation. The constraint is
 *   structurally identical to regulatory capture (captured regulator
 *   subordinates public interest to regulated industry) but operates through
 *   identity-based binding rather than primarily through material barriers.
 *   Members are typically structurally mobile — they could exit, relocate, or
 *   practice independently — but are identity-locked: their self-concept,
 *   social bonds, and spiritual identity are constituted through the
 *   organization. This creates a hybrid coordination-extraction mechanism:
 *   the organization genuinely coordinates religious practice and community
 *   while simultaneously extracting resources and labor justified by
 *   spiritual framing. The constraint's evolution shows increasing theater
 *   (doctrine-action gap widening) and extractiveness (administrative
 *   overhead and mission drift accumulating). Six perspectives from different
 *   structural positions yield all six constraint types, making this a
 *   diagnostic exemplar for identity-based capture and perspectival
 *   variation.
 *
 * KEY AGENTS:
 *   - Rank-and-file believers: Primary victims (powerless/identity_locked) — bear extraction costs but cannot exit due to identity fusion; experience constraint as Snare
 *   - Committed practitioners: Secondary victims (moderate/constrained) — balance genuine coordination benefits against extraction; experience mixed Tangled Rope dynamic
 *   - Administrative leadership: Primary beneficiary (institutional/arbitrage) — captures institutional resources and decision-making power; experiences coordination function as primary
 *   - Institutional theologians: Captured intellectual class (institutional/constrained) — professionally identity-fused; coordinate intellectual work while constrained by organizational priorities
 *   - Reform coalition: Organized agents (organized/mobile) — see capture as solvable through governance structures; experience constraint as temporary Scaffold with sunset potential
 *   - Doctrinal apparatus: Formal theology and teaching structures (institutional/arbitrage) — increasingly performative; maintain legitimacy for administrative structure while disconnected from actual priorities
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing capture as inevitable rather than recognizing it as contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(religious_organizational_capture, 0.58).
domain_priors:suppression_score(religious_organizational_capture, 0.68).
domain_priors:theater_ratio(religious_organizational_capture, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(religious_organizational_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(religious_organizational_capture, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(religious_organizational_capture, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(religious_organizational_capture, tangled_rope).
narrative_ontology:human_readable(religious_organizational_capture, "Religious Organizational Capture").
narrative_ontology:topic_domain(religious_organizational_capture, "institutional/religious/governance").

domain_priors:requires_active_enforcement(religious_organizational_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(religious_organizational_capture, administrative_leadership).
narrative_ontology:constraint_beneficiary(religious_organizational_capture, institutional_power_holders).
narrative_ontology:constraint_victim(religious_organizational_capture, rank_and_file_believers).
narrative_ontology:constraint_victim(religious_organizational_capture, religious_mission_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RANK-AND-FILE BELIEVER (SNARE) — Structurally mobile (has housing, income, legal protections) but identity-fused with the organization. Exit would require abandoning their religious identity, community bonds, and life trajectory constituted through membership. Experiences the constraint as extraction: administrative priorities diverge from stated mission, resources flow to institutional maintenance rather than spiritual practice. Cannot perceive mutability from within identity frame despite structural mobility.
constraint_indexing:constraint_classification(religious_organizational_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: COMMITTED PRACTITIONER (TANGLED ROPE) — Genuinely coordinates religious practice with others (the organizational structure enables community worship, spiritual guidance, mutual support). Also experiences extraction: administrative overhead, leadership wealth accumulation, mission drift toward institutional growth rather than spiritual depth. Exit is possible (relocate, change denominations, practice independently) but carries high cost (loss of community, decades of invested relationships, children's social bonds).
constraint_indexing:constraint_classification(religious_organizational_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADMINISTRATIVE LEADERSHIP (ROPE) — Benefits from the organizational coordination structure. Experiences the constraint as solving collective action problems: managing finances, maintaining buildings, coordinating services. Net beneficiary due to arbitrage options (can move to other religious organizations, secular nonprofits, or commercial sectors). Experiences coordination function as primary; extraction is invisible to them.
constraint_indexing:constraint_classification(religious_organizational_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL THEOLOGIAN (TANGLED ROPE) — Professional identity fused with the organization; genuinely coordinates theological work and religious scholarship. Also experiences extraction: the organization constrains what theological directions can be publicly explored, scholarly resources are allocated to mission-serving rather than truth-seeking inquiries. Identity-locked exit would require abandoning professional standing. Differently positioned than leadership: sees both real function and extractive constraint.
constraint_indexing:constraint_classification(religious_organizational_capture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized agents (lay boards, accountability networks, transparency initiatives, independent oversight bodies) see the organizational capture as solvable through governance reform. Low effective extraction from their position because they have agency and exit options (move to reformed organizations, build parallel structures). Sunset logic: governance reforms (financial transparency, democratic decision-making, leadership term limits) can restructure the constraint within a generational timeframe.
constraint_indexing:constraint_classification(religious_organizational_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DOCTRINAL APPARATUS (PITON) — The formal teachings and theological positions of the organization have become largely performative, disconnected from actual resource allocation and operational priorities. The theology persists through institutional inertia and maintains legitimacy for the administrative structure, but actual decision-making is driven by institutional survival, financial management, and political considerations. Theater ratio: 0.65 reflects doctrine-action gap.
constraint_indexing:constraint_classification(religious_organizational_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION (MOUNTAIN) — Risk of false summit: the analytical frame may naturalize organizational capture as an inevitable consequence of scale ('hierarchies always become extractive,' 'power corrupts universally'). This perspective treats capture as a natural law rather than a contingent institutional arrangement. However, the structural data reveals this as false naturalization: the separation of stated mission from actual resource allocation is a design choice, not a law of nature. Alternative organizational forms (democratic governance, radical transparency, distributed leadership) demonstrate the constraint is not immutable.
constraint_indexing:constraint_classification(religious_organizational_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(religious_organizational_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(religious_organizational_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(religious_organizational_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(religious_organizational_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(religious_organizational_capture, TR),
    TR >= 0.70.

:- end_tests(religious_organizational_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The organization extracts through multiple mechanisms: administrative overhead diverts resources from stated mission, leadership compensation exceeds sector norms, facilities maintenance prioritizes prestige over spiritual function, and member labor is leveraged through theological framing of service as spiritual obligation. However, extraction is not total (0.70+) because genuine coordination does occur — the organization coordinates religious practice, provides community, and delivers some stated services. The extraction is hidden within coordination. Suppression (0.68): High. Multiple suppression mechanisms operate: identity-lock prevents cognitive exit (members cannot imagine leaving without losing identity), social isolation reduces exit information (communities discourage contact with those who leave), theological framing redefines extraction as sacrifice (resource extraction is relabeled as spiritual discipline), and economic dependency (some members rely on organization for housing, employment, or healthcare). Suppression is both structural and internalized. Theater ratio (0.65): Moderate-high. Doctrine has become increasingly disconnected from resource allocation. Theology teaches detachment from material things; administration accumulates wealth. Doctrine emphasizes service; administrative decisions prioritize institutional growth. Teaching about mission and authentic practice; actual decisions reflect institutional self-preservation. The gap has grown over the 20-year measurement interval as administrative complexity has increased while theological fidelity has become performative.
 *
 * PERSPECTIVAL GAP:
 *   The maximum gap lies between leadership (rope: low extraction, high exit, beneficiary) and believers (snare: high extraction, identity-locked exit, victim). Same organization, same constraint, opposite experienced types. The gap reveals that deferential realism correctly models the constraint's structure: institutional designs that exploit identity-fusion create extractive constraint systems that appear as coordination to beneficiaries. The analytical observer's mountain perspective represents the risk that we naturalize capture as inevitable rather than recognizing it as a design consequence of combining identity-based belonging with hierarchical authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership: beneficiary + arbitrage exit → d≈0.15 (low experienced extraction). Believers: victim + identity_locked exit → d≈0.89 (high experienced extraction). The pipeline computes f(d) via sigmoid, which produces f(0.15)≈-0.01 (leadership experiences negative extraction — benefits exceed costs) and f(0.89)≈1.28 (believers experience high extraction multiplier). This produces the chi asymmetry: leadership sees low chi (beneficial coordination), believers see high chi (extractive snare). The identity_locked exit option is key: it's structurally equivalent to trapped in terms of effective immobility, but conceptually distinct — the immobility is cognitive/identity-based rather than material. This distinction only matters for the biographical time horizon: at biographical, identity_locked → rope (can change identity frame in principle), but trapped → mountain (immutable regardless of frame). For believers in this constraint, identity shift is difficult but possible (though rare), whereas trapped agents cannot shift without external intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves mandatrophy by showing that organizational capture can be simultaneously Rope (for beneficiaries with arbitrage options) and Snare (for identity-locked victims) from the same base properties. The mandatrophy resolution principle states: when a constraint appears to be both coordination and extraction, check whether different agents have asymmetric exit options or identity-fusion. If yes, it's Tangled Rope (mixed) or perspectival variation of Rope/Snare. In this case, the leadership genuinely coordinates (Rope), the believers genuinely suffer extraction (Snare), and the organization contains both. The constraint is not mislabeled — it's a Tangled Rope that appears as different types to different agents. The theater ratio increase (0.35→0.65) indicates doctrine-action gap widening, confirming extraction layering onto coordination. The extractiveness increase (0.28→0.58) shows accumulation: initial coordination function is intact but extraction has been added and is growing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_permeability,
    'Is the identity lock holding faithful members truly cognitive (internalized framing) or is it primarily structural (economic dependency, social isolation, legal barriers)?',
    'Post-exit surveys: do members who leave report sustained identity distress, or did identity reorganize after structural exit? Longitudinal tracking of members who attempted exit and returned vs those who successfully disaffiliated.',
    'If primarily cognitive: the constraint is changeable if identity frames shift (biography → generational). If primarily structural: the constraint is effectively mountain-like for that agent (remains immutable across time horizons). Classification shifts from rope (identity_locked at biographical) to mountain (trapped at biographical).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_permeability, empirical, 'Cognitive vs structural basis of identity lock in members').

omega_variable(
    extraction_versus_overhead,
    'What proportion of administrative cost and resource diversion is genuine coordination overhead vs illegitimate extraction (salary inflation, nepotism, luxurious facilities)?',
    'Comparative accounting: cost-of-services analysis across organizations of similar size and function; benchmarking administrative overhead ratios; investigation of leadership compensation packages relative to sector norms.',
    'If overhead ≤ 10%: constraint reclassifies as pure Rope (low-extraction coordination). If overhead 15-30%: Tangled Rope classification confirmed. If overhead > 30%: reclassifies toward Snare (extraction-dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_versus_overhead, empirical, 'Proportion of costs attributable to overhead vs illegitimate extraction').

omega_variable(
    theological_constraint_independence,
    'Is the theological system genuinely independent from organizational capture, or has doctrine itself been captured to justify administrative priorities?',
    'Doctrinal genealogy analysis: tracking when theological positions shifted relative to resource allocation shifts; comparing stated doctrine to historical doctrine; assessing whether theology explains or justifies observed institutional behavior.',
    'If theology is independent: the constraint is organizational (admin captured) but theology remains a potential reform lever (can appeal to authentic doctrine against corrupted practice). If theology is captured: the constraint extends to epistemology itself — the framework for criticism has been colonized. Reclassifies victims from ''rank-and-file'' to ''epistemology of authenticity.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_constraint_independence, conceptual, 'Whether theology has been captured or remains independent').

omega_variable(
    reform_pathway_viability,
    'Can governance reform (transparency, democratization, term limits, accountability structures) actually resolve the organizational capture, or do such reforms get co-opted during implementation?',
    'Longitudinal case studies of organizations that attempted governance reform: tracking whether reforms produce measurable behavior change or become performative theater; studying reform movements that succeeded vs those that failed or were reversed.',
    'If reforms succeed: Scaffold classification is accurate, sunset is real, generational timeframe is plausible. If reforms are co-opted: Scaffold perspective is aspirational rather than structural; the constraint may be more permanent than Scaffold suggests. Organization may require external intervention rather than internal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_pathway_viability, empirical, 'Whether internal governance reforms can resolve capture').

omega_variable(
    competitive_religious_market,
    'Does the existence of competing religious organizations and alternative spiritual paths constitute a real exit option (making the constraint Rope/Tangled Rope) or is religious identity so fused that switching organizations/traditions is effectively impossible (making the constraint Snare)?',
    'Exit rate analysis: what proportion of members actually leave for competing organizations vs those who leave religion entirely? Measuring cost-of-exit by tracking outcomes for those who switch vs those who disengage entirely.',
    'If switching is viable: organizational capture is constrained by competitive pressure; members have effective exit option (mobile); χ reduces. If switching is impossible: members are trapped despite market alternatives; identity_locked dominates; χ increases and Snare classification becomes more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_religious_market, empirical, 'Whether competitive religious market provides real exit options').

omega_variable(
    suppression_internalization_split,
    'What portion of suppression (0.68) is structural (legal/financial/social barriers to leaving) vs internalized (members believe they should stay despite recognizing extraction)?',
    'Survey design distinguishing perceived barriers (structural) from felt obligation (internalized). Experimental design: would members leave if structural barriers were removed? Post-exit analysis: does suppression persist after structural separation?',
    'If suppression is primarily structural: removing barriers reduces chi substantially. If suppression is primarily internalized: removing barriers may not reduce extraction experience — members carry suppression with them. Suggests need for deprogramming or identity-frame intervention beyond structural reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural vs internalized basis of suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(religious_organizational_capture, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(relorg_tr_t0, religious_organizational_capture, theater_ratio, 0, 0.35).
narrative_ontology:measurement(relorg_tr_t10, religious_organizational_capture, theater_ratio, 10, 0.5).
narrative_ontology:measurement(relorg_tr_t20, religious_organizational_capture, theater_ratio, 20, 0.65).
narrative_ontology:measurement(relorg_tr_t5, religious_organizational_capture, theater_ratio, 5, 0.42).

% Extraction over time
narrative_ontology:measurement(relorg_be_t0, religious_organizational_capture, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(relorg_be_t10, religious_organizational_capture, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(relorg_be_t20, religious_organizational_capture, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(relorg_be_t5, religious_organizational_capture, base_extractiveness, 5, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(religious_organizational_capture, attachment_coordination).
narrative_ontology:boltzmann_floor_override(religious_organizational_capture, 0.12).
narrative_ontology:affects_constraint(religious_organizational_capture, religious_institutional_identity_formation).
narrative_ontology:affects_constraint(religious_organizational_capture, theological_epistemology_capture).

% DUAL FORMULATION NOTE:
% Religious organizational capture is downstream of identity-formation mechanisms (how the organization constitutes member identity) and upstream of theological capture (whether doctrine itself has been colonized to justify administrative structures). This story focuses on resource extraction and mission drift. The identity-formation story focuses on how belonging is constructed and maintained. The theology story focuses on epistemic capture — whether critique is possible from within the doctrinal system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(religious_organizational_capture, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
