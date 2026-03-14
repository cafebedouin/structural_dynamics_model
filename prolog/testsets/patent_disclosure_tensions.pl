% ============================================================================
% CONSTRAINT STORY: patent_disclosure_tensions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_patent_disclosure_tensions, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: patent_disclosure_tensions
 *   human_readable: Patent Disclosure Tensions: Knowledge Monopoly vs. Public Innovation
 *   domain: intellectual_property/innovation_policy
 *
 * SUMMARY:
 *   Patent disclosure creates a structural tension between innovation
 *   incentives (requiring some form of monopoly exclusivity) and public
 *   knowledge advancement (requiring disclosure). The constraint exhibits all
 *   six DR classification types from different structural positions. Patent
 *   holders benefit from the monopoly; practitioners are trapped by the legal
 *   and search barriers; open-source coalitions are building alternative
 *   pathways; the patent office maintains a performative examination ritual;
 *   the system coordinates knowledge sharing while simultaneously extracting
 *   through exclusivity; and naturalizing observers risk treating the
 *   disclosure-exclusivity paradox as an immutable feature of innovation
 *   itself. The extractiveness has increased from 0.35 to 0.52 over 20 years
 *   as patent thicketing, defensive patenting, and application complexity
 *   have grown faster than examination capacity. The theater ratio has
 *   similarly increased from 0.42 to 0.58, reflecting that patent prosecution
 *   and examination are increasingly performative — claim language is
 *   designed for tactical advantage rather than clarity; prosecution
 *   strategies focus on legal argumentation rather than technical disclosure;
 *   and examination outcomes depend more on procedural skill than on
 *   substantive validity assessment.
 *
 * KEY AGENTS:
 *   - Patent Holders: Primary beneficiary (institutional/arbitrage) — capture monopoly rents from excluded practitioners; benefit from strategic licensing and cross-licensing
 *   - Field Practitioners: Primary victim (powerless/trapped) — face prohibitive patent search costs, freedom-to-operate barriers, and litigation risk; exit is not available
 *   - Subsequent Innovators: Secondary victim (moderate/constrained) — constrained by licensing costs and 18-month publication lag; can challenge or license but at significant expense
 *   - Public Knowledge Commons: Tertiary victim (powerless/identity_locked) — abstract collective good that cannot organize; constituted through the assumption that patent disclosure contributes to public knowledge; but disclosure happens too late (18-month lag) and exclusivity prevents use
 *   - Open Source/Creative Commons Coalition: Organized agents (organized/constrained) — building alternative frameworks (copyleft, GPL) that reframe or bypass patent extraction; have agency and exit pathways
 *   - Patent Office and Examination System: Institutional actor (institutional/arbitrage) — maintains the performative ritual; sees own process as degraded but persists through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the disclosure-exclusivity paradox as inherent to innovation rather than contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(patent_disclosure_tensions, 0.52).
domain_priors:suppression_score(patent_disclosure_tensions, 0.65).
domain_priors:theater_ratio(patent_disclosure_tensions, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(patent_disclosure_tensions, extractiveness, 0.52).
narrative_ontology:constraint_metric(patent_disclosure_tensions, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(patent_disclosure_tensions, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(patent_disclosure_tensions, tangled_rope).
narrative_ontology:human_readable(patent_disclosure_tensions, "Patent Disclosure Tensions: Knowledge Monopoly vs. Public Innovation").
narrative_ontology:topic_domain(patent_disclosure_tensions, "intellectual_property/innovation_policy").

domain_priors:requires_active_enforcement(patent_disclosure_tensions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(patent_disclosure_tensions, patent_holders).
narrative_ontology:constraint_beneficiary(patent_disclosure_tensions, innovation_incentive_framework).
narrative_ontology:constraint_victim(patent_disclosure_tensions, field_practitioners).
narrative_ontology:constraint_victim(patent_disclosure_tensions, subsequent_innovators).
narrative_ontology:constraint_victim(patent_disclosure_tensions, public_knowledge_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD PRACTITIONERS (SNARE) — Trapped practitioners face maximum extraction. Prior art searches impose time and cost barriers; freedom-to-operate analysis is prohibitively expensive for small innovators. Exit is not available: operating without patent clearance invites litigation or invalidation. The suppression mechanism is structural and total — legal, financial, and epistemic barriers lock practitioners into the system with no alternative pathways.
constraint_indexing:constraint_classification(patent_disclosure_tensions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUBSEQUENT INNOVATORS (TANGLED ROPE) — Constrained by patent licensing costs and disclosure delays (18-month publication lag for patent applications) but also benefit from the disclosure system itself. The patent system coordinates knowledge sharing through forced publication while enabling extraction through exclusivity. They can license, work around, or challenge patents, but at significant cost. Mixed coordination and extraction.
constraint_indexing:constraint_classification(patent_disclosure_tensions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PATENT HOLDERS (ROPE) — Institutional actors with portfolio diversification and arbitrage options. They experience the patent system as pure coordination: disclosure rules enable their monopoly control while the publication mechanism coordinates the broader field. They can license strategically, cross-license, or defend through litigation. Net beneficiary with full agency — extraction flows toward this agent.
constraint_indexing:constraint_classification(patent_disclosure_tensions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE/CREATIVE COMMONS COALITION (SCAFFOLD) — Organized agents building alternative pathways (GPL, FOSS licensing, copyleft mechanisms) that bypass or reframe patent extraction. These mechanisms have a sunset logic: as network effects grow for open-source communities, the patent monopoly's extraction leverage diminishes. Constrained by compatibility and adoption barriers but with clear exit pathway as alternatives mature. Theater is lower than traditional patent prosecution — enforcement is through community norm rather than litigation threat.
constraint_indexing:constraint_classification(patent_disclosure_tensions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PATENT OFFICE AND EXAMINATION SYSTEM (PITON) — High theater ratio (0.65+): patent examination is substantially performative. Examiners cannot evaluate non-obvious claim validity across domains; prior art searches are incomplete; claim language is opaque by design; prosecution procedures consume resources while producing contested legitimacy. The system persists through institutional inertia despite degraded function — maintained because alternatives haven't fully replaced it, not because examination reliably produces valid patents. Theater has increased as application complexity has grown faster than examination capacity.
constraint_indexing:constraint_classification(patent_disclosure_tensions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information disclosure creates inherent tension: once disclosed, information cannot be un-known; monopoly over knowledge is structurally unstable; the disclosure-exclusivity paradox is fundamental to patent law. However, this naturalizes what is actually a contingent institutional design choice. The 'inherent tension' framing obscures that alternative schemes (prize systems, patent buyouts, compulsory licensing) resolve the same problem without the suppression mechanism.
constraint_indexing:constraint_classification(patent_disclosure_tensions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(patent_disclosure_tensions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(patent_disclosure_tensions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(patent_disclosure_tensions, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(patent_disclosure_tensions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(patent_disclosure_tensions, TR),
    TR >= 0.70.

:- end_tests(patent_disclosure_tensions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over time. The base extraction reflects the monopoly premium that patent holders capture from practitioners who must license, design around, or exit the field. The increase from 0.35 to 0.52 reflects patent thicketing — defensive patenting strategies have created denser patent landscapes that impose higher search and litigation costs on practitioners, increasing the extraction mechanism's friction. Suppression (0.65): Moderate-high. Practitioners face legal barriers (infringement risk), financial barriers (FTO analysis, licensing costs), epistemic barriers (patent landscape opacity), and career risk (litigation exposure). The suppression is structural and systematic. Theater ratio (0.58): Moderate-high, increasing over time. Patent prosecution and examination have increasingly become procedurally focused: claim drafting is tactical (designed to pass examination and survive subsequent challenges rather than to communicate technical content); prosecution strategy focuses on legal argumentation; examination assessment relies on keyword searches and procedural compliance rather than substantive technical evaluation. The increase reflects growing application complexity outpacing examiner capacity and growing incentive misalignment (examiners are evaluated on throughput, not validity).
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between the holder's experience of coordination (disclosure rules protect my monopoly) and the practitioner's experience of extraction (the same disclosure rules prevent my innovation). The analytical observer risks a false mountain by naturalizing this as inherent to innovation. In fact, alternative schemes exist: patent buyouts (government purchases patents and releases them), prize systems (innovation rewards without monopoly), compulsory licensing thresholds (monopoly with mandatory licensing at regulated rates), or data commons models (structured knowledge sharing without exclusivity). Each alternative trades off different coordination costs, but the disclosure-exclusivity paradox is not inherent — it is a design choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Patent holders derive low directionality (d ≈ 0.15) from beneficiary status + arbitrage options — they can choose to license, cross-license, or defend strategically. Practitioners derive high directionality (d ≈ 0.90) from victim status + trapped exit — they face insurmountable legal and financial barriers with no alternative. The derived f(d) values map to experienced extractiveness chi: beneficiaries experience low/negative chi (coordination function); victims experience high chi (extraction). The tangency between coordination and extraction — both operate through the same disclosure-exclusivity mechanism — is what makes this a Tangled Rope rather than a Rope or Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the classification varies legitimately across structural positions. The patent holder's Rope (pure coordination) is their genuine structural experience. The practitioner's Snare (pure extraction) is their genuine structural experience. The open-source coalition's Scaffold (temporary coordination with sunset) is real — alternative frameworks ARE creating exit pathways. The patent office's Piton (performative ritual) is real — examination quality has degraded. The analytical observer's Mountain is a false summit — naturalizing the disclosure-exclusivity paradox obscures that the constraint is contingent on institutional design. The comprehensive view requires all perspectives: the patent system is simultaneously a coordination mechanism (for holders), an extraction mechanism (for practitioners), a degraded ritual (for the office), and a solvable coordination problem with alternative designs (from the analytical observer's position).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disclosure_scope_ambiguity,
    'What constitutes sufficient disclosure for a patent claim to be valid? Enablement standard vs. written description standard create conflicting measurement frames.',
    'Case law analysis and Federal Circuit precedent; empirical measurement of patent validity rates under different disclosure standards; survey of practitioners on sufficiency perception',
    'If disclosure standard is strict: more practitioners trapped (high suppression). If lenient: more patents invalidated post-grant, reducing monopoly extraction but increasing litigation uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disclosure_scope_ambiguity, empirical, 'Disclosure sufficiency standard ambiguity').

omega_variable(
    innovation_incentive_counterfactual,
    'Would equivalent innovation occur under alternative incentive schemes (prizes, patent buyouts, data commons) without the extraction mechanism?',
    'Historical analysis of innovation rates before/after patent reform; controlled comparison of innovation outcomes in jurisdictions with different IP regimes; economic modeling of alternative incentive structures',
    'If innovation is incentive-independent: patent monopoly is pure extraction (Snare). If innovation requires monopoly exclusivity: patent system is necessary coordination (Rope from broader perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_counterfactual, empirical, 'Whether patents are necessary for innovation incentives').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.65) structural (legal barriers, search costs) or partially internalized (practitioners self-censor from field entry due to perceived patent landscape)?',
    'Longitudinal surveys of field entry rates; exit interviews with practitioners who leave the field; comparison of patent filing density to practitioner population in adjacent non-patented fields',
    'If structural: suppression persists as long as patent system exists. If internalized: suppression would partially persist even post-patent reform (practitioners believe the barrier exists when it doesn''t).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in patent landscape').

omega_variable(
    patent_quality_degradation_rate,
    'What is the actual rate of patent validity degradation due to insufficient disclosure or prior art misclassification? Theater ratio may mask systematic quality loss.',
    'Post-grant review data; inter partes review outcomes; validity rates in invalidation proceedings; comparison to patent prosecution timeline and examiner workload trends',
    'If high degradation: theater ratio is understated, and the piton classification is more severe. If low: patent examination maintains baseline function despite appearance of theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patent_quality_degradation_rate, empirical, 'Patent validity degradation rate over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(patent_disclosure_tensions, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(patdis_tr_t0, patent_disclosure_tensions, theater_ratio, 0, 0.42).
narrative_ontology:measurement(patdis_tr_t10, patent_disclosure_tensions, theater_ratio, 10, 0.5).
narrative_ontology:measurement(patdis_tr_t20, patent_disclosure_tensions, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(patdis_be_t0, patent_disclosure_tensions, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(patdis_be_t10, patent_disclosure_tensions, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(patdis_be_t20, patent_disclosure_tensions, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(patent_disclosure_tensions, information_standard).
narrative_ontology:affects_constraint(patent_disclosure_tensions, trade_secret_vs_patent_tradeoff).
narrative_ontology:affects_constraint(patent_disclosure_tensions, software_patent_eligibility_ambiguity).
narrative_ontology:affects_constraint(patent_disclosure_tensions, innovation_commons_access_barriers).

% DUAL FORMULATION NOTE:
% Patent disclosure tensions decompose into domain-specific constraints: software patent eligibility (ε=0.38, Tangled Rope), trade secret strategy (ε=0.45, Snare), and commons access barriers (ε=0.55, Snare). Each has different disclosure standards, suppression mechanisms, and beneficiary/victim structures. The family shares the core disclosure-exclusivity paradox but manifests differently across biotechnology, software, mechanical systems, and chemical patents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(patent_disclosure_tensions, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
