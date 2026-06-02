% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__public_scaffold_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Constitutional Mandate for Public Domain Enrichment (Public Scaffold Reading)
 *   domain: intellectual_property_law/constitutional_law
 *
 * SUMMARY:
 *   This constraint models copyright under the PUBLIC SCAFFOLD READING of the
 *   copyright constitutional mandate kernel. The reading interprets
 *   copyright's constitutional justification (to promote the useful arts and
 *   sciences, per U.S. Constitution Article I §8; or to encourage authorship,
 *   per international conventions) as a commitment to temporary monopoly in
 *   service of public enrichment. The constraint is a scaffold because it
 *   contains built-in sunset logic: copyright terms are defined, not
 *   perpetual; fair use carves out access; and the public domain is the
 *   intended beneficiary of the bargain. The reader(s) instantiating this
 *   constraint explicitly reject the corporate enclosure interpretation
 *   (perpetual or near-perpetual exclusive rights) and the judicial ambiguity
 *   reading (copyright's purpose is indeterminate and subject to case-by-case
 *   balancing). This reading is live in contemporary jurisdictions and
 *   movements: Creative Commons, open-access publishing, fair-use advocacy,
 *   and term-limit proposals all instantiate this reading's axioms. The
 *   measurement trajectory shows modest extractiveness growth (0.08 → 0.22)
 *   driven by creeping term extensions and DRM enforcement, but the scaffold
 *   structure remains because constitutional and legislative mechanisms
 *   continue to affirm the public-good mandate, and organized actors
 *   (open-source movements, library coalitions) are actively defending the
 *   commons.
 *
 * KEY AGENTS:
 *   - Public Domain Commons: Structural beneficiary of the copyright bargain. Under this reading, the public domain is not a residual category but the primary stakeholder.
 *   - Downstream Creators: Organized agents (Creative Commons, open-source projects, library systems) defending fair use and commons enrichment. Constrained but mobile within the scaffold structure.
 *   - Original Creators: Institutional actors (authors, filmmakers, musicians, software developers) who benefit from copyright's coordination function while accepting bounded terms.
 *   - Publishing and Creative Industries: Institutional actors who depend on copyright for business models. Experience tangled rope: genuine coordination benefit alongside real extraction during the monopoly term.
 *   - Students and Researchers in Resource-Limited Regions: Powerless agents who experience extraction when copyright is weaponized against access. Their snare perspective validates the reading's normative claim.
 *   - Analytical Observer: Civilizational perspective capable of measuring whether actual practice honors the reading's commitments.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.22).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.35).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Constitutional Mandate for Public Domain Enrichment (Public Scaffold Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property_law/constitutional_law").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, 'a42630af-b5b4-4d54-9b1e-e9a9b2273e34').
narrative_ontology:cs_kernel_codification('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', formalized).
narrative_ontology:cs_authority_grounding('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', lineage).
narrative_ontology:cs_interpretation_layer_present('a42630af-b5b4-4d54-9b1e-e9a9b2273e34').
narrative_ontology:cs_reading_relation('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', foundational, copyright_term_bounded_necessity).
narrative_ontology:cs_axiom_status(copyright_term_bounded_necessity, holdable).
narrative_ontology:cs_axiom_grounding('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', copyright_term_bounded_necessity, instrumental).
narrative_ontology:cs_axiom('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', foundational, fair_use_constitutive_access).
narrative_ontology:cs_axiom_status(fair_use_constitutive_access, holdable).
narrative_ontology:cs_axiom_grounding('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', fair_use_constitutive_access, deontological).
narrative_ontology:cs_axiom('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', secondary, public_domain_default_endpoint).
narrative_ontology:cs_axiom_status(public_domain_default_endpoint, holdable).
narrative_ontology:cs_axiom_grounding('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', public_domain_default_endpoint, instrumental).
narrative_ontology:cs_reference_frame('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', bounded_monopoly_public_enrichment).
narrative_ontology:cs_drift_state('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', contemporary_post_sonny_bono, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a42630af-b5b4-4d54-9b1e-e9a9b2273e34', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain_commons).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, downstream_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC DOMAIN COMMONS (ROPE) — This reading positions the public domain as the primary beneficiary. Copyright exists to enrich, not to impoverish, the commons. The generational timescale allows new creators to build on prior work after reasonable monopoly periods expire. Mobile exit reflects that creators can choose to release work early, use Creative Commons, or publish in open-access venues. The public domain experiences this as coordination, not extraction — copyright's temporary exclusivity enables the resource transfer that fills the commons.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__public_scaffold_reading, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM CREATORS / OPEN CULTURE ADVOCATES (SCAFFOLD) — Organized agents (open-source movements, Creative Commons coalitions, library advocates) see copyright under this reading as a temporary problem with a sunset. The framework commits to periodic term shortening, expansion of fair use, and active commons enrichment. The sunset logic is structural: as copyright terms expire and fair use norms solidify, the monopoly phase ends and access expands. Constrained exit reflects that creators operate within copyright's constraints, but those constraints have built-in expiration dates and access-expansion mechanisms.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__public_scaffold_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PUBLISHING AND CREATIVE INDUSTRIES (TANGLED ROPE) — Industries dependent on copyright see genuine coordination function (they can predict market conditions and fund investment) alongside real extraction (they benefit from monopoly pricing during the term). This reading acknowledges the legitimate coordination role while committing to proportionality — terms should be no longer than necessary, and fair use should enable enough access to justify the exclusive period. Constrained exit reflects contractual obligations and sunk investment in copyright-dependent business models, but also the availability of alternative models (DRM-free, open-source, subscription).
constraint_indexing:constraint_classification(copyright_constitutional_mandate__public_scaffold_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ORIGINAL CREATOR / EARLY STAGE (ROPE) — Individual creators see copyright as a coordination mechanism: it enables them to capture value from their work and fund further creation. This reading gives them fair reward without perpetual privilege. Arbitrage exit reflects that creators can license conditionally, use open-access models, or pursue portfolio diversification. Immediate horizon captures the creator's need to fund work in the near term; the constitutional mandate ensures the mechanism exists without locking access forever.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__public_scaffold_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TRAPPED STUDENT / RESEARCHER (SNARE) — Students and researchers in resource-limited regions face extraction when copyright terms are extended indefinitely and access mechanisms (open-access journals, affordable textbooks, licensed archives) remain inaccessible due to pricing. This reading shows a snare classification when the constitutional mandate is violated — when copyright is weaponized for monopoly rather than bounded incentive. This perspective validates the reading's core claim: the contradiction reveals the constraint.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__public_scaffold_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUTIONAL READING (SCAFFOLD) — From a universal/civilizational perspective, copyright is a temporary institutional arrangement explicitly justified by enumerating its public-good purpose. The Framers (U.S. Constitution) or legislatures (Berne Convention) committed to bounded terms and public-benefit logic. This reading treats those commitments as operative — the sunset is real, the enrichment mandate is real, and the analytical task is to measure whether actual practice honors the commitment. Where it does not (term extensions, anti-circumvention locks, reduced fair use), the scaffold degrades. This perspective enables diagnostic observation.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__public_scaffold_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(copyright_constitutional_mandate__public_scaffold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyright_constitutional_mandate__public_scaffold_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.22): Low-to-moderate. Under the public scaffold reading, copyright's extractiveness is determined by how much of the work's productive life is monopolized before entering the public domain, weighted by the degree to which access is restricted during the monopoly term. A 14-year copyright with robust fair use and affordable licensing has lower extractiveness than a 120-year copyright with DRM locks and aggressive anti-circumvention enforcement. The measurement trajectory (0.08 → 0.22) reflects the historical drift from moderate copyright terms (1790 U.S. law: 14 years renewable to 28) toward extended terms (1976 onward: author's life + 50-70 years, later 95 years for corporate works), alongside the emergence of DRM and anti-circumvention mechanisms that functionally extend monopoly power beyond the legal term. The current value (0.22) reflects that while terms have expanded, the public domain is still recognized as the legitimate endpoint, and fair use norms persist. SUPPRESSION (0.35): Moderate. Suppression mechanisms include: (1) legal restrictions on circumventing DRM (Digital Millennium Copyright Act §1201), which prevent legitimate research and preservation; (2) copyright enforcement against non-commercial uses (YouTube Content ID, archival disputes) that chill downstream creation; (3) information asymmetry about what is in the public domain (orphaned works remain legally enclosed even after copyright term expiration). These are real barriers but not absolute — they can be overcome through litigation (fair use defense), legislative change (orphan works legislation), or technological workarounds. THEATER_RATIO (0.48): Moderate. The constraint involves some performative elements — copyright registration systems that are minimally enforced, term calculations that are unnecessarily complex, and fair-use doctrine that requires costly litigation to clarify — but the core coordination function (enabling creators to capture value and fund work) is genuine. The growth in theater_ratio over the measurement interval (0.32 → 0.48) reflects increasing complexity in compliance mechanisms (DRM, Content ID, license stacking) that add friction without proportional benefit.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives show the constraint through radically different lenses. The public domain and downstream creators see rope and scaffold — coordination that enables participation and sunset mechanisms that guarantee eventual access. Original creators and industries see rope and tangled rope — legitimate incentive structures alongside bounded monopoly. The trapped student sees snare — extraction when the reading's axioms are violated. The analytical observer sees scaffold — an institutional arrangement with built-in commitment to public good, but degrading as extractive pressures accumulate. The gap reveals that the reading's viability depends on whether the structural commitments (bounded terms, fair use, public-domain enrichment) are maintained in practice. When they are not — when terms extend indefinitely, fair use narrows, and DRM locks make works inaccessible even after copyright expiration — the constraint degrades from scaffold toward piton (performative commitment, actual extraction) or snare (pure extraction under the mask of incentive).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) reflects their structural relationship to copyright under this reading. The public domain and downstream creators are structural beneficiaries (low d, negative chi), so they experience copyright's temporary monopoly as acceptable cost for future access. Original creators are mixed beneficiaries (moderate d) — they benefit from the incentive but accept that their work will eventually enter the public domain. Industries are net beneficiaries during the monopoly term (moderate-to-high d depending on exit options) — they can arbitrage by licensing conditionally or adapting the reading into a purely extractive strategy, so their d reflects constrained exit. Trapped students are net targets (very high d) — they experience maximum extraction when copyright prevents access without offsetting benefit. The reading's coherence depends on maintaining the derivative structure: beneficiaries accept bounded terms because the public domain enrichment is real and legitimate; victims (those facing extraction-as-snare) are structural artifacts of the reading's violation, not intended outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not require mandatrophy resolution because its extractiveness (0.22) is below the 0.70 threshold. However, the reading's internal coherence depends on distinguishing two distinct structural scenarios: (1) COPYRIGHT AS SCAFFOLD — bounded terms, robust fair use, public-domain enrichment actually occurring. Extractiveness is low because the monopoly is temporary and the endpoint is transparency. Classification: scaffold. (2) COPYRIGHT AS PITON or SNARE (under different readings) — indefinite terms, eroded fair use, DRM-locked content that remains inaccessible even after copyright expiration. Extractiveness would be high (0.55+) because the monopoly persists and the endpoint is perpetual private control. Classification: piton (if maintained through inertia) or snare (if the monopoly is actively enforced). The measurement trajectory (rising extractiveness) warns that the constraint is drifting toward scenario 2. The reading's mandate is to maintain scenario 1 through active institutional commitment: defending bounded terms, expanding fair use, preventing anti-circumvention locks from overriding copyright expiration, and enriching the public domain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bounded_term_defensibility,
    'Is copyright''s constitutional justification compatible with indefinite or repeatedly-extended monopoly terms?',
    'Historical tracking of term length decisions and their stated rationale; comparison of actual term extensions against the original incentive-alignment thesis',
    'If indefinite terms are indefensible under the reading''s axioms: the constraint becomes a piton (performative commitment to public good, actual extraction). If bounded terms are maintained: the scaffold classification is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bounded_term_defensibility, conceptual, 'Whether bounded copyright terms remain defensible under the constitutional mandate').

omega_variable(
    fair_use_scope_sufficiency,
    'Does the current scope of fair use (as applied through judicial precedent and statute) provide genuine access for downstream creators and the public, or has it been eroded by interpretive narrowing?',
    'Empirical analysis of fair-use litigation outcomes; comparison of fair-use scope across jurisdictions and time periods; assessment of whether transformative use doctrine functionally enables derivative creation or merely permits scholarly quotation',
    'If fair use remains robust and expands over time: the public-good coordination function is maintained. If fair use narrows or fails in key cases: the reading''s axiom (copyright bounded by fair use) is overridden by practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_scope_sufficiency, empirical, 'Whether fair use scope is sufficient to support the public-good mandate').

omega_variable(
    public_domain_enrichment_trajectory,
    'Is the public domain actually growing as a percentage of human cultural production, or is copyright''s effective reach (including DRM, anti-circumvention locks, and term extensions) expanding faster than the domain is enriched by expiration?',
    'Quantitative analysis of public-domain composition and growth rates; measurement of ''orphaned works'' (copyrighted but owner not identified or locable); comparison of freely-usable cultural material before and after digitization',
    'If public domain is enriching faster than it is being enclosed: the scaffold''s sunset logic is real. If public domain is shrinking as a proportion of available cultural material: the constraint is degrading toward piton or false-summit (naturalizing extraction as constitutional mandate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_domain_enrichment_trajectory, empirical, 'Trajectory of public domain growth relative to copyright enclosure').

omega_variable(
    corporate_extension_mechanisms,
    'Are term extensions (e.g., Sonny Bono Copyright Term Extension Act) driven by genuine new incentive alignment, or by corporate lobbying to prevent works from entering the public domain?',
    'Legislative history analysis; identification of which stakeholders advocate for extensions; comparison of works entering public domain before and after extension laws; econometric analysis of whether extensions actually increase creation or merely protect existing rents',
    'If extensions prevent public-domain entry without increasing creation incentives: the constraint has drifted from scaffold toward snare or piton. The reading''s axioms are being overridden by practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corporate_extension_mechanisms, empirical, 'Whether copyright term extensions serve incentive or enclosure logic').

omega_variable(
    committer_reading_contest,
    'This is one reading of a contested kernel (copyright_constitutional_mandate). The corporate_enclosure_reading interprets the same constitutional text as authorizing perpetual or near-perpetual exclusive rights. Can both readings be held within a single legal framework?',
    'Analysis of constitutional text, legislative history, and judicial precedent under each reading; identification of which interpretive moves are necessary to sustain each reading; determination of whether the readings genuinely foreclose or merely coexist as competing doctrinal frameworks',
    'If the readings foreclose each other: one must be abandoned (or the kernel must be amended). If they coexist: the constraint''s ambiguity is structural, and the engine''s DR classification must track which reading is being applied in each jurisdiction/period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_contest, conceptual, 'Whether public-scaffold and corporate-enclosure readings of copyright kernel are logically foreclosed or structurally coexistent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copysc_tr_t0, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(copysc_tr_t50, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(copysc_tr_t100, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(copysc_be_t0, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(copysc_be_t50, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(copysc_be_t100, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 100, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(copysc_su_t0, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(copysc_su_t50, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 50, 0.28).
narrative_ontology:measurement(copysc_su_t100, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, information_standard).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, patent_term_extension_ratchet).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, drm_anti_circumvention_lock).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, orphaned_works_public_access).

% DUAL FORMULATION NOTE:
% Copyright_constitutional_mandate is a contested kernel with three structurally distinct readings (public_scaffold_reading, corporate_enclosure_reading, judicial_ambiguity_reading). Each reading produces a different constraint with different epsilon values, beneficiary/victim structures, and classification. Public scaffold reading: epsilon=0.22, no victims (coordination regime), claims type=scaffold. Corporate enclosure reading: epsilon=0.62+, victims=downstream creators/students, claims type=snare or tangled_rope. Judicial ambiguity reading: epsilon context-dependent, classification depends on which reading dominates in the specific case. All three are linked via network.affects_constraints to show that the same legal text instantiates different structural constraints depending on how the kernel is read.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
