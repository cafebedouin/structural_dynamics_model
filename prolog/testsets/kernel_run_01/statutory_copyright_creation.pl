% ============================================================================
% CONSTRAINT STORY: statutory_copyright_creation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_copyright_creation, []).

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
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: statutory_copyright_creation
 *   human_readable: Statutory Copyright Creation and the Statute of Anne (1710)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) marks the institutional moment when copyright
 *   became a legal category separate from general trade regulation. Yet the
 *   statute embodies an unresolved tension: it frames itself as protecting
 *   authors and encouraging learning ('for the Encouragement of Learning')
 *   while simultaneously perpetuating the Stationers' Company's monopoly
 *   control of the book trade. This creates a hybrid constraint combining
 *   genuine coordination (the statute does solve real problems of
 *   unauthorized reprinting and distribution) with asymmetric extraction (the
 *   statute benefits guild members and early copyright holders far more than
 *   the nominal beneficiaries — authors and the public). The constraint
 *   exhibits all six DR types from different perspectives, revealing how
 *   institutional innovations can naturalizes existing power structures in
 *   new legal language. The theater_ratio increases over time as the gap
 *   widens between the statute's public-interest rhetoric and its actual
 *   function as monopoly protection.
 *
 * KEY AGENTS:
 *   - Stationers' Company: Primary institutional beneficiary (powerful/mobile) — guild members capture perpetual copyright benefits through ownership and assignment strategies
 *   - Authors: Nominal beneficiaries (moderate/constrained) — gain legal protection but surrender copyright to booksellers through commercial necessity
 *   - Reading public: Primary victim (powerless/trapped) — face expanding monopoly on printed texts with no exit or alternative access
 *   - Competing printers outside guild: Secondary victim (moderate/trapped) — legally barred from entering the trade; statute legitimizes guild exclusion as property law
 *   - Parliament: Institutional actor (institutional/arbitrage) — frames monopoly extension as public interest; benefits from guild support and tax collection
 *   - Analytical observer (post-colonial context): Analytical position (analytical/analytical) — views copyright as contingent institutional arrangement rather than natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_copyright_creation, 0.52).
domain_priors:suppression_score(statutory_copyright_creation, 0.48).
domain_priors:theater_ratio(statutory_copyright_creation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_copyright_creation, extractiveness, 0.52).
narrative_ontology:constraint_metric(statutory_copyright_creation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(statutory_copyright_creation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_copyright_creation, tangled_rope).
narrative_ontology:human_readable(statutory_copyright_creation, "Statutory Copyright Creation and the Statute of Anne (1710)").
narrative_ontology:topic_domain(statutory_copyright_creation, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statutory_copyright_creation).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(statutory_copyright_creation, formalized).
narrative_ontology:cs_authority_grounding(statutory_copyright_creation, lineage).
narrative_ontology:cs_interpretation_layer_present(statutory_copyright_creation).
narrative_ontology:cs_reading_relation(statutory_copyright_creation, guild_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation(statutory_copyright_creation, knowledge_commons_reading, coexists_with).
narrative_ontology:cs_axiom(statutory_copyright_creation, foundational, authorial_labor_property_rights).
narrative_ontology:cs_axiom_status(authorial_labor_property_rights, holdable).
narrative_ontology:cs_axiom(statutory_copyright_creation, secondary, intellectual_goods_excludable).
narrative_ontology:cs_axiom_status(intellectual_goods_excludable, holdable).
narrative_ontology:cs_reference_frame(statutory_copyright_creation, author_as_natural_property_owner).
narrative_ontology:cs_drift_state(statutory_copyright_creation, contemporary_digital_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_copyright_creation, bookseller_guilds).
narrative_ontology:constraint_beneficiary(statutory_copyright_creation, authors).
narrative_ontology:constraint_victim(statutory_copyright_creation, public_learning).
narrative_ontology:constraint_victim(statutory_copyright_creation, competing_printers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: READING PUBLIC (SNARE) — Trapped within an expanding copyright monopoly with no exit mechanism. The public cannot avoid purchased books or create derivative works; trapped by both legal prohibition and economic dependency on printed texts. Maximum experienced extraction with zero alternatives.
constraint_indexing:constraint_classification(statutory_copyright_creation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETING PRINTERS (SNARE) — Structurally unable to enter the trade; trapped by guild enforcement and legal prohibition on unlicensed printing. The statute legitimizes what was formerly trade monopoly as legal monopoly, eliminating any hope of market entry. High coercion, minimal coordination benefit.
constraint_indexing:constraint_classification(statutory_copyright_creation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: BOOKSELLER GUILDS (TANGLED ROPE) — Genuinely coordinate the book trade (collective action against unauthorized printers, distribution networks, quality assurance) while extracting monopoly rents through perpetual copyright claims. Mobile — could exit by abandoning the trade — but benefits exceed costs. Hybrid: real coordination function + asymmetric extraction.
constraint_indexing:constraint_classification(statutory_copyright_creation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: AUTHORS (ROPE) — The statute ostensibly protects authors with limited copyright (14 years, renewable once). This is coordination: authors benefit from legal protection against unauthorized reprinting. Constrained exit (selling rights to booksellers trades one form of dependence for another) but genuine benefit. Low-extraction coordination from the author's perspective.
constraint_indexing:constraint_classification(statutory_copyright_creation, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PARLIAMENTARY FRAMING (PITON) — The statute is narrated as protecting authors ('for the Encouragement of Learning') but functions as guild privilege formalized in law. The legislative theater performs public-interest justification while delivering private monopoly benefit. High theater ratio — the normative framing has little functional relationship to actual extraction mechanisms. The statute persists through institutional inertia and legitimacy claims, not because the mechanism works efficiently.
constraint_indexing:constraint_classification(statutory_copyright_creation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL-LAW VIEW (MOUNTAIN) — From a Lockean labor-theory perspective, the statute enshrines a natural law: authors own the fruits of their intellectual labor. Copyright emerges as an inevitable category once authorship becomes economically valuable. This perspective naturalizes copyright as immutable law rather than contingent institutional arrangement. However, the beneficiary declarations and asymmetric extraction data will trigger FSM — revealing this as a false summit.
constraint_indexing:constraint_classification(statutory_copyright_creation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_copyright_creation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(statutory_copyright_creation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statutory_copyright_creation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_copyright_creation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(statutory_copyright_creation, TR),
    TR >= 0.70.

:- end_tests(statutory_copyright_creation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The statute creates legal monopoly on printed texts for 28 years maximum (14 + 14 renewable), extracting from the reading public and competing printers. However, the extraction is not maximal because (1) booksellers genuinely solve coordination problems (printing, distribution, quality control), (2) authors receive some benefit (though less than guild members), and (3) the time-limited nature (formally, at least) creates some public access expectation. The measured 0.52 reflects that coordination function exists alongside extraction — neither is trivial. Suppression (0.48): Moderate. Legal prohibition on unauthorized printing is a hard constraint (trapped agents cannot exit), but the suppression mechanism is not maximal because some market entry remains possible through waiting out copyright terms or engaging in gray-market practices. Theater ratio (0.58): Moderately high. Parliamentary framing emphasizes author protection and learning encouragement, but the statute's actual distribution of benefits flows primarily to guild members. The theater is substantial (the normative framing shapes legitimacy) but not maximal (guild monopoly is not purely performative — it does control actual distribution).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximum perspectival divergence. The bookseller guild sees rope — genuine coordination of the book trade with side benefits to themselves. Authors see rope — legal protection for their work. The reading public sees snare — expanding monopoly with no escape. Competing printers see snare — legal prohibition on entry. Parliament sees piton — the statute's theater of public interest masks institutional continuity with guild practice. The Lockean natural-law perspective sees mountain — copyright as inevitable once authors labor to create intellectual goods. The gap reveals the structural truth: what appears as coordination (rope) from the beneficiary's perspective is extraction (snare) from the victim's perspective. The statute resolves this by creating separate legal regimes for each actor, but the underlying asymmetry persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position: beneficiary guild members with arbitrage options experience low or negative effective extraction (d ≈ 0.20, producing rope classification); authors with constrained exits experience moderate extraction despite formal protection (d ≈ 0.55, producing rope because constrained + some benefit); the trapped reading public experiences maximum extraction (d ≈ 0.92, producing snare); competing printers trapped outside the guild experience high extraction (d ≈ 0.85, producing snare); Parliament as institutional beneficiary experiences arbitrage-level low extraction (d ≈ 0.15, rope). The analytical observer at civilizational scope derives d from the natural-law framing's beneficiary status — the Lockean framing benefits those who own intellectual property (d ≈ 0.15, mountain). However, the false-summit detector will flag this: the beneficiary declarations (bookseller guilds, authors) combined with the snare classifications from the public and competing printer perspectives reveal that the 'natural law' framing naturalizes a contingent extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing the kernel ambiguity: is copyright a natural category that emerged when authors became economically valuable, or a contingent institutional arrangement that beneficiaries (guild members, early copyright holders, landlords of intellectual property) have naturalized as inevitable? The statute itself is positioned at this juncture — it creates the legal possibility for copyright as a property category, but only by repackaging existing guild monopoly as author-centered language. The snare and tangled-rope classifications from different perspectives are both correct: the statute simultaneously coordinates the book trade (rope function) and extracts from the public (snare effect). The mandatrophy resolves when we recognize that the 'author protection' framing is genuine coordination language masking asymmetric extraction. The statute needed the author-protection narrative to justify what was functionally a guild monopoly extension. Without that narrative, the Stationers' Company's perpetual control could not be publicly legitimized. The theater ratio's increase over time (0.35 → 0.58) reflects exactly this: as the gap between the author-protection narrative and the actual distribution of benefits (primarily to guild members) becomes empirically obvious over the 28-year period, the theater ratio increases — more performative work is required to maintain legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    copyright_emergence_conceptual,
    'Did statutory copyright create a new conceptual category of intellectual property (intellectual goods as ownable), or did it merely repackage existing trade monopoly practices (guild control of printing) in a new legal language?',
    'Historical analysis of pre-1710 trade regulation language vs post-1710 copyright language; identification of conceptual rupture or continuity in how monopoly rights are justified',
    'If new category: copyright is a genuine institutional innovation with structural discontinuity. If repackaging: the statute creates the appearance of novelty while perpetuating unchanged extraction mechanisms under new framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(copyright_emergence_conceptual, conceptual, 'Whether copyright emerged as new concept or repackaged existing trade control').

omega_variable(
    bookseller_monopoly_persistence,
    'To what extent did the statute transform guild de-facto monopoly into legal de-jure monopoly, versus enabling genuinely new author participation in the copyright regime?',
    'Empirical analysis of copyright ownership distribution 1710-1730: percentage held by guild members vs authors vs other parties; tracking of author bargaining power before/after statute',
    'If transformed guild monopoly: the statute is primarily redistributive (taking guild monopoly and legitimizing it). If enabled author participation: the statute created genuine new access to IP rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bookseller_monopoly_persistence, empirical, 'Whether statute transformed guild monopoly or enabled author participation').

omega_variable(
    perpetual_copyright_intent,
    'Did booksellers accept the 14-year limit as genuine constraint on monopoly duration, or manipulate it through perpetual renewal, assignment, and legal fiction to maintain indefinite control?',
    'Legal history of perpetual copyright claims post-1710; analysis of how booksellers circumvented time limits through ownership transfer, copyright assignment fictions, and litigation strategy',
    'If accepted limit: statute achieved genuine time-limited protection. If circumvented: the formal limit is theater masking functional perpetuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetual_copyright_intent, empirical, 'Whether 14-year limit was sustained or circumvented toward perpetuity').

omega_variable(
    public_learning_coordinate,
    'Was public learning genuinely protected as a coordinate goal of the statute, or was it merely rhetorical cover for bookseller monopoly extension?',
    'Statutory text analysis and parliamentary intent documentation; empirical measurement of public access to printed works before/after statute; analysis of how copyright term limits supposedly served public benefit',
    'If genuine coordinate: statute succeeded in balancing private monopoly with public benefit. If rhetorical: coordination language masks pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_learning_coordinate, conceptual, 'Whether public learning was genuine coordinate goal or rhetorical cover').

omega_variable(
    false_summit_naturalization,
    'Is the Lockean labor-theory framing of copyright a genuine natural law of intellectual property, or a contingent institutional arrangement that benefits identifiable parties and can be reframed?',
    'Cross-cultural and cross-historical analysis: IP systems absent Lockean framing (Islamic waqf systems, guild-based attribution without exclusive ownership); empirical assessment of whether labor-based ownership is inevitable or contingent',
    'If natural law: copyright is immutable and necessarily protects authors. If contingent: copyright is one of many possible IP arrangements, and alternative framings are structurally available.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether copyright is natural law or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_copyright_creation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statcopy_tr_t0, statutory_copyright_creation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(statcopy_tr_t5, statutory_copyright_creation, theater_ratio, 5, 0.48).
narrative_ontology:measurement(statcopy_tr_t10, statutory_copyright_creation, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(statcopy_be_t0, statutory_copyright_creation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(statcopy_be_t5, statutory_copyright_creation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(statcopy_be_t10, statutory_copyright_creation, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_copyright_creation, resource_allocation).
narrative_ontology:affects_constraint(statutory_copyright_creation, perpetual_copyright_extension).
narrative_ontology:affects_constraint(statutory_copyright_creation, guild_monopoly_legitimacy).
narrative_ontology:affects_constraint(statutory_copyright_creation, author_bargaining_asymmetry).

% DUAL FORMULATION NOTE:
% Statutory copyright creation should be understood as decomposing into three structurally distinct constraints: (1) guild_monopoly_legitimacy — the legal transformation of de-facto trade control into de-jure property monopoly; (2) author_bargaining_asymmetry — the formal author-protection regime that permits unequal assignment of rights to publishers; (3) public_learning_constraint — the legal restriction on access and derivative use. Each has different ε values and different victim sets. The statute links all three, but they are analytically distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statutory_copyright_creation, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
