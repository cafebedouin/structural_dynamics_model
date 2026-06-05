% ============================================================================
% CONSTRAINT STORY: institutional_adoption_lag
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_adoption_lag, []).

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
 *   constraint_id: institutional_adoption_lag
 *   human_readable: Institutional Adoption Lag: Zero's Entry into Western Mathematics
 *   domain: history_of_mathematics/epistemology
 *
 * SUMMARY:
 *   The adoption of zero into Western mathematics between the 5th and 13th
 *   centuries presents a structural constraint where institutional resistance
 *   to a mathematically superior system produced measurable extraction.
 *   Zero-as-placeholder existed in Hindu-Arabic numerals from at least the
 *   5th century and was incorporated into Islamic mathematics with full
 *   mathematical status (zero-as-additive-identity, zero in division
 *   operations) by the 9th century. Yet Western European mathematics resisted
 *   zero's adoption for nearly 800 years, maintaining Roman numerals despite
 *   their computational inferiority. The constraint operates through
 *   institutional gatekeeping: merchant guilds maintained monopolies on
 *   numerical literacy, universities taught Boethian arithmetic (which treats
 *   zero as absence rather than entity), and religious authority resisted the
 *   theological implications of 'nothing as something.' Zero-adoption would
 *   have democratized mathematical practice — Hindu-Arabic numerals with zero
 *   are learnable by non-specialists, reducing the monopoly power of
 *   guild-trained calculators. The extractive dimension is institutional:
 *   those benefiting from Roman numeral complexity (merchants controlling
 *   trade networks, arithmetic monopolists, university authorities)
 *   suppressed zero adoption through formal restrictions (guild bans on
 *   alternative numerals), epistemological gatekeeping (framing zero as
 *   philosophically incoherent), and authority capture (preventing
 *   transmission texts from entering the curriculum). The constraint exhibits
 *   both genuine coordination function (standardization of merchant
 *   accounting, consistency across city-states' ledgers) and asymmetric
 *   extraction (the standard that is enforced is one that maintains elite
 *   privilege). This is the Tangled Rope signature: coordination mechanism +
 *   active enforcement + beneficiary (elite numerical establishment) + victim
 *   (computational accessibility, mathematical progress).
 *
 * KEY AGENTS:
 *   - Merchant Guilds & Arithmetic Monopolists: Primary beneficiary (institutional/arbitrage) — hold computational monopoly; zero-adoption would democratize numerical literacy and eliminate their gatekeeping power. Active enforcers of Roman numeral requirement for apprentices.
 *   - Commercial Scribes & Calculators: Primary victim (powerless/trapped) — know zero works better but locked into apprenticeship structures and guild requirements. Trapped by debt and institutional dependency; no alternative employment pathway.
 *   - Islamic Mathematical Tradition: Non-extracted institutional actor (institutional/arbitrage) — zero is already adopted and integrated; transmission texts exist. Experiences constraint as coordination problem (creating pedagogical materials for Western transmission).
 *   - Translation Movement (12th-13th centuries): Organized agents (organized/constrained) — Toledo, Sicily, Cordoba translation schools systematically translating Islamic mathematics texts. Building alternative educational pathways that bypass guild monopolies. Constrained by resistance from established authorities but growing in institutional power.
 *   - Medieval Universities: Dual-position institutional actor (institutional/arbitrage) — teach Euclidean geometry and Boethian arithmetic; engage in disputational logic about zero's philosophical status but avoid computational commitment. Piton classification: degraded functional role (neither reject nor truly adopt zero).
 *   - Theological Authority (Church): Secondary beneficiary/enforcer — resists zero as philosophically incoherent or theologically problematic; aligns with institutional gatekeeping. Benefits from maintaining authority over what counts as legitimate knowledge.
 *   - Analytical Observer: Detached analytical position (analytical/analytical) — risks seeing mathematical necessity where institutional resistance operates.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_adoption_lag, 0.38).
domain_priors:suppression_score(institutional_adoption_lag, 0.62).
domain_priors:theater_ratio(institutional_adoption_lag, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_adoption_lag, extractiveness, 0.38).
narrative_ontology:constraint_metric(institutional_adoption_lag, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(institutional_adoption_lag, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_adoption_lag, tangled_rope).
narrative_ontology:human_readable(institutional_adoption_lag, "Institutional Adoption Lag: Zero's Entry into Western Mathematics").
narrative_ontology:topic_domain(institutional_adoption_lag, "history_of_mathematics/epistemology").

domain_priors:requires_active_enforcement(institutional_adoption_lag).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_adoption_lag, roman_numeral_merchant_establishment).
narrative_ontology:constraint_beneficiary(institutional_adoption_lag, arithmetic_monopolists).
narrative_ontology:constraint_victim(institutional_adoption_lag, mathematical_progress).
narrative_ontology:constraint_victim(institutional_adoption_lag, computational_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MERCHANT SCRIBE (SNARE) — Trapped by institutional requirement to use Roman numerals despite knowing zero-based Hindu-Arabic systems work better for accounting. Cannot exit: guild apprenticeship debt, no alternative employment, institutional monopoly on commercial literacy training. The scribe's knowledge that zero solves real problems is suppressed by the coercive overhead of the numerical establishment. Maximum experienced extraction.
constraint_indexing:constraint_classification(institutional_adoption_lag, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: COMMERCIAL MATHEMATICS COMMUNITY (TANGLED ROPE) — Experiences genuine coordination function (common standards for accounting, consistent ledger practices across city-states) alongside asymmetric extraction. Zero-adoption would reduce coordination costs and increase transparency — but also threatens the monopoly on mathematical knowledge that constitutes commercial power. The constraint both enables (standardization) and extracts (artificial complexity maintaining elite status).
constraint_indexing:constraint_classification(institutional_adoption_lag, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ISLAMIC MATHEMATICAL TRADITION (ROPE) — Zero-as-number is already established; algebra is already developed; transmission texts exist. From this perspective, the constraint is pure coordination: developing commentaries, improving pedagogical clarity, creating better transmission mechanisms for knowledge already discovered. No extraction — only coordination overhead. This agent has arbitrage options (trade routes, institutional prestige, but also freedom to teach as they choose).
constraint_indexing:constraint_classification(institutional_adoption_lag, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSLATION MOVEMENT (SCAFFOLD) — Organized agents (Toledo, Sicily, Cordoba translation schools) are building alternative pathways for mathematical knowledge transmission. These represent a sunset for the old extraction regime: as translated texts proliferate and students learn zero-based arithmetic outside guild structures, the Roman numeral monopoly loses force. High suppression initially (institutional pushback, orthodox mathematical authority resistance) declining over the time horizon as new authorities emerge. Theater initially high (ritual debates about number philosophy) declining as pragmatic utility becomes undeniable.
constraint_indexing:constraint_classification(institutional_adoption_lag, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: MEDIEVAL UNIVERSITY SYSTEM (PITON) — Universities teach zero-as-mathematical-entity in principle (derived from transmission of Euclid, Boethius) but treat it as a philosophical curiosity rather than a practical tool. Theater ratio is high: disputational logic around 'can nothing be something?' maintains the appearance of mathematical rigor while avoiding the computational commitment. The university system has arbitrage (can align with trade, with church, with Islamic transmission) but has decayed functionally — it neither fully rejects nor adopts zero.
constraint_indexing:constraint_classification(institutional_adoption_lag, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an analytical/civilizational perspective, the mathematical properties of zero are invariant: zero-as-additive-identity is a logical necessity, not a discovery. The constraint appears as an immutable feature of how institutions resist epistemic shifts, an irreducible friction between knowledge and institutional capacity. However, this risks naturalizing what is historically contingent — the extraction mechanism (monopoly preservation) is institutional, not natural.
constraint_indexing:constraint_classification(institutional_adoption_lag, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_adoption_lag_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_adoption_lag, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_adoption_lag, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_adoption_lag, TR),
    TR >= 0.70.

:- end_tests(institutional_adoption_lag_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The extraction is real — institutional gatekeeping prevents access to a superior computational tool, maintaining artificial complexity. But it is not maximal because: (1) legitimate coordination benefits exist (Roman numerals do provide standardized accounting frameworks, even if inefficient), (2) the constraint gradually decays as transmission becomes inevitable, (3) the extraction mechanism is not coercive violence but institutional monopoly (weaker than pure snare mechanisms). The measurement trajectory shows extractiveness rising from 0.28 to 0.38 as institutional pressure increases (8th-13th centuries: maximum resistance phase). Suppression (0.62): High. Multiple coercive mechanisms operate: (1) guild apprenticeship as debt-bondage (merchant scribes cannot exit), (2) epistemological gatekeeping (zero presented as philosophically invalid), (3) institutional authority capture (universities prevent zero from entering curriculum), (4) publication bias against transmission texts. But suppression is not total (0.85+) because alternative routes exist (trade with Islamic lands, self-study of imported texts, eventual translation movement). Theater ratio (0.65): Moderate-high. The performative component includes disputational philosophy about whether zero is a 'true' number, theological debates about the coherence of 'nothing as number,' and academic ritual around Boethian arithmetic that has become disconnected from computational practice. Theater rises from 0.48 to 0.65 across the interval as institutional resistance becomes more explicit and performatively defended. The theater itself is extractive: the elaborate philosophical and theological arguments against zero preserve the constraint's legitimacy even as its practical inferiority becomes obvious.
 *
 * PERSPECTIVAL GAP:
 *   The merchant scribe (powerless/trapped) experiences pure extraction (Snare): they know zero works, cannot use it, and bear the cost of Roman numeral inefficiency in their labor. The commercial mathematics community (moderate/constrained) experiences mixed coordination-extraction (Tangled Rope): the constraint does coordinate merchant practice across regions and standardize accounting, but also extracts from them the cost of maintaining artificial complexity. The Islamic tradition (institutional/arbitrage) experiences pure coordination (Rope): zero is solved; the constraint is only the communication problem of transmitting the solution. The translation movement (organized/constrained) experiences a temporary problem with a sunset (Scaffold): the old monopoly is being bypassed by alternative institutional pathways; translation creates competition that makes the old constraint's extraction unsustainable. The university system (institutional/arbitrage) sees its own degraded function (Piton): disputational philosophy about zero-qua-entity persists as institutional ritual even as practical commitment to zero-based arithmetic becomes inevitable. The analytical observer (analytical/analytical) risks seeing mathematical inevitability (Mountain) where institutional extraction operates — a false summit. The perspectival gaps reveal that institutional adoption lag is not a single phenomenon viewed from different angles but a true constraint structure where different agents experience different constraint types based on their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is computed from base extractiveness (ε = 0.38) scaled by directionality d and scope. Beneficiaries have low d (0.05–0.20): arithmetic monopolists benefit from the constraint, so they experience negative or near-zero chi — the constraint subsidizes them. The merchant scribe (victim + trapped) has high d (0.92), producing high chi: the constraint extracts maximum from them. The translation movement (organized + constrained with partial arbitrage) has moderate d (0.45), producing moderate chi: they have some options (can access transmission texts, build competing institutions) but face institutional resistance. The asymmetric d-distribution drives the tangled-rope classification: the same base constraint produces near-zero experienced extraction for beneficiaries but high extraction for victims. Suppression (0.62) is unscaled — it applies uniformly to all agents. The merchant scribe is suppressed by guild debt + epistemological gatekeeping; the commercial mathematics community is suppressed by standardization requirements that embed Roman numerals; the translation movement is suppressed by institutional resistance. But all experience the same suppression magnitude — the constraint's coercive overhead.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint includes both a genuine coordination function and asymmetric extraction. The coordination function is real: merchant guilds do solve the legitimate problem of standardizing numerical accounting across distributed trade networks. Roman numerals, while computationally inferior, serve as a common standard that enables ledger reconciliation, accounting verification, and inter-merchant communication. Any switch to zero-based numerals would incur coordination costs (retraining, ledger conversion, standard-setting). The extraction is also real: the specific standard that is enforced (Roman numerals) is chosen not because it minimizes coordination costs but because it maximizes the gatekeeping power of those who control numerical literacy. An alternative standard (zero-based Hindu-Arabic) would reduce total coordination costs but eliminate the monopoly. This is exactly the tangled rope pattern: genuine coordination problem + active enforcement of a standard that benefits gatekeepers + presence of victims (scribes trapped by the standard) + presence of beneficiaries (monopolists). The constraint is not merely coordination (rope) because beneficiary/victim asymmetry and suppression mechanisms are central. It is not merely extraction (snare) because coordination genuinely matters and the system does solve real standardization problems — it solves them in a way that maintains monopoly. Mandatrophy is resolved by recognizing that institutions can truthfully describe themselves as coordination solutions while operating as extraction mechanisms. The merchant guild can honestly say 'we maintain Roman numeral standards to ensure accounting consistency' while simultaneously extracting from merchant scribes who could do the work faster and cheaper with zero.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_single_constraint,
    'Is this a contested kernel (different readings of what zero IS) or a single constraint with different perspectival readings (resistance to institutional adoption of what zero already was)?',
    'Historical analysis of whether zero''s mathematical identity was genuinely contested (different senses of ''number'' led to different definitions of zero) vs. whether zero''s identity was fixed and only institutional adoption was delayed. Examine: did Islamic mathematicians debate what zero IS, or did they establish zero''s mathematical properties and Western institutional resistance delayed adoption of settled claims?',
    'If kernel: decompose into separate constraint stories (zero-as-placeholder vs zero-as-number have different ε values and different beneficiary/victim structures). If single constraint: the current tangled-rope analysis is correct — the extraction mechanism is institutional lag, not mathematical contestation. Changes classification framing and network decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_single_constraint, conceptual, 'Whether this is a contested kernel or a single institutional constraint').

omega_variable(
    monopoly_intentionality,
    'Did institutional resistance to zero adoption constitute deliberate monopoly preservation, or was it epistemic conservatism and legitimate intellectual caution about new number concepts?',
    'Textual analysis of guild records, university regulations, and merchant correspondence. Look for: (1) explicit statements of zero-threat to established authority, (2) barriers imposed specifically on zero-teaching vs general mathematical innovation resistance, (3) guild privileges tied to numerical monopoly.',
    'If deliberate monopoly: suppression metric (0.62) is justified; extraction component is core. If epistemic caution: suppression should be lower; constraint reclassifies toward Rope (coordination with legitimate uncertainty). Changes the victim class definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopoly_intentionality, empirical, 'Whether resistance was deliberate monopoly preservation or epistemic caution').

omega_variable(
    extraction_vs_coordination_boundary,
    'At what point does institutional resistance to a more powerful computational tool transition from coordination (maintaining standards during uncertain transition) to extraction (actively preventing superior alternatives)?',
    'Comparative analysis: identify constraints where similar institutional lag produced tangled_rope vs pure rope classifications. Establish criteria for when suppression + beneficiary combination justifies extraction framing vs pure coordination framing.',
    'If boundary is at suppression > 0.55: current classification correct. If boundary is higher: reclassify toward Rope. If boundary is lower: reclassify toward Snare. Affects whether the constraint resolves mandatrophy as mixed or pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Threshold distinguishing coordination resistance from extractive monopoly').

omega_variable(
    false_summit_naturalization,
    'Does the mountain perspective (analytical/civilizational) represent a legitimate natural-law reading of mathematical necessity, or is it naturalizing what is contingent institutional extraction?',
    'Analysis of whether zero''s mathematical properties are logically necessary (independent of any institutional framework) vs whether zero''s functional role depends on institutional acceptance. Test: could a different institutional path have developed equivalent mathematics without zero by some other means?',
    'If natural law legitimate: mountain classification stands; no false summit. If naturalization: engine''s false-summit detector fires; this perspective reveals institutional lock-in as mathematical inevitability. Affects whether tangled_rope is the ''true'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether mountain perspective naturalizes institutional extraction as mathematical necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_adoption_lag, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(instadopt_tr_t0, institutional_adoption_lag, theater_ratio, 0, 0.48).
narrative_ontology:measurement(instadopt_tr_t3, institutional_adoption_lag, theater_ratio, 3, 0.58).
narrative_ontology:measurement(instadopt_tr_t6, institutional_adoption_lag, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(instadopt_be_t0, institutional_adoption_lag, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(instadopt_be_t3, institutional_adoption_lag, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(instadopt_be_t6, institutional_adoption_lag, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_adoption_lag, information_standard).
narrative_ontology:affects_constraint(institutional_adoption_lag, algorithmic_gatekeeping_modern).
narrative_ontology:affects_constraint(institutional_adoption_lag, computational_skill_monopoly).

% DUAL FORMULATION NOTE:
% Institutional adoption lag for zero decomposes into two related but distinct constraints: (1) zero-as-mathematical-entity vs zero-as-placeholder (different ε values reflecting the empirical/philosophical contestation), (2) institutional resistance to adoption (institutional gatekeeping mechanism). The current story addresses the second. If the first is a genuine kernel (different readings of what zero fundamentally IS), a separate constraint story should model the kernel structure. Historical analysis is needed to determine whether the mathematical identity of zero was contested (kernel) or fixed-from-Islamic-mathematics (single constraint with perspectival readings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_adoption_lag, institutional, 0.08).
constraint_indexing:directionality_override(institutional_adoption_lag, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
