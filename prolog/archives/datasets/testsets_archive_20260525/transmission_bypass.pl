% ============================================================================
% CONSTRAINT STORY: transmission_bypass
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transmission_bypass, []).

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
 *   constraint_id: transmission_bypass
 *   human_readable: Transmission Bypass in Zero's Entry to Western Mathematics
 *   domain: history_of_mathematics/epistemology
 *
 * SUMMARY:
 *   The transmission bypass represents a structural constraint on
 *   mathematical knowledge: zero-as-number existed in Islamic mathematics by
 *   the 8th century and in Indian mathematics earlier, but did not enter
 *   systematic European practice until the 13th century at earliest, with
 *   widespread adoption delayed until the 15th-16th centuries. This 600-800
 *   year lag cannot be explained by transmission impossibility alone —
 *   Fibonacci had access to al-Khwarizmi's work by 1202 — but rather by the
 *   suppression of zero-as-entity through institutional enforcement. Roman
 *   numeral practitioners, abacus guilds, theological institutions, and
 *   scribal traditions all benefited from zero remaining a placeholder rather
 *   than becoming a number, and all possessed the institutional power to
 *   regulate what counted as legitimate mathematics. The constraint exhibits
 *   tangled rope structure: genuine coordination functions exist (theological
 *   meaning of nothingness, certification of calculational expertise,
 *   preservation of texts) layered atop asymmetric extraction (preventing a
 *   superior notation system from displacing the beneficiary groups). The
 *   theater ratio rises over the interval as scholastic resistance becomes
 *   increasingly performative — by the 13th century, the mathematical
 *   arguments against Hindu-Arabic numerals are largely rhetorical (appeals
 *   to tradition, Latin purity) rather than substantive. The constraint's
 *   resolution is not a single acceptance moment but a gradual institutional
 *   shift as merchants and astronomers build parallel computational networks
 *   (university mathematics, trading house record-keeping) that eventually
 *   outflank the traditional gatekeepers.
 *
 * KEY AGENTS:
 *   - Roman Numeral Practitioners: Primary beneficiary (institutional/arbitrage) — monopoly on calculation expertise threatened by zero-as-entity; can adopt new systems but choose not to
 *   - Abacus Guild Specialists: Primary beneficiary (institutional/arbitrage) — expertise and economic position depend on abacus remaining primary tool; zero-as-number makes manipulation teachable to commoners
 *   - Theological Institutions: Mixed beneficiary-victim (institutional/constrained) — benefit from zero-as-placeholder (theological language of nothingness) but constrained by logical problem zero-as-number creates (God creating something rather than nothing)
 *   - Mathematical Progress: Primary victim (powerless/trapped) — the field itself, represented in computational capacity, algebraic scope, astronomical calculation; bears full cost of delayed transmission
 *   - Computability: Primary victim (powerless/trapped) — merchants, engineers, astronomers unable to scale calculations within Roman numeral bounds
 *   - Scribal Tradition: Secondary actor (institutional/arbitrage) — maintains notation through inertia; performative copying role persists despite zero-as-entity existing elsewhere
 *   - Islamic and Indian Mathematicians: External reference (institutional/mobile) — possess the knowledge but lack institutional power within Western Christendom to enforce its adoption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transmission_bypass, 0.51).
domain_priors:suppression_score(transmission_bypass, 0.62).
domain_priors:theater_ratio(transmission_bypass, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transmission_bypass, extractiveness, 0.51).
narrative_ontology:constraint_metric(transmission_bypass, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(transmission_bypass, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transmission_bypass, tangled_rope).
narrative_ontology:human_readable(transmission_bypass, "Transmission Bypass in Zero's Entry to Western Mathematics").
narrative_ontology:topic_domain(transmission_bypass, "history_of_mathematics/epistemology").

domain_priors:requires_active_enforcement(transmission_bypass).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transmission_bypass, roman_numeral_practitioners).
narrative_ontology:constraint_beneficiary(transmission_bypass, abacus_guild_specialists).
narrative_ontology:constraint_beneficiary(transmission_bypass, theological_institutions).
narrative_ontology:constraint_victim(transmission_bypass, mathematical_progress).
narrative_ontology:constraint_victim(transmission_bypass, computability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL PROGRESS (SNARE) — Cannot exit the verification/acceptance crisis. The epistemic commons (the body of mathematics itself) bears full cost of delayed transmission. Zero-as-entity exists, but cannot be freely adopted. Maximum extraction from the constraint: the field is trapped in Roman numeral arithmetic despite superior alternatives existing. No advocate, no exit option.
constraint_indexing:constraint_classification(transmission_bypass, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: COMPUTABILITY CAPACITY (SNARE) — Merchants, astronomers, and engineers are trapped below the mathematical ceiling that Roman numerals and finger-counting impose. Cannot perform long multiplication, cannot solve algebraic equations efficiently, cannot scale calculations. Suppression is structural: economic incentives reward speed, but the tool-set does not deliver it. Exit barriers are absolute within the institutional frame: using Hindu-Arabic numerals is heresy or worse for centuries.
constraint_indexing:constraint_classification(transmission_bypass, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: ROMAN NUMERAL GUILD (ROPE) — Benefits from monopoly on calculation expertise. Training takes years. Social status derives from mathematical knowledge as esoteric skill. Zero-as-entity threatens this monopoly — if calculation becomes mechanical (Hindu-Arabic digit manipulation), expertise becomes commodity. But the guild also solves a genuine coordination problem: maintaining standards of calculation and certification. Experience is pure coordination (Rope) because they benefit directly from the constraint and have arbitrage options (can learn new systems but choose not to).
constraint_indexing:constraint_classification(transmission_bypass, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ABACUS GUILD (ROPE) — Specialized practitioners whose expertise and economic position depend on the abacus remaining the primary calculation tool. Zero-as-number would make abacus manipulation teachable to commoners (positional notation requires fewer expert tricks). Arbitrage option: learn Hindu-Arabic numerals, but maintain abacus as cultural artifact and prestige tool. Experiences the constraint as coordination — their guild coordinates reliable calculation within Roman numeral bounds, and they benefit from the certification monopoly.
constraint_indexing:constraint_classification(transmission_bypass, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: THEOLOGICAL INSTITUTION (TANGLED ROPE) — Benefits from zero-as-placeholder (nothing/void language maps to theological cosmology). BUT is constrained by the logical problem zero-as-number creates: if zero is a number, what does it mean for God to create from nothing? Zero becomes an object, not an absence. The constraint coordinates theological language about nullity (genuine coordination function) while extracting a cost: the institution must regulate which mathematical concepts are heretical. Active enforcement is required to suppress the logical implications of zero-as-entity. This is hybrid: coordination (theology of creation) + extraction (suppression of contradictions).
constraint_indexing:constraint_classification(transmission_bypass, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: SCRIBAL TRADITION (PITON) — By the 13th century, the scribal copyist guild maintains Roman numeral notation through institutional inertia. The notation works well enough for transmitted texts; zero-as-entity is absent from classical authorities. Theater ratio is high: scribal practice (copying, certification, preservation) is largely performative — it looks authoritative but doesn't require understanding zero's mathematical properties. The tradition persists not because it solves problems but because it is inherited, and alternatives have not yet achieved institutional legitimacy.
constraint_indexing:constraint_classification(transmission_bypass, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL NECESSITY VIEW (MOUNTAIN) — From civilizational scope, zero-as-number is a mathematical necessity — it is required for positional notation, for algebraic closure, for the number line itself. The constraint appears as resistance to inevitable truth. This perspective risks false summitry: naturalizing what is actually a contingent power structure (priestly/guild monopoly) as a law of mathematical logic. The engine's false summit detector should flag the declared beneficiaries and institutional interest as evidence of constructed constraint rather than natural law.
constraint_indexing:constraint_classification(transmission_bypass, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transmission_bypass_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transmission_bypass, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transmission_bypass, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(transmission_bypass, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(transmission_bypass, TR),
    TR >= 0.70.

:- end_tests(transmission_bypass_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.51): High-moderate. The constraint extracts significant value for beneficiary groups — centuries of preserved monopoly on calculational expertise, continued social status and economic advantage for abacus specialists and Roman numeral practitioners, theological regulation over cosmological language. However, the extraction is not maximal (≥0.66 for snare) because the constraint's days are numbered by the 13th century — merchants' computational needs are outpacing the system's capacity, and alternatives are becoming visible. The extraction is substantial but temporally bounded. Suppression (0.62): High. Institutional barriers are formidable: theological authority suppresses zero-as-entity as logically problematic, guild certification controls what counts as legitimate calculation, scribal tradition enforces Roman numeral notation in copied texts, university curricula teach Euclidean geometry (which does not require zero) as mathematics' foundation. But suppression is not total (≤0.95 for snare) — merchants develop underground networks, Fibonacci publishes in 1202, university curriculum eventually shifts. Theater ratio (0.68): High and rising. By the 13th century, scholastic arguments against Hindu-Arabic numerals are largely performative: appeals to tradition, concerns about forgery in notation (the digit 0 could be added to change numbers), invocation of classical authorities who knew nothing of zero. The theater increases over the interval as the practical arguments weaken and institutional resistance becomes increasingly rhetorical. Claimed type (tangled_rope): Required because the constraint possesses both genuine coordination functions (theological regulation, calculational certification, text preservation standards) and asymmetric extraction (suppression of a superior notational system). The beneficiaries genuinely solve problems within the Roman numeral frame; the constraint simultaneously prevents alternatives from displacing them.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's hybrid character. The Roman numeral and abacus guilds experience Rope (pure coordination of expertise and certification) because they benefit directly and voluntarily participate. The theological institution experiences Tangled Rope (mixed coordination of theological language + extraction of logical consequences) because it must actively enforce coherence. The merchants and astronomers experience Snare (pure extraction, trapped below a computational ceiling) because they bear costs with no exit. The scribal tradition experiences Piton (degraded ritual) because copying persists through institutional inertia despite zero's mathematical necessity becoming clear. The analytical observer risks false summitry by seeing Mountain (zero-as-number is mathematically necessary) without recognizing that the 600-year delay is not a law of knowledge transmission but a deliberate institutional suppression. The engine's false summit detector should flag this through the beneficiary declarations: the analytical mountain perspective coexists with institutional beneficiaries, signaling that 'natural law' framing masks constructed power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Roman numeral practitioners, abacus specialists, theological institutions) face arbitrage or constrained exit options and occupy institutional power positions. Their directionality (d) is low to moderate — they experience the constraint as beneficial coordination and have capacity to maintain it. The theological institution is partially victim (constrained by logical contradictions zero-as-number creates) and partially beneficiary (benefits from zero-as-placeholder framing), giving it intermediate d. The primary victims (mathematical progress, computability) are powerless and trapped — high d, experiencing maximum effective extraction. The scribal tradition is institutional with arbitrage options (could adopt new notation) but chooses not to, placing it in the beneficiary coalition. The analytical observer's d depends on whether the perspective treats zero-as-number as necessary (high d — resistance to inevitable truth) or conventional (moderate d — institutional suppression of one option among alternatives). The derivation chain produces: institutional beneficiaries → low/moderate d → low/negative χ → Rope experience. Powerless victims → high d → high f(d) → Snare experience. Mixed beneficiary-victim (theology) → intermediate d → intermediate χ → Tangled Rope experience.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the coordinate frame determines whether an agent sees coordination or extraction. The Roman numeral guild genuinely coordinates calculational expertise (Rope truth for them). The merchants genuinely experience extraction (Snare truth for them). These are not contradictory — they are the same constraint producing opposite experiences because the beneficiary and victim are different agents with opposite relationships to the institutional structure. The mandatrophy is resolved by recognizing that 'the constraint' is perspectival: what looks like coordination from inside the guild looks like suppression from outside it. The analytical observer cannot declare a single 'true' type without choosing a perspective. If we choose the powerless-victim perspective (computational progress), the type is Snare. If we choose the institutional-beneficiary perspective (guild coordination), the type is Rope. The Tangled Rope classification at the theological perspective captures the hybrid nature: the institution coordinates linguistic meaning but extracts the cost of suppressing logical consequences. This is not ambiguity — it is the correct structure: Tangled Rope is the honest classification when the same constraint does both coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_or_constraint_ambiguity,
    'Is transmission_bypass a single constraint observed from multiple positions, or a contested kernel where different readings of WHAT ZERO IS produce different mathematical structures?',
    'Examine historical texts: do the beneficiary groups disagree about zero''s mathematical properties (kernel reading distinction) or only about whether to accept it (constraint position)? If Islamic mathematicians understood zero-as-number but Western institutions rejected it, that is constraint+positions. If different cultures developed incommensurable number theories, that is kernel readings.',
    'If single constraint: the six perspectives correctly decompose it. If kernel: some perspectives should be reframed as reading_relations (forecloses/coexists_with/influences) in cs_structure, not as alternative views of one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_or_constraint_ambiguity, conceptual, 'Whether this is one constraint with multiple perspectives or a contested kernel with incommensurable readings').

omega_variable(
    suppression_internalization_mechanism,
    'Is the suppression of zero-as-entity primarily structural (institutional barriers to adoption) or internalized (Western mathematicians genuinely doubted zero''s coherence)?',
    'Historical analysis of mathematical writings: do objections to zero focus on practical barriers (guild economics, notation incompatibility) or on logical coherence (zero''s mathematical properties)? Distinguish between ''we cannot use this system'' and ''this concept is incoherent''.',
    'If primarily structural: suppression value is accurate (0.62). If primarily internalized: suppression may be lower (barriers are weaker) but identity_locked exit_options should appear in some perspectives. The mechanism determines which perspectives experience the constraint as material blockade vs cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression of zero operates through institutional barriers or internalized doubt').

omega_variable(
    transmission_speed_irreducibility,
    'Could zero''s entry to Western mathematics have been faster given optimal institutional incentives, or does it take ~800 years regardless because of fundamental knowledge-transmission limits?',
    'Counterfactual analysis: what if merchants in 10th-century Italy had immediate access to al-Khwarizmi''s texts with royal backing? Model: transmission time vs access vs institutional support. Examine other fast-adoption cases (printing press, heliocentrism) for baseline transmission timelines.',
    'If irreducible (~800 years is minimum): theater_ratio represents necessary translation/verification work, not performance. If reducible: the constraint is more extractive than classified — institutional resistance (beneficiary suppression) accounts for centuries of preventable delay.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transmission_speed_irreducibility, empirical, 'Whether zero''s ~800-year transmission lag is unavoidable or contingent on institutional resistance').

omega_variable(
    zero_as_necessity_vs_convention,
    'Is zero-as-number a discovered mathematical necessity (required for positional notation and algebraic closure) or a conventional symbol choice that happened to become dominant?',
    'Examine alternative notational systems (Babylonian sexagesimal, Roman numerals, finger-counting): can equivalent computational power be achieved without zero-as-entity? Are the limitations of Roman numerals fundamental or contingent on notation choice?',
    'If necessary: mountain perspective is justified — resistance to inevitable truth, constraint correctly classified as tangled_rope with mountain shadow. If conventional: the constraint is purely extractive (Snare) with no natural law shadow — beneficiary groups simply suppressed an alternative notation to preserve status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_as_necessity_vs_convention, conceptual, 'Whether zero-as-number is mathematically necessary or conventionally chosen').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transmission_bypass, 5, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(txby_tr_t0, transmission_bypass, theater_ratio, 0, 0.4).
narrative_ontology:measurement(txby_tr_t4, transmission_bypass, theater_ratio, 4, 0.58).
narrative_ontology:measurement(txby_tr_t8, transmission_bypass, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(txby_be_t0, transmission_bypass, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(txby_be_t4, transmission_bypass, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(txby_be_t8, transmission_bypass, base_extractiveness, 8, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transmission_bypass, information_standard).
narrative_ontology:affects_constraint(transmission_bypass, algebraic_closure_over_integers).
narrative_ontology:affects_constraint(transmission_bypass, positional_notation_necessity).

% DUAL FORMULATION NOTE:
% Transmission bypass is downstream of the mathematical necessity of zero (positional notation closure) but represents a distinct structural constraint on institutional adoption. The upstream constraints have ε values reflecting mathematical necessity; transmission bypass has ε reflecting institutional resistance. They are linked: the mathematical necessity creates pressure on the institutional constraint, and institutional adoption eventually forces reconsideration of zero's necessity. Decomposition is justified because the beneficiary/victim structure differs: mathematical properties affect all agents uniformly; institutional suppression benefits guild specialists and harms progress differently per power level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transmission_bypass, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
