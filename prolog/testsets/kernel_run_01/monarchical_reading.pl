% ============================================================================
% CONSTRAINT STORY: monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monarchical_reading, []).

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
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monarchical_reading
 *   human_readable: Monarchical Legitimacy via Inherited Status and Divine Sanction
 *   domain: political_theory/constitutional_law
 *
 * SUMMARY:
 *   Monarchical legitimacy grounds authority in two elements: (1) inherited
 *   status — kingship passes through bloodline as a natural or divinely
 *   ordained mechanism, and (2) divine sanction — the monarch's authority is
 *   legitimated by transcendent source (God, cosmic order, natural law). This
 *   reading instantiates one interpretation of the contested kernel
 *   'sovereign legitimacy.' It competes with republican (consent-based) and
 *   constitutional-hybrid (limited hereditary authority within
 *   electoral/representative constraints) readings. The monarchical reading's
 *   core claim is that legitimate authority flows from descent and
 *   supernatural grounding, not from popular consent or rational design. This
 *   produces an asymmetric structural dynamic: the hereditary elite (and the
 *   institution of monarchy itself) benefit from the constraint by
 *   concentrating authority and wealth succession; excluded subjects bear the
 *   cost through restricted political agency and compliance obligations. The
 *   constraint exhibits high suppression (legitimacy doctrine declares
 *   exclusion natural/divinely ordained, foreclosing the question 'why am I
 *   excluded?') and moderate theater (coronation rituals, genealogical
 *   pageantry, divine-right invocations perform legitimacy continuously). The
 *   empirical status of the divine sanction claim is irreducible — it makes
 *   assertions about transcendent authority that cannot be falsified by
 *   observation. This irreducibility defines the constraint's
 *   commitment-system nature.
 *
 * KEY AGENTS:
 *   - Hereditary Elite: Primary beneficiary (institutional/arbitrage) — inherits political authority, property succession, and social rank automatically by birth; experiences constraint as coordination mechanism providing stability
 *   - Excluded Subjects: Primary victim (powerless/trapped) — born outside succession line; permanently excluded from authority regardless of talent or consent; faces maximal suppression (legitimacy doctrine declares exclusion natural)
 *   - Reigning Monarch: Institutional beneficiary (institutional/arbitrage) — receives concentrated authority, legitimacy backing, and succession certainty; experiences the constraint as solving the fundamental coordination problem of authority transfer
 *   - Constitutional Reform Movement: Organized challenger (organized/mobile) — intellectuals, merchants, emerging middle classes seeing hereditary mechanism as efficiency failure; building alternative legitimacy (electoral, representative, constitutional); represent the sunset dynamic
 *   - Clergy/Legitimacy Authorities: Institutional gatekeeper (institutional/constrained) — maintain and interpret divine sanction claims; experience constraint as both beneficiary (institutional authority enhanced by association with monarchy) and victim (if divine sanction claim is falsified, their own authority suffers)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement; false-summit detector identifies whether the mountain framing (natural/divinely ordered) is genuine or cover story for extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monarchical_reading, 0.58).
domain_priors:suppression_score(monarchical_reading, 0.72).
domain_priors:theater_ratio(monarchical_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monarchical_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(monarchical_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(monarchical_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monarchical_reading, tangled_rope).
narrative_ontology:human_readable(monarchical_reading, "Monarchical Legitimacy via Inherited Status and Divine Sanction").
narrative_ontology:topic_domain(monarchical_reading, "political_theory/constitutional_law").

domain_priors:requires_active_enforcement(monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(monarchical_reading, fixed_text).
narrative_ontology:cs_authority_grounding(monarchical_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(monarchical_reading).
narrative_ontology:cs_kernel_id(monarchical_reading, sovereign_legitimacy).
narrative_ontology:cs_reading_relation(monarchical_reading, republican_reading, coexists_with).
narrative_ontology:cs_reading_relation(monarchical_reading, constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom(monarchical_reading, foundational, inheritance_legitimacy_foundational).
narrative_ontology:cs_axiom_status(inheritance_legitimacy_foundational, holdable).
narrative_ontology:cs_axiom(monarchical_reading, foundational, divine_authority_grounding).
narrative_ontology:cs_axiom_status(divine_authority_grounding, holdable).
narrative_ontology:cs_reference_frame(monarchical_reading, hereditary_divine_authority).
narrative_ontology:cs_drift_state(monarchical_reading, contemporary_constitutional_era, gap(authority_erosion, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monarchical_reading, hereditary_elite).
narrative_ontology:constraint_beneficiary(monarchical_reading, monarchy_institution).
narrative_ontology:constraint_victim(monarchical_reading, excluded_subjects).
narrative_ontology:constraint_victim(monarchical_reading, political_agency_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED SUBJECT (SNARE) — Powerless agents born outside the succession line face permanent structural exclusion from political authority. No exit: birth determines civil status across lifetime and generations. The constraint's suppression mechanism is total — legitimacy doctrine declares exclusion natural and divinely ordained. Maximum experienced extraction: subjects bear compliance costs (taxation, military service, obedience) with zero participatory benefit.
constraint_indexing:constraint_classification(monarchical_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ARISTOCRATIC COURTIER (TANGLED ROPE) — Moderate power agents within the hereditary elite experience mixed dynamics. Genuine coordination function: dynastic succession stabilizes property, alliance networks, and governance continuity across generations. But asymmetric extraction: only those in direct succession line reap dynastic benefit; cadet branches and non-inheriting siblings face constraints despite their status. Exit cost is social death — abandoning aristocratic identity and patronage networks is existentially costly but theoretically possible.
constraint_indexing:constraint_classification(monarchical_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REIGNING MONARCH (ROPE) — Institutional beneficiary with full arbitrage options. The constraint solves the succession coordination problem: hereditary mechanism prevents civil war over throne, provides clear authority continuity, and concentrates power efficiently. The monarch experiences the constraint as pure coordination — legitimacy doctrine allocates them authority, the extraction flows toward them, exit is unthinkable but not because barriers block it (sovereignty itself defines the constraint). Theater is performative (coronation rituals, divine right appeals) but functional: rituals generate binding legitimacy.
constraint_indexing:constraint_classification(monarchical_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Organized agents (emergent reform factions, enlightenment intellectuals, merchant classes) experience the monarchical constraint as a temporary coordination failure being solved by an alternative mechanism: constitutional separation of powers, electoral succession, citizen participation. Theater is high (invocation of 'natural rights,' appeals to reason) because the reform movement needs to delegitimize the inherited-status axiom. Exit path is clear: constitutional limits on hereditary authority provide alternative governance coordination. Sunset trajectory: constitutional documents and institutions are replacing dynastic legitimacy over 1-2 generational spans.
constraint_indexing:constraint_classification(monarchical_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: DEGRADED MONARCHY (PITON) — In societies where electoral or constitutional mechanisms have partially displaced hereditary succession, the monarchy persists as symbolic/ceremonial authority. The theater_ratio is high (ritual, pageantry, symbolic deference) but the functional extraction has declined — the monarch no longer controls taxation, legislation, or war. The constraint persists through institutional inertia and theatrical maintenance (coronations, state visits, hereditary titles) rather than through genuine extraction. This is piton: a former rope or tangled_rope that has atrophied but is sustained by performative ritual.
constraint_indexing:constraint_classification(monarchical_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL ORDER VIEW (MOUNTAIN) — From a civilizational universal scope, the natural law perspective treats inheritance and hierarchy as immutable features of human social structure. All societies must allocate authority; hierarchy is inescapable from biology and human nature; hereditary succession is the most efficient mechanism for stable authority. Theater_ratio approaches zero from this view: the mechanism simply reflects natural law. However, this perspective is subject to false-summit detection — the empirical data shows the constraint benefits identifiable agents (hereditary elite) and harms others (excluded subjects), contradicting the natural-law framing.
constraint_indexing:constraint_classification(monarchical_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monarchical_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monarchical_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monarchical_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(monarchical_reading, TR),
    TR >= 0.70.

:- end_tests(monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The monarchical reading concentrates authority asymmetrically — the hereditary elite capture succession benefits and governance power while excluded subjects bear compliance costs with zero participatory input. The extractiveness is not maximal (0.70+) because the reading provides genuine coordination function: hereditary succession does stabilize authority transfer and prevent succession warfare (coordination function genuine). But the coordination benefit is monopolized by the beneficiary class — subjects get stability without agency. Over the interval, extractiveness rises modestly (0.48 → 0.58) as the monarchy consolidates extraction (increased absolutism, expanded court bureaucracy concentrating authority). Suppression (0.72): High. The legitimacy doctrine (divine sanction + natural inheritance) forecloses questioning: subjects taught that exclusion is natural, divinely ordained, or earned through virtue in prior lives — mechanisms that make resistance unthinkable or heretical. Property law, succession codes, and religious doctrine all reinforce the suppression. Birth determines civil status; no exit mechanism exists within the reading's framework. Theater ratio (0.65): Moderate-high. Coronation rituals, genealogical pageantry, divine-right appeals, and ceremonial deference perform legitimacy continuously. The theater has a genuine functional component (the rituals do generate binding social recognition) but increasingly becomes decorative as subjects develop alternative legitimacy framings (Enlightenment rationalism, contractual theory, republicanism).
 *
 * PERSPECTIVAL GAP:
 *   The monarchical reading produces a sharp perspectival divide. The hereditary elite (institutional/arbitrage) experience rope — they see the constraint as solving the succession coordination problem efficiently and legitimately. Excluded subjects (powerless/trapped) experience snare — they perceive only extraction, suppression, and permanent exclusion. The analytical observer risks mountain classification (naturalizing inherited authority as immutable feature of human society) but the structural data reveals this as false summit: identifiable beneficiaries, systematic suppression mechanisms, and constructed legitimacy claims indicate extraction, not natural law. The constitutional reform movement (organized/mobile) sees scaffold — they perceive the monarchical reading as a temporary coordination failure being replaced by electoral/representative mechanisms with genuine termination points (constitutional documents, legal reforms). This perspectival gap is the defining feature of the constraint: its classification depends entirely on the agent's structural position relative to the inheritance and succession mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's relationship to the extraction flow and their exit options. Hereditary elite (beneficiaries with arbitrage options): d ≈ 0.10 (full beneficiaries), producing low or negative f(d), so they experience effective extraction χ as beneficial or neutral. Excluded subjects (victims with trapped exit): d ≈ 0.95 (full targets), producing high f(d) ≈ 1.42, so they experience maximum extraction χ. The constitutional reformers (organized agents with mobile exit options) have d ≈ 0.45, producing moderate f(d) ≈ 0.55, because they experience some extraction pressure (establishment attempts to suppress reform) but retain significant agency and clear exit paths (constitutional alternatives). The analytical observer cannot use d directly — they occupy the civilizational/analytical position that sees the constraint's full structure, not a position within it. Their mountain classification is perspectival and subject to false-summit correction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading instantiates a core mandatrophy: the constraint has a genuine coordination function (hereditary succession prevents authority chaos) but is structured to concentrate the coordination benefit asymmetrically (only the hereditary elite receive the stabilization advantage; subjects get order without voice). The reading resolves mandatrophy by acknowledging both poles: yes, there is coordination (rope elements); yes, there is asymmetric extraction (snare elements); yes, the constraint is actively enforced (enforcement required field). This combination defines tangled_rope. The false-summit risk reveals the second mandatrophy layer: is the claim that inheritance and divine sanction are 'natural' an empirical assertion (falsifiable) or a purely metaphysical/theological one (not subject to falsification)? If empirical, contradictory evidence (successful non-hereditary systems, no observable evidence of divine favor) undermines the mountain classification. If metaphysical, the reading remains internally coherent but loses explanatory power in secular analytical contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_empirical_status,
    'Is the divine sanction claim empirically decidable or purely normative/metaphysical?',
    'Theological vs. secular analysis: does the reading depend on theological propositions that cannot be falsified by observation, or does it make empirical claims (e.g., about the monarch''s personal virtue, divine favor, or natural order) that can be tested?',
    'If purely normative: the reading''s legitimacy rests on faith claims and cannot be challenged empirically — a pure commitment system constraint. If empirical: contradictory evidence undermines the legitimacy claim, revealing the constraint as constructed rather than natural. Classification may shift from mountain toward tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_sanction_empirical_status, conceptual, 'Whether divine sanction is empirically decidable or purely metaphysical').

omega_variable(
    inherited_status_efficiency_claim,
    'Does hereditary succession actually produce more stable governance and lower succession conflict than elective or constitutional alternatives?',
    'Historical comparative analysis: conflict frequency and severity during succession transitions (hereditary vs. elective vs. constitutional systems); regime stability metrics (lifespan, institutional continuity) across system types.',
    'If hereditary produces superior stability: the rope perspective (pure coordination) is correct. If non-hereditary systems show equivalent or superior stability: the constraint is not justified by coordination function, revealing it as extraction justified by false efficiency claims. Classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherited_status_efficiency_claim, empirical, 'Whether hereditary succession produces superior governance stability').

omega_variable(
    bifurcation_mechanisms_across_readings,
    'What specific doctrinal elements distinguish the monarchical reading from the republican and constitutional-hybrid readings at the deepest level?',
    'Textual and doctrinal analysis: trace the foundational claim (inherited legitimacy grounded in divine sanction) through competing readings; identify where the axioms diverge irreducibly.',
    'If axioms foreclose each other: readings are logically incompatible (forecloses relation). If axioms coexist: readings compete in public discourse but neither rules out the other (coexists_with). If one reading''s axioms create conditions that pressure the other: influences relation. Classification of reading_relations and clarity of axiom distinctiveness depend on this analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bifurcation_mechanisms_across_readings, conceptual, 'Doctrinal elements distinguishing monarchical from sibling readings').

omega_variable(
    appeal_to_naturalness_as_cover_story,
    'Is the invocation of divine sanction and natural inheritance a genuine metaphysical claim or a rhetorical cover story for institutional extraction?',
    'Discourse analysis: examine whether appeals to divine sanction or naturalness are deployed strategically to foreclose questioning, or whether they constitute sincere metaphysical commitments held by authorities and subjects alike. Track whether challenges to the sanction claim are treated as blasphemy/heresy (foreclosing debate) or as legitimate intellectual disagreement.',
    'If genuinely believed: the constraint operates through sincere commitment to the reading''s axioms — a mountain from the believer''s perspective. If strategic: the constraint is extraction camouflaged in legitimacy language — reveals snare dynamics. This is the core FSM (false-summit-mountain) diagnostic for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appeal_to_naturalness_as_cover_story, conceptual, 'Whether divine/natural appeals are sincere metaphysical claims or rhetorical cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monarchical_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mon_tr_t0, monarchical_reading, theater_ratio, 0, 0.58).
narrative_ontology:measurement(mon_tr_t2, monarchical_reading, theater_ratio, 2, 0.62).
narrative_ontology:measurement(mon_tr_t4, monarchical_reading, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(mon_be_t0, monarchical_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(mon_be_t2, monarchical_reading, base_extractiveness, 2, 0.54).
narrative_ontology:measurement(mon_be_t4, monarchical_reading, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monarchical_reading, identity_coordination).
narrative_ontology:affects_constraint(monarchical_reading, republican_reading).
narrative_ontology:affects_constraint(monarchical_reading, constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% Monarchical legitimacy is one reading of sovereign_legitimacy kernel. The constraint family includes republican_reading (consent-based authority) and constitutional_hybrid_reading (limited hereditary authority). Each reading instantiates a different ε, different beneficiary/victim structure, and different classification type. They are separate constraints (distinct stories) not variations of one constraint. The network links represent doctrinal influence: constitutional mechanisms influence (constrain) the monarchical reading's operational space, not logical foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
