% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_extraction_permissive, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: OST Article II Non-Appropriation as Resource Extraction Permission (Extraction-Permissive Reading)
 *   domain: international_law/space_law/commons_governance
 *
 * SUMMARY:
 *   The Outer Space Treaty (OST) Article II prohibition on territorial
 *   appropriation (sovereignty claims) is subject to radically different
 *   interpretations depending on whether 'appropriation' encompasses resource
 *   extraction or applies only to formal sovereignty assertions. Under the
 *   extraction-permissive reading instantiated here, Article II bars
 *   territorial claims while permitting private resource extraction by
 *   flag-state operators. This creates a de facto enclosure mechanism: states
 *   and their operators can extract resources, establish de facto control
 *   through operational presence, and accumulate economic claims without
 *   formal annexation. The constraint is a Tangled Rope because it
 *   simultaneously coordinates (the OST provides legal clarity enabling space
 *   activities) and extracts (first-mover operators capture resources while
 *   non-spacefaring states remain excluded). The extraction-permissive
 *   reading treats non-appropriation as a prohibition on formal legal claims
 *   only, leaving extraction itself ungoverned except by flag-state
 *   regulation. The alternative readings (commons-conservation,
 *   international-regime) treat non-appropriation as barring effective
 *   control and requiring collective benefit-sharing mechanisms. These
 *   readings are structurally distinct because they generate different
 *   beneficiary/victim sets, different suppression mechanisms, and different
 *   extraction values. The instantiation chosen here emphasizes that resource
 *   access is gated by technological capability and capital, creating a
 *   structural privilege for spacefaring states with no compensation
 *   mechanism for excluded states.
 *
 * KEY AGENTS:
 *   - Spacefaring States (USA, Russia, China, ESA members): Primary beneficiaries (institutional/arbitrage) — flag-state control permits operators to extract resources without sovereignty risk; first-mover advantage is permanent
 *   - Non-Spacefaring States (majority of UN membership): Primary victims (powerless/trapped) — excluded by technological barriers; nominal OST membership provides no access; no compensation mechanism
 *   - Private Extractive Operators (Luxembourg, private asteroid mining ventures): Secondary beneficiaries (powerful/mobile) — OST non-appropriation reading permits extraction; flag-state provides legal cover; can exit to other jurisdictions
 *   - Common Heritage Advocates (developing states, environmental coalitions, Moon Agreement signatories): Suppressed voices (powerful/constrained) — perceive OST as violated; constrained by lack of enforcement capacity; can advocate but cannot block extraction
 *   - OST Institutional Governance (UNCOPUOS, treaty conferences): Theater maintenance (institutional/arbitrage) — maintain consultation procedures that create appearance of collective control; actual allocation is market-driven
 *   - Analytical Observer (civilizational/universal): Risks naturalizing contingent policy as immutable law — non-appropriation framed as logically necessary rather than as strategic policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.58).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.42).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.58).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "OST Article II Non-Appropriation as Resource Extraction Permission (Extraction-Permissive Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_law/space_law/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '0a01202a-78a6-421c-92bd-5a1924687739').
narrative_ontology:cs_kernel_codification('0a01202a-78a6-421c-92bd-5a1924687739', fixed_text).
narrative_ontology:cs_authority_grounding('0a01202a-78a6-421c-92bd-5a1924687739', extraction).
narrative_ontology:cs_interpretation_layer_present('0a01202a-78a6-421c-92bd-5a1924687739').
narrative_ontology:cs_reading_relation('0a01202a-78a6-421c-92bd-5a1924687739', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_reading_relation('0a01202a-78a6-421c-92bd-5a1924687739', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('0a01202a-78a6-421c-92bd-5a1924687739', foundational, appropriation_territorial_claims_only).
narrative_ontology:cs_axiom_status(appropriation_territorial_claims_only, holdable).
narrative_ontology:cs_axiom_grounding('0a01202a-78a6-421c-92bd-5a1924687739', appropriation_territorial_claims_only, conventional).
narrative_ontology:cs_axiom('0a01202a-78a6-421c-92bd-5a1924687739', foundational, flag_state_regulation_sufficiency).
narrative_ontology:cs_axiom_status(flag_state_regulation_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('0a01202a-78a6-421c-92bd-5a1924687739', flag_state_regulation_sufficiency, instrumental).
narrative_ontology:cs_reference_frame('0a01202a-78a6-421c-92bd-5a1924687739', non_appropriation_as_formal_claims_only).
narrative_ontology:cs_drift_state('0a01202a-78a6-421c-92bd-5a1924687739', contemporary_extraction_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0a01202a-78a6-421c-92bd-5a1924687739', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, private_extractive_operators).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, common_heritage_principle_adherents).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-SPACEFARING STATES (SNARE) — Structurally excluded from resource access by technological and capital barriers. No exit option. The Article II reading permits extraction by flag-state operators without compensation mechanism, collective benefit-sharing, or governance participation. Trapped in a commons that is being enclosed via fait accompli. Maximum experienced extraction — nominal sovereignty over celestial resources is meaningless without capacity to claim them.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__extraction_permissive, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING SPACE-CAPABLE STATES (TANGLED ROPE) — Can participate in extraction but face high capital and technical barriers. Constrained by resource requirements and governance exclusion from extraction norms. Benefit from coordination framework (the OST itself enables space activities legally) but bear asymmetric extraction risk — early operators lock in resource claims before later entrants can mobilize capacity. Mixed coordination-extraction relationship.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SPACEFARING STATES AND OPERATORS (ROPE) — Primary beneficiaries. Article II reading permits resource extraction by flag-state operators without appropriation (sovereignty) claims. This is pure coordination from their perspective: the rule enables their activities, reduces legal risk, and permits first-mover resource capture. Zero or minimal extraction from their own position — they are net beneficiaries. High exit optionality (can exit the OST, can operate independently, can arbitrage jurisdictions).
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__extraction_permissive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMON HERITAGE ADVOCATES / ENVIRONMENTAL COALITION (TANGLED ROPE) — Powerful advocacy position but operationally constrained. Perceive the extraction-permissive reading as violating the spirit of common heritage framing. Experience mixed extraction (their governance vision is suppressed, their environmental concerns are discounted) and coordination (OST does provide a negotiation forum, collective action is possible). Mobile exit option — can withdraw from OST framework, can build parallel regimes (Antarctic Treaty Model, deep-sea governance). Experienced extraction is moderate because they have voice and exit capacity.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OST INSTITUTIONAL GOVERNANCE (PITON) — The UNCOPUOS and OST conference structures maintain elaborate consultation, treaty amendment, and common interest procedures that create appearance of collective governance over space resources. In the extraction-permissive reading, these procedures are largely theater — real resource allocation decisions are made through market and flag-state mechanisms, not treaty amendment. The institutional apparatus persists through inertia, legitimating the extraction-permissive interpretation via performative consultation. Theater ratio 0.38 reflects that some coordination function (dispute resolution, legal clarity) is genuine, but much of the governance ritual is decoupling from actual allocation.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__extraction_permissive, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the non-appropriation clause can be read as a natural law constraint: it is logically impossible to 'appropriate' (claim sovereignty) over celestial territory under the OST, therefore territorial claims are inherently prohibited by the structure of the framework itself. This perspective treats non-appropriation as an immutable feature of space law, not a contingent policy choice. However, the structural data contradicts this — the extraction-permissive reading shows that resource capture (economically meaningful appropriation) is fully permitted; the ban applies only to sovereignty claims. This is a false summit: naturalizing a policy choice as logical necessity.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__extraction_permissive, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__extraction_permissive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__extraction_permissive, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, TR),
    TR >= 0.70.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The extraction-permissive reading permits resource capture by flag-state operators without compensation to excluded states or enforcement of common heritage benefit-sharing. The mechanism is sophisticated — it uses the non-appropriation clause (which nominally protects the commons) to shield extraction from governance scrutiny. First-mover operators gain permanent resource claims; later entrants face fait accompli. However, extractiveness is not as severe as pure snare (0.72+) because some coordination function is genuine: the OST does clarify space law, enabling activities that would otherwise face legal uncertainty. The extraction is layered on top of coordination, making it a Tangled Rope rather than pure Snare. Suppression (0.42): Moderate. Non-spacefaring states are excluded by technological and capital barriers, not by explicit legal prohibition. They cannot exit the commons, but they are suppressed through structural capacity rather than coercive enforcement. Some suppression is passive (lack of technology) and some is active (flag-states and operators control access, deny participation in governance). Theater ratio (0.38): Moderate-low. The OST conference structures and UNCOPUOS consultations provide genuine coordination (dispute resolution, legal clarification) but create theater insofar as actual resource allocation is market-driven and first-mover determined. The ratio has declined over time (from 0.52 to 0.38) because early OST implementation created hope for equitable governance; extractive reality has progressively decoupled from governance theater.
 *
 * PERSPECTIVAL GAP:
 *   The extraction-permissive reading creates maximum perspectival gap. Spacefaring states and operators see Rope (pure coordination enabling their activities). Non-spacefaring states see Snare (permanent exclusion, no exit, no compensation). Common heritage advocates see Tangled Rope (mixed coordination and suppression of their governance vision). The analytical observer risks seeing Mountain (non-appropriation as a natural/logical law) when the constraint is actually a contingent policy choice benefiting spacefaring actors. The piton perspective on institutional governance captures the theater: UNCOPUOS maintains elaborate consultation rituals while actual allocation is market-determined and first-mover dependent.
 *
 * DIRECTIONALITY LOGIC:
 *   The extraction-permissive reading assigns directionality based on structural position relative to resource access. Spacefaring states and operators are beneficiaries with arbitrage exit options (d ≈ 0.10): they can extract, can exit the OST, can operate in other jurisdictions, or can enforce their preferred interpretation through fait accompli. Non-spacefaring states are victims with trapped exit options (d ≈ 0.95): they cannot access resources, cannot credibly exit (leaving OST provides no access to space), and cannot organize sufficient pressure to enforce alternative readings. Common heritage advocates are powerful but constrained (d ≈ 0.60): they have voice but lack enforcement capacity. The piton perspective derives from the theater gate rather than from high experienced extraction — the institutional governance apparatus performs coordination functions but these are increasingly decoupled from actual allocation.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL-READING CONFIGURATION: This constraint is instantiation of the extraction-permissive reading of OST Article II. The mandatrophy is resolved by acknowledging that Article II non-appropriation is contested: the reading that interprets it as permitting extraction produces a Tangled Rope with asymmetric beneficiary/victim structure. Alternative readings (commons-conservation: extraction requires benefit-sharing; international-regime: extraction requires international governance body) produce different extractiveness and suppression values. The constraint story captures the extraction-permissive reading as a coherent, internally consistent interpretation with measurable structural consequences. No single reading is 'correct' — rather, the OST instantiates a committer-axis kernel where different parties hold competing readings with different material impacts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    appropriation_definition_boundary,
    'Does ''appropriation'' in Article II refer only to sovereignty claims (territorial designation) or also to effective resource control via extraction?',
    'Textual analysis of Article II alongside Article III (freedom of use) and Article V (benefit-sharing expectations). Historical negotiation records. State practice: have states treated resource extraction as forbidden appropriation or as permitted use? Has any state formally objected to extraction as appropriation?',
    'If appropriation = sovereignty only (extraction-permissive reading): constraints the classification as Tangled Rope with high extraction. If appropriation includes effective control: reclassifies as commons-conservation reading with low extraction and genuine coordination. This is the central committer-axis divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriation_definition_boundary, conceptual, 'Definitional scope of ''appropriation'' in Article II: sovereignty vs. resource control').

omega_variable(
    flag_state_governance_sufficiency,
    'Do flag-state regulations of their operators constitute adequate governance of space resource extraction, or is additional international regime required?',
    'Comparative analysis of flag-state environmental standards, labor protections, and benefit-sharing provisions. Empirical tracking of operator compliance. International pressure for harmonized standards. Treaty amendment attempts (Moon Agreement, International Space Resources Authority proposals).',
    'If flag-state governance sufficient: extraction-permissive reading stands, extraction mechanism persists. If inadequate: pressure for international regime grows, undermining the extraction-permissive reading and strengthening commons-conservation and international-regime readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(flag_state_governance_sufficiency, empirical, 'Whether flag-state regulation adequately governs operator conduct').

omega_variable(
    fait_accompli_legitimacy,
    'Does the extraction-permissive reading''s reliance on first-mover fait accompli create legitimate property claims in space resources, or does it constitute illegitimate enclosure of the commons?',
    'Legal challenge to extraction claims in international courts (ICJ, tribunal proceedings). State recognition patterns: do other states recognize extraction rights? Parallel governance: do states build competing regimes (Moon Agreement signatories, space mining regulations)? Historical comparison: how did ocean enclosure shift from commons to EEZ regime?',
    'If fait accompli viewed as legitimate: extraction persists, property-like regime crystallizes de facto. If viewed as illegitimate enclosure: political pressure for reallocation, potential treaty amendment forcing benefit-sharing or compensation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fait_accompli_legitimacy, preference, 'Legitimacy of resource claims based on extraction precedent (first-mover advantage)').

omega_variable(
    common_heritage_enforceability,
    'Is the ''common heritage of mankind'' language in OST Preamble enforceable constraint on the extraction-permissive reading, or is it aspirational framing without legal teeth?',
    'State practice: have states or plaintiffs invoked common heritage to challenge extractions? Moon Agreement signature and ratification patterns (current: 6 parties, predominantly developing states). Attempted amendments to OST establishing benefit-sharing mechanisms. International arbitration or ICJ cases.',
    'If enforceable: extraction-permissive reading faces legal challenge, transitions toward international-regime or commons-conservation readings. If aspirational: extraction-permissive reading persists as de facto governing interpretation despite preamble language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(common_heritage_enforceability, empirical, 'Enforceability of common heritage principle against extraction-permissive interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost_extract_tr_t0, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ost_extract_tr_t15, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 15, 0.43).
narrative_ontology:measurement(ost_extract_tr_t30, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(ost_extract_be_t0, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ost_extract_be_t15, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ost_extract_be_t30, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ost_extract_su_t0, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ost_extract_su_t15, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(ost_extract_su_t30, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, enforcement_mechanism).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, moon_agreement_international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, common_heritage_principle_enforceability).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, asteroid_mining_property_rights).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, flag_state_environmental_governance).

% DUAL FORMULATION NOTE:
% The OST Article II non-appropriation principle is a single textual kernel subject to multiple readings. This file models the extraction-permissive reading (ε=0.58, Tangled Rope). Sibling readings (commons-conservation, international-regime) are separate constraint stories with different ε values, beneficiary/victim structures, and temporal trajectories. All three stories are linked via network.affects_constraints to show the constraint family structure. The different readings arise not from different observables but from different interpretations of the treaty text and different understandings of what conduct ('appropriation') is barred. The ε-invariance principle requires separate stories because the readings generate genuinely different suppression mechanisms, beneficiary/victim sets, and extraction pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
