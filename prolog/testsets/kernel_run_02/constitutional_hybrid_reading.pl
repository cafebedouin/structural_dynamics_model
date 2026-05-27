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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Constitutional Hybrid Authority: Ceremonial Inheritance + Political Delegation
 *   domain: political_philosophy/constitutional_theory/legitimacy
 *
 * SUMMARY:
 *   The constitutional hybrid authority structure emerges as a compromise
 *   between monarchical and republican legitimacy claims. In this reading,
 *   legitimate authority derives from TWO sources operating in separated
 *   institutional domains: ceremonial authority inherited through dynastic
 *   succession (the monarch retains symbolic authority, reserve powers, and
 *   constitutional primacy in formal acts), and political authority delegated
 *   through electoral democracy (elected government exercises ordinary
 *   executive and legislative power). The constitutional framework mediates
 *   the boundary through law, convention, and judicial interpretation. This
 *   reading does NOT claim that this arrangement is natural or inevitable —
 *   it is one institutional solution among alternatives. The extractiveness
 *   (0.38) reflects that the hybrid generates costs beyond the coordination
 *   benefits: both pure monarchists and pure republicans experience the
 *   arrangement as a constraint on their preferred legitimacy source, and the
 *   ambiguity in the boundary between ceremonial and political creates
 *   ongoing interpretive disputes that consume resources and create
 *   legitimacy uncertainty. The suppression (0.42) indicates significant
 *   barriers to exit: constitutional amendment is typically very difficult,
 *   and the boundary's interpretation through precedent constrains how far
 *   political power can drift without triggering constitutional crisis.
 *   Theater ratio (0.55) reflects moderate performative content: the
 *   ceremonial functions are substantially ritual-based (state openings,
 *   honors systems, formal appointments), while the actual constraints
 *   operate through constitutional law and convention.
 *
 * KEY AGENTS:
 *   - Hereditary Monarch: Institutional beneficiary (arbitrage exit) — retains status, income, ceremonial authority, reserve powers; delegates onerous political governance
 *   - Elected Government: Institutional beneficiary (arbitrage exit) — receives political power and democratic legitimacy; monarch's reserve functions provide constitutional backstop for trust
 *   - Republican Absolutists: Powerless victim (trapped exit) — seeks unified democratic legitimacy; hybrid forecloses this by institutionalizing hereditary authority
 *   - Monarchical Absolutists: Powerless victim (trapped exit) — seeks hereditary legitimacy with executive power; hybrid forecloses this by delegating power to elected officials
 *   - Constitutional Jurists: Moderate institutional actor (constrained exit) — interprets and enforces the boundary; benefits from system's need for expertise but constrained by precedent
 *   - Political Opposition: Powerful actor (constrained exit) — cannot directly challenge ceremonial authority; must accept monarch's reserve powers symmetrically constrain sitting government
 *   - Historical Evolution: Longue durée observer (mobile exit) — sees hybrid as temporary compromise between absolute monarchy and pure republic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_hybrid_reading, 0.38).
domain_priors:suppression_score(constitutional_hybrid_reading, 0.42).
domain_priors:theater_ratio(constitutional_hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(constitutional_hybrid_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_hybrid_reading, "Constitutional Hybrid Authority: Ceremonial Inheritance + Political Delegation").
narrative_ontology:topic_domain(constitutional_hybrid_reading, "political_philosophy/constitutional_theory/legitimacy").

domain_priors:requires_active_enforcement(constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_hybrid_reading, '00f15064-710b-459a-9ddd-799e38bf459a').
narrative_ontology:cs_created_at('00f15064-710b-459a-9ddd-799e38bf459a', '').
narrative_ontology:cs_kernel_codification('00f15064-710b-459a-9ddd-799e38bf459a', formalized).
narrative_ontology:cs_authority_grounding('00f15064-710b-459a-9ddd-799e38bf459a', lineage).
narrative_ontology:cs_interpretation_layer_present('00f15064-710b-459a-9ddd-799e38bf459a').
narrative_ontology:cs_kernel_id(constitutional_hybrid_reading, sovereign_legitimacy).
narrative_ontology:cs_reading_relation('00f15064-710b-459a-9ddd-799e38bf459a', monarchical_reading, influences).
narrative_ontology:cs_reading_relation('00f15064-710b-459a-9ddd-799e38bf459a', republican_reading, influences).
narrative_ontology:cs_axiom('00f15064-710b-459a-9ddd-799e38bf459a', foundational, dual_sourcing_legitimate).
narrative_ontology:cs_axiom_status(dual_sourcing_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('00f15064-710b-459a-9ddd-799e38bf459a', dual_sourcing_legitimate, deontological).
narrative_ontology:cs_axiom('00f15064-710b-459a-9ddd-799e38bf459a', foundational, constitutional_boundary_stability).
narrative_ontology:cs_axiom_status(constitutional_boundary_stability, holdable).
narrative_ontology:cs_axiom_grounding('00f15064-710b-459a-9ddd-799e38bf459a', constitutional_boundary_stability, instrumental).
narrative_ontology:cs_reference_frame('00f15064-710b-459a-9ddd-799e38bf459a', separated_authorities_constitutional_mediation).
narrative_ontology:cs_drift_state('00f15064-710b-459a-9ddd-799e38bf459a', contemporary_legitimacy_challenge, gap(authority_erosion, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(constitutional_hybrid_reading, legitimacy_seekers).
narrative_ontology:constraint_victim(constitutional_hybrid_reading, boundary_dispute_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REPUBLICAN ABSOLUTIST (SNARE) — Seeks a unified, transparent source of political authority grounded in democratic legitimacy alone. The constitutional hybrid forecloses this by institutionalizing inherited ceremonial authority as co-equal, creating permanent structural opposition. Trapped by the constitutional settlement; cannot exit without constitutional amendment (nearly impossible). Experiences high extraction: ceremonial authority consumes resources and legitimacy without democratic mandate, yet cannot be removed.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MONARCHICAL ABSOLUTIST (SNARE) — Seeks unified, transparent source of authority grounded in hereditary legitimacy and executive power concentrated in the throne. The constitutional hybrid forecloses this by delegating political power to elected officials, reducing the monarch to ceremonial and reserve functions. Trapped by the constitutional settlement; experiences extraction as loss of authority despite retained status and income.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL JURIST (TANGLED ROPE) — Operates within the hybrid framework to interpret and enforce the boundary between ceremonial and political authority. Genuine coordination function: the constitution enables coexistence of both authority sources through careful doctrine (reserve powers, conventions, parliamentary sovereignty). Constrained by precedent and interpretive tradition, but also benefits from the system's need for their expertise. Experiences mixed coordination and extraction: interprets law (benefit) but also enforces constraints that limit both ceremonial and political authority.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTED GOVERNMENT (ROPE) — Primary beneficiary of the hybrid system. Receives political power and democratic legitimacy through delegation while the monarch provides ceremonial authority, historical continuity, and constitutional stability. Experiences the constraint as coordination: the monarch's reserve functions (dissolution, veto, emergency powers) provide safeguards that enable trust in the elected government's ordinary authority. Net beneficiary with arbitrage capacity — can exit by abolishing the monarchy (though politically costly).
constraint_indexing:constraint_classification(constitutional_hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HEREDITARY MONARCH (ROPE) — Secondary beneficiary. Retains status, income, and symbolic authority while delegating the onerous political power to elected officials. Experiences the constraint as coordination: the constitutional boundary protects the monarch's status against democratic abolition pressure while providing legitimacy through historical continuity and ceremonial functions. Arbitrage capacity exists (formal head of state powers) though typically exercised through elected government advice.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: POLITICAL OPPOSITION (TANGLED ROPE) — Constrained by the constitutional framework: cannot directly challenge the ceremonial authority without constitutional amendment, yet must accept the monarch's reserve powers. Experiences both coordination (the constitution constrains the governing party symmetrically) and extraction (the monarch's reserve functions can be wielded asymmetrically by the sitting government through advice). Forced to work within the hybrid system even when opposing both ceremonial inheritance and current political power.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: HISTORICAL EVOLUTION (PITON) — From a civilizational timescale, the constitutional hybrid appears as an evolutionary compromise from an earlier era (absolute monarchy) toward a future endpoint (pure republicanism or pure constitutional monarchy). The hybrid persists partly through institutional inertia and partly through theatrical performance: ceremonial authority functions primarily through ritual and spectacle, not through active governance. Theater ratio (0.55) reflects that much of the hybrid's cohesion is maintained through constitutional convention and ceremonial performance rather than through active enforcement of the boundary.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the dual-sourcing of legitimate authority appears as an immutable requirement: all stable political systems must balance inherited legitimacy (continuity, tradition, stability) against delegated legitimacy (consent, responsiveness, change). The constitutional hybrid instantiates this as law. However, this perspective risks naturalizing what is historically contingent: the specific institutional arrangement (ceremonial/political split) is one solution among many, not a law of nature.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   EXTRACTIVENESS (0.38): Moderate. The constitutional hybrid reduces extractiveness compared to either pure form — neither the monarch nor elected officials can unilaterally impose their preferred legitimacy source. But extractiveness is not zero because: (1) Monarchists experience loss of executive power despite retained status; (2) Republicans experience imposed ceremonial authority without democratic mandate; (3) Taxpayers bear costs of ceremonial functions (0.15 estimated); (4) Constitutional litigation and boundary disputes consume resources (0.12 estimated); (5) The ambiguity itself creates legitimacy uncertainty (0.11 estimated). The remainder (0.00) is genuine coordination benefit — the boundary actually does stabilize both ceremonial and political authority against challenge. SUPPRESSION (0.42): Moderate. Barriers to exit the hybrid framework are substantial: constitutional amendment requires supermajority votes and extended procedural timelines (effectively one-direction asymmetry toward incumbents). But suppression is not total because: elected governments can expand their authority through interpretation; monarchs have already lost executive power and cannot be easily restored; citizens can emigrate or form new states (low-cost exit for individuals, high-cost for collectives). THEATER RATIO (0.55): Moderate-high. Significant performative content in: state openings and parliamentary ceremonies (purely symbolic); honors systems (ritual legitimation); formal appointment rituals (authority derived from process rather than outcome); constitutional conventions (unstated but enforced through social pressure). But the theater is not dominant because: constitutional law genuinely constrains both ceremonial and political authority; reserve powers operate through real legal mechanisms (dissolution, veto); boundary disputes are resolved through genuine interpretation, not pure theater. The trajectory shows theater_ratio increasing over the 50-year interval (0.48 → 0.55) as ceremonial functions become more purely symbolic and less functionally necessary; extractiveness increases correspondingly (0.32 → 0.38) as the boundary requires more interpretation to maintain coherence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint displays radically divergent classifications across observer positions. Both pure-form absolutists (republican and monarchical) see a Snare — they are trapped by a constitutional settlement that forecloses their preferred legitimacy source. The constitutional jurist sees a Tangled Rope — genuine coordination function (the boundary enables coexistence) alongside extraction (disputes are costly). Both beneficiaries (elected government and monarch) see a Rope — the constraint solves the coordination problem of how to have both hereditary legitimacy and democratic power. The opposition sees a Tangled Rope — they benefit from symmetric constraint on sitting government but are asymmetrically constrained by the monarch's reserve powers. The historical evolution sees a Piton — the hybrid persists through inertia and theater as it transitions toward pure republicanism (or toward pure constitutional monarchy without substance). The analytical observer risks seeing a Mountain — dual-sourcing as immutable law — but this is a false summit that naturalizes one institutional solution as inevitable. The perspectival gap reveals the hybrid's fragility: it appears stable only from beneficiary positions; from absolutist positions it appears as permanent constraint; from interpretive positions it appears increasingly performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from structural position: power level, exit options, and beneficiary/victim status. Beneficiaries with arbitrage capacity (elected government, monarch) experience low d → negative χ; pure-form absolutists with trapped exit experience high d → high χ. The jurist's moderate power with constrained exit produces mid-range d. The opposition's powerful position with constrained exit creates asymmetry that depends on whether they hold government or opposition status — in opposition, they experience high d (constrained by monarch's reserve powers); in government, they experience low d (they control how reserve powers are interpreted). This captures the perspectival truth: the hybrid's constraint operates asymmetrically depending on political alignment. The falsehood of the mountain perspective is revealed through this: if dual-sourcing were truly immutable, directionality would be uniform across all observers, and all perspectives would classify the same. Instead, the perspectival gap is extreme, indicating that the constraint is contingent institutional choice, not natural law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pure_form_foreclosure,
    'Does the constitutional hybrid logically foreclose the possibility of either pure republicanism or pure monarchy within a single legitimacy framework?',
    'Constitutional interpretation analysis: can a state holding the hybrid''s constitutional commitments coherently adopt pure monarchy or pure republicanism without internal contradiction? Examine attempted transitions (Canada''s Maple Leaf Accord, Australia''s 1999 referendum).',
    'If YES: this reading forecloses sibling readings (rare, high-confidence axiom overriding). If NO: the readings coexist, and the choice between them is preference or convention, not logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pure_form_foreclosure, conceptual, 'Whether constitutional hybrid logically forecloses pure forms').

omega_variable(
    boundary_stability_threshold,
    'What proportion of political disputes can be resolved through constitutional interpretation of the ceremonial/political boundary before the framework itself becomes incoherent?',
    'Historical analysis of boundary disputes across constitutional monarchies; correlation between frequency of interpretation challenges and institutional stability; measurement of instances where the boundary interpretation itself became subject of fundamental constitutional change.',
    'If threshold < 30%: boundary is brittle, hybrid approaches instability. If threshold > 70%: boundary is robust, hybrid can accommodate significant pressure. Affects whether the hybrid is genuinely durable (Rope) or temporarily holding back conflicting forces (Scaffold with implicit sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_stability_threshold, empirical, 'Stability threshold for ceremonial/political boundary under interpretation stress').

omega_variable(
    ceremonial_extraction_measurement,
    'What portion of the extractiveness (0.38) derives from ceremonial authority consuming resources without democratic mandate, versus from the boundary dispute costs themselves?',
    'Cost accounting: resources consumed by ceremonial functions (monarchy, veto power, dissolution prerogatives, honors systems) versus resources consumed by constitutional litigation and boundary-definition disputes; comparison with control jurisdictions (pure republic, pure monarchy).',
    'If ceremonial extraction > 0.25: the ceremonial authority itself is the primary extractor (victims are taxpayers). If boundary disputes > 0.20: the ambiguity is the primary extraction mechanism (victims are both sides seeking clarity). Reshapes beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_extraction_measurement, empirical, 'Source decomposition of measured extractiveness').

omega_variable(
    reading_kernel_identity,
    'Is the contested kernel ''sovereign legitimacy'' or is it more precisely ''the source and validation of executive power''? Different kernels would yield different reading_relations classifications.',
    'Examination of constitutional texts and judicial doctrine: does the debate center on WHERE legitimacy comes from (heredity vs consent) or on HOW executive power is distributed once legitimacy is established? If both, are they separable questions?',
    'If kernel is WHO IS SOVEREIGN: constitutional hybrid forecloses pure monarchy (monarch is not sole sovereign). If kernel is HOW IS POWER DISTRIBUTED: constitutional hybrid coexists with both pure forms (different distribution, same sovereign source). Affects reading_relations axiom status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Clarification of the contested kernel identity').

omega_variable(
    dual_sourcing_necessity,
    'Is the dual-sourcing of authority (ceremonial/political) a functional necessity for stable transition, or is it a contingent institutional choice that other mechanisms could replace?',
    'Comparative constitutional analysis: do transitions without dual-sourcing (France''s Fifth Republic, Germany''s Federal Republic, post-1922 Ireland) achieve comparable stability? What alternative mechanisms (ceremonial presidency separate from executive prime minister, strong constitutional court, formal constitutional amendment processes) achieve the same stabilization function?',
    'If necessary: foundational axiom (dual_sourcing_inherent) is holdable. If contingent: the axiom is overridable by alternatives; the reading is not foreclosed but alternatives are available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_sourcing_necessity, empirical, 'Whether dual-sourcing is necessary or contingent for constitutional stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_hybrid_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cons_tr_t25, constitutional_hybrid_reading, theater_ratio, 25, 0.53).
narrative_ontology:measurement(cons_tr_t50, constitutional_hybrid_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_hybrid_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cons_be_t25, constitutional_hybrid_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(cons_be_t50, constitutional_hybrid_reading, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_hybrid_reading, monarchical_reading).
narrative_ontology:affects_constraint(constitutional_hybrid_reading, republican_reading).
narrative_ontology:affects_constraint(constitutional_hybrid_reading, constitutional_reserve_powers).
narrative_ontology:affects_constraint(constitutional_hybrid_reading, dissolution_prerogative).

% DUAL FORMULATION NOTE:
% The sovereign_legitimacy kernel has three readings, each constituting a distinct constraint: monarchical_reading (ε~0.25, pure hereditary legitimacy), republican_reading (ε~0.20, pure democratic legitimacy), constitutional_hybrid_reading (ε~0.38, mixed). The ε values differ because they measure different extraction mechanisms: pure monarchy's extraction from non-hereditary population; pure republicanism's extraction from traditionalists; the hybrid's extraction from both pure-form seekers plus boundary dispute costs. All three link bidirectionally; the hybrid influences and is influenced by both pure readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_hybrid_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
