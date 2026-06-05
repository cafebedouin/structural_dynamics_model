% ============================================================================
% CONSTRAINT STORY: symbol_emperor__kokutai_severed_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_symbol_emperor__kokutai_severed_reading, []).

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
 *   constraint_id: symbol_emperor__kokutai_severed_reading
 *   human_readable: Kokutai Severed: The Mystical National Polity Demoted to Ceremony
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   The 1947 constitutional clause that demoted the emperor from a ruling
 *   divinity to a symbol represents a unique constraint: the simultaneous
 *   preservation and severance of a foundational political theology. The
 *   clause maintains the emperor as head of state, permits imperial
 *   ceremonies, and preserves the institution's continuity — but it decouples
 *   the throne from any theological claim to rule through divine descent.
 *   What remained was a state office performing continuity rituals, not a
 *   divine authority wielding power through mystical national polity. This
 *   constraint exhibits the structure of mandatrophy resolution: two
 *   incompatible cosmologies (theological statehood vs. secular democracy)
 *   cannot coexist in a unified legal order, yet the constitution attempts to
 *   preserve the symbol while severing its substance. The reading
 *   instantiated here — the kokutai_severed_reading — interprets the clause
 *   as a deliberate institutional choice to suppress theological statehood
 *   while maintaining ceremonial continuity. Other readings would see the
 *   same clause differently: as a device for preserving state continuity
 *   across a revolutionary break (continuity_device_reading) or as the
 *   deepest institutional change disguised in the gentlest words
 *   (sovereignty_relocated_reading). This reading uniquely emphasizes the
 *   extraction mechanism: suppression of belief as a mechanism for
 *   restructuring civic standing.
 *
 * KEY AGENTS:
 *   - Kokutai Believers: Powerless identity-locked agents — those for whom imperial divinity is constitutive of national identity and civic legitimacy. Compressed into private space or identity degradation.
 *   - Imperial Priesthood and Shinto Apparatus: Moderate institutional actors — maintain ceremonial function but are delegitimized as explainers of state cosmology. Constrained exit: cannot restore theological grounding without constitutional amendment.
 *   - Secular Constitutionalism (Occupation Authority): Institutional beneficiary — the clause solves the governance problem of how to preserve the state apparatus while eliminating the theological claim justifying militarism.
 *   - State Office of the Emperor: Institutional performer — the office persists through constitutional mandate but functions largely as ceremony, not as ruling authority.
 *   - Analytical Observer: Sees the constraint from civilizational perspective — risks naturalizing a historical choice as law of nature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symbol_emperor__kokutai_severed_reading, 0.58).
domain_priors:suppression_score(symbol_emperor__kokutai_severed_reading, 0.68).
domain_priors:theater_ratio(symbol_emperor__kokutai_severed_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symbol_emperor__kokutai_severed_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(symbol_emperor__kokutai_severed_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(symbol_emperor__kokutai_severed_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symbol_emperor__kokutai_severed_reading, tangled_rope).
narrative_ontology:human_readable(symbol_emperor__kokutai_severed_reading, "Kokutai Severed: The Mystical National Polity Demoted to Ceremony").
narrative_ontology:topic_domain(symbol_emperor__kokutai_severed_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(symbol_emperor__kokutai_severed_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(symbol_emperor__kokutai_severed_reading, '6601c733-c672-4e9a-b559-edaacc406c5a').
narrative_ontology:cs_kernel_codification('6601c733-c672-4e9a-b559-edaacc406c5a', fixed_text).
narrative_ontology:cs_authority_grounding('6601c733-c672-4e9a-b559-edaacc406c5a', extraction).
narrative_ontology:cs_interpretation_layer_present('6601c733-c672-4e9a-b559-edaacc406c5a').
narrative_ontology:cs_reading_relation('6601c733-c672-4e9a-b559-edaacc406c5a', symbol_emperor__continuity_device_reading, forecloses).
narrative_ontology:cs_reading_relation('6601c733-c672-4e9a-b559-edaacc406c5a', symbol_emperor__sovereignty_relocated_reading, coexists_with).
narrative_ontology:cs_axiom('6601c733-c672-4e9a-b559-edaacc406c5a', foundational, theological_statehood_fundamentally_incompatible).
narrative_ontology:cs_axiom_status(theological_statehood_fundamentally_incompatible, holdable).
narrative_ontology:cs_axiom_grounding('6601c733-c672-4e9a-b559-edaacc406c5a', theological_statehood_fundamentally_incompatible, deontological).
narrative_ontology:cs_axiom('6601c733-c672-4e9a-b559-edaacc406c5a', secondary, belief_suppression_mechanism_legitimate).
narrative_ontology:cs_axiom_status(belief_suppression_mechanism_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('6601c733-c672-4e9a-b559-edaacc406c5a', belief_suppression_mechanism_legitimate, instrumental).
narrative_ontology:cs_reference_frame('6601c733-c672-4e9a-b559-edaacc406c5a', secular_constitutionalism).
narrative_ontology:cs_drift_state('6601c733-c672-4e9a-b559-edaacc406c5a', contemporary_post_1947, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6601c733-c672-4e9a-b559-edaacc406c5a', '').
narrative_ontology:cs_kernel_id(symbol_emperor__kokutai_severed_reading, symbol_emperor).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(symbol_emperor__kokutai_severed_reading, secular_constitutionalism).
narrative_ontology:constraint_beneficiary(symbol_emperor__kokutai_severed_reading, postwar_state_apparatus).
narrative_ontology:constraint_victim(symbol_emperor__kokutai_severed_reading, kokutai_orthodoxy).
narrative_ontology:constraint_victim(symbol_emperor__kokutai_severed_reading, theological_statehood_enforcement).
narrative_ontology:constraint_victim(symbol_emperor__kokutai_severed_reading, imperial_divinity_claim).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KOKUTAI BELIEVER (SNARE) — Identity fused with theological statehood. The clause does not merely prohibit divine-descent doctrine; it severs the believer's identity from civic participation. Holding kokutai orthodoxy becomes incompatible with legitimate state speech. The constraint extracts belief itself — believers must either abandon identity or retreat from public space. Identity_locked because the barrier is not legal prohibition (believers can technically speak) but the internalized recognition that articulating kokutai cosmology disqualifies them from being heard as rational civic actors. Maximum experienced extraction.
constraint_indexing:constraint_classification(symbol_emperor__kokutai_severed_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: IMPERIAL PRIESTHOOD / SHINTO APPARATUS (TANGLED ROPE) — Genuine coordination function: the clause permits ritual continuity and shrine maintenance. Imperial ceremonies persist, though their cosmological grounding is severed. But extraction is embedded: priests' authority to interpret state cosmology is delegitimized; they function as ceremonial functionaries, not as authoritative explainers of kokutai. They coordinate the continuity ritual while bearing the cost of doctrinal suppression. Constrained exit: the apparatus cannot abandon its function (ritual continuity is mandated) but also cannot restore the theological grounding.
constraint_indexing:constraint_classification(symbol_emperor__kokutai_severed_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SECULAR CONSTITUTIONALISM / OCCUPATION AUTHORITY (ROPE) — Pure coordination function from this perspective. The clause solves a legitimate coordination problem: how to preserve the state apparatus (and its population's willingness to be governed) while eliminating the theological claim that had justified militarism and imperial expansion. The constraint is experienced as coordination: maintaining symbols while relocating sovereignty. Beneficiary with arbitrage options — can interpret the clause expansively or narrowly depending on governance needs. No extraction experienced because the constraint serves the beneficiary's preferred order.
constraint_indexing:constraint_classification(symbol_emperor__kokutai_severed_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE OFFICE OF THE EMPEROR (PITON) — The emperor becomes a performed role: head of state performing continuity. Theater ratio is high (0.65) — the office functions largely through ceremonial action that has no direct governance power. Yet the office persists because it performs a legitimacy function that the constitution itself requires. The role is constrained (cannot be abolished without constitutional amendment) but its substance has atrophied. Piton classification: the office remains through institutional inertia and because its ceremonial function is mandated, not because it exercises extractive power.
constraint_indexing:constraint_classification(symbol_emperor__kokutai_severed_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, the severance of theological statehood appears as a natural law: once a state embraces secular constitutionalism, theological cosmology cannot ground sovereignty without contradiction. The clause is not extraction but logical necessity — incompatible doctrines cannot coexist in a unified legal order. This perspective risks false-summit naturalization of a historically contingent political choice. The structural data reveals this as deliberate constitutional design, not inevitable law.
constraint_indexing:constraint_classification(symbol_emperor__kokutai_severed_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symbol_emperor__kokutai_severed_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(symbol_emperor__kokutai_severed_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(symbol_emperor__kokutai_severed_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(symbol_emperor__kokutai_severed_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(symbol_emperor__kokutai_severed_reading, TR),
    TR >= 0.70.

:- end_tests(symbol_emperor__kokutai_severed_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The clause achieves its extraction through belief suppression rather than direct coercion. Those who hold kokutai orthodoxy are not imprisoned or executed; instead, their cosmology is treated as incompatible with legitimate civic discourse. The extractive mechanism operates through identity incompatibility — the believer must either abandon the belief or accept civic delegitimization. This is more severe than mere constraint (high cost to exit) but operates through internalized identity rather than external force. The measurement trajectory (0.35 → 0.48 → 0.58) reflects the intensification of this mechanism: initial pragmatic accommodation of ceremony (low extraction) → post-occupation normalization of secular constitutionalism (moderate extraction) → contemporary entrenchment where theological statehood is culturally unthinkable in mainstream discourse (high extraction). Suppression (0.68): High. Multiple barriers prevent the restoration of kokutai orthodoxy as a legitimate state doctrine: constitutional prohibition of amending Article 1 without super-majority consensus, social delegitimization of theological claims, international treaties incorporating secular statehood as a condition of sovereignty recognition, and the believer's own internalized understanding that such claims are 'unscientific' or 'irrational.' Suppression is not merely legal but epistemic and identity-constitutive. Theater ratio (0.65): Moderate-high. The imperial ceremonies (state funerals, enthronement rites, shrine visits) perform a continuity function but are substantially theater. They maintain symbolic legitimacy but have no direct governance effect. The priest must perform the ritual while not claiming any theological grounding for it — the performance is separated from its original meaning. This is not pure theater (coordination of legitimacy is real) but the theological substance has been evacuated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates mandatrophy resolution through perspectival divergence. The beneficiary (secular constitutionalism) sees coordination — a legitimate solution to the governance problem of how to preserve the state across a revolution. The priesthood sees mixed coordination and extraction — they coordinate continuity but bear the cost of delegitimization. The believer sees pure extraction disguised as ceremony — they must abandon identity or accept civic exclusion. The analytical observer risks seeing natural law — as if secular constitutionalism necessarily forbids theological claims — when the structure reveals a contingent institutional choice that benefits one party at the cost of delegitimizing another. The piton perspective shows how the emperor's role atrophies into ceremonial inertia, maintained not because it wields power but because its performance is mandated by the constitution itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective flows from structural position. Secular constitutionalism as beneficiary with arbitrage options (can interpret clause broadly or narrowly, control enforcement mechanisms) derives d ≈ 0.05-0.15 → low/negative effective extraction chi. The priesthood as moderate institutional actor constrained by constitutional mandate derives d ≈ 0.50-0.60 → moderate chi reflecting mixed benefit (maintain ceremonies) and cost (delegitimized authority). The believer as powerless identity-locked agent derives d ≈ 0.90 → high chi because identity fusion prevents exit even where structural mobility might exist (the believer could technically speak kokutai claims, but identity-locked exit makes this unthinkable). The analytical observer derives d ≈ 0.73 → moderate-high chi reflecting the epistemic distance required to see the constraint's true structure. Suppression is unscaled (0.68 throughout) because it is a structural property of the constraint, not observer-relative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_suppression_mechanism,
    'Is the suppression of kokutai orthodoxy enforced through legal prohibition, social delegitimization, or internalized identity incompatibility?',
    'Historical analysis of enforcement mechanisms: explicit censorship vs. soft power vs. internalized exclusion. Comparison with explicit prohibitions of other doctrines in the same period.',
    'If purely legalized (explicit prohibition): constraint is snare with clear coercive boundaries. If social delegitimization: extraction is more diffuse and identity_locked is accurate. If internalized: constraint operates through the believer''s own identity reconstruction rather than external force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_suppression_mechanism, empirical, 'Mechanism of suppression: legal, social delegitimization, or internalized identity incompatibility').

omega_variable(
    continuity_performance_functionality,
    'Does the emperor''s ceremonial role perform a genuine state function (legitimacy maintenance, institutional continuity) or is it purely theatrical with no functional load?',
    'Comparative analysis: states without imperial symbols vs. states with ceremonial monarchies. Measurement of legitimacy indices and institutional stability with/without the imperial office. Analysis of which governance functions depend on imperial performance.',
    'If genuinely functional: piton classification is correct — the constraint coordinates legitimacy while appearing degraded. If purely theatrical: the constraint is closer to snare disguised as rope — the believer''s internalized acceptance of irrelevance IS the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_performance_functionality, empirical, 'Whether the emperor''s ceremonial role performs genuine state function or is purely theatrical').

omega_variable(
    reading_contest_constitutional_interpretation,
    'Which reading of the symbol-clause correctly describes the constitutional reality: Does the clause preserve kokutai through ceremonial continuity (continuity_device_reading), sever it from law while maintaining its social role (kokutai_severed_reading), or relocate sovereignty from emperor to people (sovereignty_relocated_reading)?',
    'Jurisprudential analysis: Japanese constitutional court decisions, scholarly consensus on Article 1 interpretation, historical intent documentation from the occupation period. Structural test: which reading predicts actual constraint behavior (what gets suppressed, who benefits, where extraction occurs).',
    'Different readings produce different classification outcomes. Continuity_device produces higher coordination value (Rope for more perspectives). Kokutai_severed emphasizes extraction and delegitimization (Snare for believer, Tangled Rope for apparatus). Sovereignty_relocated emphasizes institutional restructuring (Mountain or Rope depending on continuity maintenance). The three readings are not empirically equivalent — they predict different constraint behavior patterns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_constitutional_interpretation, conceptual, 'Which reading of the symbol-clause correctly describes constitutional reality and constraint structure').

omega_variable(
    false_summit_natural_law,
    'Is the theological suppression a natural consequence of secular constitutionalism (law of nature for political orders) or a historical choice that could have been made differently?',
    'Comparative analysis: other democracies that maintain theological statehood elements (Denmark, United Kingdom, Israel). Counterfactual history: could Japan have adopted secular democracy while preserving theological imperial claims? What alternatives existed in 1947?',
    'If natural law: mountain classification is correct for analytical observer. If historical choice: mountain is a false summit disguising an institutional arrangement that benefits secular constitutionalism and extracts from believers. FSM signature will trigger if beneficiaries are confirmed (secular constitutionalism does benefit; theological enforcement apparatus does bear costs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether theological suppression is natural law or historical institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symbol_emperor__kokutai_severed_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kokutai_sev_theater_t0, symbol_emperor__kokutai_severed_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(kokutai_sev_theater_t5, symbol_emperor__kokutai_severed_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(kokutai_sev_theater_t10, symbol_emperor__kokutai_severed_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(kokutai_sev_extract_t0, symbol_emperor__kokutai_severed_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(kokutai_sev_extract_t5, symbol_emperor__kokutai_severed_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(kokutai_sev_extract_t10, symbol_emperor__kokutai_severed_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(kokutai_sev_suppression_t0, symbol_emperor__kokutai_severed_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(kokutai_sev_suppression_t5, symbol_emperor__kokutai_severed_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(kokutai_sev_suppression_t10, symbol_emperor__kokutai_severed_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symbol_emperor__kokutai_severed_reading, identity_coordination).
narrative_ontology:affects_constraint(symbol_emperor__kokutai_severed_reading, symbol_emperor__continuity_device_reading).
narrative_ontology:affects_constraint(symbol_emperor__kokutai_severed_reading, symbol_emperor__sovereignty_relocated_reading).

% DUAL FORMULATION NOTE:
% The symbol-emperor kernel generates three constraint stories with distinct epsilon values and beneficiary/victim structures. The kokutai_severed_reading emphasizes suppression of theological statehood (ε=0.58, Tangled Rope). The continuity_device_reading emphasizes preservation through institutional pragmatism (lower ε, higher Rope classification). The sovereignty_relocated_reading emphasizes the magnitude of institutional change (possibly higher ε and more prominent Snare from the traditionalist perspective). All three are linked through network.affects_constraints because the same constitutional clause is being read differently by each framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(symbol_emperor__kokutai_severed_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
