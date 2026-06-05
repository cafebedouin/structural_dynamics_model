% ============================================================================
% CONSTRAINT STORY: symbol_emperor__continuity_device_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_symbol_emperor__continuity_device_reading, []).

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
 *   constraint_id: symbol_emperor__continuity_device_reading
 *   human_readable: The Symbol Emperor as Continuity Device (Postwar Japan Reading)
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   The symbol emperor constraint represents one contested reading of Japan's
 *   postwar constitutional settlement. The 1947 Constitution formally demoted
 *   the emperor from sovereign to 'symbol of the State and of the unity of
 *   the people,' a reframing widely interpreted as rendering the throne
 *   ceremonial rather than executive. This constraint story instantiates the
 *   'continuity device' reading: the throne's preservation was the
 *   occupation's keystone bargain, a strategic choice to make postwar Japan
 *   governable by preserving the oldest institutional face in the room.
 *   Comprehensibility and legitimacy for the occupation's new constitutional
 *   order flowed through imperial continuity. The extraction mechanism was
 *   the suppression of competing political outcomes — abolitionist movements,
 *   prosecutorial momentum for war accountability, and alternative
 *   constitutional framings — that would have destabilized the occupation's
 *   preferred settlement. The beneficiary was not the throne itself but the
 *   transition's stability: the occupation authority, the Japanese political
 *   establishment, and the postwar order all benefited from the throne's
 *   preservation as a coordination device. The victims were accountability
 *   processes and the abolitionist political movement, whose goals were
 *   structurally suppressed by the foundational bargain. This reading
 *   coexists with two siblings: the 'kokutai severed' reading (the mystical
 *   national polity was genuinely dismantled and the throne is now only
 *   ceremony) and the 'sovereignty relocated' reading (the constitution
 *   performed a revolution in plain language — power moved from emperor to
 *   people). These readings represent different parties' competing
 *   interpretations of the same constitutional text, not logical
 *   contradictions. The constraint demonstrates how a single doctrinal kernel
 *   can be read as either a coordination mechanism, a severance, or a
 *   revolution depending on the observer's structural position and framing
 *   commitments.
 *
 * KEY AGENTS:
 *   - Occupation Authority (institutional/arbitrage) — beneficiary of throne preservation as governance stabilization mechanism; primary agent enforcing the constraint
 *   - Japanese Political Establishment (moderate/constrained) — mixed position: benefits from throne's legitimacy bridge but also constrained by occupation control and amnesty compact
 *   - Abolitionist Coalition (powerless/trapped) — victim; their core political goal is structurally suppressed by the non-negotiable throne preservation
 *   - War Accountability Movement (powerless/trapped) — victim; prosecutorial momentum systematically suppressed to protect the throne's political viability
 *   - Imperial Institution (institutional/arbitrage from post-enforcement perspective; degraded to piton classification generationally) — nominally beneficiary but functionally diminished; the throne's sacredness extracted but sacred-kingship authority severed
 *   - Analytical Observer (analytical/analytical) — risks naturalizing the contingent occupation bargain as a structural law of regime transitions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symbol_emperor__continuity_device_reading, 0.62).
domain_priors:suppression_score(symbol_emperor__continuity_device_reading, 0.68).
domain_priors:theater_ratio(symbol_emperor__continuity_device_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symbol_emperor__continuity_device_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(symbol_emperor__continuity_device_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(symbol_emperor__continuity_device_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symbol_emperor__continuity_device_reading, tangled_rope).
narrative_ontology:human_readable(symbol_emperor__continuity_device_reading, "The Symbol Emperor as Continuity Device (Postwar Japan Reading)").
narrative_ontology:topic_domain(symbol_emperor__continuity_device_reading, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(symbol_emperor__continuity_device_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(symbol_emperor__continuity_device_reading, 'ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3').
narrative_ontology:cs_kernel_codification('ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3', formalized).
narrative_ontology:cs_authority_grounding('ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3', extraction).
narrative_ontology:cs_interpretation_layer_present('ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3').
narrative_ontology:cs_reading_relation('ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3', symbol_emperor__kokutai_severed_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3', symbol_emperor__sovereignty_relocated_reading, coexists_with).
narrative_ontology:cs_axiom('ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3', foundational, throne_preservation_governance_necessity).
narrative_ontology:cs_axiom_status(throne_preservation_governance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3', throne_preservation_governance_necessity, instrumental).
narrative_ontology:cs_axiom('ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3', foundational, accountability_suppression_acceptable_transition_cost).
narrative_ontology:cs_axiom_status(accountability_suppression_acceptable_transition_cost, holdable).
narrative_ontology:cs_axiom_grounding('ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3', accountability_suppression_acceptable_transition_cost, instrumental).
narrative_ontology:cs_reference_frame('ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3', imperial_continuity_governance_stability).
narrative_ontology:cs_drift_state('ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3', contemporary_constitutional_accountability_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ed3a2cf0-3f0a-4800-ada5-0a0ae62036d3', '2026-02-27T14:32:00Z').
narrative_ontology:cs_kernel_id(symbol_emperor__continuity_device_reading, symbol_emperor).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(symbol_emperor__continuity_device_reading, occupation_stabilization).
narrative_ontology:constraint_beneficiary(symbol_emperor__continuity_device_reading, imperial_institution).
narrative_ontology:constraint_beneficiary(symbol_emperor__continuity_device_reading, allied_governance_apparatus).
narrative_ontology:constraint_victim(symbol_emperor__continuity_device_reading, war_accountability).
narrative_ontology:constraint_victim(symbol_emperor__continuity_device_reading, prosecutorial_process).
narrative_ontology:constraint_victim(symbol_emperor__continuity_device_reading, abolitionist_political_movement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ABOLITIONIST COALITION (SNARE) — Powerless to exit; their core political goal (imperial system abolition) is structurally suppressed by the occupation's foundational bargain. The throne's preservation is non-negotiable; abolition advocates face marginalization in occupation policy and no institutional pathway to challenge the constraint. Maximum suppression, zero perceived coordination benefit.
constraint_indexing:constraint_classification(symbol_emperor__continuity_device_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WAR ACCOUNTABILITY MOVEMENT (SNARE) — Trapped by the amnesty compact embedded in the symbol emperor bargain. Prosecutorial momentum is systematically suppressed to preserve the throne's political viability. No exit from this constraint without breaking the occupation's foundational deal. High extraction — the constraint extracts legitimacy from accountability advocates to transfer it to continuity.
constraint_indexing:constraint_classification(symbol_emperor__continuity_device_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: JAPANESE POLITICAL ESTABLISHMENT (TANGLED ROPE) — Constrained by occupation presence and constitutional framework but also benefits from the throne's preservation as a coordination mechanism for rebuilding governance legitimacy. The symbol emperor bargain both extracts (external control over domestic political settlement) and enables (institutional continuity for state reconstruction). Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(symbol_emperor__continuity_device_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ALLIED OCCUPATION AUTHORITY (ROPE) — Experiences the symbol emperor constraint as pure coordination from their governance standpoint. Preserving the throne solves the acute problem of making postwar Japan governable without prohibitive direct military administration. The throne legitimates the occupation-installed constitutional order. Net beneficiary — extraction flows toward occupation stabilization, not away.
constraint_indexing:constraint_classification(symbol_emperor__continuity_device_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SYMBOLIC EMPEROR INSTITUTION (PITON) — Generationally, the emperor office becomes a diminished ceremonial role whose primary function is performative continuity. The sacred-kingship extraction mechanism (divine mandate) has been severed by the 1947 constitutional reframing. What remains is theatrical preservation of a role that no longer executes its original legitimating function. The institution persists through inertia and constitutional piety, not because it governs.
constraint_indexing:constraint_classification(symbol_emperor__continuity_device_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY (MOUNTAIN) — From a civilizational perspective, regime transitions always require legitimacy bridges. The symbol emperor reading presents the throne's preservation as a structurally necessary immutable feature: without continuity facing, revolution becomes ungovernable. This perspective risks naturalizing a contingent political choice as a law of transition dynamics. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(symbol_emperor__continuity_device_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symbol_emperor__continuity_device_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(symbol_emperor__continuity_device_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(symbol_emperor__continuity_device_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(symbol_emperor__continuity_device_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(symbol_emperor__continuity_device_reading, TR),
    TR >= 0.70.

:- end_tests(symbol_emperor__continuity_device_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint extracts legitimacy from accountability and abolitionist movements and transfers it to throne preservation and occupation stabilization. The extraction is substantial but not maximal because the throne's preservation also solved genuine coordination problems — postwar Japan required legitimacy bridges, and the occupation's preference for the throne happened to align with pragmatic governance necessity. Distinguishing the extraction from the coordination is the empirical problem posed by omega 1. Suppression (0.68): High. The constraint systematically suppresses abolitionist and prosecutorial outcomes through the occupation's foundational bargain. Alternative paths — republican settlement, transitional accountability councils, direct constitutional ratification without imperial face — were not given policy space. The suppression was active and deliberate, not accidental. Theater ratio (0.55): Moderate. The symbol emperor constraint involves both genuine governance function (the throne provides institutional continuity for the constitutional order) and performative function (the constitutional reframing of the throne as 'symbol' rather than sovereign is partly semantic theater that allows the occupation to appear to democratize while preserving the institution). The theater has increased slightly across the postwar interval as the throne's actual powers have diminished and its role has become more purely ceremonial.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The occupation authority perceives pure coordination (rope) — the throne is solving the problem of making postwar Japan stable and governable. The abolitionist coalition perceives pure extraction (snare) — their political goal is structurally suppressed with no institutional pathway to challenge it. The accountability movement perceives snare with the same structure (victims trapped by amnesty compact). The Japanese political establishment perceives tangled rope — they benefit from throne legitimacy but are constrained by occupation control and the suppression of alternative constitutional paths. The throne institution itself, viewed generationally, appears as a piton (performing ceremonial function, actual governing power severed by the constitution). The analytical observer risks seeing structural necessity (mountain) — regime transitions require legitimacy bridges, and the throne's preservation was necessary. This mountain perspective is a false summit: the preservation was a choice favoring continuity over accountability, not an inevitable structural feature. The engine's false summit detector will identify this because beneficiaries are declared, revealing that the 'structural necessity' framing naturalizes a contingent political bargain.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality differs sharply across agents. Abolitionist and accountability agents face high d values (near 1.0) — they are victims with no exit options; they experience maximum chi. The occupation authority faces low d (near 0.0) — they are beneficiaries with full arbitrage; they experience negative or minimal chi. The Japanese political establishment faces moderate d (near 0.55) — they benefit from the throne's legitimacy but are constrained by occupation control. These different d values derive from the beneficiary/victim declarations and exit options, producing a perspectival gap: the powerless agents see snare (high chi); the institutional beneficiary sees rope (negative chi); the moderate agent sees tangled rope (moderate chi). The derivation chain is: (beneficiary + arbitrage → low d → negative f(d)), (victim + trapped → high d → high f(d)). The scope modifier σ(S) = 1.0 (national scope) applies to all agents equally.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the classification is reading-dependent, not observer-relative within a single reading. Within the 'continuity device' reading (which this story instantiates), the constraint is genuinely tangled rope: it coordinates governance stability (rope function) while suppressing accountability and abolition (snare function). Both functions are real; neither is theater. The mandatrophy does not arise from ambiguity about which type is 'correct' — it arises from the coexistence of coordination and extraction in a single constraint. The sibling readings ('kokutai severed' and 'sovereignty relocated') would shift the classification by reframing the suppression: if kokutai doctrine is genuinely severed, the constraint becomes purely coordinative (rope). If sovereignty is relocated, the constraint becomes a revolution (a different structure entirely, potentially a scaffold with constitutional sunset). The three readings are live competing interpretations held by different parties; they do not resolve into a single true answer, but rather exhaust the interpretive space around the contested kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_coordination_vs_extraction_cover,
    'Is the throne''s preservation primarily a genuine coordination mechanism for postwar governance, or primarily a mechanism for suppressing accountability and abolitionist politics under the guise of governance necessity?',
    'Counterfactual analysis: comparison of occupation policy decisions with/without the throne preservation constraint; examination of whether occupation authorities actively blocked prosecutorial and abolitionist pathways that might have been governable alternatives; archival evidence of deliberate choice vs. structural inevitability in maintaining the throne.',
    'If primarily coordination: the constraint reclassifies toward Rope from multiple perspectives; suppression and extraction values decrease. If primarily extraction cover: the constraint confirms Tangled Rope/Snare classifications; the ''structural necessity'' framing is diagnosed as false-summit naturalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genuine_coordination_vs_extraction_cover, empirical, 'Whether throne preservation is genuine coordination necessity or extraction mechanism disguised as necessity').

omega_variable(
    alternative_legitimacy_pathways,
    'Were other legitimacy bridges available to stabilize postwar Japan without preserving the throne — e.g., a republican restoration framed as ''new beginning,'' a transitional council, or direct popular constitutional ratification without imperial face?',
    'Historical counterfactual analysis; examination of occupation debates and rejected alternatives; comparative analysis of other postwar regimes (Germany, Italy, South Korea) and their legitimacy mechanisms; polling or archival evidence about Japanese public support for alternatives.',
    'If alternatives existed but were actively rejected: the throne preservation becomes a choice favoring continuity over accountability, confirming the suppression diagnosis. If no genuine alternatives existed: the constraint was structurally necessary, reclassifying toward Rope/Mountain from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legitimacy_pathways, empirical, 'Whether alternative postwar legitimacy pathways existed and why they were rejected').

omega_variable(
    kokutai_doctrine_severance_scope,
    'Did the constitutional 1947 reframing actually sever the kokutai (mystical national polity) doctrine completely, or did it merely relocate the doctrine beneath the constitutional text where it continues to ground legitimacy in practice?',
    'Doctrinal analysis of kokutai claims in postwar Japanese legal scholarship and imperial commentary; examination of whether the throne''s role is genuinely understood as ceremonial-only or still carries legitimacy implications that exceed the constitutional text; analysis of how the constraint has functioned across postwar decades — does suppression of alternative constitutional interpretations persist as though kokutai remained operative?',
    'If kokutai doctrine was genuinely severed: this reading is accurate; the symbol emperor is a coordination device for a desacralized throne. If kokutai persists beneath the text: the suppression mechanism is deeper; the amnesty compact serves to preserve an implicit sacred-kingship extraction that the constitution formally denies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kokutai_doctrine_severance_scope, conceptual, 'Whether the constitutional reframe genuinely severed kokutai doctrine or relocated it beneath the text').

omega_variable(
    reading_contest_foreclosure_structure,
    'Do the three readings of the symbol emperor kernel logically foreclose each other in any framework, or do they coexist as different parties'' competing framings of the same constitutional text?',
    'Doctrinal analysis of logical premises: continuity_device_reading assumes the throne is preserved for governance stability; kokutai_severed_reading assumes the sacred-kingship doctrine was actually dismantled; sovereignty_relocated_reading assumes power fundamentally moved from emperor to people. Determine whether these premises are logically incompatible (one must be false if another is true) or whether they describe different aspects of the same constitutional event that different parties emphasize differently.',
    'If any reading forecloses others: update cs_structure.reading_relations to ''forecloses'' rather than ''coexists_with''. If all coexist: confirm the coexists_with relations. If some coexist and some have influence relationships: differentiate the relations accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure_structure, conceptual, 'Logical foreclosure structure among the three readings of the symbol emperor kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symbol_emperor__continuity_device_reading, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(symbol_emp_theater_t0, symbol_emperor__continuity_device_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(symbol_emp_theater_t3, symbol_emperor__continuity_device_reading, theater_ratio, 3, 0.52).
narrative_ontology:measurement(symbol_emp_theater_t7, symbol_emperor__continuity_device_reading, theater_ratio, 7, 0.55).

% Extraction over time
narrative_ontology:measurement(symbol_emp_extract_t0, symbol_emperor__continuity_device_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(symbol_emp_extract_t3, symbol_emperor__continuity_device_reading, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(symbol_emp_extract_t7, symbol_emperor__continuity_device_reading, base_extractiveness, 7, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(symbol_emp_suppression_t0, symbol_emperor__continuity_device_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(symbol_emp_suppression_t3, symbol_emperor__continuity_device_reading, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(symbol_emp_suppression_t7, symbol_emperor__continuity_device_reading, suppression_requirement, 7, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symbol_emperor__continuity_device_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(symbol_emperor__continuity_device_reading, symbol_emperor__kokutai_severed_reading).
narrative_ontology:affects_constraint(symbol_emperor__continuity_device_reading, symbol_emperor__sovereignty_relocated_reading).

% DUAL FORMULATION NOTE:
% The symbol emperor kernel has three readings: continuity_device_reading (this story), kokutai_severed_reading, and sovereignty_relocated_reading. Each reading has distinct extractiveness and classification. Continuity device = ε ≈ 0.62, tangled rope (mixed coordination and suppression). Kokutai severed = lower ε, more rope-like (if doctrine is genuinely severed, suppression of abolitionism becomes ideological rather than structural). Sovereignty relocated = different ε, scaffold or transitional structure (if power transferred, the constraint has built-in sunset). The three readings are not observable-dependent variations of one constraint — they instantiate genuinely different claim structures about what the constitutional text accomplished. Network links enable contamination analysis: if the continuity device reading's extraction mechanisms become public, the kokutai severed reading's credibility as 'the doctrine was genuinely dismantled' may weaken.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
