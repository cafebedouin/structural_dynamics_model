% ============================================================================
% CONSTRAINT STORY: structural_housekeeping_amendments__twenty_second_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_housekeeping_amendments__twenty_second_amendment, []).

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
 *   constraint_id: structural_housekeeping_amendments__twenty_second_amendment
 *   human_readable: The Twenty-Second Amendment: Presidential Term Limits
 *   domain: constitutional_law/executive_power
 *
 * SUMMARY:
 *   The Twenty-Second Amendment, ratified in 1951, constitutionalizes the
 *   unwritten two-term presidential tradition that Franklin D. Roosevelt
 *   broke in 1940 and 1944. The constraint operates as a
 *   structural-housekeeping amendment that suppresses the executive
 *   accumulation of power across terms while preserving democratic rotation
 *   and opposition-party access to the presidency. It is one reading within a
 *   family of structural amendments that repair or clarify the mechanics of
 *   constitutional governance (the Twelfth Amendment fixed the electoral
 *   college after 1800, the Twentieth Amendment moved inauguration and killed
 *   the lame-duck session, the Twenty-First repealed Prohibition, and the
 *   Twenty-Seventh delayed congressional pay raises). The Twenty-Second
 *   differs from pure housekeeping in that it actively redistributes power
 *   from popular incumbents and their coalitions to the rotation mechanism
 *   and opposition parties. The constraint exhibits classic tangled-rope
 *   characteristics: genuine coordination function (prevents autocratic
 *   entrenchment, enables peaceful power transfer) combined with extraction
 *   from the target group (popular incumbents lose the option to continue).
 *   The false-summit risk is substantial: the analytical perspective risks
 *   naturalizing the amendment as immutable law of governance when it is
 *   actually a contingent constitutional choice that benefits specific
 *   political actors.
 *
 * KEY AGENTS:
 *   - Popular Incumbent: Primary target (powerless/trapped) — faces absolute prohibition regardless of electoral appeal or necessity
 *   - Incumbent Coalition: Secondary target (moderate/constrained) — forced dispersal when the incumbent becomes term-limited; high cost to exit
 *   - Opposition Party: Primary beneficiary (organized/mobile) — guaranteed periodic access to power through rotation; low-cost mechanism for opposition turnover
 *   - Executive Branch Institution: Mixed (institutional/arbitrage) — benefits from legitimacy through rotation but faces extraction of accumulated institutional continuity
 *   - Democratic Rotation System: Beneficiary (institutional/arbitrage) — pure coordination function; ensures peaceful power transfer without entrenchment
 *   - Analytical Observer: Risks naturalizing contingent design (analytical/analytical) — may classify as mountain (natural law) when structural data indicates false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_housekeeping_amendments__twenty_second_amendment, 0.32).
domain_priors:suppression_score(structural_housekeeping_amendments__twenty_second_amendment, 0.68).
domain_priors:theater_ratio(structural_housekeeping_amendments__twenty_second_amendment, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_housekeeping_amendments__twenty_second_amendment, extractiveness, 0.32).
narrative_ontology:constraint_metric(structural_housekeeping_amendments__twenty_second_amendment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(structural_housekeeping_amendments__twenty_second_amendment, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_housekeeping_amendments__twenty_second_amendment, tangled_rope).
narrative_ontology:human_readable(structural_housekeeping_amendments__twenty_second_amendment, "The Twenty-Second Amendment: Presidential Term Limits").
narrative_ontology:topic_domain(structural_housekeeping_amendments__twenty_second_amendment, "constitutional_law/executive_power").

domain_priors:requires_active_enforcement(structural_housekeeping_amendments__twenty_second_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_housekeeping_amendments__twenty_second_amendment, '6ff7db79-17d3-40d5-82bf-2a21f0670b64').
narrative_ontology:cs_kernel_codification('6ff7db79-17d3-40d5-82bf-2a21f0670b64', formalized).
narrative_ontology:cs_authority_grounding('6ff7db79-17d3-40d5-82bf-2a21f0670b64', lineage).
narrative_ontology:cs_interpretation_layer_present('6ff7db79-17d3-40d5-82bf-2a21f0670b64').
narrative_ontology:cs_reading_relation('6ff7db79-17d3-40d5-82bf-2a21f0670b64', structural_housekeeping_amendments__twelfth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('6ff7db79-17d3-40d5-82bf-2a21f0670b64', structural_housekeeping_amendments__twentieth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('6ff7db79-17d3-40d5-82bf-2a21f0670b64', structural_housekeeping_amendments__twenty_first_amendment, coexists_with).
narrative_ontology:cs_reading_relation('6ff7db79-17d3-40d5-82bf-2a21f0670b64', structural_housekeeping_amendments__twenty_seventh_amendment, influences).
narrative_ontology:cs_axiom('6ff7db79-17d3-40d5-82bf-2a21f0670b64', foundational, executive_accumulation_is_constrain_able).
narrative_ontology:cs_axiom_status(executive_accumulation_is_constrain_able, holdable).
narrative_ontology:cs_axiom_grounding('6ff7db79-17d3-40d5-82bf-2a21f0670b64', executive_accumulation_is_constrain_able, conventional).
narrative_ontology:cs_axiom('6ff7db79-17d3-40d5-82bf-2a21f0670b64', foundational, rotation_preserves_democratic_legitimacy).
narrative_ontology:cs_axiom_status(rotation_preserves_democratic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6ff7db79-17d3-40d5-82bf-2a21f0670b64', rotation_preserves_democratic_legitimacy, deontological).
narrative_ontology:cs_reference_frame('6ff7db79-17d3-40d5-82bf-2a21f0670b64', two_term_tradition_constitutionalized).
narrative_ontology:cs_drift_state('6ff7db79-17d3-40d5-82bf-2a21f0670b64', contemporary_norm_testing, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('6ff7db79-17d3-40d5-82bf-2a21f0670b64', '').
narrative_ontology:cs_kernel_id(structural_housekeeping_amendments__twenty_second_amendment, structural_housekeeping_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_housekeeping_amendments__twenty_second_amendment, electoral_rotation_mechanism).
narrative_ontology:constraint_beneficiary(structural_housekeeping_amendments__twenty_second_amendment, opposition_parties).
narrative_ontology:constraint_victim(structural_housekeeping_amendments__twenty_second_amendment, popular_incumbents).
narrative_ontology:constraint_victim(structural_housekeeping_amendments__twenty_second_amendment, incumbent_coalitions).
narrative_ontology:constraint_victim(structural_housekeeping_amendments__twenty_second_amendment, executive_continuity_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POPULAR INCUMBENT (SNARE) — A president with demonstrated electoral appeal and governing continuity faces absolute prohibition from seeking a third term, regardless of constituent preference or strategic necessity. No exit option exists: the constitutional amendment cannot be circumvented through campaign, negotiation, or succession strategy. The incumbent experiences maximum suppression (cannot run) coupled with extraction (power accumulated over two terms cannot be leveraged for continued governance). This is pure snare: high suppression, no coordination benefit for the target.
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_second_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT COALITION (SNARE) — Political coalition aligned with a sitting president (party apparatus, allied donors, appointed officials) faces forced dispersal when the president becomes term-limited. Exit is costly but partially possible: members can join successor candidates or new coalitions, but they lose the organizational advantage and patron relationship of incumbent power. High suppression (cannot continue the original alliance), moderate extraction (accumulated political capital must be surrendered). Classified as snare because the suppression dominates—the coalition has no option to remain intact.
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_second_amendment, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPPOSITION PARTY (ROPE) — The opposing party benefits from guaranteed turnover: term limits ensure that even popular presidents cannot accumulate indefinite power, creating periodic opportunities for power transfer. This party experiences the amendment as pure coordination—the mechanism solves the collective action problem of 'how does power transfer peacefully without allowing one faction to entrench?' The opposition has exit options and agency (can compete in open elections every four/eight years). No extraction occurs from their perspective; the constraint coordinates their interests with democratic rotation. Beneficiary classification applies.
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_second_amendment, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE BRANCH INSTITUTION (TANGLED ROPE) — The presidency as an institution benefits from term limits (prevents hereditary-style perpetuation, maintains legitimacy through rotation) but faces extraction costs (loss of institutional continuity, accumulated executive knowledge, long-term policy implementation). The institution has arbitrage options: can develop succession mechanisms, strengthen institutional memory through career bureaucracy, delegate long-term planning to Congress. The constraint is hybrid: genuine coordination function (prevents autocratic entrenchment) combined with real extraction (caps accumulated executive power). Requires active enforcement (constitutional amendment is hard to change, creates legal and cultural barriers to circumvention).
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_second_amendment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DEMOCRATIC ROTATION SYSTEM (ROPE) — The constraint is perceived as pure coordination from the standpoint of the democratic system itself: it solves the structural problem of how to ensure peaceful power transfer without allowing democratic winners to consolidate permanent control. The system benefits from the constraint (prevents autocratic accumulation), experiences low extraction costs (rotation is the intended function), and has multiple arbitrage options (amendment process is slow but exists, institutional adaptation is possible). No significant suppression of democratic function; the mechanism enables democratic process rather than blocking it.
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_second_amendment, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some limit on individual tenure in supreme power is an immutable requirement of sustainable governance systems. The amendment constitutionalizes what appears as a natural law of political organization: unchecked accumulation of executive power degenerates into autocracy or dynasticism. Under this reading, the Twenty-Second Amendment does not create a constraint so much as acknowledge an existing natural law—rotation is necessary, and the amendment merely formalizes what all viable democracies discover through trial. However, the structural data contradicts this: the constraint has identifiable beneficiaries (opposition parties, the rotation mechanism itself), clear victims (popular incumbents), and real suppression that requires active enforcement. The engine will identify this as a false summit candidate.
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_second_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_housekeeping_amendments__twenty_second_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_second_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_second_amendment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(structural_housekeeping_amendments__twenty_second_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The amendment extracts from popular incumbents (caps their ability to continue) and from incumbent coalitions (forces dispersal), but the extraction is not severe because it is bounded, known in advance, and rationally accepted as part of the constitutional system. No president enters office unaware of the term limit. The extraction increases over time as a president accumulates power—by the end of the first term or during the second term, the extractive force intensifies (measurement shows rise from 0.18 at ratification to 0.32 by 1953 when Eisenhower's implicit succession began). Suppression (0.68): High. The constitutional text is the hardest form of legal suppression—it cannot be circumvented through campaign, political pressure, or institutional adaptation. A popular incumbent with majority support cannot run for a third term; this is absolute suppression of that specific exit option. However, suppression is not total (suppression ≤ 1.0) because other executive options exist (vice-presidential candidacy for a successor, influence over succession, domestic governance during the final term). The suppression increased from 0.45 (informal norm in 1945) to 0.68 (constitutional requirement by 1953) as the amendment was ratified and internalized. Theater ratio (0.35): Low. The amendment has genuine functional content—it directly prevents third-term candidacy and reshapes power succession. The mechanism is not performative (unlike peer review, which is substantially theater). The text is clear, the enforcement is automatic, and the compliance is universal. Low theater indicates the constraint is primarily functional rather than symbolic.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a pronounced perspectival gap between beneficiaries and victims. The opposition party and the democratic rotation system see a pure coordination mechanism (Rope)—the amendment solves the collective action problem of ensuring power transfer without entrenchment. Popular incumbents and their coalitions see pure extraction (Snare)—high suppression, no exit, loss of accumulated power. The executive branch institution occupies the middle (Tangled Rope)—it benefits from legitimacy through rotation but pays extraction costs through loss of continuity. The analytical observer risks seeing mountain (natural law of governance) when the structural data reveals false summit: the amendment benefits identifiable parties (opposition, rotation mechanism) and suppresses identifiable others (incumbents). The magnitude of the perspectival gap is large—from Rope (beneficiary) to Snare (victim) to Mountain (false summit) across the same structural phenomena.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the beneficiary/victim declarations and exit options. Popular incumbents are victims with trapped exit—maximum d (~0.95). Incumbent coalitions are victims with constrained exit—high d (~0.80). Opposition parties are beneficiaries with mobile exit—low d (~0.25). The executive institution is mixed (both benefits from rotation and pays continuity costs) with arbitrage options—moderate d (~0.50). The analytical observer's d is around 0.72 (canonical analytical value). The beneficiary/victim structure and exit options determine f(d) via the sigmoid function. Victims with trapped exit produce high f(d) values (~1.42), amplifying effective extraction. Beneficiaries with mobile/arbitrage exit produce low or negative f(d) values (~-0.01 to 0.40), reducing experienced extraction. The scope modifier σ(S) for national scope is 1.0 (no amplification or dampening). Thus χ = ε × f(d) × σ(S) produces: for trapped victims, χ = 0.32 × 1.42 × 1.0 ≈ 0.45 (snare territory); for beneficiaries, χ = 0.32 × 0.40 × 1.0 ≈ 0.13 (rope territory). This perspectival disparity is the diagnostic signal of tangled rope from the institutional midpoint perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying the hybrid nature: the amendment genuinely coordinates (ensures peaceful rotation) while genuinely extracting (from popular incumbents). It is not pure coordination (Rope) because it suppresses popular incumbency; it is not pure extraction (Snare) because it preserves democratic legitimacy through rotation. The institutional perspective (Tangled Rope) is the accurate canonical classification. The false-summit risk arises from the analytical perspective's temptation to naturalize the amendment: seeing it as an immutable requirement of sustainable governance (Mountain). However, the existence of identifiable beneficiaries (opposition parties, rotation mechanism) and clear suppression targets (popular incumbents, their coalitions) indicates this is a contingent constitutional choice, not a natural law. The omega variables document the ambiguity: whether the two-term limit is genuinely necessary (natural law) or merely beneficial to certain political actors (false summit). The resolution requires empirical analysis of democracies with different or absent term limits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_contingent_choice,
    'Is the two-term limit a natural law of governance (inherent to any sustainable democratic system) or a contingent constitutional choice made by the U.S. political system?',
    'Comparative analysis: survey longevity constraints in other democracies (France, Germany, Mexico, etc.) and their historical stability; examine whether democracies without explicit term limits show systematic pathologies requiring term limits; study counterfactual scenarios (U.S. without 22nd Amendment)',
    'If natural law: many democracies would converge on similar term limits; democracies without them would show degradation. If contingent: other democracies use different or no formal limits; U.S. governance would remain stable without the amendment (only norm would change). Classification ranges from Mountain (natural law) to Tangled Rope (contingent institutional choice with benefits and costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_choice, empirical, 'Whether the two-term limit is natural law or contingent constitutional design').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of third-term incumbency primarily enforced by external constitutional mechanism (legal prohibition) or internalized through cultural expectation and democratic norm?',
    'Historical analysis of informal enforcement: study norm-breaking attempts (e.g., Trump 2024 context); examine whether pre-amendment two-term norm held without constitutional force; measure compliance rates and political cost of norm violation',
    'If externally enforced: suppression = 0.68 (hard constitutional gate). If internalized: suppression could be lower (the norm holds even without text, amendment is performative). If hybrid: suppression value is accurate but mechanism understanding changes interpretation of whether this is mountain (natural norm hardened into text) or snare (artificial limitation enforced by text).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is constitutionally enforced or culturally internalized').

omega_variable(
    false_summit_diagnosis,
    'Does the analytical ''natural law'' perspective (mountain classification) reflect an inherent natural law of governance or a false summit—the naturalization of a contingent institutional arrangement that beneficiaries (opposition parties, rotation mechanism) promote as inevitable?',
    'Cross-position analysis: comparative democracies, historical counterfactuals, beneficiary discourse analysis (do opponents of term limits argue it violates natural law, or do proponents argue it enforces natural law?).',
    'If false summit: reclassify to Tangled Rope (genuine coordination function + identifiable extraction from incumbents). The ''natural law'' framing is revealed as a cover story for the beneficiary coalition''s interests. If genuine natural law: the beneficiaries are aligned with a real structural necessity, not extracting value from incumbents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_diagnosis, conceptual, 'Whether the mountain classification reflects a true natural law or a false summit naturalizing contingent institutional design').

omega_variable(
    two_vs_three_term_threshold,
    'Why is two terms the natural boundary rather than three, four, or one? Is there structural logic to the two-term limit or is it convention?',
    'Historical analysis of term-limit debates; comparative analysis of other democracies'' thresholds; study of accumulation dynamics (does fourth-term power create qualitatively different entrenchment than third-term power?)',
    'If conventional: the precise limit is a political choice, not natural law; different limits would work equally well or better. If structural: there is a threshold of accumulated power beyond which democratic vulnerability increases sharply. Affects the mountain classification—natural law would imply the specific limit is necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_vs_three_term_threshold, empirical, 'Whether the two-term limit is structurally natural or conventionally chosen').

omega_variable(
    reading_ambiguity_housekeeping_amendment,
    'Is the Twenty-Second Amendment fundamentally a housekeeping amendment (technical correction to executive structure after Roosevelt''s norm-breaking, like the Twelfth and Twentieth Amendments) or a substantive redistribution of power (limiting executive capacity in favor of rotation, like the Twenty-Seventh Amendment''s approach to Congress)?',
    'Authorial intent analysis (Congressional record, proponent arguments); structural analysis (does the amendment change power relationships or merely codify a boundary?); comparative analysis with true housekeeping amendments (Twelfth, Twentieth) versus power-redistribution amendments (Twenty-Seventh)',
    'If housekeeping: the amendment is seen as technical, not extractive—it prevents a loophole (indefinite tenure) and restores the original design. If substantive redistribution: the amendment redistributes power from incumbents to opposition parties and the rotation mechanism—it is intentionally extractive from popular incumbents. Affects narrative framing in kernel_context and reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_ambiguity_housekeeping_amendment, conceptual, 'Whether the Twenty-Second Amendment is housekeeping (technical correction) or substantive power redistribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_housekeeping_amendments__twenty_second_amendment, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(twa_extract_1945, structural_housekeeping_amendments__twenty_second_amendment, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(twa_extract_1953, structural_housekeeping_amendments__twenty_second_amendment, base_extractiveness, 2, 0.32).
narrative_ontology:measurement(twa_extract_2000, structural_housekeeping_amendments__twenty_second_amendment, base_extractiveness, 4, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(twa_suppress_1945, structural_housekeeping_amendments__twenty_second_amendment, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(twa_suppress_1953, structural_housekeeping_amendments__twenty_second_amendment, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(twa_suppress_2000, structural_housekeeping_amendments__twenty_second_amendment, suppression_requirement, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_housekeeping_amendments__twenty_second_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_housekeeping_amendments__twenty_second_amendment, twelfth_amendment).
narrative_ontology:affects_constraint(structural_housekeeping_amendments__twenty_second_amendment, twentieth_amendment).
narrative_ontology:affects_constraint(structural_housekeeping_amendments__twenty_second_amendment, executive_succession_mechanism).
narrative_ontology:affects_constraint(structural_housekeeping_amendments__twenty_second_amendment, opposition_party_access).

% DUAL FORMULATION NOTE:
% The Twenty-Second Amendment is part of the structural-housekeeping amendment family (twelfth, twentieth, twenty-first, twenty-seventh). Its ε value (0.32) reflects moderate extractiveness from popular incumbents. Sibling amendments have different ε values reflecting their different structural roles: the Twelfth focuses on electoral clarity (low extraction), the Twentieth on timing (low extraction), the Twenty-First on repeal (novel mechanism), and the Twenty-Seventh on congressional restraint (similar power-redistribution logic but different target). Each amendment story should be linked via network edges documenting that they jointly form the constitutional housekeeping family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
