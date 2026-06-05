% ============================================================================
% CONSTRAINT STORY: progressive_era_amendments__nineteenth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_progressive_era_amendments__nineteenth_amendment, []).

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
 *   constraint_id: progressive_era_amendments__nineteenth_amendment
 *   human_readable: The Nineteenth Amendment: Sex-Based Disfranchisement Prohibition
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Nineteenth Amendment forbids denying the vote on account of sex,
 *   ending the world's longest suffrage campaign with fourteen words. This
 *   constraint operates at the intersection of democratic theory (where equal
 *   franchise is a first principle) and political practice (where women's
 *   franchise was fiercely contested and required constitutional amendment).
 *   The constraint is one reading of the Progressive Era constitutional
 *   amendments kernel — itself a cluster of four constitutional modifications
 *   (Sixteenth, Seventeenth, Eighteenth, Nineteenth) enacted between
 *   1909–1920, each restructuring core relationships between state and
 *   citizen. This particular reading instantiates the amendment's normative
 *   commitment: that the sex-based disfranchisement rule was unjust and
 *   unconstitutional, and that democratic legitimacy requires franchise based
 *   on citizenship independent of sex. The structural delta relative to the
 *   pre-amendment regime is elimination of sex-based suppression (0.62 →
 *   0.18) and shift from snare (women's perspective, pre-amendment) to rope
 *   (post-amendment democratic system perspective). The amendment coordinates
 *   franchise expansion at the cost of gatekeeper extraction — a tangled-rope
 *   dynamic from the perspective of male legislatures required to ratify, but
 *   pure rope from the perspective of democratic representation as a system.
 *
 * KEY AGENTS:
 *   - Women voters: Primary beneficiary (powerless/trapped pre-amendment, mobile post-amendment) — disfranchised population gaining franchise rights
 *   - Suffrage movement coalition: Primary advocate (organized/mobile) — women's organizations, progressive legislators, labor unions organizing constitutional amendment
 *   - State legislatures and male electorate: Secondary gatekeeper (powerful/constrained) — entities required to ratify; experience franchise loss as asymmetric extraction alongside coordination benefit
 *   - Democratic representation system: Institutional beneficiary (institutional/arbitrage) — gains legitimacy through franchise expansion
 *   - American constitutional tradition: Authority structure grounding legitimacy — the amendment writes new constitutional commitment into fundamental law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(progressive_era_amendments__nineteenth_amendment, 0.18).
domain_priors:suppression_score(progressive_era_amendments__nineteenth_amendment, 0.62).
domain_priors:theater_ratio(progressive_era_amendments__nineteenth_amendment, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(progressive_era_amendments__nineteenth_amendment, extractiveness, 0.18).
narrative_ontology:constraint_metric(progressive_era_amendments__nineteenth_amendment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(progressive_era_amendments__nineteenth_amendment, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(progressive_era_amendments__nineteenth_amendment, rope).
narrative_ontology:human_readable(progressive_era_amendments__nineteenth_amendment, "The Nineteenth Amendment: Sex-Based Disfranchisement Prohibition").
narrative_ontology:topic_domain(progressive_era_amendments__nineteenth_amendment, "political/legal/constitutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(progressive_era_amendments__nineteenth_amendment, '91402be3-34f0-4c68-a6a7-8973ddf76f38').
narrative_ontology:cs_kernel_codification('91402be3-34f0-4c68-a6a7-8973ddf76f38', formalized).
narrative_ontology:cs_authority_grounding('91402be3-34f0-4c68-a6a7-8973ddf76f38', lineage).
narrative_ontology:cs_interpretation_layer_present('91402be3-34f0-4c68-a6a7-8973ddf76f38').
narrative_ontology:cs_reading_relation('91402be3-34f0-4c68-a6a7-8973ddf76f38', progressive_era_amendments__eighteenth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('91402be3-34f0-4c68-a6a7-8973ddf76f38', progressive_era_amendments__seventeenth_amendment, influences).
narrative_ontology:cs_reading_relation('91402be3-34f0-4c68-a6a7-8973ddf76f38', progressive_era_amendments__sixteenth_amendment, influences).
narrative_ontology:cs_axiom('91402be3-34f0-4c68-a6a7-8973ddf76f38', foundational, sex_not_valid_franchise_restriction).
narrative_ontology:cs_axiom_status(sex_not_valid_franchise_restriction, holdable).
narrative_ontology:cs_axiom_grounding('91402be3-34f0-4c68-a6a7-8973ddf76f38', sex_not_valid_franchise_restriction, deontological).
narrative_ontology:cs_axiom('91402be3-34f0-4c68-a6a7-8973ddf76f38', secondary, franchise_basis_must_include_all_adult_citizens).
narrative_ontology:cs_axiom_status(franchise_basis_must_include_all_adult_citizens, holdable).
narrative_ontology:cs_axiom_grounding('91402be3-34f0-4c68-a6a7-8973ddf76f38', franchise_basis_must_include_all_adult_citizens, deontological).
narrative_ontology:cs_reference_frame('91402be3-34f0-4c68-a6a7-8973ddf76f38', democratic_citizenship_independent_of_sex).
narrative_ontology:cs_drift_state('91402be3-34f0-4c68-a6a7-8973ddf76f38', contemporary_voting_rights, gap(stable, minor, true)).
narrative_ontology:cs_created_at('91402be3-34f0-4c68-a6a7-8973ddf76f38', '').
narrative_ontology:cs_kernel_id(progressive_era_amendments__nineteenth_amendment, progressive_era_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(progressive_era_amendments__nineteenth_amendment, women_voters).
narrative_ontology:constraint_beneficiary(progressive_era_amendments__nineteenth_amendment, democratic_representation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN VOTERS BEFORE PASSAGE (SNARE) — Trapped by constitutional and statutory sex-based disfranchisement with no exit option. No franchise, no mechanism to change franchise law, no political voice. Maximum suppression; zero agency. This is the structural condition the amendment addresses. From this position, the constraint being amended (sex-based voting restriction) appears as pure extraction: systematic denial of political voice coupled with legal prohibition on changing the rule.
constraint_indexing:constraint_classification(progressive_era_amendments__nineteenth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SUFFRAGE MOVEMENT COALITION (ROPE) — Organized actors (women's suffrage organizations, sympathetic male legislators, labor unions, progressive reformers) experienced the amendment process as pure coordination: solving a collective action problem (how to amend the Constitution) to extend the franchise. Mobile exit options (can organize, can lobby, can publicize), powerful collective voice. The amendment itself is the solution to a coordination problem — how to secure consent for franchise expansion. Low extractiveness because the mechanism (constitutional amendment) is genuinely participatory and consensus-driven, not coercive.
constraint_indexing:constraint_classification(progressive_era_amendments__nineteenth_amendment, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: MALE ELECTORATE AND STATE LEGISLATURES (TANGLED ROPE) — For the actors controlling the franchise (exclusively male electorate in most states, state legislatures, Senate as elected by state legislatures pre-17th Amendment), the Nineteenth Amendment represents coordination of electoral expansion (a genuine function — the franchise becomes more broadly legitimate when women vote) coupled with asymmetric extraction: male gatekeepers lose monopoly control over who counts as a political actor. The suppression requirement is high (36 states needed to ratify; opposition was organized and fierce), but the coordination function is real — expanded franchise increases democratic legitimacy even for those losing gatekeeper status. Constrained exit because state legislatures must ratify or fail to ratify; cannot avoid the choice.
constraint_indexing:constraint_classification(progressive_era_amendments__nineteenth_amendment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC REPRESENTATION SYSTEM POST-1920 (ROPE) — From the standpoint of constitutional democracy as a system, the Nineteenth Amendment is pure coordination: it solves the legitimacy problem created by sex-based disfranchisement. A democratic system excluding half the population on an immutable characteristic faces a structural inconsistency — the amendment repairs this inconsistency by extending the franchise basis. No extraction; high net benefit for system legitimacy. Institutional exit options are natural (democracy maintains itself by updating franchise rules when demographic categories demand inclusion). The franchise expansion is theoretically costless to the system itself (in fact, beneficial for legitimacy) even though it is costly to male gatekeepers.
constraint_indexing:constraint_classification(progressive_era_amendments__nineteenth_amendment, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / DEMOCRATIC FIRST PRINCIPLES (MOUNTAIN) — From a civilizational/universal perspective grounded in democratic first principles, the disfranchisement of any population on an immutable characteristic is structurally incompatible with democratic legitimacy. The Nineteenth Amendment does not create this constraint; it removes a constraint that violated the foundational principle. The underlying principle (equal franchise independent of sex) appears as an immutable feature of democratic theory itself. This perspective risks false-summit naturalization — treating democratic inclusion as a law of nature rather than a historical achievement. However, the structural data supports mountain classification: the foundational principle has zero extractiveness (it is purely prescriptive), zero suppression (it is not enforced through coercion but through logic), and high accessibility (the principle is universally understood once articulated).
constraint_indexing:constraint_classification(progressive_era_amendments__nineteenth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: THE SUFFRAGE ARGUMENT THEATER (PITON) — The decades of suffrage debate before 1920 involved high theater: public speeches, parades, petitions, legislative hearings, judicial appeals — most of which had no direct functional effect on the outcome. The actual amendment required state legislative votes, which were largely predetermined by party machinery and regional politics, not by argument quality. The suffrage theater persisted through institutional inertia (the tradition of petition and debate) despite low functional impact on ratification outcomes. The piton classification captures the observation that much of the suffrage movement's activity was performative (establishing moral claim, building constituency, maintaining coalition cohesion) while the actual mechanism of change (constitutional amendment requiring 36-state ratification) operated orthogonally to the rhetorical force of the arguments. Theater ratio ≈ 0.35 reflects that some activity was substantive (lobbying, organizing ratification campaigns) but much was theatrical (mass meetings, marches) by instrumental measure.
constraint_indexing:constraint_classification(progressive_era_amendments__nineteenth_amendment, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(progressive_era_amendments__nineteenth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(progressive_era_amendments__nineteenth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(progressive_era_amendments__nineteenth_amendment, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(progressive_era_amendments__nineteenth_amendment, TR),
    TR >= 0.70.

:- end_tests(progressive_era_amendments__nineteenth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18 post-amendment): Low. The Nineteenth Amendment itself (the constitutional commitment) has minimal extractiveness — it is a prescriptive rule forbidding disfranchisement, not a mechanism extracting value from anyone post-1920. The low value reflects that the amendment's function is to eliminate extraction (the pre-amendment disfranchisement rule), not to impose new extraction. Pre-amendment disfranchisement extractiveness was ~0.62 (high suppression, zero agency for women, systematic denial of political voice). The amendment reduces extractiveness by shifting from a suppressive rule (you cannot vote because of sex) to an inclusive rule (sex cannot be grounds for disfranchisement). Suppression (0.62 pre-amendment, reduced post-amendment): Reflects the structural barriers women faced before 1920 — legal prohibition on voting in all states, no alternative mechanism for political voice, no constitutional protection against disfranchisement on sex grounds. The amendment eliminates this suppression (post-amendment suppression ≈ 0.05, reflecting only residual enforcement barriers). Theater ratio (0.35): Moderate-low. The suffrage movement involved substantial theater (marches, petitions, public speeches) but also functional organizing (state-by-state ratification campaigns, legislative lobbying). The ratio reflects that significant activity was organizational/functional rather than purely performative, distinguishing this from pure piton activity (which would be ≥0.70 theater). The amendment's actual mechanism (state legislative ratification) bypassed much of the rhetorical theater, suggesting that functional organizing (not persuasion) drove the outcome.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits striking perspectival divergence across power positions. Women trapped under disfranchisement (perspective 1) experience the pre-amendment rule as pure snare with maximum suppression. The suffrage coalition (perspective 2) experiences the amendment process as pure rope — solving a coordination problem. Male legislatures (perspective 3) experience tangled rope — coordination of franchise expansion coupled with loss of gatekeeper monopoly. The democratic system itself (perspective 4) experiences rope — a legitimacy repair mechanism. The analytical observer (perspective 5) risks seeing mountain (immutable democratic first principle) but this risks naturalizing a historical achievement. The piton perspective (6) captures the observation that much suffrage activity was theater despite functional outcome. The gap reveals how the same structural change (women gaining franchise) appears as extraction-elimination (perspective 1), coordination-solving (perspective 2), gatekeeper-loss (perspective 3), legitimacy-repair (perspective 4), and natural-law-alignment (perspective 5) depending on structural position. No single perspective captures the full constraint — the presheaf over all positions is required.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position and exit options. Women pre-amendment (perspective 1) have d ≈ 1.0 (trapped victims with zero exit) — experiencing maximum extraction and suppression. The suffrage coalition (perspective 2) has d ≈ 0.35 (organized beneficiaries with mobile exit) — experiencing low extraction, high coordination benefit. Male legislators (perspective 3) have d ≈ 0.60 (constrained actors losing gatekeeper status) — experiencing moderate-high extraction (loss of monopoly control), but coordination benefit (franchise expansion legitimizes the system). Post-amendment democratic system (perspective 4) has d ≈ 0.1 (institutional beneficiary with high exit mobility) — experiencing no extraction, pure coordination benefit. The analytical observer (perspective 5) has d ≈ 0.73 (analytical position on universal principle) — experiencing the constraint as alignment with immutable principle, not as extraction or coordination in practical terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by having distinct structural roles for women (beneficiary), male gatekeepers (experiencing extraction from monopoly loss), suffrage coalition (coordinators), and democratic system (legitimacy beneficiary). The coordination function is genuine — franchise expansion does legitimize democracy and is not reducible to extraction. The extraction from gatekeepers is real but asymmetric — they lose control but retain voting power. The amendment is tangled rope from the gatekeeper perspective (coordination + extraction) and rope from other perspectives (pure benefit or pure coordination). The mandatrophy does not arise because each perspective has a defensible classification and the gap between them reflects genuine structural differences in position, not measurement ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_versus_constructed_franchise,
    'Is the principle of equal franchise independent of sex a natural law of democratic theory, or a historical achievement contingent on movement politics?',
    'Historical analysis: did the principle emerge from first-principles democratic reasoning (suggesting natural-law status) or from organized political struggle (suggesting constructed status)? Comparison with other democratic franchise expansions (race, property, age) to determine whether principle-driven or struggle-driven is the pattern.',
    'If natural law: the mountain perspective is justified; sex-based disfranchisement violated immutable principles. If constructed: the mountain perspective naturalizes a historical achievement, and the constraint is better classified as tangled_rope throughout (coordination of franchise expansion against residual gatekeeper extraction). This affects how we understand the 19th Amendment''s historical role.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_versus_constructed_franchise, conceptual, 'Whether equal franchise is natural law or historical construction').

omega_variable(
    extraction_asymmetry_magnitude,
    'How much extraction did male gatekeepers experience from losing franchise monopoly? Was the loss primarily symbolic (lost gatekeeper status) or material (changed electoral outcomes)?',
    'Empirical analysis: voting patterns before and after 1920; electoral outcome changes attributable to women''s franchise; legislative representation shifts. Did women voters change electoral outcomes significantly, or did they largely align with existing male-voting patterns initially?',
    'If women voters significantly changed outcomes: extraction for male gatekeepers was high (they lost material control). If voting patterns initially aligned: extraction was primarily symbolic. This affects whether the tangled_rope classification (mixed coordination + extraction) is accurate or whether the constraint is better classified as pure rope (coordination, minimal extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_asymmetry_magnitude, empirical, 'Magnitude of material extraction from male franchise monopoly loss').

omega_variable(
    reading_versus_amendment_distinction,
    'Is this constraint the pre-1920 sex-disfranchisement rule that the Nineteenth Amendment addressed, or is it the amendment itself as a constitutional commitment?',
    'Clarify: the constraint''s kernel is the sex-disfranchisement rule (what the amendment forbids) or the amendment text itself (the constitutional commitment that forbids it)? These have different extractiveness values: pre-amendment rule ≈ 0.62 suppression; post-amendment constitutional text ≈ 0.18 extractiveness (enforcement of inclusion). Distinct constraints if measured differently.',
    'Affects which structural properties are primary. If constraint is pre-amendment rule: beneficiary is the state/male electorate; victim is women. If constraint is post-amendment text: beneficiary is democratic representation; victim is none (the rule is prescriptive, not extractive). This affects classification type and directionality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_amendment_distinction, conceptual, 'Whether constraint is the disfranchisement rule or the amendment forbidding it').

omega_variable(
    suffrage_theater_versus_organizing,
    'What proportion of suffrage movement activity (1840–1920) was theater/moral persuasion versus organizing/coalition-building that directly affected ratification?',
    'Historical institutional analysis: track which activities correlate with state ratification decisions. Did states ratify because of suffrage rhetoric, or because of party machinery decisions, regional politics, and organized lobbying? Disentangle theater from functional organizing.',
    'Affects theater_ratio value. If activity was mostly organizing: theater_ratio should be lower (0.20–0.30). If mostly theater: theater_ratio ≈ 0.50–0.70. This affects whether the piton perspective is valid or whether the movement should be classified as high-function organizing with lower theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffrage_theater_versus_organizing, empirical, 'Proportion of suffrage activity that was theater versus functional organizing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(progressive_era_amendments__nineteenth_amendment, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nineteenth_theater_baseline, progressive_era_amendments__nineteenth_amendment, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nineteenth_theater_mid, progressive_era_amendments__nineteenth_amendment, theater_ratio, 50, 0.35).
narrative_ontology:measurement(nineteenth_theater_post, progressive_era_amendments__nineteenth_amendment, theater_ratio, 80, 0.35).

% Extraction over time
narrative_ontology:measurement(nineteenth_baseline_pre_amendment, progressive_era_amendments__nineteenth_amendment, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(nineteenth_mid_campaign, progressive_era_amendments__nineteenth_amendment, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(nineteenth_post_amendment, progressive_era_amendments__nineteenth_amendment, base_extractiveness, 80, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(progressive_era_amendments__nineteenth_amendment, identity_coordination).
narrative_ontology:affects_constraint(progressive_era_amendments__nineteenth_amendment, eighteenth_amendment).
narrative_ontology:affects_constraint(progressive_era_amendments__nineteenth_amendment, seventeenth_amendment).
narrative_ontology:affects_constraint(progressive_era_amendments__nineteenth_amendment, sixteenth_amendment).

% DUAL FORMULATION NOTE:
% The Nineteenth Amendment is one component of the Progressive Era constitutional amendments kernel. It shares authority structure (Constitutional amendment requiring state ratification) and temporal context (1909-1920) with the Sixteenth, Seventeenth, and Eighteenth Amendments. The constraint's extractiveness (0.18 post-amendment) reflects the amendment text itself as a prescriptive commitment; the pre-amendment sex-disfranchisement rule had higher extractiveness (~0.62). These are distinct constraints in the family, linked through the shared kernel and overlapping beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(progressive_era_amendments__nineteenth_amendment, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
