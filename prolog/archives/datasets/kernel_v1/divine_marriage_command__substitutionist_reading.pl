% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Divine Marriage Command (Substitutionist Reading): Doctrinal Monogamy Mandate
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   The substitutionist reading of the divine_marriage_command kernel
 *   represents the institutional church's official doctrine in the
 *   post-Manifesto era: monogamy is not a prudential accommodation to federal
 *   coercion but a divine command that supersedes the prior authorization of
 *   polygamy. This reading is one of three structurally distinct claims about
 *   the same kernel (the divine will regarding marriage). The substitutionist
 *   reading frames the Manifesto as revelation, making polygamy doctrinally
 *   invalid and fundamentalist adherents heretical. The constraint's
 *   extractiveness comes from the doctrinal inversion itself — what was
 *   commanded is now condemned — combined with the suppression of
 *   alternatives (excommunication of those who maintain the prior doctrine).
 *   The theater ratio reflects the elaborate theological justifications
 *   required to explain how prior revelation could have been wrong, or how
 *   the new revelation supersedes without invalidating the divine source. The
 *   measurements trace the increasing extraction over the first 15 years
 *   post-Manifesto: as the institutional apparatus consolidates around the
 *   new doctrine, the cost of dissent rises (suppression increases), and the
 *   performative work required to justify the reversal increases (theater
 *   ratio rises).
 *
 * KEY AGENTS:
 *   - Church Hierarchy: Institutional beneficiary (institutional/arbitrage) — consolidates doctrinal authority by eliminating fundamentalist dissent; arbitrages between federal pressure and claims of divine revelation
 *   - Fundamentalist Believers: Primary victims (powerless/identity_locked) — face excommunication for maintaining pre-Manifesto doctrine; identity constituted through belief in doctrinal continuity and continuous revelation
 *   - Reformist Leadership: Secondary beneficiary (moderate/constrained) — orchestrate the doctrinal pivot; coordinate institutional survival with theological legitimacy; constrained by need to maintain credibility
 *   - Historical Scholarship / Future Record: Organized observers (organized/mobile) — can eventually expose the coercion through archival research; see the constraint as having a sunset as evidence accumulates
 *   - Doctrinal Authority Apparatus: Institutional performer (institutional/arbitrage) — maintains the theological justifications for the reversal; theater increases as challenges to the new doctrine require ever more elaborate interpretation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks accepting the divine command framing without recognizing it as institutional legitimation theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.58).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, snare).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Divine Marriage Command (Substitutionist Reading): Doctrinal Monogamy Mandate").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, '7dc6659d-f641-4ba8-9498-3ce6afc5cacb').
narrative_ontology:cs_kernel_codification('7dc6659d-f641-4ba8-9498-3ce6afc5cacb', formalized).
narrative_ontology:cs_authority_grounding('7dc6659d-f641-4ba8-9498-3ce6afc5cacb', extraction).
narrative_ontology:cs_interpretation_layer_present('7dc6659d-f641-4ba8-9498-3ce6afc5cacb').
narrative_ontology:cs_reading_relation('7dc6659d-f641-4ba8-9498-3ce6afc5cacb', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('7dc6659d-f641-4ba8-9498-3ce6afc5cacb', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('7dc6659d-f641-4ba8-9498-3ce6afc5cacb', foundational, manifesto_is_new_revelation).
narrative_ontology:cs_axiom_status(manifesto_is_new_revelation, holdable).
narrative_ontology:cs_axiom_grounding('7dc6659d-f641-4ba8-9498-3ce6afc5cacb', manifesto_is_new_revelation, theological).
narrative_ontology:cs_axiom('7dc6659d-f641-4ba8-9498-3ce6afc5cacb', foundational, polygamy_doctrinally_invalid_post_manifesto).
narrative_ontology:cs_axiom_status(polygamy_doctrinally_invalid_post_manifesto, holdable).
narrative_ontology:cs_axiom_grounding('7dc6659d-f641-4ba8-9498-3ce6afc5cacb', polygamy_doctrinally_invalid_post_manifesto, deontological).
narrative_ontology:cs_reference_frame('7dc6659d-f641-4ba8-9498-3ce6afc5cacb', continuous_divine_revelation_with_progressive_supersession).
narrative_ontology:cs_drift_state('7dc6659d-f641-4ba8-9498-3ce6afc5cacb', contemporary_post_manifesto_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7dc6659d-f641-4ba8-9498-3ce6afc5cacb', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, institutional_legitimacy_apparatus).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, fundamentalist_adherents).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, doctrinal_continuity_claim).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, historical_revelation_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUNDAMENTALIST BELIEVER (SNARE) — Identity constituted through belief in restored revelation and doctrinal continuity. Faces excommunication (spiritual death in the tradition) for maintaining pre-Manifesto doctrine. Structurally mobile (could physically leave the faith community) but identity-fused with the tradition's truth claims. Cannot exit without ceasing to be the person they understand themselves to be. Maximum extraction: doctrinal inversion (what was commanded is now condemned) with no escape route except identity dissolution.
constraint_indexing:constraint_classification(divine_marriage_command__substitutionist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: REFORMIST LEADERSHIP (TANGLED ROPE) — Church leaders who orchestrate the doctrinal pivot. Coordinate genuine problem (federal coercion, institutional survival) alongside asymmetric extraction (consolidate power by eliminating doctrinal dissent). High suppression through excommunication. Benefits from framing coercion as revelation: insulates the institution from acknowledging external pressure. Constrained by the need to maintain theological credibility; cannot simply declare policy change without doctrinal cover.
constraint_indexing:constraint_classification(divine_marriage_command__substitutionist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHURCH INSTITUTION (ROPE) — Experiences the constraint as coordination solution: reframing coercion as revelation solves the coordination problem of institutional legitimacy under federal pressure. The framing enables unified doctrine (monogamy becomes universal law) and clear boundary enforcement (fundamentalists are heretics, not conscientious resisters). Net beneficiary through arbitrage — the institution can exit the federal standoff by switching doctrine while maintaining the claim that doctrine, not coercion, drove the change.
constraint_indexing:constraint_classification(divine_marriage_command__substitutionist_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HISTORICAL RECORD (SCAFFOLD) — Sees the constraint as temporary: future scholars will expose the federal coercion and reveal the Manifesto as politically motivated, not divinely revealed. The constraint's extraction mechanism (framing coercion as revelation) has a sunset: sustained historical research and document analysis will accumulate evidence of the actual negotiation. Organized scholars with access to archives can exit the constraint's legitimacy claims through investigation. Theater will eventually collapse as archival evidence emerges.
constraint_indexing:constraint_classification(divine_marriage_command__substitutionist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DOCTRINAL AUTHORITY STRUCTURE (PITON) — The apparatus of interpreting revelation (theological councils, canonical texts, hermeneutic traditions) is maintained through institutional inertia despite degraded function. Once the Manifesto is declared revelation, the authority structure must police doctrinal orthodoxy (excommunicate fundamentalists) to preserve the fiction of continuity. The theater is high (elaborate theological justifications for the reversal) and increasing (each new challenge requires new interpretation). The structure persists because the institution depends on it, not because it functionally adjudicates truth.
constraint_indexing:constraint_classification(divine_marriage_command__substitutionist_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DIVINE COMMAND VIEW (MOUNTAIN) — From the perspective of classical theological realism, God's commands are immutable natural law (divine will is the ground of all order). If monogamy is divinely required, then pre-Manifesto polygamy could not have been divinely commanded — it must have been cultural accommodation or error. The mountain perspective sees the doctrinal reversal as correction, not coercion. However, this perspective requires that the Manifesto actually carries divine authority, which depends on the institution's legitimacy claims. The engine will detect this as a false summit — the theological 'naturalness' of the reversal depends on accepting the church's framing of coercion as revelation.
constraint_indexing:constraint_classification(divine_marriage_command__substitutionist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(divine_marriage_command__substitutionist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(divine_marriage_command__substitutionist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, TR),
    TR >= 0.70.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantial value from fundamentalists through forced doctrinal inversion: the very teaching they were taught is now condemned. But the extraction is not maximal because the institution coordinates a genuine problem (federal legal pressure that threatened institutional survival). The extractiveness reflects the asymmetry: the institution solves its survival crisis while shifting all costs onto those who resist the new doctrine. The measurement trajectory shows rising extraction over 15 years as the institutional consolidation proceeds and dissent becomes increasingly penalized. Suppression (0.72): High. The enforcement mechanism is excommunication — spiritual death within the tradition. This is near-total suppression for identity-locked agents: they cannot exit without ceasing to be members of their faith community. Pre-Manifesto, dissent on other doctrines was tolerated (different interpretations coexisted); post-Manifesto, polygamy doctrine is mandatory. The suppression trajectory shows intensification as the institution hardens the boundary between orthodox and heretical. Theater ratio (0.68): High and rising. The theological justifications for the doctrinal reversal are elaborate: the institution must explain how prior revelation could have been wrong without invalidating the revelatory process itself, or how the new revelation supersedes without explicit scriptural foundation. Each new challenge to the doctrine (from historical scholars, from continuationist voices) requires new interpretive work, driving the theater ratio upward. The measurements show this trajectory: initial theater (0.52) reflects uncertainty; by year 15, theater has stabilized at high level (0.68) as the institution routinizes the justifications.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range: the fundamentalist believer sees snare (doctrinal inversion with no exit); the reformist leadership sees tangled rope (coordinated institutional solution with asymmetric extraction); the church institution sees rope (solving a genuine coordination problem); the historical record sees scaffold (the constraint has a sunset as evidence emerges); the doctrinal authority apparatus sees piton (elaborate theater maintaining an incoherent position); the analytical observer risks seeing mountain (divine law, immutable). The perspectival gap is extreme because the constraint's legitimacy depends on the dominant reading (substitutionist) suppressing the others. The continuationist reading (polygamy remains valid, Manifesto is prudential suspension) and coercion-visibility reading (acknowledge federal pressure, reframe as prudential) are institutionally marginalized through excommunication and doctrinal policing. The constraint's Snare classification depends on the suppression of these alternative readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) reflect each agent's structural relationship to the constraint. The fundamentalist believer is a victim with no exit (identity_locked): d ≈ 0.89, high f(d), high experienced extraction. The reformist leadership orchestrates the constraint for institutional benefit: d ≈ 0.15, moderate f(d), lower experienced extraction (they control the process). The church institution is the beneficiary with full arbitrage: d ≈ 0.05, near-zero f(d), negative experienced extraction (the constraint subsidizes their survival). The future historical record has mobile exit options (research can expose the coercion): d ≈ 0.55, moderate f(d), moderate experienced extraction (they are constrained by current institutional suppression but can eventually escape through investigation). The analytical observer is neutral: d ≈ 0.72, f(d) ≈ 1.15, moderate experienced extraction (the observer is not directly targeted but must work within suppressive epistemic conditions). No directionality overrides are needed — the structural derivation produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED at extractiveness 0.58 (above 0.46 threshold): The constraint exhibits genuine mandatrophy because the institution frames coercion (federal pressure) as revelation (divine command). The mandatrophy is the gap between the real coordination problem (institutional survival under legal attack) and the framing mechanism (claiming the Manifesto is divinely revealed, not coerced). The resolution requires acknowledging that this is a snare: the institution extracts doctrinal conformity from fundamentalists by reframing coercion as law, and maintains the snare through suppression (excommunication). The mandatrophy is resolved by recognizing that the theater (elaborate theological justifications) is the binding mechanism — if the institution could openly acknowledge federal pressure, the constraint would shift to rope (coordinated survival response) or even scaffold (temporary accommodation with a sunset). The snare classification holds because the institution cannot exit the theater without losing legitimacy, and therefore cannot exit the snare without existential risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_coercion_frame,
    'Is the Manifesto a genuine divine revelation that corrects prior understanding, or a pragmatic institutional response to federal coercion reframed as revelation for legitimacy?',
    'Historical analysis of contemporaneous documents (church correspondence, federal pressure communications, internal deliberations). Examination of whether theological justifications for the reversal predate the federal legal threat or postdate it. Comparison with other instances of claimed revelation — does the Manifesto follow patterns of genuine doctrinal development or patterns of institutional self-preservation framing?',
    'If revelation: substitutionist reading is structurally sound; the constraint becomes a genuine doctrinal requirement with legitimate authority. If coercion: the constraint is a snare maintained through theological theater; the institutional lie becomes the binding mechanism. Classification changes from snare (coercion framed as law) to rope (coordinated response to external pressure) if coercion is acknowledged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revelation_vs_coercion_frame, empirical, 'Whether the Manifesto represents divine revelation or institutionalized response to federal coercion').

omega_variable(
    doctrinal_continuity_intelligibility,
    'Can the reversal from polygamy to monogamy be coherently understood as doctrinal development (not contradiction) within the tradition''s own theological framework?',
    'Theological analysis of the tradition''s interpretation models: does the framework allow for progressive revelation (later revelation supersedes earlier)? If yes, is this model applied consistently, or only when institutional survival requires it? Examination of other doctrinal reversals in the tradition — are they explained via the same development model, or is the polygamy reversal unique in its invocation of supersessionist revelation?',
    'If coherent development: the constraint is a legitimate (if coerced) doctrinal shift with internal theological logic. If incoherent or inconsistently applied: the constraint is theatrical — doctrinal authority is being wielded to legitimate external coercion, not to adjudicate theological truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_continuity_intelligibility, conceptual, 'Whether the doctrinal reversal is coherently intelligible as development within the tradition''s theology').

omega_variable(
    fundamentalist_exit_capacity,
    'Can a fundamentalist believer exit the constraint (reject the new doctrine) while remaining a member in good standing, or does rejection require excommunication?',
    'Institutional documentation of excommunication policy post-Manifesto. Examination of whether dissent is permitted, tolerated, or penalized. Comparison with pre-Manifesto treatment of doctrinal dissent on other issues.',
    'If exit is possible without extreme penalty: suppression is overstated; the constraint is more rope than snare. If excommunication is mandatory for dissent: the constraint''s suppression is structural and high; identity_locked classification is accurate; snare classification is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fundamentalist_exit_capacity, empirical, 'Whether fundamentalist dissent permits exit or mandates excommunication').

omega_variable(
    institutional_legitimacy_dependence,
    'Does the church institution''s post-Manifesto legitimacy depend on the claim that the reversal was revealed (not coerced), or could the institution survive by acknowledging the federal pressure and reframing the change as prudential without claiming divine authority?',
    'Counterfactual institutional analysis: what happens to member loyalty, donor support, and doctrinal authority if the institution publicly acknowledges that federal coercion drove the change? Comparative analysis with other religions that have made similar pragmatic shifts while acknowledging external pressure (e.g., some Islamic contexts with secular law, some Christian contexts with slavery abolitionism). Does acknowledged pragmatism destroy institutional legitimacy?',
    'If legitimacy strictly requires the revelation claim: the theater is mandatory (not optional), and the snare is locked in place — the institution cannot exit the lie without existential risk. If legitimacy could survive pragmatic acknowledgment: the constraint has an alternative pathway (rope, not snare) that the institution is choosing not to take.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_legitimacy_dependence, preference, 'Whether institutional survival depends on framing coercion as revelation').

omega_variable(
    reader_position_in_kernel_contest,
    'This constraint instantiates ONE reading of the contested divine_marriage_command kernel. What distinguishes the substitutionist reading from its siblings (continuationist, coercion_visibility) in the actual historical record?',
    'Documentary evidence of institutional framing choices: Does the church claim the reversal is revelation, or does it acknowledge federal pressure? Does the tradition maintain that polygamy was always doctrinally invalid, or does it claim doctrinal development? What happens to agents who hold the sibling readings (continuationists are excommunicated; coercion-visibility advocates are marginalized)? The actual institutional stance determines which reading is operative.',
    'The substitutionist reading as authored here is the church''s official institutional position. The sibling readings are the perspectives of those the constraint marginalizes or excommunicates. The kernel contest is real and unresolved — the three readings coexist (different parties hold different readings) and influence each other (the substitutionist reading''s dominance depends on suppressing the sibling readings).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reader_position_in_kernel_contest, empirical, 'Which reading of the divine_marriage_command kernel is operant in the actual institutional practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmc_sub_theater_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(dmc_sub_theater_t5, divine_marriage_command__substitutionist_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(dmc_sub_theater_t15, divine_marriage_command__substitutionist_reading, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(dmc_sub_extract_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(dmc_sub_extract_t5, divine_marriage_command__substitutionist_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dmc_sub_extract_t15, divine_marriage_command__substitutionist_reading, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dmc_sub_suppress_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dmc_sub_suppress_t5, divine_marriage_command__substitutionist_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(dmc_sub_suppress_t15, divine_marriage_command__substitutionist_reading, suppression_requirement, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% The divine_marriage_command kernel admits three structurally distinct readings, each instantiating a different constraint with different ε values and classifications. The substitutionist reading frames coercion as revelation (ε=0.58, Snare). The continuationist reading frames the Manifesto as prudential suspension (ε≈0.42, Tangled Rope). The coercion-visibility reading frames the constraint as acknowledged institutional response (ε≈0.35, Rope). Each reading is a separate constraint story. The network links show that the substitutionist reading's success depends on suppressing the sibling readings — the constraint's classification as snare depends on the excommunication of continuationists and the silencing of coercion-visibility advocates. If either sibling reading gained institutional ground, the substitutionist reading's snare classification would shift toward rope or tangled_rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
