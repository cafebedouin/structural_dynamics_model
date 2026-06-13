% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Self-Determination and Statehood Right (Liberal-Nationalist Reading)
 *   domain: political_philosophy/nationalism/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents one reading of a contested kernel: the claim
 *   that Jewish people possess a collective right to self-determination and
 *   that statehood in the historical Jewish homeland is a legitimate exercise
 *   of that right. This reading specifically instantiates a
 *   liberal-nationalist interpretation: it grounds the legitimacy claim in
 *   universal principles of national self-determination (applicable to all
 *   peoples) rather than in religious covenant or demographic majority
 *   rights. Critically, this reading ACKNOWLEDGES Palestinians as co-equal
 *   bearers of self-determination rights, treating their territorial and
 *   political claims as structurally parallel to Jewish claims. The
 *   constraint thus produces a tangled-rope structure: genuine coordination
 *   function (Jewish collective agency and security guaranteed) paired with
 *   asymmetric extraction (Palestinian displacement and territorial loss) —
 *   and the justification for the asymmetry is that both groups possess
 *   self-determination rights but one group's historical dispossession and
 *   persecution grant it remedial priority in implementing statehood. The
 *   kernel itself (Jewish sovereignty in Palestine / Eretz Yisrael) is the
 *   same across all readings; what differs is the GROUNDING
 *   (liberal-universal, religious-theological, cultural-spiritual,
 *   postcolonial-critical) and the SCOPE (whether Palestinians are
 *   acknowledged as claimants, whether statehood is necessary or merely
 *   beneficial, whether the project is remedial or inaugural).
 *
 * KEY AGENTS:
 *   - Jewish collective as nation: beneficiary and agenda-setter; possesses institutional power, civilizational time horizon, identity-locked exit (Jewishness is not exit-able); sets foundational claim to self-determination.
 *   - Palestinian population (displaced 1948–67): victim; powerless, generational horizon, trapped exit; no return option, territorial loss permanent.
 *   - Palestinian population under state control: victim and constrained-exit payer; powerless, generational horizon; bears military rule, resource asymmetry, legal subordination; can emigrate or accept subordinate citizenship.
 *   - Liberal-nationalist theorists: beneficiary; analytical power; this reading vindicates the possibility of liberal nationalism and symmetric national rights.
 *   - Diaspora Jewish communities: beneficiary; organized power, mobile exit; benefit from symbolic and security value without territorial stakes.
 *   - International liberal order (non-agent): beneficiary vindicated; the constraint's legitimacy rests on universal self-determination rights.
 *   - Regional states/excluded actors: excluded from foundational claim; contend with enforcement costs.
 *   - International observers: analytical seats; monitor compliance with co-equal self-determination principle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.62).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.41).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Self-Determination and Statehood Right (Liberal-Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51').
narrative_ontology:cs_kernel_codification('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', distributed).
narrative_ontology:cs_authority_grounding('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', distributed).
narrative_ontology:cs_reading_relation('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', jewish_sovereignty_palestine__religious_zionist_reading, influences).
narrative_ontology:cs_reading_relation('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', foundational, collective_self_determination_as_universal_right).
narrative_ontology:cs_axiom_status(collective_self_determination_as_universal_right, holdable).
narrative_ontology:cs_axiom_grounding('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', collective_self_determination_as_universal_right, deontological).
narrative_ontology:cs_axiom('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', foundational, remedial_priority_for_historically_dispossessed_groups).
narrative_ontology:cs_axiom_status(remedial_priority_for_historically_dispossessed_groups, holdable).
narrative_ontology:cs_axiom_grounding('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', remedial_priority_for_historically_dispossessed_groups, deontological).
narrative_ontology:cs_axiom('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', secondary, symmetric_recognition_of_palestinian_self_determination).
narrative_ontology:cs_axiom_status(symmetric_recognition_of_palestinian_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', symmetric_recognition_of_palestinian_self_determination, deontological).
narrative_ontology:cs_reference_frame('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', universal_self_determination_principle_remedial_application).
narrative_ontology:cs_drift_state('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', contemporary_occupation_indefinite_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0d7a05ec-4aaf-41a0-93bb-12daeaeb8a51', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_population_displaced_from_territory).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinians_under_state_control).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.62 at interval end. The measurement series rises from 0.48 to 0.63 over 50 years, then stabilizes — this trajectory matches the pattern of territorial consolidation and security entrenchment: early years saw maximum territorial expansion and displacement; the plateau suggests boundaries have stabilized and extraction has found an equilibrium. Suppression is moderate (0.41) and stable, reflecting the constraint's reliance on active state enforcement (military control, administrative rule) rather than overwhelming coercion — Palestinians maintain organizational capacity and cultural coherence despite control. Theater is low-to-moderate (0.28), rising from 0.12, reflecting the increasing gap between the founding justification (self-determination, security) and the actual operation (territorial control, resource allocation asymmetry, permanent subordination of Palestinian self-determination). This rising theater-ratio is the signal that the constraint's original coordination function (remedy for Jewish dispossession through achieving self-determination) is being progressively substituted by the extraction function (maintaining territorial and demographic hegemony). Accessibility-collapse is moderate (0.58): alternatives exist in theory (binational state, federated arrangement, Palestinian autonomous state) but each is politically costly, institutionally difficult, and opposed by stakeholders, making them effectively inaccessible once the state structure consolidates. Resistance is high (0.72) and persistent, reflecting sustained Palestinian opposition to the constraint's operation and regular international criticism. The coercion-grid shows level differentiation: suppression is highest at the individual Palestinian level (0.46), lower at structural level (0.28) — the constraint maintains itself through direct control of individuals rather than through systemic inevitability. Resistance is distributed across all levels (0.68–0.76), with organizational resistance higher than individual (0.73 vs 0.70) — Palestinians maintain collective opposition despite individual-level constraints. This pattern is diagnostic: if suppression were structural (everyone accepts it as inevitable), resistance would be lower at all levels; instead, high organized resistance indicates the constraint is actively defended, not passively accepted.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Jewish institutional power) and the victim seats (Palestinian powerless) should compute dramatically different types. From the Jewish institutional seat, the arrangement is genuine coordination achieved and maintained: a dispersed people secured political agency through statehood, universal self-determination principles vindicated, security established. From the Palestinian victim seats, the same structure is enforced extraction: territorial displacement permanent and non-negotiable, self-determination acknowledged theoretically but blocked practically, legal status subordinate. The engine computes these divergent types from the stark difference in power, exit-options, and directionality: the Jewish seat faces d near 0.0 (beneficiary), the Palestinian seats face d near 1.0 (target). From the analytical seat, the constraint is a tangled-rope: both functions present, but asymmetrically weighted. From excluded regional seats, the constraint is an enforced binary that prevents their participation. This reading depends on acknowledging the perspectival gap explicitly — the constraint's legitimacy under liberal-nationalism REQUIRES that both groups' self-determination rights be recognized as equal in principle, even though implementation produces asymmetry. Failure to maintain this distinction (treating Jewish self-determination as supreme) collapses the reading into settler-colonial or religious-zionist structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish collective as nation is the beneficiary (d → 0.1–0.2): achieves statehood, political agency, security guarantee, vindicates universal self-determination rights. The constraint's entire structure is built to secure this group's interests. No meaningful exit for a Jewish majority (identity-locked). Moderate power allows the group to shape enforcement. Palestinian displaced and controlled populations are the targets (d → 0.8–0.95): lose territory, have no return option, live under military/administrative rule, self-determination permanently subordinated. Powerless, trapped or heavily constrained exit, generational horizon (the subordination will outlast individual lives). Diaspora Jewish communities are beneficiaries (d → 0.2–0.3): gain symbolic and security value without territorial costs; mobile exit (can choose not to participate). Liberal-nationalist theorists are beneficiaries (d → 0.0–0.15): the constraint vindicates their framework. International observers are symmetric (d ≈ 0.5): neither collect nor pay materially; have analytical interest in monitoring compliance with stated principles. The strong directionality asymmetry (Jewish near 0.1, Palestinian near 0.85) is inherent to the constraint's structure: the same arrangement that benefits one group structurally harms the other.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint claims to solve the problem of Jewish political dispossession and vulnerability (founding problem: live, per this reading's posture). The measurement series shows extractiveness rising from 0.48 to 0.63 over 50 years — this is extraction accumulation, not coordination stability. Theater ratio rises from 0.12 to 0.28 — the original justification (remedying dispossession) is being progressively substituted by justification of territorial control and demographic hegemony. This is the signature of Goodhart drift: the original metric (self-determination for Jews) is achieved; subsequent enforcement defends a derivative metric (territorial control, demographic majority) that becomes the actual function. The tangled-rope classification holds because the coordination function (Jewish self-determination) is real and achieved. But the rising extraction and theater over time signal that the constraint is accumulating a secondary, extractive function (Palestinian subordination) that was originally framed as a tragic but necessary cost of remedy. The mandatrophy question: Is the rising extraction part of the coordination function (necessary cost of securing a nation-state for a stateless people) or a drifting-toward-snare (using the coordination claim as cover for territorial hegemony)? The constraint's own logic requires Palestinian self-determination to be achievable through partition or co-governance; if the measurement series continues rising and the Palestinian political capacity continues subordinated, mandatrophy is present: the founding justification (remedying dispossession) no longer maps to the operating function (maintaining hegemony).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_equal_self_determination_boundary,
    'Can two co-equal self-determination claims to the same territory be resolved through partition, binational framework, or federal arrangement, or does the liberal-nationalist premise collapse into irresolvable zero-sum competition?',
    'Long-term empirical observation of partition or federated arrangements where both populations retain substantive self-determination; alternatively, theoretical demonstration of how liberal principles adjudicate conflicting self-determination claims at the same scale.',
    'If resolvable, the constraint operates as genuine tangled-rope: coordination function (both groups achieve self-determination) paired with asymmetric extraction (territory and timing favor one party). If not resolvable, the constraint drifts toward snare: the self-determination claim becomes cover for one group''s domination. The reading itself requires the resolvability premise to hold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(co_equal_self_determination_boundary, conceptual, 'Whether liberal nationalism can accommodate two co-equal national claims to the same territory.').

omega_variable(
    prior_occupancy_vs_historical_ties,
    'When historical ties and recent continuous occupancy conflict, which grounds the stronger self-determination claim — ancestral connection or documented residency?',
    'Comparative study of international law precedent (Native American land claims, Kashmiri partition, partition of India), or theoretical demonstration of how liberalism weights these competing bases.',
    'The liberal-nationalist reading privileges ''historical ties to ancestral homeland'' for Jewish self-determination. If prior occupancy (Palestinian residency 1948–present) proves the stronger ground under liberal law, the reading must either reformulate to center Jewish persecution as the self-determination basis (rather than territorial claim) or acknowledge Palestinians'' co-equal claim overrides territorial partition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_occupancy_vs_historical_ties, conceptual, 'Which self-determination principle (historical ties vs. continuous occupancy) liberalism prioritizes.').

omega_variable(
    settler_colonial_pattern_contamination,
    'Can a population''s self-determination claim remain valid if its historical instantiation follows the structural pattern of European settler-colonialism (migration, displacement, institutional hierarchy), even if the motivation differs?',
    'Structural comparison of intent vs. outcome; empirical analysis of whether outcomes (displacement, resource asymmetry, legal hierarchy) persist regardless of founding narrative.',
    'The settler_colonial_reading (a sibling constraint) interprets Zionism as instantiating settler-colonial structures regardless of intent. If structures and outcomes match settler-colonialism, the liberal-nationalist reading''s validity depends on whether pattern-matching outcome determines classification or whether intent and motivation are materially decisive. This is a conceptual boundary condition: if the reading cannot accommodate the structural pattern, it foreclosed by the settler-colonial reading''s descriptive claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_colonial_pattern_contamination, conceptual, 'Whether settler-colonial structure undermines self-determination legitimacy regardless of motivation.').

omega_variable(
    state_consolidation_extraction_ratchet,
    'Does the constraint''s extractiveness (measured suppression and displacement) increase durably over time, or does it stabilize once statehood is consolidated and territorial boundaries are fixed?',
    'Temporal measurement of Palestinian legal status, resource allocation asymmetry, territorial expansion, and Palestinian political agency from 1948 to present and forward.',
    'The measurement series shows extractiveness rising from 0.48 to 0.63 over 50 years then stabilizing. If extraction continues rising, the constraint drifts from tangled-rope toward snare: the coordination function (Jewish self-determination) persists but subordinates Palestinian self-determination permanently. If stable, the reading can argue equilibrium: Jewish self-determination is secured; Palestinian self-determination remains achievable through partition or negotiated co-governance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_consolidation_extraction_ratchet, empirical, 'Whether extractiveness (suppression/displacement) increases durably after statehood consolidation.').

omega_variable(
    reading_vs_religious_zionist_distinction,
    'Can this reading (liberal-nationalist, grounded in universal self-determination rights) remain logically distinct from the religious-zionist reading (grounded in divine promise), or do they collapse into a single justification when both are deployed to defend the same territorial claim?',
    'Rhetorical and policy analysis: do liberal-nationalist advocates and religious-zionist advocates invoke different principles to resolve territorial disputes, or do they converge on the same territorial outcomes justified through different vocabularies? If they converge, the readings are linguistically distinct but functionally identical.',
    'If distinct, this reading can claim to operate within liberal universalism. If functionally identical (both defending identical territorial claims through different justifications), the reading is exposed as a translation of the religious-zionist reading into liberal vocabulary — making it dependent on religious premises while claiming secular neutrality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_religious_zionist_distinction, conceptual, 'Whether liberal-nationalist and religious-zionist readings remain logically distinct or collapse functionally.').

omega_variable(
    diaspora_jewish_vulnerability_hypothesis,
    'Is persistent diaspora antisemitism and Jewish political vulnerability empirically necessary to justify Jewish statehood, or can the reading ground statehood in self-determination alone, independent of persecution narrative?',
    'Counterfactual: if diaspora antisemitism were eliminated while Jewish cultural distinctiveness remained, would the self-determination claim retain its force? If yes, the claim is autonomous; if no, the claim rides on persecution narrative.',
    'If autonomous, Jewish self-determination can be evaluated as a self-standing rights claim, comparable to other nationalist movements. If dependent on persecution, the reading binds statehood legitimacy to an empirical condition (antisemitism) that might change, and the justification becomes conditional rather than categorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_jewish_vulnerability_hypothesis, empirical, 'Whether Jewish statehood''s legitimacy is independent of diaspora vulnerability or contingent on it.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of Palestinian self-determination measured here (0.41 across the interval) structural (external legal barriers, military control, demographic engineering) or internalized (Palestinians internalize constraints as inevitable, reducing overt resistance)?',
    'Post-independence counterfactual: if Israeli control were suddenly removed, would Palestinian political capacity and self-organization capacity recover (suggesting structural suppression) or persist in constrained form (suggesting internalization)? Alternatively, comparative analysis of Palestinian political movements in different territorial contexts (diaspora, autonomous zones, external exile).',
    'If structural, the constraint''s suppression is an artifact of state enforcement; if internalized, Palestinians carry the constraint forward post-exit. The mechanism matters for whether the constraint is sustainable long-term and for what remedies would be adequate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of Palestinian self-determination is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jewi_tr_t10, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(jewi_tr_t20, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(jewi_tr_t30, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(jewi_tr_t40, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(jewi_tr_t50, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(jewi_tr_t60, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement(jewi_tr_t70, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 70, 0.28).
narrative_ontology:measurement(jewi_tr_t80, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 80, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(jewi_be_t10, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(jewi_be_t20, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(jewi_be_t30, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(jewi_be_t40, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(jewi_be_t50, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(jewi_be_t60, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(jewi_be_t70, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 70, 0.61).
narrative_ontology:measurement(jewi_be_t80, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 80, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(jewi_su_t10, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(jewi_su_t20, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(jewi_su_t30, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement(jewi_su_t40, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(jewi_su_t50, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(jewi_su_t60, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 60, 0.41).
narrative_ontology:measurement(jewi_su_t70, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 70, 0.41).
narrative_ontology:measurement(jewi_su_t80, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 80, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.18).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__post_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_self_determination_right__national_liberation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel jewish_sovereignty_palestine. All five readings (settler_colonial, liberal_nationalist, religious_zionist, post_zionist, cultural_zionist) are separate constraint stories, each with its own ε-invariant structure and metrics. They are linked through network.affects_constraints to indicate they are readings of the same contested kernel. The liberal-nationalist reading acknowledges Palestinians as co-equal self-determination claimants; the Palestinian_self_determination constraint (separate story) models Palestinian claims from Palestinian seats. These are not redundant — they are different constraints because ε differs: from the Jewish institutional seat, Jewish self-determination is the primary function; from Palestinian seats, Palestinian self-determination is the primary function. The network links indicate the kernel structure; the separate metrics indicate the reading-specific operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__liberal_nationalist_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
