% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: P5 Veto Power (Oligopoly Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   The UN Security Council's P5 veto (Article 27, UN Charter) is read here
 *   as a structural entrenchment of geopolitical oligopoly. The charter
 *   grants each of five permanent members unilateral authority to block any
 *   substantive Security Council resolution, and further requires unanimous
 *   P5 consent to amend Article 27 itself. This reading focuses on how the
 *   veto, originally justified as protection for great powers against
 *   coercive collective action, now functions as a permanent lock against
 *   institutional reform that would redistribute power to rising geopolitical
 *   actors or multipolarity. The non-P5 majority cannot exit (post-colonial
 *   identity fusion with UN membership, lack of alternative global legitimacy
 *   forum) and cannot reform (blocked by the very veto they wish to
 *   constrain). The P5 extract ongoing authority rent by maintaining
 *   institutional sclerosis. This is ONE READING of a contested kernel;
 *   sibling readings (coordination and sovereignty) interpret the same
 *   Charter provision as solving different problems. This story generates the
 *   oligopoly reading as a clean, ε-invariant constraint and routes the
 *   committer structure (contest between readings) to omega variables.
 *
 * KEY AGENTS:
 *   - permanent_five_states: Institutional power, global scope, arbitrage exit (can threaten withdrawal). Set the veto rules, enforce them, benefit from institutional paralysis.
 *   - non_p5_un_member_states: Moderate power collectively, constrained exit (trapped by post-colonial identity + lack of alternative legitimacy forum). Bear the extraction cost through blocked institutional evolution.
 *   - global_south_coalition: Organized at class level, identity-locked to UN membership (post-colonial sovereignty recognition). Repeatedly propose reforms; all blocked by P5 veto of the amendment process.
 *   - rising_geopolitical_powers: Powerful but excluded by Charter structure. Would claim permanent seats if amendment were possible; locked out structurally.
 *   - structural_reformers: Analytical observers. Propose alternative designs; excluded from decision-making.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.81).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.88).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "P5 Veto Power (Oligopoly Reading)").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, 'd44e476e-a677-49e8-9494-4b3cfcd73833').
narrative_ontology:cs_kernel_codification('d44e476e-a677-49e8-9494-4b3cfcd73833', formalized).
narrative_ontology:cs_authority_grounding('d44e476e-a677-49e8-9494-4b3cfcd73833', extraction).
narrative_ontology:cs_interpretation_layer_present('d44e476e-a677-49e8-9494-4b3cfcd73833').
narrative_ontology:cs_reading_relation('d44e476e-a677-49e8-9494-4b3cfcd73833', article_27_veto_power__coordination_reading, influences).
narrative_ontology:cs_reading_relation('d44e476e-a677-49e8-9494-4b3cfcd73833', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('d44e476e-a677-49e8-9494-4b3cfcd73833', foundational, veto_as_authority_rent_extraction).
narrative_ontology:cs_axiom_status(veto_as_authority_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d44e476e-a677-49e8-9494-4b3cfcd73833', veto_as_authority_rent_extraction, empirically_contingent).
narrative_ontology:cs_axiom('d44e476e-a677-49e8-9494-4b3cfcd73833', foundational, institutional_immutability_as_extraction_lock).
narrative_ontology:cs_axiom_status(institutional_immutability_as_extraction_lock, holdable).
narrative_ontology:cs_axiom_grounding('d44e476e-a677-49e8-9494-4b3cfcd73833', institutional_immutability_as_extraction_lock, empirically_contingent).
narrative_ontology:cs_reference_frame('d44e476e-a677-49e8-9494-4b3cfcd73833', great_power_veto_as_war_prevention).
narrative_ontology:cs_drift_state('d44e476e-a677-49e8-9494-4b3cfcd73833', contemporary_multipolar_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('d44e476e-a677-49e8-9494-4b3cfcd73833', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, permanent_five_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_un_member_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, global_majority_nations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.81 because the veto's primary function has shifted from protecting against great-power coercion (1945 founding problem) to preserving P5 institutional monopoly against multipolarity. The measurement series show rising extractiveness from 1945 (0.45, projected) to 2026 (0.81, observed): as decolonization expanded the non-P5 majority and geopolitical power diffused, the veto's role in blocking institutional reform became more visible. Theater ratio rises from 0.12 to 0.42, indicating that P5 rhetoric about 'preventing great-power war' has become increasingly decoupled from actual veto use, which is concentrated on regional conflicts and humanitarian access (unrelated to P5 nuclear stakes). Suppression rises from 0.65 to 0.88 because the institutional machinery to prevent reform (unanimity amendment rule, P5 agenda-setting power, threat of exit) has hardened and become more transparent. Accessibility collapse is 0.72: alternatives to the UN exist (regional organizations, ad-hoc coalitions) but carry severe legitimacy costs for non-P5 states; alternatives to P5 veto do not exist within the institution. Resistance is 0.68: the non-P5 majority actively resists through repeated reform proposals, but the resistance is structurally suppressed by the veto itself.
 *
 * PERSPECTIVAL GAP:
 *   P5 states (beneficiaries, agenda-setters) compute the veto as rope: a genuine mechanism coordinating great-power interests and preventing unwanted intervention. From their seat, the veto is low-extraction coordination. Non-P5 states (payers, victims) compute the same structure as snare: they are locked in without voice or exit path, and the constraint's persistence prevents institutional evolution that would shift power. The engine computes per-seat classification from the structural data: same constraint, opposite signs. The authored claim (snare) and metrics (high extraction, high suppression, rising theater) generate this gap. This is the measurement the corpus should capture — the perspective-dependent classification that emerges when the same institutional feature is viewed from the beneficiary vs. victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states hold directionality near 0.0 (full beneficiary): they set the rules, they benefit from institutional immobility, they have arbitrage exit (can withdraw and maintain geopolitical standing through other forums). Non-P5 states have directionality near 1.0 (full target): they bear the extraction cost (blocked institutional reform), they lack exit (post-colonial identity lock), they have no voice on substantive matters despite majority membership. The global_south_coalition is particularly trapped: powerful as an organized bloc but identity-locked to UN membership (post-colonial sovereignty was constituted through UN recognition); exit means international erasure. Rising powers like India and Brazil have powerful-state directionality but are structurally excluded (trapped at the institutional boundary). The metric of extraction (0.81) reflects the asymmetry: P5 benefit from the constraint at zero cost; non-P5 pay the cost without benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy signal: founding_problem_status=dead, disappearance_verdict=world_rearranges, but constraint persists unchanged for 80 years. The founding problem (preventing great-power coercion into military action) was solved by nuclear deterrence; the veto's continued persistence is not required by that problem. The non-P5 majority would rearrange institutional structures immediately if they could. The theater_ratio rising from 0.12 to 0.42 indicates performative maintenance: P5 rhetoric about 'preserving the international order' and 'preventing great-power war' has become decoupled from actual veto use (concentrated on humanitarian veto blocks unrelated to P5 defense). The constraint exhibits mandatrophy because it persists despite its original mandate (coordination against great-power war) being obsolete, replaced by pure extraction (authority rent from blocking reform). This reading classifies the constraint as snare to capture that mandatrophy signal — the foundational problem has atrophied but extraction machinery remains intact and active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_oligopoly_framing,
    'Is the veto fundamentally a coordination mechanism preventing great-power war, or a rent-extraction mechanism preserving P5 oligopoly by blocking institutional evolution?',
    'Historical counterfactual: if the veto were removed and the founding problem (preventing great-power coercion) remained live, P5 states would refuse UN membership or withdraw. Observed behavior: P5 states resist veto removal to preserve authority structure, not to prevent coercion (nuclear deterrence suffices). Comparative institutional analysis: does veto use correlate with preventing great-power confrontation or with blocking non-P5 agency in regional conflicts unrelated to P5 nuclear stakes?',
    'If coordination framing holds: constraint is Rope (genuine mechanism solving a real problem). If oligopoly framing holds: constraint is Snare (extraction mechanism using immutability cover). The two readings are structurally incommensurable — classification depends on whether the constraint''s primary function remains the founding problem or has become authority rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_oligopoly_framing, empirical, 'Whether veto-blocking patterns track great-power war prevention or P5 interest protection.').

omega_variable(
    trapped_vs_constrained_exit,
    'Do non-P5 states experience exit options as constrained (they could leave but pay a high cost) or trapped (exit is structurally impossible)?',
    'Ethnographic and interview data from non-P5 diplomats and government decision-makers: when confronting veto paralysis, do they calculate cost-benefit of UN exit vs. staying, or experience exit as unthinkable? Post-colonial identity analysis: to what extent is international legitimacy fused with UN membership for states whose sovereignty was recognized through UN admission?',
    'If constrained: directionality for non-P5 states is high but not maximal; they retain theoretical exit leverage. If trapped: directionality is maximal; they are locked into the extraction. The difference affects whether coalition exit-threat could leverage reform or whether reform is purely a side-effect of P5 internal realignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trapped_vs_constrained_exit, empirical, 'Structural vs. identity-fusion basis of non-P5 exit constraint.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the oligopoly reading logically foreclose the coordination reading and sovereignty reading within a single institutional framework, or do all three remain coexistent?',
    'Charter interpretation: can a single reading of Article 27 simultaneously instantiate all three functions (great-power protection, oligopoly preservation, sovereignty preservation)? Or do the readings require incompatible institutional structures?',
    'If forecloses: this reading is the dominant interpretation and the other two are analytically defunct (discovered false). If coexists_with: all three readings are structurally defensible and the contest is over power, not truth — the kernel is genuinely under-determined. If influences: this reading establishes conditions that pressure the others toward reformulation (e.g., revealing them as cover stories).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Relationship between oligopoly reading and sibling readings in the Article 27 kernel contest.').

omega_variable(
    charter_immutability_as_enforcement,
    'Is the immutability of Article 27 (requiring P5 unanimity for amendment) a structural feature of the constraint or a separate enforcement mechanism?',
    'Genealogical analysis: did P5 states design the unanimity amendment rule to protect Article 27 specifically, or is it a general protection for any Charter provision opposed by a veto-holder? Counterfactual: if Article 27 could be amended by supermajority, would P5 veto persist as a practical matter, or would the non-P5 majority immediately vote to eliminate it?',
    'If immutability is structural (intrinsic to the veto''s operation): the extraction depends on Charter-level immunity from reform. If immutability is separate enforcement: the veto could theoretically persist even under supermajority amendment rules if P5 states defend it individually. The distinction clarifies whether the constraint is self-protecting or requires institutional scaffolding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_immutability_as_enforcement, empirical, 'Whether immutability is intrinsic to veto power or a separable enforcement mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__oligopoly_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(arti_tr_t1965, article_27_veto_power__oligopoly_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(arti_tr_t1985, article_27_veto_power__oligopoly_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(arti_tr_t2000, article_27_veto_power__oligopoly_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(arti_tr_t2015, article_27_veto_power__oligopoly_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement(arti_tr_t2026, article_27_veto_power__oligopoly_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__oligopoly_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(arti_be_t1965, article_27_veto_power__oligopoly_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(arti_be_t1985, article_27_veto_power__oligopoly_reading, base_extractiveness, 1985, 0.69).
narrative_ontology:measurement(arti_be_t2000, article_27_veto_power__oligopoly_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(arti_be_t2015, article_27_veto_power__oligopoly_reading, base_extractiveness, 2015, 0.79).
narrative_ontology:measurement(arti_be_t2026, article_27_veto_power__oligopoly_reading, base_extractiveness, 2026, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__oligopoly_reading, suppression_requirement, 1945, 0.65).
narrative_ontology:measurement(arti_su_t1965, article_27_veto_power__oligopoly_reading, suppression_requirement, 1965, 0.72).
narrative_ontology:measurement(arti_su_t1985, article_27_veto_power__oligopoly_reading, suppression_requirement, 1985, 0.78).
narrative_ontology:measurement(arti_su_t2000, article_27_veto_power__oligopoly_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(arti_su_t2015, article_27_veto_power__oligopoly_reading, suppression_requirement, 2015, 0.85).
narrative_ontology:measurement(arti_su_t2026, article_27_veto_power__oligopoly_reading, suppression_requirement, 2026, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__oligopoly_reading, 0.18).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__sovereignty_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, un_general_assembly_voting_asymmetry).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, permanent_seat_amendment_gate).

% DUAL FORMULATION NOTE:
% The Article 27 veto is a contested kernel with three structural readings. The oligopoly reading frames veto use as extraction (blocking institutional evolution that would shift power). The coordination reading frames it as genuine coordination (preventing great-power military coercion). The sovereignty reading frames it as applying a principle (unilateral consent for binding law). These are not disagreements about measurement or interpretation of a single constraint; they are genuinely different constraints instantiated by the same Charter text, classified differently depending on which reading is adopted. Each reading has its own beneficiary/victim structure, ε value, and classification. The readings coexist as live positions held by different parties; none logically forecloses the others within separate institutional frameworks, though the oligopoly reading exerts structural pressure on the coordination and sovereignty readings by revealing their functions as increasingly atrophied.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
