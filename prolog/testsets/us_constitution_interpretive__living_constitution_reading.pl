% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__living_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__living_constitution_reading, []).

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
 *   constraint_id: us_constitution_interpretive__living_constitution_reading
 *   human_readable: Living Constitution Interpretive Authority Reading
 *   domain: constitutional_law/legal_interpretation
 *
 * SUMMARY:
 *   This constraint story documents ONE reading of a contested constitutional
 *   kernel: the interpretation that constitutional meaning evolves with
 *   societal values and interpretive authority derives from reasoned judicial
 *   adaptation to contemporary conditions. This is the 'living constitution'
 *   reading, dominant in elite US legal institutions from the 1960s onward.
 *   The constraint simultaneously coordinates (provides shared interpretive
 *   framework, enables constitutional adjudication across changing
 *   circumstances) and extracts (concentrates interpretive authority in
 *   federal judges and legal scholars, expands federal regulatory power,
 *   marginalizes originalist methodology, forecloses state autonomy). The
 *   claim/metric gap is deliberate: the reading is CLAIMED as tangled_rope
 *   (mixing coordination and extraction) while the metrics describe
 *   moderately high extractiveness with suppression of the competing
 *   originalist reading. The engine's per-seat computation will show this
 *   reading as tangled rope from the beneficiary seats (civil rights
 *   claimants, federal agencies) and as snare-leaning from the victim seats
 *   (states rights advocates, original-meaning textualists).
 *
 * KEY AGENTS:
 *   - progressive_constitutional_scholars: institutional power, framework setters, beneficiary of interpretive authority concentration
 *   - progressive_federal_judges: institutional power, operationalize doctrine, secondary beneficiary of policymaking scope expansion
 *   - civil_rights_expansion_claimants: organized power, constrained exit, primary beneficiary of unenumerated rights recognition
 *   - states_rights_advocates: powerful but constrained, victim of federalism erosion, experience jurisdiction loss
 *   - original_meaning_textualists: institutional power but marginalized, payer of methodological suppression, excluded from consensus doctrine
 *   - federal_regulatory_agencies: institutional power, beneficiary of broad commerce clause readings, expand authority under evolved meanings
 *   - constitutional_law_analysts: analytical observers, see coordination function and extraction asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, 0.68).
domain_priors:suppression_score(us_constitution_interpretive__living_constitution_reading, 0.42).
domain_priors:theater_ratio(us_constitution_interpretive__living_constitution_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__living_constitution_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__living_constitution_reading, "Living Constitution Interpretive Authority Reading").
narrative_ontology:topic_domain(us_constitution_interpretive__living_constitution_reading, "constitutional_law/legal_interpretation").

domain_priors:requires_active_enforcement(us_constitution_interpretive__living_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__living_constitution_reading, '3e655676-53e1-4938-bb09-16218679df31').
narrative_ontology:cs_kernel_codification('3e655676-53e1-4938-bb09-16218679df31', formalized).
narrative_ontology:cs_authority_grounding('3e655676-53e1-4938-bb09-16218679df31', lineage).
narrative_ontology:cs_interpretation_layer_present('3e655676-53e1-4938-bb09-16218679df31').
narrative_ontology:cs_reading_relation('3e655676-53e1-4938-bb09-16218679df31', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e655676-53e1-4938-bb09-16218679df31', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('3e655676-53e1-4938-bb09-16218679df31', foundational, constitutional_meaning_evolves_with_society).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_society, holdable).
narrative_ontology:cs_axiom_grounding('3e655676-53e1-4938-bb09-16218679df31', constitutional_meaning_evolves_with_society, instrumental).
narrative_ontology:cs_axiom('3e655676-53e1-4938-bb09-16218679df31', foundational, judicial_reason_tracks_contemporary_values).
narrative_ontology:cs_axiom_status(judicial_reason_tracks_contemporary_values, holdable).
narrative_ontology:cs_axiom_grounding('3e655676-53e1-4938-bb09-16218679df31', judicial_reason_tracks_contemporary_values, empirically_contingent).
narrative_ontology:cs_reference_frame('3e655676-53e1-4938-bb09-16218679df31', adaptive_judicial_constitutionalism).
narrative_ontology:cs_drift_state('3e655676-53e1-4938-bb09-16218679df31', contemporary_post_dobbs_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3e655676-53e1-4938-bb09-16218679df31', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, lgbtq_plus_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, judicial_policymakers).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, states_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, federalism_constraining_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__living_constitution_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__living_constitution_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__living_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__living_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (1960) to 0.68 (2024) because living constitutionalism increasingly operates as a mechanism for expanding judicial and federal power, not merely as a framework for contemporaneous interpretation. Early living constitutionalism addressed genuine coordination problems (how to apply a 1789 document to 1960s society). By 2024, the framework has become routinized; judges apply it to overturn state laws (abortion, gun regulation, religious accommodation) and expand federal authority (environmental, labor, health regulation) without strong countervailing pressure. Theater rises modestly (0.12 to 0.31) because an increasing share of judicial work frames itself as neutral 'adaptation to contemporary values' when it is actually contentious policy choice—performative neutrality that conceals distributional consequence. Suppression remains moderate and stable (0.28 to 0.42) because originalist scholars are not silenced; they publish, litigate, sit on the Court. But they are institutionally marginalized: lower citation rates, reduced hiring at elite schools, their methodology treated as methodologically suspect rather than legitimate alternative. The asymmetry between originals' institutional presence and their influence on doctrine production is the suppression mechanism. Accessibility collapse (0.58) is moderate because originalism remains intellectually coherent and accessible; scholars and judges can switch readings without cognitive impossibility. But the cost of switching is high: loss of prestige, citation marginalization, exclusion from consensus doctrine production. Resistance remains high (0.72) because states rights advocates, textualists, and affected communities actively contest the framework through litigation, academic critique, and political opposition—living constitutionalism is not passively accepted but actively defended against.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (progressive scholars, progressive judges) compute the constraint as a coordinating mechanism that enables constitutional government across time and prevents constitutional obsolescence—they emphasize the genuine coordination problem (founding problem) it solves. The beneficiary seats (civil rights claimants, federal agencies) compute it as opening new avenues for rights and regulatory authority—they emphasize outcomes (privacy rights, marriage equality, environmental protection). The victim seats (states rights advocates, original textualists) compute it as judicial overreach and democratic illegitimacy—they emphasize the extraction of interpretive authority from the people and the text. These are not different measurements of the same constraint; they are computed per-seat from the structural data (power, exit, beneficiary/victim declarations). The engine produces seat-specific types; divergence between seats is the apparatus's core function.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive constitutional scholars and federal judges are structural beneficiaries (d near 0.0-0.15): they set interpretive frameworks, their work shapes doctrine, their institutional prestige is high, their exit options are numerous (academic prestige, international work, elite practice). Civil rights expansion claimants and LGBTQ+ rights advocates are moderate beneficiaries of the arrangement (d near 0.20-0.35): they gain unenumerated rights recognition, but they depend on judges' willingness to expand rights—they cannot exit if a conservative Court reverses course; their beneficiary status is conditional on continued judicial sympathy. Federal regulatory agencies are beneficiaries (d near 0.20-0.30): they expand authority under broad federal power interpretations, but their authority is delegated and reversible; they cannot independently defend the reading against originalist challenge. States rights advocates are targets (d near 0.85-0.95): they experience jurisdiction loss, federalism erosion, preemption of state lawmaking; their exit is trapped (they cannot leave federalism) and their resistance is constrained by judicial supremacy in constitutional interpretation. Original-meaning textualists are targets (d near 0.75-0.85): they bear methodological marginalization, are excluded from consensus doctrine production, face institutional disincentives for their scholarship; their exit is constrained (they cannot abandon textualism and retain integrity), though they have more power and options than powerless groups. Federalism-constraining communities are targets (d near 0.80-0.90): they experience federal mandates, lost policymaking autonomy, inability to set local standards; their exit is trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to apply a 1789 document to changed circumstances) is contested in status. Living constitutionalists attest it is live and unsolved by originalism. Originalists attest it is a false dilemma and the real problem is judicial overreach. The constraint's disappearance verdict is world_rearranges: if living constitutionalism vanished and the Court reverted to originalism, the entire federalism-rights-regulatory landscape would restructure. This asymmetry (contested founding problem, unambiguous disappearance consequence) suggests the constraint's mandate has shifted from solving the founding problem to redistributing power among constitutional stakeholders. Early living constitutionalism (1960–1980) genuinely addressed coordinate problems that originalism could not solve (racial equality, sex discrimination, privacy rights when the framers had no conception of these issues). Contemporary living constitutionalism (1990–2024) often applies the framework to contested policy questions where originalism provides coherent answers (gun regulation, religious accommodation, abortion), suggesting the mandate has shifted from founding-problem-solving to power-redistribution. The theater ratio rise (0.12 to 0.31) documents this drift: an increasing share of judicial work frames itself as neutral value-tracking when it is actually contentious policy choice. Mandatrophy is not yet fully resolved (the constraint still has some coordinating function), but the trajectory shows mandate drift from coordination to extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_interpretation_vs_lawmaking,
    'Is reading the Constitution to recognize unenumerated rights based on evolving values constitutional interpretation, or is it judicial lawmaking masquerading as interpretation?',
    'Philosophical and methodological debate; empirical study of whether judge-authored rights are functionally equivalent to legislated rights (scrutiny level, enforceability, reversibility); comparative analysis of how other democracies distinguish interpretation from legislation.',
    'If interpretation: living constitution doctrine is a legitimate reading of the Constitution''s adaptation capacity. If lawmaking: the constraint represents federal judges exercising power that belongs to elected representatives, making it pure extraction. This distinction affects the legitimacy categorization of the entire constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_interpretation_vs_lawmaking, conceptual, 'Whether living constitution doctrine constitutes interpretation or judicial legislation.').

omega_variable(
    contemporary_values_identification,
    'What are ''contemporary values'' and who determines them? Are they societal consensus, intellectual elite consensus, judge-perceived values, or something else?',
    'Empirical polling on whether Americans endorse the ''evolved'' constitutional meanings judges have announced; study of which stakeholders'' values are actually incorporated into judicial reasoning (academics, activists, experts, ordinary citizens); analysis of whose values are excluded.',
    'If contemporary values reflect broad democratic consensus: living constitution doctrine redistributes power from undemocratic legislatures to judges closer to societal sentiment. If contemporary values reflect coastal elite, legal academy, and judge preferences: the constraint is extractive, concentrating interpretive authority in a narrow class. This affects the beneficiary/victim classification fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_values_identification, empirical, 'Whether ''contemporary values'' are empirically representative or elite-skewed.').

omega_variable(
    federation_power_redistribution,
    'Does expanding federal power under living constitution doctrine create net benefits (uniform civil rights, economies of scale in regulation) or net costs (reduced local autonomy, one-size-fits-all policies imposed on diverse communities)?',
    'Policy outcome analysis: comparative health, environmental, and social outcomes under federal vs. state regimes; study of whether federal standards protect minorities or impose tyranny of the majority; examination of whether states would have achieved the same rights expansions through democratic processes (counterfactual).',
    'If net benefits: federalism redistribution is coordination benefit justified by living constitution. If net costs: the constraint represents extraction of state power that benefits certain classes (civil rights claimants, federal bureaucrats) at the cost of others (federalism advocates, local communities). This affects whether to classify the constraint as rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federation_power_redistribution, empirical, 'Whether federal power expansion under living constitutionalism produces net social benefit or net extraction.').

omega_variable(
    kernel_contest_reading_asymmetry,
    'Is this reading (living constitution) a legitimate alternative interpretation of the same constitutional kernel, or does it represent a fundamentally different constitution than originalism?',
    'Originalist scholars argue living constitution abandons the Constitution for a different document (whatever judges want it to be); living constitutionalists argue both readings instantiate the same Constitution under different interpretive methodologies. The question is whether the sibling readings coexist under one semantic kernel or are actually competing kernels with different names.',
    'If coexist: this reading is one legitimate reading among others, justifying the coexists_with relation to originalist reading. If separate kernels: living constitutionalism is the constraint documented here, and originalism is a separate constraint story entirely, not a sibling reading—the network relationship changes from reading_relations to affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_reading_asymmetry, conceptual, 'Whether this reading and originalism are alternative interpretations of one kernel or separate constitutional arrangements.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.42) structural—external legal barriers and institutional sanctions against originalist scholarship—or partly internalized, where textualists have absorbed the judgment that their methodology is intellectually inferior?',
    'Post-exit trajectories: if originalist scholars gain acceptance outside the US legal system (comparative constitutional analysis, private practice, international courts) their suppression drops, indicating externality. If they remain epistemically marginalized even in those contexts, indicating internalized methodological doubt, the suppression is partly internalized.',
    'If structural: suppression persists because institutions enforce the living constitution frame; removing enforcement would shift the balance. If internalized: the suppression carries with the agent; even removing institutional barriers wouldn''t restore confidence in originalist methodology. This affects how we count the extraction cost and whether it would change if the constraint were removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of originalist methodology is structural or internalized.').

omega_variable(
    constitutional_authority_sovereignty,
    'Can the people amend or replace the Constitution to override a living constitutionalist reading, or has living constitutionalism effectively captured the amendment process?',
    'Historical analysis of whether the Court has ever reversed a major living constitution expansion through its own interpretation (rare); analysis of whether constitutional amendments overturning judicial readings are feasible (high supermajority thresholds make them nearly impossible); observation of whether the constraint prevents the people from creating an alternative constitutional authority.',
    'If amendment is feasible: the people retain theoretical sovereignty and can override living constitution doctrine. If amendment is so difficult that it is effectively impossible: living constitutionalism becomes entrenched and cannot be challenged democratically—the constraint''s extraction is not reversible by exit or collective action. This affects whether to classify as snare (irreversible) or tangled rope (reversible with political effort).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_authority_sovereignty, empirical, 'Whether popular sovereignty can amend away living constitutionalism or whether the constraint is institutionally entrenched.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__living_constitution_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1960, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(us_c_tr_t1975, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(us_c_tr_t2005, us_constitution_interpretive__living_constitution_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_interpretive__living_constitution_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_interpretive__living_constitution_reading, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1960, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1960, 0.38).
narrative_ontology:measurement(us_c_be_t1975, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1990, 0.61).
narrative_ontology:measurement(us_c_be_t2005, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1960, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1960, 0.28).
narrative_ontology:measurement(us_c_su_t1975, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1975, 0.33).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(us_c_su_t2005, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 2015, 0.41).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__living_constitution_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__living_constitution_reading, 0.18).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__popular_constitutionalism_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, federal_regulatory_expansion).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, judicial_supremacy_doctrine).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, unenumerated_rights_production).

% DUAL FORMULATION NOTE:
% This story is one reading of a contested constitutional kernel. The originalist reading instantiates a different ε (lower extraction, lower federation power expansion) from the same constitutional text. These are not two observables of one constraint; they are two readings of one kernel that produce structurally distinct constraints. Both stories must be authored (separate files) and linked via network.affects_constraints. The constraint family includes at least three stories (originalist, living constitution, popular constitutionalism), each with its own ε, beneficiary/victim structure, and claimed type. Compute-per-seat types will show different classifications from different stakeholder positions for the same reading, and will also show that the originalist reading computes differently (lower extraction, higher mountain characteristics) than the living constitution reading when evaluated from the same seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
