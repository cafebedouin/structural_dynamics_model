% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__living_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_interpretive__living_constitution_reading
 *   human_readable: Living Constitution Interpretive Framework
 *   domain: legal/constitutional/political_theory
 *
 * SUMMARY:
 *   The living constitution reading claims constitutional meaning evolves
 *   through reasoned judicial adaptation to contemporary values and
 *   conditions. It presents itself as the necessary coordination mechanism
 *   for applying an 18th-century charter to modern governance. The structural
 *   reality: it transfers interpretive authority to the federal judiciary,
 *   expands federal power beyond original scope, and recognizes unenumerated
 *   rights — benefiting rights claimants and federal power while extracting
 *   autonomy from states, originalist interpreters, and those subject to
 *   expanded federal reach. The constraint is a tangled rope: genuine
 *   coordination (solving Article V rigidity) combined with asymmetric
 *   extraction (judicial and federal power expansion). The claim/metric
 *   independence is maintained: claimed_type is tangled_rope (the reading's
 *   structural reality), metrics reflect descriptively measured extraction,
 *   suppression, and theater.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, 0.65).
domain_priors:suppression_score(us_constitution_interpretive__living_constitution_reading, 0.45).
domain_priors:theater_ratio(us_constitution_interpretive__living_constitution_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__living_constitution_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__living_constitution_reading, "Living Constitution Interpretive Framework").
narrative_ontology:topic_domain(us_constitution_interpretive__living_constitution_reading, "legal/constitutional/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__living_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__living_constitution_reading, 'dd3d08d9-20f2-43a7-8753-9f649605e8db').
narrative_ontology:cs_kernel_codification('dd3d08d9-20f2-43a7-8753-9f649605e8db', formalized).
narrative_ontology:cs_authority_grounding('dd3d08d9-20f2-43a7-8753-9f649605e8db', lineage).
narrative_ontology:cs_interpretation_layer_present('dd3d08d9-20f2-43a7-8753-9f649605e8db').
narrative_ontology:cs_reading_relation('dd3d08d9-20f2-43a7-8753-9f649605e8db', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd3d08d9-20f2-43a7-8753-9f649605e8db', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('dd3d08d9-20f2-43a7-8753-9f649605e8db', foundational, constitutional_meaning_evolves_with_societal_values).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_societal_values, holdable).
narrative_ontology:cs_axiom_grounding('dd3d08d9-20f2-43a7-8753-9f649605e8db', constitutional_meaning_evolves_with_societal_values, conventional).
narrative_ontology:cs_axiom('dd3d08d9-20f2-43a7-8753-9f649605e8db', foundational, judicial_reasoned_adaptation_legitimate).
narrative_ontology:cs_axiom_status(judicial_reasoned_adaptation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('dd3d08d9-20f2-43a7-8753-9f649605e8db', judicial_reasoned_adaptation_legitimate, instrumental).
narrative_ontology:cs_reference_frame('dd3d08d9-20f2-43a7-8753-9f649605e8db', ratification_era_understanding).
narrative_ontology:cs_drift_state('dd3d08d9-20f2-43a7-8753-9f649605e8db', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dd3d08d9-20f2-43a7-8753-9f649605e8db', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, civil_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, federal_legislative_executive).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, states_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, originalist_textualists).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, state_governments_constrained).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, constrained_by_federal_reach).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, constitutional_adaptability_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, judicial_review_legitimacy).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, unenumerated_rights_recognition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the authoritative interpretive role through judicial review. Expands doctrinal reach via evolving standards (substantive due process, Commerce Clause, equal protection). Collects institutional authority and legitimacy from being the designated adapter of constitutional meaning. Constrained by appointment politics and legitimacy concerns but faces no exit from the role.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Gain enforceable rights against state and private discrimination through evolving equal protection and due process readings (Brown, Loving, Obergefell, Bostock). Their claims succeed because the living framework treats equality as an expanding principle. Exit means abandoning judicial protection for legislative-only strategies, which historically failed them.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, civil_rights_claimants, beneficiary,
    organized, biographical, constrained, national).

% Secured recognition of privacy and autonomy rights (Roe, Casey, Dobbs dissent) through substantive due process evolution. The living framework treated liberty as encompassing intimate decisions. After Dobbs, exit means state-by-state legislative fights where they lack structural power — the framework's retraction demonstrates their dependency on judicial maintenance.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates, beneficiary,
    organized, biographical, constrained, national).

% Achieved marriage equality, anti-discrimination protection, and dignity recognition through evolving liberty and equality doctrines (Lawrence, Windsor, Obergefell, Bostock). The living framework's openness to new understandings of liberty enabled these claims. Exit means reliance on democratic majorities that historically opposed them.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants, beneficiary,
    organized, biographical, constrained, national).

% Gains expanded regulatory authority through evolving Commerce Clause (Wickard, Raich), Necessary and Proper Clause, and Section 5 enforcement power. The living framework treats federal power as adaptable to national problems. Can also appoint judges who sustain the framework. Exit is not needed — they benefit from and help maintain the arrangement.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_legislative_executive, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__living_constitution_reading, federal_legislative_executive, agenda_setter).

% Lose regulatory autonomy as federal power expands and judicial review invalidates state laws under evolving standards (preemption, dormant commerce clause, substantive due process). Their constitutional vision — enumerated powers, state police powers — is marginalized by the living framework. Exit means constitutional amendment (Article V), which is structurally near-impossible.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, states_rights_advocates, payer,
    organized, generational, constrained, national).

% Bear the cost of seeing their interpretive methodology excluded from authoritative status. Their professional identity (judges, scholars, advocates) is fused to originalism as a methodological commitment. The living framework treats originalism as one contested approach among others, not the binding rule. Exit means abandoning their professional identity and the intellectual project they built — identity-locked, not merely constrained.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, originalist_textualists, payer,
    organized, generational, identity_locked, national).

% Face binding federal judicial mandates on voting rights, reproductive regulation, education policy, environmental regulation, and criminal procedure. The living framework's nationalizing drift converts state policy choices into constitutional violations. Exit means interposition/nullification (legally foreclosed) or seeking Supreme Court reversal (uncertain, slow).
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, state_governments_constrained, payer,
    institutional, generational, constrained, national).

% Individuals and local entities subject to expanded federal regulatory and criminal reach (e.g., federalization of crime, regulatory compliance costs, loss of local governance options). No meaningful exit — cannot change jurisdiction, cannot afford lobbying, no standing to challenge structural drift. The living framework's expansion of federal power operates on them without consent.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, constrained_by_federal_reach, payer,
    powerless, biographical, trapped, national).

% Studies the framework's operation across doctrinal domains, historical periods, and comparative systems. Produces the theoretical vocabulary (living constitutionalism, common law constitutionalism, representation reinforcement) that shapes how the constraint is understood. Neither collects nor pays; sees the full structure.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, legal_academy_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable but adaptable framework for applying an 18th-century constitutional text to modern conditions without requiring constant formal amendment. Solves the coordination problem of legitimate governance across changing societal conditions by authorizing judicial updating of doctrinal meaning.
% TRANSFER_FUNCTION: Moves interpretive authority and policy-making power from state legislatures and the original constitutional design to the federal judiciary and federal political branches. Transfers the cost of adaptation from the Article V amendment process (supermajority consensus) to judicial decision-making (majority of nine).
% ABSENT_VOICES: Founding-era ratifiers (dead, cannot consent to new readings); future generations (not yet born, bear long-term consequences of doctrinal drift); non-litigants affected by judicial policy-making (no standing, no voice in confirmation process); international persons subject to US constitutional projections (no representation).
% DISAPPEARANCE_RATIONALE: If the living constitution framework vanished overnight, the Supreme Court would lose its claimed authority to recognize unenumerated rights and expand federal power beyond original understanding. Rights recognized solely through substantive due process (privacy, marriage, bodily autonomy) would lose constitutional footing, reverting to state legislative control. Federal regulatory authority would contract to original Commerce Clause scope. The entire post-1937 constitutional order would restructure.
% FOUNDING_PROBLEM: The Constitution's fixed text, ratified in 1788-1791, could not anticipate modern conditions: industrial economy, digital surveillance, medical technology, national markets, global power. The Article V amendment process proved too rigid (27 amendments in 230+ years, only 17 substantive after Bill of Rights). A governance system needed a mechanism to adapt constitutional meaning without formal amendment.
% FOUNDING_PROBLEM_CORROBORATION: Ackerman (We the People), Strauss (The Living Constitution), Balkin (Living Originalism) attest from inside the tradition. Critics: Whittington (Constitutional Interpretation), Rappaport (Originalism and the Good Constitution), Vermeule (Common Good Constitutionalism) attest from outside the beneficiary set that the founding problem (rigidity of Article V) is real but contest whether judicial adaptation is the legitimate solution. The Federalist Society's existence and originalism's dominance in conservative legal thought corroborate that the problem is live and the living framework's solution is contested.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__living_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__living_constitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__living_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__living_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__living_constitution_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.65) reflects substantial transfer of decision-making authority from states and democratic processes to federal courts. Suppression (0.45) is moderate — alternative interpretive methods (originalism) persist and compete but are structurally disadvantaged in judicial practice. Theater ratio (0.25) is low — the adaptation is real, not performative, though post-Dobbs the framework shows some performative maintenance of precedent language while substance shifts. Accessibility collapse (0.55) is moderate — originalism remains a live interpretive option with institutional footholds (Federalist Society, originalist judges) but operates from a marginalized position. Resistance (0.60) is high — organized originalist movement, state resistance, academic critique, and political appointments contest the framework continuously.
 *
 * PERSPECTIVAL GAP:
 *   From the federal judiciary's seat, the constraint is a rope — genuine coordination solving Article V rigidity. From states' rights advocates and originalists, it is a snare — extraction of their interpretive authority and state autonomy. From rights claimants, it is a rope that became a scaffold (rights recognized but now partially retracted). From constrained individuals, it is a snare with no exit. The engine computes this divergence from the structural data; the authored claim (tangled_rope) captures the aggregate structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal judiciary sits at beneficiary end (d ~ 0.15): collects institutional authority, sets the agenda, faces no exit. Civil rights/reproductive/LGBTQ claimants are beneficiaries with constrained exit (d ~ 0.30): gain rights but depend on judicial maintenance. Federal legislative/executive are beneficiaries with arbitrage exit (d ~ 0.10): gain power, can appoint judges to sustain it. States' rights advocates and originalist textualists are payers: originalists identity-locked (d ~ 0.85) — professional identity fused to methodology; states' rights advocates constrained (d ~ 0.70) — institutional role but limited exit. State governments institutional payers (d ~ 0.75). Constrained individuals trapped (d ~ 0.95). Legal academy analytical (d ~ 0.50).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Article V rigidity) remains live — the Constitution is still hard to amend. But the living framework has accumulated extraction beyond the coordination function: judicial power now reaches domains (abortion, marriage, federal regulatory scope) that the founding problem did not require solving. The mandatrophy is unresolved: the framework persists partly because it benefits the judiciary and federal power, not solely because Article V remains rigid. The Dobbs reversal shows the framework can retract — but the retraction itself was a judicial act, not a democratic correction, confirming the extractive structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the living constitution reading instantiate a distinct constraint from the originalist and popular constitutionalism readings, or are they measurement perspectives on one constraint?',
    'Test ε-invariance: if the standing arrangement (judicial review with evolving standards) is assessed from each reading''s lights, do they author different ε values for the same referent? The living reading sees moderate extraction (coordination with costs); originalist reading sees high extraction (usurpation); popular constitutionalism sees misallocated extraction (judicial capture of popular authority). Different ε → distinct constraints per DP-001.',
    'If distinct constraints, each gets its own story with own classification. If one constraint, the framework must model perspective-relative classification — which the engine does not support. The ε-invariance principle requires decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether kernel readings are distinct constraints or perspectives on one constraint.').

omega_variable(
    coordination_extraction_boundary,
    'How much of the federal judiciary''s expanded authority is necessary coordination (applying old text to new problems) versus extracted power (policy-making displaced from democratic branches)?',
    'Counterfactual: if Article V were functional (lower threshold, regular use), how much of the living constitution''s doctrinal expansion would have occurred via amendment instead? The difference measures extracted power. Historical comparison: pre-1937 Court struck down economic regulation (Lochner) — was that coordination or extraction?',
    'If most expansion is extractive, classification shifts toward snare. If mostly coordinative, stays tangled_rope. If negligible extraction, approaches rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Boundary between genuine adaptive coordination and judicial power extraction.').

omega_variable(
    originalist_identity_lock_mechanism,
    'Is the originalist textualist''s identity_lock professional (career path dependence), ideological (worldview), or institutional (movement capture of courts)?',
    'Track originalist judges/scholars who exit the movement: do they lose professional standing? Do they describe exit as identity crisis? Compare to Federalist Society pipeline data — what fraction of members are identity-locked vs. career-aligned?',
    'If professional, d ~ 0.75 (constrained but exit possible). If ideological, d ~ 0.85 (identity_locked proper). If institutional capture, d ~ 0.90 (movement controls appointments). Changes effective extraction calculation for this seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_identity_lock_mechanism, empirical, 'Mechanism of identity_lock for originalist textualists.').

omega_variable(
    post_dobbs_framework_stability,
    'Does the Dobbs retraction of Roe/Casey represent framework correction (reducing extraction) or framework adaptation (maintaining judicial supremacy by conceding one right to preserve the structure)?',
    'Measure post-Dobbs: does judicial review scope contract (fewer cases, narrower doctrines) or merely shift (new unenumerated rights, procedural innovations)? Track cert grants, doctrinal novelty, and federal power cases.',
    'If correction, extractiveness and theater_ratio should decline sustainably. If adaptation, extractiveness stabilizes at new equilibrium, theater may rise (performative maintenance of legitimacy).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_dobbs_framework_stability, empirical, 'Whether Dobbs signals living framework''s extraction reduction or structural adaptation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__living_constitution_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement(us_c_tr_t1965, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(us_c_tr_t1973, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1973, 0.22).
narrative_ontology:measurement(us_c_tr_t1992, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1992, 0.28).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_interpretive__living_constitution_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_interpretive__living_constitution_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1937, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(us_c_be_t1954, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1954, 0.42).
narrative_ontology:measurement(us_c_be_t1965, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(us_c_be_t1973, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1973, 0.62).
narrative_ontology:measurement(us_c_be_t1992, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1992, 0.68).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 2015, 0.72).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1937, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1937, 0.25).
narrative_ontology:measurement(us_c_su_t1954, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1954, 0.4).
narrative_ontology:measurement(us_c_su_t1965, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(us_c_su_t1973, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1973, 0.45).
narrative_ontology:measurement(us_c_su_t1992, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1992, 0.55).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__living_constitution_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__living_constitution_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, commerce_clause_expansion).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, substantive_due_process_unenumerated_rights).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, section5_enforcement_power).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, dormant_commerce_clause).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, federal_preemption_doctrine).

% DUAL FORMULATION NOTE:
% Part of us_constitution_interpretive kernel family with originalist_reading and popular_constitutionalism_reading. This reading claims judicial adaptive authority; originalist_reading claims ratifier-fixed meaning; popular_constitutionalism_reading claims popular-movement authority. The three decompose the single label 'constitutional interpretation' into structurally distinct constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__living_constitution_reading, institutional, 0.15).
constraint_indexing:directionality_override(us_constitution_interpretive__living_constitution_reading, organized, 0.3).
constraint_indexing:directionality_override(us_constitution_interpretive__living_constitution_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
