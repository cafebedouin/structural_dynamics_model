% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Interpretation of the U.S. Constitution
 *   domain: legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The living constitutionalist reading of the U.S. Constitution holds that
 *   the document's broad principles (due process, equal protection, cruel and
 *   unusual punishment) retain their original semantic anchors but their
 *   application must evolve with changing social attitudes and circumstances.
 *   This reading treats the Constitution as a framework for ongoing
 *   democratic self-governance rather than a fixed code. Judges are
 *   constrained by text and history but empowered — indeed obligated — to
 *   interpret open-ended provisions in light of contemporary moral
 *   understanding. The constraint coordinates the resolution of novel rights
 *   claims (same-sex marriage, reproductive autonomy, digital privacy) that
 *   the ratifiers could not have anticipated, while the evolutionary
 *   mechanism itself requires no active enforcement beyond the judicial role.
 *   The coordination function is genuine: it prevents constitutional
 *   stagnation and allows the legal system to recognize new forms of dignity
 *   and equality. Extraction is low because the primary transfer is from
 *   static textual literalism to living application — a transfer of
 *   interpretive authority, not material resources. The victims declared
 *   (counter_majoritarian_legitimacy, institutional_stability) bear diffuse,
 *   institutional costs rather than concentrated material extraction. Theater
 *   ratio is moderate-low (0.22) and rising: some opinions perform
 *   evolutionary reasoning while reaching results predetermined by judicial
 *   preference, but the core mechanism remains functional.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, 0.18).
domain_priors:suppression_score(us_constitution_meaning__living_constitutionalist_reading, 0.12).
domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__living_constitutionalist_reading, rope).
narrative_ontology:human_readable(us_constitution_meaning__living_constitutionalist_reading, "Living Constitutionalist Interpretation of the U.S. Constitution").
narrative_ontology:topic_domain(us_constitution_meaning__living_constitutionalist_reading, "legal_theory/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__living_constitutionalist_reading, 'c6e1bdca-c0da-40a3-b656-291711bbe7bf').
narrative_ontology:cs_kernel_codification('c6e1bdca-c0da-40a3-b656-291711bbe7bf', fixed_text).
narrative_ontology:cs_authority_grounding('c6e1bdca-c0da-40a3-b656-291711bbe7bf', lineage).
narrative_ontology:cs_interpretation_layer_present('c6e1bdca-c0da-40a3-b656-291711bbe7bf').
narrative_ontology:cs_reading_relation('c6e1bdca-c0da-40a3-b656-291711bbe7bf', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6e1bdca-c0da-40a3-b656-291711bbe7bf', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('c6e1bdca-c0da-40a3-b656-291711bbe7bf', foundational, constitutional_principles_evolve_through_judicial_application).
narrative_ontology:cs_axiom_status(constitutional_principles_evolve_through_judicial_application, holdable).
narrative_ontology:cs_axiom_grounding('c6e1bdca-c0da-40a3-b656-291711bbe7bf', constitutional_principles_evolve_through_judicial_application, conventional).
narrative_ontology:cs_axiom('c6e1bdca-c0da-40a3-b656-291711bbe7bf', foundational, contemporary_moral_consensus_informs_constitutional_meaning).
narrative_ontology:cs_axiom_status(contemporary_moral_consensus_informs_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('c6e1bdca-c0da-40a3-b656-291711bbe7bf', contemporary_moral_consensus_informs_constitutional_meaning, instrumental).
narrative_ontology:cs_reference_frame('c6e1bdca-c0da-40a3-b656-291711bbe7bf', ratification_era_constitutional_principles).
narrative_ontology:cs_drift_state('c6e1bdca-c0da-40a3-b656-291711bbe7bf', contemporary_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c6e1bdca-c0da-40a3-b656-291711bbe7bf', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, marginalized_groups_seeking_recognition).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, social_movements_for_expanded_equality).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, counter_majoritarian_legitimacy).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, institutional_stability_of_constitutional_text).
narrative_ontology:constraint_vindicates(us_constitution_meaning__living_constitutionalist_reading, constitutional_principles_endure_through_evolving_application).
narrative_ontology:constraint_vindicates(us_constitution_meaning__living_constitutionalist_reading, contemporary_moral_consensus_informs_constitutional_meaning).
narrative_ontology:constraint_vindicates(us_constitution_meaning__living_constitutionalist_reading, judicial_empowerment_for_rights_expansion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and groups whose rights claims (same-sex marriage, reproductive autonomy, gender identity, digital privacy) were not contemplated at ratification but become constitutionally cognizable through evolving application. They gain access to constitutional protection without waiting for Article V amendment. Their exit from this constraint would mean reverting to originalist fixity — losing the pathway for novel rights recognition — so they are constrained rather than mobile.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_contexts, beneficiary,
    moderate, biographical, constrained, national).

% Groups historically excluded from constitutional protection (racial minorities, women, LGBTQ+ persons) who rely on evolving standards of equality and dignity to make the Constitution's promises operational for their circumstances. The living reading's coordination function was built substantially for and by these groups' struggles. Exit would mean abandoning the interpretive framework that made Brown, Loving, Obergefell possible — identity_locked in practice, but authored as constrained because the constraint itself provides the recognition they seek.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, marginalized_groups_seeking_recognition, beneficiary,
    organized, generational, constrained, national).

% Organized movements (civil rights, women's rights, marriage equality, trans rights) that mobilize to shift contemporary moral consensus and then invoke that consensus in constitutional litigation. They both benefit from the living reading's receptivity and actively shape the 'evolving standards' it tracks. Their exit is mobile: they can pursue legislative or amendment strategies instead, and often do both simultaneously.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, social_movements_for_expanded_equality, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__living_constitutionalist_reading, social_movements_for_expanded_equality, agenda_setter).

% Article III judges who interpret and apply the Constitution's open-ended provisions. Under this reading they hold the authoritative power to declare what contemporary moral consensus requires. They benefit from expanded interpretive authority but bear legitimacy costs when their decisions are perceived as policy-making. Their exit is arbitrage-grade: they can adopt originalist or positivist methodologies within the same institutional role.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, federal_judges, agenda_setter,
    institutional, biographical, arbitrage, national).

% The institutional legitimacy of judicial review itself, which erodes when courts invalidate democratic enactments on grounds that the political branches and public contest as judicial overreach. This is not a human agent but an institutional good — the constraint's operation transfers legitimacy from democratic majorities to judicial minorities. It is trapped because the legitimacy deficit accumulates structurally whenever the living reading produces results the political process would not; there is no exit from this dynamic within the constraint's logic.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, counter_majoritarian_legitimacy, payer,
    institutional, generational, trapped, national).

% The Constitution's function as a fixed coordination anchor — a stable reference point that enables intergenerational legal continuity. The living reading treats the text as perpetually open to new application, which erodes its fixity. This institutional good is trapped: the more the text's meaning evolves, the less it serves as a stable anchor, and there is no mechanism within the living reading to restore fixity.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, institutional_stability_of_constitutional_text, payer,
    institutional, civilizational, trapped, national).

% Judges, scholars, and political actors who advocate for the originalist reading. They are excluded from the living reading's interpretive community — their methodological premises are treated as foreclosed rather than engaged. They would object that the living reading substitutes judicial will for constitutional law. Their exit is mobile: they maintain a parallel interpretive framework and compete for institutional control (judicial appointments, academic influence, public discourse).
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, originalist_judges_and_scholars, excluded,
    powerful, generational, mobile, national).

% Scholars who analyze the structural dynamics of constitutional interpretation across readings. They neither collect nor pay under any single reading; they track how the three readings compete, coevolve, and structure constitutional politics. Their analytical seat sees the full constraint family.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of applying an 18th-century constitutional text to 21st-century rights claims (same-sex marriage, reproductive autonomy, digital surveillance, AI governance) without requiring Article V amendment for each novel context. The living reading provides a structured, precedent-constrained mechanism for the Constitution to 'grow' with the society it governs.
% TRANSFER_FUNCTION: Transfers interpretive authority from fixed historical meaning to evolving judicial application. The 'cost' is borne by the counter-majoritarian legitimacy of judicial review (when courts override democratic choices) and the institutional stability of the constitutional text (when fixity erodes). The 'gain' flows to rights claimants whose claims become cognizable and to the judicial institution that wields the adaptive authority.
% ABSENT_VOICES: Future generations whose constitutional inheritance is shaped by today's evolving standards — they cannot consent to or contest the interpretations that will bind them. Also absent: the ratifiers themselves, whose original understanding is treated as a starting point rather than a binding command. The originalist reading gives these absent voices a structural seat (through historical meaning); the living reading does not.
% DISAPPEARANCE_RATIONALE: If the living constitutionalist reading vanished overnight (e.g., a constitutional amendment or Court majority adopting originalism as exclusive methodology), the immediate effect would be the foreclosure of novel rights claims not grounded in original meaning. Same-sex marriage recognition, reproductive autonomy protections beyond original understanding, and evolving Eighth Amendment standards would lose their constitutional footing. The legal landscape would rearrange toward originalist fixity, legislative amendment would become the sole path for new rights, and the judicial role would contract. The world would not stay the same.
% FOUNDING_PROBLEM: The Constitution's broad provisions (due process, equal protection, cruel and unusual punishment) were drafted at a time when their full moral implications were not realized — slavery existed, women were disenfranchised, same-sex relationships were criminalized. The founding problem of the living reading is: how can a constitutional text written by and for a deeply unequal society become a vehicle for genuine equality and liberty for all persons, without requiring the impossible politics of continual formal amendment?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record of constitutional litigation: the NAACP's incremental strategy (culminating in Brown) explicitly relied on evolving standards; the marriage equality movement invoked Lawrence's 'evolving understanding' doctrine; reproductive rights advocates grounded Roe and Casey in the living reading's methodology. These are corroborations from the beneficiary side. From outside the beneficiary set: originalist scholars (Scalia, Bork, Barnett) concede the living reading was built to solve the 'dead hand' problem but argue it solves it illegitimately. Political scientists (Whittington, Graber) document the living reading's emergence as a response to the New Deal Court's confrontation with democratic majorities — a structural corroboration independent of normative commitment.
narrative_ontology:disappearance_verdict(us_constitution_meaning__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(us_constitution_meaning__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__living_constitutionalist_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint's operation primarily transfers interpretive authority from a fixed historical meaning to an evolving application — a coordination gain for rights claimants in novel contexts, not a material extraction from a defined victim class. Suppression is low (0.12) because the mechanism operates through judicial reasoning and precedent, not coercion; alternatives (originalist interpretation, legislative amendment) remain available and actively contested. Theater ratio (0.22) reflects genuine but imperfect fidelity: some evolutionary opinions show result-driven reasoning, but the methodology as a whole produces outcomes (Brown v. Board, Obergefell, Lawrence) that tracking contemporary moral consensus genuinely explains. Accessibility collapse (0.35) is moderate — originalist and positivist alternatives persist and compete. Resistance (0.48) is significant: originalist judges, scholars, and political movements actively contest this reading's legitimacy. The claimed_type 'rope' reflects genuine coordination with minimal extraction; the engine will compute per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the rights claimant's seat, this constraint is a rope — it solves the genuine coordination problem of recognizing new rights without textual amendment. From the counter-majoritarian legitimacy seat, it approaches tangled_rope: there is a coordination function (rights recognition) but also asymmetric extraction (judicial invalidation of democratic choices). From the originalist seat, it may compute as snare — the coordination story is read as cover for judicial policy-making. The engine computes these per-seat types from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights claimants in evolving contexts (beneficiaries) gain the capacity to have their claims recognized without waiting for constitutional amendment — their directionality is near-beneficiary (d ~ 0.15). Marginalized groups and social movements benefit from a framework that treats their demands as constitutionally cognizable rather than textually foreclosed. The counter-majoritarian legitimacy concern (victim) bears diffuse institutional cost: when courts invalidate democratic enactments on evolving-standards grounds, the constraint extracts legitimacy from the majoritarian branches — directionality near-target (d ~ 0.75) for this institutional seat. The institutional stability of constitutional text (victim) bears a related but distinct cost: the text's fixity as a coordination anchor erodes when its meaning is treated as perpetually open — directionality moderate-target (d ~ 0.60). Judges (agenda_setter) sit near-symmetric: they gain interpretive authority but bear legitimacy costs when their adaptations are contested.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing constitutional stagnation while maintaining textual fidelity) remains live — new social contexts (AI personhood, climate rights, algorithmic due process) continually generate novel claims the fixed text cannot directly address. The arrangement has not outlived its function; if anything, accelerating social change increases the need for an evolutionary interpretive mechanism. However, the rising theater_ratio and the judicial_overreach omega signal a risk of mandatrophy: if the evolutionary mechanism becomes a vehicle for judicial preference rather than genuine consensus-tracking, the constraint would degrade toward piton or tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is this constraint one reading of a contested kernel (us_constitution_meaning), and does the living constitutionalist reading have a distinct ε from the originalist and positivist readings?',
    'Compare structural metrics and beneficiary/victim profiles across the three declared readings of the same kernel. If ε values differ significantly, the kernel decomposes into distinct constraints per the ε-invariance principle.',
    'If the living constitutionalist reading shows substantially lower extractiveness and different beneficiary structure than the originalist reading, the ''constitutional interpretation'' label conflates multiple constraints. The engine must treat them as separate constraint stories linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the living constitutionalist reading constitutes a structurally distinct constraint from sibling readings of the same kernel.').

omega_variable(
    judicial_empowerment_vs_overreach,
    'Does the living constitutionalist reading''s empowerment of judges to adapt constitutional application create a genuine coordination function (resolving hard cases in evolving social contexts) or does it function as a snare where judicial discretion extracts from democratic legitimacy?',
    'Track the trajectory of rights_expansion_cases vs. judicial_invalidation_of_democratic_enactments over time. If the ratio of protective to invalidating decisions shifts toward the latter, the coordination function degrades toward extraction.',
    'If judicial overreach becomes the dominant pattern, the constraint''s claimed_type would shift from rope toward tangled_rope or snare. The theater_ratio would rise as rights-expansion rhetoric covers legitimacy extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_empowerment_vs_overreach, empirical, 'Whether judicial adaptation under this reading functions as coordination or extraction.').

omega_variable(
    contemporary_consensus_measurement,
    'How is ''contemporary moral consensus'' identified and measured for constitutional application? Is it a genuine social fact or a construct that judges can selectively invoke?',
    'Analyze methodological transparency in opinions invoking evolving standards: Do they cite specific evidence (polling, legislative trends, international norms) or assert consensus without verification?',
    'If consensus is judicially constructed rather than discovered, the constraint''s suppression score understates the actual coercive force — judges impose their own moral preferences under the guise of reading society''s mind.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contemporary_consensus_measurement, conceptual, 'Epistemic status of the contemporary moral consensus that purportedly guides evolving application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__living_constitutionalist_reading, 1789, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1789, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(us_c_tr_t1868, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 1868, 0.08).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 1937, 0.12).
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 1954, 0.15).
narrative_ontology:measurement(us_c_tr_t1973, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 1973, 0.18).
narrative_ontology:measurement(us_c_tr_t2003, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement(us_c_tr_t2026, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1789, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 1789, 0.05).
narrative_ontology:measurement(us_c_be_t1868, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 1868, 0.08).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 1937, 0.12).
narrative_ontology:measurement(us_c_be_t1954, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 1954, 0.14).
narrative_ontology:measurement(us_c_be_t1973, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 1973, 0.16).
narrative_ontology:measurement(us_c_be_t2003, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 2003, 0.17).
narrative_ontology:measurement(us_c_be_t2026, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 2026, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1789, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 1789, 0.05).
narrative_ontology:measurement(us_c_su_t1868, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 1868, 0.07).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 1937, 0.09).
narrative_ontology:measurement(us_c_su_t1954, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 1954, 0.1).
narrative_ontology:measurement(us_c_su_t1973, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 1973, 0.11).
narrative_ontology:measurement(us_c_su_t2003, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 2003, 0.11).
narrative_ontology:measurement(us_c_su_t2026, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 2026, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__living_constitutionalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__living_constitutionalist_reading, 0.08).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the us_constitution_meaning kernel. The living constitutionalist reading (this story) claims rope-type coordination with low extraction (ε=0.18) and beneficiaries among rights claimants. The originalist reading claims mountain-type fixity with near-zero extraction but victims among rights claimants in novel contexts. The positivist reading claims scaffold-type procedural validity with institutional beneficiaries. Their ε values differ because they describe different structural arrangements: living constitutionalism coordinates evolving application; originalism coordinates fixed meaning; positivism coordinates procedural legitimacy. The kernel label 'constitutional interpretation' conflates these.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__living_constitutionalist_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
