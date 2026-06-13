% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Courts claim final authority to interpret the Constitution and to nullify
 *   legislative acts deemed unconstitutional. This reading of the
 *   constitutional kernel asserts that the judiciary, as guardian of
 *   fundamental rights and supreme law, must possess power to override
 *   coordinate branches when they violate constitutional limits. Elected
 *   legislatures and electoral majorities bear the cost of this
 *   supremacy—their legislative choices can be voided without their consent.
 *   The foundational claim is that rights protection requires judicial veto
 *   power over majorities; the competing readings (parliamentary supremacy,
 *   coordinate construction) reject this claim and assert legislatures or
 *   inter-branch dialogue can protect rights equally or better. This is ONE
 *   READING of the constitutional authority kernel; sibling readings exist
 *   and are documented in omega variables and cs_structure.
 *
 * KEY AGENTS:
 *   - Judiciary: Institutional agenda-setter, claims final interpretive authority, enforces constraint via review power, unelected.
 *   - Legislature: Institutional payer, subject to nullification, claims coordinate interpretive role but structurally subordinated.
 *   - Electoral majorities: Organized payer, can have legislative choices overridden by courts; recourse requires supermajority (amendment) or long time horizon (justice replacement).
 *   - Constitutional rights claimants: Powerless beneficiary, lack direct legislative power but gain a forum to challenge majoritarian legislation.
 *   - Constitutional scholars: Analytical observer, document and critique the constraint itself.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.72).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "political/constitutional").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '56c1ff2c-aaef-4d60-86d3-52e59c82ac2b').
narrative_ontology:cs_kernel_codification('56c1ff2c-aaef-4d60-86d3-52e59c82ac2b', fixed_text).
narrative_ontology:cs_authority_grounding('56c1ff2c-aaef-4d60-86d3-52e59c82ac2b', extraction).
narrative_ontology:cs_interpretation_layer_present('56c1ff2c-aaef-4d60-86d3-52e59c82ac2b').
narrative_ontology:cs_reading_relation('56c1ff2c-aaef-4d60-86d3-52e59c82ac2b', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('56c1ff2c-aaef-4d60-86d3-52e59c82ac2b', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('56c1ff2c-aaef-4d60-86d3-52e59c82ac2b', foundational, judicial_veto_necessary_for_rights).
narrative_ontology:cs_axiom_status(judicial_veto_necessary_for_rights, holdable).
narrative_ontology:cs_axiom_grounding('56c1ff2c-aaef-4d60-86d3-52e59c82ac2b', judicial_veto_necessary_for_rights, deontological).
narrative_ontology:cs_axiom('56c1ff2c-aaef-4d60-86d3-52e59c82ac2b', foundational, constitutional_text_constrains_judges).
narrative_ontology:cs_axiom_status(constitutional_text_constrains_judges, holdable).
narrative_ontology:cs_axiom_grounding('56c1ff2c-aaef-4d60-86d3-52e59c82ac2b', constitutional_text_constrains_judges, deontological).
narrative_ontology:cs_reference_frame('56c1ff2c-aaef-4d60-86d3-52e59c82ac2b', constitutional_supremacy_with_judicial_guardianship).
narrative_ontology:cs_drift_state('56c1ff2c-aaef-4d60-86d3-52e59c82ac2b', contemporary_contestation_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('56c1ff2c-aaef-4d60-86d3-52e59c82ac2b', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_rights_claimants).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, electoral_majorities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measured at 0.68 (endpoint) reflects the judiciary's power to void legislative acts without legislative consent, decoupled from clear constitutional text (the Constitution does not explicitly assign final authority). The measurement series shows accumulation from 0.45 to 0.68 over the interval, tracking the historical expansion of judicial review from Marbury v. Madison (1803) through the 20th-century civil rights era to contemporary originalist and living-constitutionalist contention. Theater ratio (0.42) indicates substantial functional activity—courts genuinely adjudicate cases and apply constitutional doctrine—but also growing performative maintenance as courts justify their authority in dissents and scholarly writing. Suppression (0.72) reflects machinery that prevents legislative override (constitutional amendment is prohibitively difficult; court-packing and jurisdiction-stripping attempts are rare and mostly unsuccessful). The constraint persists because overturning it requires either constitutional amendment (supermajority) or sustained political coalitions capable of reshaping the court, both high barriers. The measured metrics describe judicial supremacy as operating with substantial extraction (overriding democratic will), genuine but challenged coordination function (final authority needed to resolve interpretation disputes), and active enforcement (courts defending their authority against encroachment). The claim is tangled_rope; the metrics sit between rope (moderate extraction, clear function) and snare (high extraction, debatable function).
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, the constraint is coordinate authority over a limited domain (constitutional interpretation). Judges experience themselves as bound by law, not making policy. From the legislature's and electoral majority's seats, the constraint is asymmetric veto power—their choices are nullified without consent, and they experience the court as imposing judicial policy preferences under the guise of constitutional interpretation. The engine computes this divergence from structural data: the judiciary is named as agenda-setter (sets the rules of interpretation, enforces nullification) and beneficiary (collects interpretive authority). The legislature and electoral majority are named as payers (their acts are subject to nullification). The constitutional rights claimants are beneficiaries (they gain a forum to challenge majoritarian legislation). These role assignments feed directionality derivation: judiciary approaches d=0.0 (full beneficiary of the constraint), while legislature and electoral majorities approach d=1.0 (full targets of nullification power). The seats experience the constraint structurally differently because the constraint's structure is fundamentally asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary: beneficiary seat (d near 0.0). They gain final interpretive authority, can nullify legislation, set rules of constitutional meaning, and face no power that can override their decisions short of constitutional amendment (supermajority barrier). Their exit is analytical—they sit above the system they govern. Legislature: payer seat (d near 1.0). Subject to nullification, constrained by courts' interpretation, must operate within boundaries courts define, cannot override judicial decisions except through amendment. Time horizon is generational because constitutional amendment is rare; exit is trapped because legislatures cannot leave the constitutional system. Electoral majorities: payer seat (d near 1.0 to 0.9). Their electoral choices can be voided; they have no direct veto over courts; their recourse is amendment (supermajority) or long-term justice replacement. Exit is constrained (cannot abandon the political system) and biographical (feel the constraint's effect within electoral lifespans). Constitutional rights claimants: beneficiary seat (d near 0.0 to 0.3). Powerless agents who gain a forum for rights claims; they lack the power to enforce rights themselves but benefit from judicial power to override majorities on their behalf. Their exit is constrained—they cannot avoid being governed—but their spatial scope as claimants is local to case-by-case disputes. The directionality structure is asymmetric: powerful institutional agenda-setters who benefit, and democratic majorities who pay. No directionality override is needed; the structural derivation already captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is 'What entity has final power to interpret the Constitution?' This reading asserts the judiciary must have that power to protect rights against majoritarian violation. The founding problem's status is contested: courts and their defenders (judges, legal scholars) claim the problem is live and justifies judicial supremacy; elected branches and their defenders claim the problem is solved better through coordinate construction or legislative supermajority gates. The disappearance_verdict is world_rearranges—if judicial review vanished, the entire institutional structure would reorganize. The classification prevents mislabeling: this constraint could be rope (genuine coordination, both sides benefit from having a final arbiter). But the measurement metrics and structural data show extraction: the legislature and majorities are subordinated without their consent, suppression is high (they have no realistic way to override courts), and theater is elevated (courts defend their authority in doctrine more than pure adjudication requires). The tangled_rope classification captures both the genuine coordination function (resolving interpretation disputes) and the asymmetric extraction (democratic subordination to judicial will). The mandatrophy risk is that the founding problem becomes obsolete: if coordinate construction or legislative supermajority gates prove equally effective at protecting rights AND more democratically legitimate, then judicial supremacy persists as pure extraction (reclassifies toward snare). The omega variables on kernel reading contestation and counter-majoritarian necessity document this mandatrophy dynamic: the founding problem's relevance depends on whether rights actually require judicial veto, not on whether such veto is possible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_neutrality_vs_preference,
    'Are courts genuinely constrained by constitutional text and precedent, or do they exercise substantial discretion to impose their interpretive preference on contested constitutional questions?',
    'Systematic empirical study: (1) Document the range of reasonable interpretations for contested constitutional questions (originalist, living-constitution, historical, textual, purposive). (2) Analyze whether justices'' votes correlate with which interpretation they adopt—do ideologically similar justices reach the same outcomes? (3) Study whether the same constitutional text, when interpreted by justices of different ideologies, yields different results. (4) Compare outcomes when justices hold identical ideology versus divergent ideology on identical constitutional questions.',
    'If courts are genuinely constrained: judicial supremacy is neutral arbitration of coordinate branches'' disputes; the constraint is tangled_rope with a legitimate coordination function. If courts exercise substantial discretion: judicial supremacy is disguised judicial veto power; the constraint reclassifies toward snare (extraction with coordination cover). This resolution determines whether the foundational axiom (constitutional_text_constrains_judges) is holdable or overridden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_neutrality_vs_preference, empirical, 'Whether judicial constitutional interpretation is determinate law or discretionary power exercised under a law-like cover.').

omega_variable(
    counter_majoritarian_necessity,
    'Are constitutional rights actually protected better under judicial supremacy (courts can nullify majoritarian legislation) or equally well or better under coordinate construction (courts coequal with legislatures) or parliamentary supremacy (legislatures supreme)?',
    'Comparative historical and empirical analysis: (1) Document rights outcomes in jurisdictions with strong judicial review (US) versus coordinate authority (post-war Germany, Canada) versus parliamentary supremacy (UK pre-1998, Australia). (2) Measure whether fundamental rights (speech, due process, equal protection, etc.) survive better in judicial-supremacy systems. (3) Study whether majorities overrode rights more often in non-judicial-supremacy systems. (4) Examine whether constitutional amendments, legislative supermajority requirements, or inter-branch negotiation provided adequate protection.',
    'If rights require judicial veto: the counter-majoritarian mechanism is necessary, the founding problem is live, and judicial supremacy is justified coordination-with-extraction. If rights are equally or better protected through other mechanisms: judicial supremacy is unnecessary extraction, the founding problem is dead, and the constraint reclassifies toward snare with zombie maintenance. If outcomes are mixed (rights better on some dimensions, equal on others): the founding problem remains contested and the constraint stays tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_majoritarian_necessity, empirical, 'Whether judicial supremacy is necessary for rights protection or one option among several equally effective mechanisms.').

omega_variable(
    kernel_reading_contestation,
    'Which reading of the constitutional_interpretive_authority kernel is correct: judicial supremacy, parliamentary supremacy, or coordinate construction?',
    'Irreducible: the Constitution does not explicitly assign final interpretive authority. Different methodologies (originalism, living constitutionalism, textualism, purposivism, historical practice) yield different readings. No objective standard resolves which methodology is correct or which reading best instantiates the Constitution''s true meaning.',
    'This omega locates the kernel contestation itself: the kernel is not a fact waiting to be discovered; it is a constitutional choice point where the political system has adopted judicial supremacy as its reading, but alternative readings remain logically coherent and institutionally viable (evidenced by other democracies'' choices). The constraint''s legitimacy depends on whether the judicial-supremacy reading is the correct reading or merely one politically powerful reading. No empirical or conceptual resolution is possible because the question is fundamentally about what the Constitution authorizes, and the Constitution''s authorization is itself the contested terrain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the constitutional text resolves the interpretive-authority question or leaves it as a persistent site of political contestation.').

omega_variable(
    enforcement_machinery_extraction,
    'To what extent does the measured suppression (0.72) reflect institutional machinery required to enforce a genuine coordinate-authority boundary (preventing inter-branch chaos), versus machinery deployed to prevent legislative override of judicial nullifications?',
    'Historical analysis: (1) Document how often legislatures have attempted to override or constrain judicial decisions (court-packing, jurisdiction-stripping, constitutional amendment, refusal to enforce judicial decrees). (2) Measure how often courts have issued injunctions against legislatures or executive to enforce their decrees. (3) Analyze the frequency and intensity of inter-branch standoffs. (4) Compare enforcement machinery in systems with different authority distributions (coordinate vs. parliamentary supremacy) to identify which machinery is ''normal'' boundary maintenance versus ''excessive'' suppression.',
    'If suppression is primarily defensive (courts blocking legislative override attempts): courts must actively suppress challenges to their authority, suggesting extraction-motivated suppression. If suppression is primarily preventive (courts maintaining their authority ex ante through precedent and doctrine): the machinery measures coordination cost. Measured suppression should track enforcement asymmetry—the more a subordinated party needs active coercion to stay subordinated, the more the suppression reflects extraction rather than neutral coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_machinery_extraction, empirical, 'Whether measured suppression reflects coordination-infrastructure cost or extractive suppression of legitimate challenges to judicial authority.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the legislature''s and majorities'' acceptance of judicial supremacy a result of structural barriers (constitutional amendment requires supermajority, court-packing is difficult) or internalized deference (they have come to believe judicial authority is legitimate)?',
    'Post-exit analysis: If the constitutional amendment process were simplified (lowered supermajority threshold) or court-packing became viable, would legislatures and majorities attempt to override judicial supremacy or would they continue to defer? If deference persists despite removal of structural barriers, suppression is internalized. If override attempts increase, suppression was structural and internalized.',
    'If suppression is primarily structural: the measured 0.72 represents barrier-maintained subordination; if barriers erode, the constraint''s stability erodes. If suppression is internalized: majorities have accepted judicial authority as legitimate; removing structural barriers might not destabilize the constraint because of internalized acceptance. This distinction affects the omega on suppression_mechanism from interpersonal_constraint_guidance—the legislatures'' and majorities'' suppression may carry internalized components (they have been socialized into accepting judicial review as legitimate) in addition to structural barriers (amendment is hard).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the suppression maintaining judicial supremacy is structural (external barriers) or internalized (learned deference to judicial authority).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cons_tr_t5, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(cons_tr_t15, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cons_tr_t25, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cons_be_t5, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cons_be_t15, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(cons_be_t25, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cons_su_t5, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(cons_su_t15, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(cons_su_t25, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__judicial_supremacy_reading, 0.2).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% The constitutional_interpretive_authority kernel has three structurally distinct readings: judicial supremacy (this constraint), parliamentary supremacy, and coordinate construction. Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and different measured extraction. The three constraints form a family linked by network.affects_constraints because they are competing readings of the same constitutional kernel. The judicial-supremacy reading is upstream in the causal structure (it was the first reading to dominate; the parliamentary and coordinate readings define themselves partly as alternatives to judicial supremacy). Each reading should be authored independently with its own metrics and stakeholders; the readings are not viewpoints on one constraint but structurally distinct constraints that share a contested origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
