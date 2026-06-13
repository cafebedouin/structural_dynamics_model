% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_meaning__living_constitutionalist_reading
 *   human_readable: Constitutional Meaning as Living, Evolving Instrument (Living Constitutionalist Reading)
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   The living constitutionalist reading asserts that the Constitution's
 *   enduring principles—liberty, equal protection, due process—apply to new
 *   contexts and social circumstances not contemplated at ratification.
 *   Constitutional meaning evolves through judicial interpretation informed
 *   by contemporary moral understanding, changing social conditions, and
 *   accumulated constitutional experience. This is ONE READING of the
 *   contested kernel 'US Constitution meaning' (sibling readings:
 *   originalism, positivism). The living reading benefits rights claimants in
 *   evolving contexts by allowing them to invoke constitutional protection
 *   for claims that postdate the text's enumeration. It burdens institutional
 *   actors and stability defenders who prefer settled constitutional
 *   meanings. The constraint is Tangled Rope because it coordinates
 *   constitutional adaptation (solving stagnation) while extracting
 *   interpretive authority from originalists, conservatives, and
 *   institutional actors who prefer frozen meaning or amendment-based change.
 *
 * KEY AGENTS:
 *   - Rights claimants in evolving contexts (beneficiary) — women, minorities, LGBTQ+ individuals, privacy seekers, modern marginalized groups seeking constitutional protection for historically unrecognized rights
 *   - Progressive justices and legal scholars (agenda_setter) — set the frame for legitimate constitutional argument; articulate the reading through opinions and pedagogy
 *   - Institutional stability defenders (payer) — legislatures, executives, institutions whose policies face invalidation on evolved constitutional grounds
 *   - Conservative originalists (excluded) — institutionally constrained from setting the primary interpretive frame; argue the living approach dissolves rule of law
 *   - Legislative majorities (payer + beneficiary) — bear cost of reopened questions but can attempt judicial appointments and amendment
 *   - Constitutional observers (observer) — scholars and analysts studying whether the reading is principle-driven or discretionary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_meaning__living_constitutionalist_reading, 0.42).
domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__living_constitutionalist_reading, "Constitutional Meaning as Living, Evolving Instrument (Living Constitutionalist Reading)").
narrative_ontology:topic_domain(us_constitution_meaning__living_constitutionalist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__living_constitutionalist_reading, 'bcc2d343-4a8b-4bd3-85ab-b1abc15aae64').
narrative_ontology:cs_kernel_codification('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', fixed_text).
narrative_ontology:cs_authority_grounding('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', lineage).
narrative_ontology:cs_interpretation_layer_present('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64').
narrative_ontology:cs_reading_relation('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', us_constitution_meaning__positivist_reading, influences).
narrative_ontology:cs_axiom('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', foundational, enduring_principles_transcend_ratification_moment).
narrative_ontology:cs_axiom_status(enduring_principles_transcend_ratification_moment, holdable).
narrative_ontology:cs_axiom_grounding('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', enduring_principles_transcend_ratification_moment, deontological).
narrative_ontology:cs_axiom('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', foundational, contemporary_moral_consensus_illuminates_constitutional_meaning).
narrative_ontology:cs_axiom_status(contemporary_moral_consensus_illuminates_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', contemporary_moral_consensus_illuminates_constitutional_meaning, empirically_contingent).
narrative_ontology:cs_axiom('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', secondary, judicial_interpretation_legitimate_constitutional_authority).
narrative_ontology:cs_axiom_status(judicial_interpretation_legitimate_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', judicial_interpretation_legitimate_constitutional_authority, conventional).
narrative_ontology:cs_reference_frame('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', enduring_principles_adaptive_application).
narrative_ontology:cs_drift_state('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', contemporary_crisis_point, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('bcc2d343-4a8b-4bd3-85ab-b1abc15aae64', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, marginalized_and_historically_excluded_groups).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, institutional_stability_defenders).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, counter_majoritarian_constraint_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, society_broadly).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, legislative_majorities).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, society_broadly).
narrative_ontology:constraint_vindicates(us_constitution_meaning__living_constitutionalist_reading, moral_progress_is_constitutionally_recognizable).
narrative_ontology:constraint_vindicates(us_constitution_meaning__living_constitutionalist_reading, judicial_adaptation_serves_enduring_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and groups—women, minorities, LGBTQ+ individuals, privacy seekers—who gain constitutional standing and recognition under the living constitutionalist reading. They benefit from judicial willingness to apply enduring constitutional principles (liberty, equal protection) to their historically excluded or newly emerged situations. They cannot exit the constitutional framework; their alternative is to seek statutory or state constitutional protection, which is less stable and authoritative.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_contexts, beneficiary,
    organized, biographical, constrained, national).

% Judges and constitutional theorists who articulate and enforce the living constitutionalist reading through Supreme Court and appellate opinions, law review articles, and law school pedagogy. They set the frame for what counts as a legitimate constitutional argument, determine which rights claims are justiciable, and interpret the Constitution's meaning in light of contemporary moral understanding. They have mobile exit because they can shift to different interpretive methodologies (e.g., originalism) or different institutional roles, though doing so would require abandoning the living framework.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, progressive_justices_and_legal_scholars, agenda_setter,
    institutional, generational, mobile, national).

% Government institutions, legislatures, and executives whose policies and actions become subject to judicial reinterpretation as constitutional meaning evolves. They bear the cost of having settled questions reopened, precedents overturned, and policies invalidated on evolved constitutional grounds that did not exist when those policies were enacted. They cannot exit the constitutional framework; they can only attempt to influence judicial appointments or seek constitutional amendment.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, institutional_stability_defenders, payer,
    institutional, generational, constrained, national).

% Judges, scholars, and justices who advance originalist, textualist, or historicist readings of the Constitution. They argue that the living constitutionalist approach dissolves the rule of law, transforms the Constitution into an instrument of judicial will, and violates the separation of powers by enabling judges to usurp legislative functions. They are excluded from setting the primary interpretive frame in this reading, though they remain part of the legal conversation and can file dissenting opinions. Their exit is constrained by the judicial system's hierarchical structure and by the dominance of the living reading in the moment (though they retain hope through future appointments).
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, conservative_originalists, excluded,
    institutional, generational, constrained, national).

% Elected representatives and majorities whose ability to set policy is constrained by judicial reinterpretation of constitutional limits. They bear the cost when courts invalidate legislation on evolved constitutional grounds. However, they also benefit when judicial expansion of constitutional rights protects their constituents (e.g., voting rights expansion, privacy protection). They cannot exit the constitutional framework but can attempt to appoint sympathetic justices, seek constitutional amendment, or adapt their legislation to new judicial interpretations.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, legislative_majorities, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__living_constitutionalist_reading, legislative_majorities, beneficiary).

% The polity and citizenry as a whole, which benefits from constitutional adaptation to new social understanding (recognizing previously invisible injustices, preventing stagnation, enabling rights expansion for marginalized groups) but also bears the cost of judicial unpredictability, the risk of judges imposing majority contemporary values under constitutional guise, and the erosion of rule-of-law predictability. They are trapped by the fact that they cannot opt out of being governed by constitutional interpretation; they can only participate through democratic processes, voting, and (eventually) constitutional amendment.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, society_broadly, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__living_constitutionalist_reading, society_broadly, payer).

% Academics, historians, political scientists, and jurists who study constitutional doctrine and practice from an external analytical position. They observe whether the living constitutionalist reading is applied consistently across cases and time, whether it genuinely tracks enduring principles or serves as cover for judicial discretion, and whether its operation produces stable or unstable constitutional law. They take testimony and conduct comparative analysis on how the constraint actually operates.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, constitutional_scholars_and_observers, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the constitutional stagnation problem: how to maintain stable fundamental law while permitting application to radically changed social, technological, and moral contexts not contemplated at ratification. The living constitutionalist reading allows the Constitution to evolve through judicial interpretation rather than requiring constant amendment or wholesale replacement of the constitutional instrument.
% TRANSFER_FUNCTION: Transfers interpretive authority from the historical ratification moment and original public meaning to contemporary judicial consensus about evolving moral understanding, contemporary circumstances, and the Constitution's enduring principles. Contemporary rights claimants receive constitutional standing and recognition; progressive justices gain the authority to declare what the Constitution demands in light of contemporary values; institutional actors and conservatives bear the cost of having settled questions reopened.
% ABSENT_VOICES: Originalists and conservative jurisprudents who dispute the living constitutionalist reading would object strenuously. Institutional actors and policymakers who prefer stable constitutional meaning are not excluded from the conversation but are systematically disadvantaged by the constraint's operation. Future generations whose interests might be harmed by today's expansion of rights cannot speak in the present moment. Originalists retain formal voice (dissenting opinions) but are excluded from setting the interpretive frame when progressive justices dominate the Court.
% DISAPPEARANCE_RATIONALE: If the living constitutionalist reading vanished overnight and originalism or pure positivism took over, constitutional recognition of rights claims beyond those explicitly enumerated at ratification would largely evaporate. Voting rights for women and minorities (recognized through Fourteenth Amendment reinterpretation), privacy rights (recognized through substantive due process), equal protection in evolving contexts, and many other rights recognized under the living reading would lose constitutional standing. The judiciary would revert to a narrower role. This would trigger massive political conflict as groups sought statutory or state constitutional protection for rights they had come to regard as constitutionally guaranteed, and as marginalized groups lost hard-won constitutional recognition. Institutional actors might welcome the predictability but would face political backlash from constituencies losing constitutional protection.
% FOUNDING_PROBLEM: The Constitution was drafted in 1787 and addresses many contemporary questions only obliquely if at all. Slavery (not formally addressed until 1865), women's suffrage (1920), privacy, digital surveillance, and most modern social questions were not contemplated. If constitutional meaning were frozen at ratification, the document would become increasingly obsolete and disconnected from contemporary life, requiring constant amendment for basic adaptation or would eventually be abandoned for new constitutional instruments. The founding problem is maintaining a stable constitutional framework while permitting meaningful adaptation to radically different social and technological circumstances.
% FOUNDING_PROBLEM_CORROBORATION: Progressive justices and constitutional scholars attest the founding problem remains live: the Court regularly encounters questions the Framers could not have imagined (privacy in digital contexts, voting access in modern elections, equal protection in evolving social contexts) and cannot resolve them by appeal to historical ratifier intent alone. Conservative originalists and institutional actors dispute this characterization: they argue that the founding problem is better solved through amendment, statutory law, or accepting that some matters are not constitutionally regulated, rather than through judicial reinterpretation. Independent constitutional historians (Akhil Amar, Jack Balkin, Randy Barnett) recognize the stagnation problem but dispute whether the living constitutionalist solution genuinely solves it consistently or merely substitutes judge-made law for constitutional constraint. Legislative testimony and political history show that groups denied constitutional recognition (women, minorities, LGBTQ+ individuals) have persistently demanded constitutional status and have turned to courts when legislatures failed, which supports the claim that the underlying problem is real, even if the living reading's solution is contested.
narrative_ontology:disappearance_verdict(us_constitution_meaning__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__living_constitutionalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__living_constitutionalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the reading genuinely solves a coordination problem (constitutional stagnation) while simultaneously enabling judges to impose contemporary will under the guise of evolving principles. The trajectory shows rising extractiveness from 0.35 to peak 0.56 (t=40), then declining slightly as originalist resistance hardens and the Supreme Court shifts rightward (t=80-100). This suggests the constraint's extractiveness depended on relatively stable progressive dominance of the judiciary; as that dominance eroded, the extraction moderated because the reading no longer commanded universal acceptance. Suppression is lower (0.42) than extraction because the living reading explicitly tolerates dissent (originalist opinions are published, the intellectual case is engaged) rather than suppressing originalist voices through law. Theater is moderate (0.28): the constraint includes genuine principle-tracking (civil rights expansion tracking evolving moral consensus) but also performative justification (invoking 'contemporary understanding' to justify outcomes that might not follow from enduring principles themselves). The shared measurement grid tracks all three metrics at every time point, enabling temporal analysis of how the constraint's character changed as institutional power shifted.
 *
 * PERSPECTIVAL GAP:
 *   The perspective divide runs between agenda-setters (judges/scholars who benefit from interpretive authority) and payers (institutions bearing the cost of reopened questions). From the justice's seat, the constraint is genuine coordination solving stagnation while respecting enduring principles—a Rope. From the legislature's seat, the constraint is extractive overreach imposing contemporary will on settled questions—a Snare. From the rights claimant's seat, it is genuine coordination enabling access to constitutional protection that was always promised but denied—a Rope with delayed justice. From the originalist's seat, it is a false legitimacy structure masking judicial lawmaking—a Snare. The engine should compute these divergent seat-level types from the structural data (power, exit, beneficiary/victim position), producing a per-seat classification that captures the perspectival gap without reconciling it.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive justices (institutional power, mobile exit) derive d near 0.2-0.3 (beneficiary): they gain interpretive authority and shape constitutional development. Rights claimants (organized power, constrained exit) derive d near 0.3-0.4 (net beneficiary but trapped): they gain standing for claims but cannot exit the constitutional framework. Institutional stability defenders (institutional power, constrained exit) derive d near 0.7-0.8 (target): they bear the cost of reopened questions and policy invalidation, and cannot opt out of constitutional constraint. Conservative originalists (institutional power, constrained exit) derive d near 0.75 (target-ish): excluded from setting the frame, constrained by the reading's dominance, but retain dissenting voice and hope through appointments. No directionality overrides needed: the structural data (beneficiary/victim declarations + power + exit) produce accurate d values through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The living constitutionalist reading's founding problem is constitutional stagnation: how to apply 1787 principles to 2024 realities without constant amendment or loss of constitutional stability. The reading claims this problem is LIVE and ongoing: contemporary issues (privacy, gender, voting access in digital contexts) cannot be solved by frozen originalist meaning. However, originalists dispute this: they argue amendment is the legitimate path and that many 'new rights' are judicial inventions not tracking genuine constitutional principles. The constraint exhibits mandatrophy risk if: (1) the living reading becomes so expansive that it loses principled boundaries and becomes pure judicial discretion, or (2) conservative justices successfully restrict the reading's scope, making it a zombie constraint (Piton) that persists as institutional habit rather than substantive operation. The measurement series shows the constraint's extractiveness peaked at t=40 (0.56) then declined as originalist power grew, suggesting the reading's vitality depends on continued progressive judicial dominance. If originalism achieves durable dominance (e.g., through appointments), the constraint might become Piton: formally acknowledged but operationally sidelined by justices preferring originalist methodology. The theater ratio's stability at 0.28 suggests performative justification persists even as extractiveness declined, consistent with Piton-phase inertia (the language of 'evolving principles' persists even when justices are constraining its operation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_discretion_vs_principled_constraint,
    'Does the living constitutionalist reading genuinely constrain judges through enduring principles, or is it a cover for judges imposing contemporary moral preferences without meaningful constitutional constraint?',
    'Empirical analysis: examine decisions across ideologically diverse judicial panels and across time. If the reading produces consistent decisions (similar outcomes despite different judges and eras), it tracks principles. If decisions flip with judicial composition and contemporary fashion, the reading is discretionary. Comparative study of constitutional courts in other democracies adopting similar living-constitution approaches.',
    'If genuinely principle-constrained, the reading is legitimate coordination adapted to new contexts—a Rope or Tangled Rope with real content. If discretionary, the constraint becomes a Snare using constitutional language as cover for judicial power-making, with no meaningful constraint on judges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_vs_principled_constraint, empirical, 'Whether living constitutionalism constrains judges through enduring principles or empowers discretionary lawmaking.').

omega_variable(
    kernel_reading_contest_irreducibility,
    'Are the three readings of the Constitution (living constitutionalist, originalist, positivist) competing interpretations of the same kernel, or is one reading correct and the others incorrect?',
    'This is a conceptual question at the kernel level. The three readings represent fundamentally different theories of constitutional authority: what determines the Constitution''s meaning, what constrains interpretation, and what counts as legitimate constitutional argument. No single framework—empirical evidence, historical scholarship, or logical analysis—can adjudicate among them without already taking a position on what legitimates constitutional interpretation. The question is irreducible.',
    'If the readings are genuinely competing frameworks (kernel-level contest), they should coexist as live positions, each with institutional support and adherents. If one reading is objectively correct, it should foreclose the others. The engine computes whether the readings foreclose or coexist based on the axioms and reading_relations declared in cs_structure. This omega documents the irreducible contest itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_irreducibility, conceptual, 'The kernel contest: whether the three constitutional readings are competing frameworks or whether one is objectively correct.').

omega_variable(
    suppression_of_originalism_structural_or_internalized,
    'Is the suppression of originalist voices and interpretive alternatives structural (institutional gatekeeping by progressive judges, excluding originalists from law schools and courts) or internalized (originalists have genuinely weaker arguments and accept progressive dominance)?',
    'Institutional analysis: measure representation of originalist scholars in law schools, frequency of originalist opinions in appellate courts, citational networks in constitutional scholarship. If suppression is structural, originalists will show high resistance and clear institutional barriers despite intellectual vigor. If internalized, originalists will retreat to particular domains (statutory interpretation) where their approach is more accepted and will show lower resistance to progressive dominance.',
    'If structural suppression, the living constitutionalist constraint actively suppresses competing readings through gatekeeping in institutions. If internalized, originalists have accepted the court''s and academy''s authority to privilege progressive constitutional interpretation. The distinction affects whether the constraint''s suppression score (0.42) is legitimate (deferring to stronger arguments) or coercive (excluding competing frameworks).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_originalism_structural_or_internalized, empirical, 'Whether suppression of originalism is structural gatekeeping or internalized deference.').

omega_variable(
    moral_progress_presumption_validity,
    'Is contemporary moral consensus a reliable guide to constitutional principles, or does it embed the time-bound biases of the present moment that future generations will reject or deeply revise?',
    'Historical analysis: examine constitutional claims based on contemporary moral consensus from 50, 100, 150 years ago (e.g., eugenics, forced sterilization of the disabled, coverture for married women). Determine whether they have been affirmed or overturned by later constitutional interpretation and moral development. Empirical test: does the constraint''s operation—interpreting the Constitution through contemporary moral understanding—produce outcomes that survive generational transitions and later constitutional development, or are they overturned as moral understanding shifts?',
    'If contemporary moral consensus reliably tracks enduring constitutional principles, the reading is legitimate coordination. If moral consensus frequently embeds present bias that future generations reject, the reading enables present majorities to impose their contested values as eternal constitutional law—transforming the constraint into a Snare using moral-progress language as cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_progress_presumption_validity, empirical, 'Whether contemporary moral consensus is a reliable guide to enduring constitutional principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__living_constitutionalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement_basis(us_c_tr_t60, observed).
narrative_ontology:measurement(us_c_tr_t80, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement_basis(us_c_tr_t80, observed).
narrative_ontology:measurement(us_c_tr_t100, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(us_c_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t60, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement_basis(us_c_be_t60, observed).
narrative_ontology:measurement(us_c_be_t80, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 80, 0.59).
narrative_ontology:measurement_basis(us_c_be_t80, observed).
narrative_ontology:measurement(us_c_be_t100, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(us_c_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(us_c_su_t40, observed).
narrative_ontology:measurement(us_c_su_t60, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement_basis(us_c_su_t60, observed).
narrative_ontology:measurement(us_c_su_t80, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 80, 0.42).
narrative_ontology:measurement_basis(us_c_su_t80, observed).
narrative_ontology:measurement(us_c_su_t100, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 100, 0.42).
narrative_ontology:measurement_basis(us_c_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__living_constitutionalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint readings of the kernel 'US Constitution meaning.' The sibling readings (originalist and positivist) are separate constraint stories with their own ε values, beneficiary/victim structures, and computed types. The three readings coexist as live positions in the legal system, each with different institutional bases and different implications for who benefits and who bears costs. The living constitutionalist reading creates downstream pressure on originalism by establishing a competing frame for legitimate constitutional argument; it influences positivism by privileging judicial interpretation of moral principles as the source of constitutional authority. The three readings are structurally distinct constraints (different ε-values, different victim/beneficiary sets), not measurements of the same constraint from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__living_constitutionalist_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
