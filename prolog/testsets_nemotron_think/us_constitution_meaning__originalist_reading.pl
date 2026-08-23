% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Constraint on Constitutional Meaning
 *   domain: legal/political
 *
 * SUMMARY:
 *   This constraint story models the originalist reading of the U.S.
 *   Constitution as a structural constraint on judicial interpretation. The
 *   reading asserts that constitutional meaning was fixed at each provision's
 *   ratification moment (1788, 1791, 1868, etc.) and that judges are bound by
 *   the historical public meaning of the text. The constraint operates
 *   through judicial appointments, precedent, and methodological enforcement
 *   within the legal profession. It coordinates by providing stable meaning
 *   but extracts by foreclosing rights claims that lack historical pedigree —
 *   disproportionately burdening groups excluded from the founding-era
 *   political process. The claimed_type (tangled_rope) reflects the
 *   analytical judgment that the constraint has both genuine coordination
 *   function (stability, democratic legitimacy) and asymmetric extraction
 *   (suppression of living constitutionalist outcomes, disadvantage to
 *   marginalized claimants). The originalist movement's own framing claims
 *   mountain (fixed meaning as natural law); the metrics describe the
 *   constraint's actual operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.72).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.85).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Constraint on Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '15768b77-3a4f-49f9-8f43-c214e78f847c').
narrative_ontology:cs_kernel_codification('15768b77-3a4f-49f9-8f43-c214e78f847c', fixed_text).
narrative_ontology:cs_authority_grounding('15768b77-3a4f-49f9-8f43-c214e78f847c', lineage).
narrative_ontology:cs_interpretation_layer_present('15768b77-3a4f-49f9-8f43-c214e78f847c').
narrative_ontology:cs_reading_relation('15768b77-3a4f-49f9-8f43-c214e78f847c', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('15768b77-3a4f-49f9-8f43-c214e78f847c', us_constitution_meaning__positivist_reading, influences).
narrative_ontology:cs_axiom('15768b77-3a4f-49f9-8f43-c214e78f847c', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('15768b77-3a4f-49f9-8f43-c214e78f847c', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('15768b77-3a4f-49f9-8f43-c214e78f847c', foundational, judges_bound_by_original_public_meaning).
narrative_ontology:cs_axiom_status(judges_bound_by_original_public_meaning, holdable).
narrative_ontology:cs_axiom_grounding('15768b77-3a4f-49f9-8f43-c214e78f847c', judges_bound_by_original_public_meaning, deontological).
narrative_ontology:cs_axiom('15768b77-3a4f-49f9-8f43-c214e78f847c', secondary, democratic_legitimacy_requires_fixed_meaning).
narrative_ontology:cs_axiom_status(democratic_legitimacy_requires_fixed_meaning, holdable).
narrative_ontology:cs_axiom_grounding('15768b77-3a4f-49f9-8f43-c214e78f847c', democratic_legitimacy_requires_fixed_meaning, instrumental).
narrative_ontology:cs_reference_frame('15768b77-3a4f-49f9-8f43-c214e78f847c', founding_era_public_meaning).
narrative_ontology:cs_drift_state('15768b77-3a4f-49f9-8f43-c214e78f847c', contemporary_originalist_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('15768b77-3a4f-49f9-8f43-c214e78f847c', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_judges).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, legislative_majorities).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_support).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, marginalized_groups_seeking_new_rights).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, living_constitutionalist_judges).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, progressive_legal_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, legal_academy_originalists).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, judicial_restraint_principle).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, democratic_legitimacy_through_fixed_meaning).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, constitutional_fixation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Appointed through a pipeline (Federalist Society, conservative legal movement) that selects for originalist commitment. They author opinions binding lower courts to historical meaning analysis. Their professional identity and career advancement are fused with the methodology; exit would require repudiating their own jurisprudence and the coalition that elevated them.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, identity_locked, national).

% Activists, scholars, and politicians who view judicial restraint as essential to democratic legitimacy. They gain a structural constraint on courts that might otherwise invalidate legislative majorities' policy choices. Their investment is ideological and strategic; they can shift to other constraint theories if originalism falters.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    organized, generational, mobile, national).

% The institutional infrastructure (Federalist Society, law school networks, judicial nomination apparatus) that produces and sustains originalist judges. The movement's coherence, fundraising, and political influence depend on originalism as its animating methodology. Abandoning it would dissolve the coalition's intellectual identity.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, conservative_legal_movement, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, conservative_legal_movement, agenda_setter).

% Elected majorities whose policy enactments are insulated from judicial invalidation when courts adhere to original meaning. They benefit from the constraint without administering it. Their exit is trivial — they simply legislate; the constraint operates on the judiciary, not on them directly.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, legislative_majorities, beneficiary,
    powerful, biographical, arbitrage, national).

% Litigants asserting constitutional rights (e.g., abortion access, LGBTQ+ equality, voting rights expansions) that lack clear 1788 or 1868 historical pedigree. Their claims are structurally foreclosed by the constraint's methodology. Exit means abandoning constitutional litigation for legislative or state-level strategies, which are often unavailable or insufficient.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_support, payer,
    moderate, biographical, constrained, national).

% Communities historically excluded from the ratification-era political process (enslaved persons, women, non-property-holders, indigenous nations) whose contemporary rights claims depend on evolved constitutional understandings. The constraint's fixation on founding-era meaning structurally disadvantages them; they cannot access the historical record as ratifiers and face near-total exclusion from the originalist interpretive community.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, marginalized_groups_seeking_new_rights, payer,
    powerless, generational, trapped, national).

% Judges who interpret the Constitution as evolving with social circumstances. Under originalist precedent (e.g., Dobbs, Bruen), their methodological discretion is suppressed; they must either adopt originalist analysis in opinions or write dissents that carry no binding force. Exit means leaving the bench or conforming to the dominant methodology.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_judges, payer,
    institutional, biographical, constrained, national).

% Academics and advocates whose work is marginalized in originalist-dominated courts and law journals. They bear the cost of having their interpretive framework treated as illegitimate. Unlike marginalized groups, they retain professional exit options (scholarship, teaching, non-judicial advocacy) but lose influence over binding law.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, progressive_legal_scholars, payer,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, progressive_legal_scholars, excluded).

% Law professors and scholars whose careers, citations, and institutional positions are built on originalist methodology. They benefit from the constraint's dominance in elite legal institutions. Their professional identity is fused with the methodology; methodological pluralism threatens their field's coherence.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, legal_academy_originalists, beneficiary,
    organized, generational, identity_locked, national).

% Historians, political scientists, and legal theorists who study the constraint from outside the advocacy coalition. They neither collect rents nor bear extraction; they observe the constraint's operation, its historical accuracy, and its distributive effects across the other seats.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, determinate, and democratically pedigreed constitutional meaning that binds judges across time, preventing judicial policy-making and enabling citizens and legislatures to rely on fixed constitutional rules.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary judges to historical ratifiers (1788 for original Constitution, 1791 for Bill of Rights, 1868 for Reconstruction Amendments); moves policy discretion from courts to legislative majorities; transfers the cost of constitutional ambiguity from the political branches to rights claimants whose claims lack historical support.
% ABSENT_VOICES: Future generations whose circumstances the Framers could not anticipate; the enslaved, women, indigenous nations, and non-property-holders excluded from the 1788/1791/1868 ratification processes; non-originalist methodological traditions (common-law constitutionalism, structural inference, ethical reading) that are treated as illegitimate rather than contested.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight, the Supreme Court would revert to living constitutionalist or pluralist methodology within one appointments cycle. Rights claims currently barred by lack of historical support (abortion, LGBTQ+ equality, affirmative action, voting rights expansions) would become viable. Legislative majorities would face renewed judicial review of economic and social regulation. The Federalist Society's judicial pipeline would lose its unifying methodology.
% FOUNDING_PROBLEM: The perceived problem of unconstrained judicial power under the Warren and Burger Courts (1953-1986), where the Supreme Court was seen as imposing policy preferences (school desegregation remedies, abortion rights, criminal procedure expansions, voting rights enforcement) without democratic pedigree or textual authorization.
% FOUNDING_PROBLEM_CORROBORATION: Originalists (Bork, Scalia, Federalist Society founders) attest the founding problem persists: courts still make policy. Critics (living constitutionalist scholars: Tribe, Balkin, Strauss; historians: Rakove, Wood) attest the founding problem is substantially solved or mischaracterized: originalism itself produces activist outcomes (e.g., striking down campaign finance regulation, Voting Rights Act provisions, affirmative action) and the Warren Court's interventions responded to democratic failures originalism ignores.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers substantial interpretive authority and policy outcomes from contemporary actors to historical ones, with the cost falling on rights claimants. Suppression (0.85) is very high because the constraint's persistence depends on active enforcement: judicial selection pipelines, precedent stare decisis pressure, law school hiring, Federalist Society gatekeeping, and methodological policing in opinions (e.g., Bruen's history-and-tradition test). Theater ratio (0.38) is moderate — originalist historical analysis is often genuine scholarly work, but a growing share serves to rationalize predetermined outcomes (e.g., selective history in Dobbs, Bruen). Accessibility collapse (0.78) is high because once originalism is accepted as the only legitimate methodology, alternatives (living constitutionalism, common-law constitutionalism) are treated as lawless rather than contestable. Resistance (0.58) is significant: living constitutionalist judges, progressive scholars, and rights movements actively contest the constraint, but their resistance is channeled into dissent and scholarship rather than institutional power.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist judge/advocate seat, the constraint appears as a rope or mountain: it solves the genuine coordination problem of judicial lawlessness and its meaning is 'fixed' by history. From the rights claimant/marginalized group seat, it appears as a snare: the coordination story is cover for entrenching founding-era power structures. From the living constitutionalist judge seat, it appears as a tangled rope: there is a real coordination function (stability) but the extraction is asymmetric and the enforcement is coercive. The engine computes this divergence from the structural data — the authored claim (tangled_rope) is the analytical synthesis, not any seat's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges (institutional, identity_locked) sit near the beneficiary end of directionality (d ~ 0.2): they administer the constraint and their careers depend on it, but they are also constrained by it — they cannot freely adopt preferred outcomes. Counter-majoritarian advocates and legislative majorities (organized/powerful, mobile/arbitrage) are clear beneficiaries (d ~ 0.1): they gain constraint on courts without bearing its methodological costs. Conservative legal movement (institutional, identity_locked) is a hybrid beneficiary/agenda_setter (d ~ 0.15): it built the enforcement infrastructure and its identity is fused to the methodology. Rights claimants without historical support (moderate, constrained) and marginalized groups (powerless, trapped) are full targets (d ~ 0.9-1.0): they bear the extraction with minimal exit. Living constitutionalist judges (institutional, constrained) are targets (d ~ 0.7): they retain institutional position but lose methodological autonomy. Progressive scholars (organized, mobile) are payers with exit (d ~ 0.5): they bear professional marginalization but retain non-judicial platforms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Warren/Burger Court judicial activism) is contested: originalists say it persists; critics say originalism has become its own form of activism (striking down democratic enactments on originalist grounds). If the founding problem is dead (courts no longer make 'activist' policy), the constraint persists as mandatrophy — a solution whose problem has vanished. If contested, the constraint is a live tangled rope. The coronation of originalism in Dobbs/Bruen suggests the constraint has metastasized beyond its founding justification into a self-sustaining institutional order — the conservative legal movement now extracts benefit from the constraint itself, not merely from the problem it was built to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_natural_vs_constructed,
    'Is the originalist constraint a discovery of the Constitution''s natural fixed meaning (mountain) or a constructed interpretive methodology that serves identifiable beneficiaries (tangled_rope/snare)?',
    'Historical investigation: did the Framers/ratifiers understand themselves as fixing meaning for all time, or did they expect evolutionary application? Comparative analysis: do other fixed-text constitutional systems (e.g., civilian codes) exhibit similar originalist/exclusionary dynamics?',
    'If natural law (mountain), the constraint''s extraction is zero and suppression is justified by the text''s own nature. If constructed (tangled_rope/snare), the beneficiary/victim structure is analytically central and the constraint''s legitimacy depends on its coordination function outweighing its extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalism_natural_vs_constructed, conceptual, 'Whether originalism discovers or constructs constitutional fixity.').

omega_variable(
    historical_accuracy_of_originalist_method,
    'Does originalist methodology reliably recover a single determinate historical public meaning, or does it selectively deploy contested historical evidence to reach predetermined outcomes?',
    'Empirical study of originalist opinions: inter-coder reliability of historical analysis; correlation between judges'' policy preferences and originalist conclusions; comparison with professional historian consensus on same questions.',
    'If originalism reliably converges on determinate meaning, its coordination function is genuine and extraction is lower. If it is indeterminate and outcome-driven, the coordination story is cover and the constraint is snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_accuracy_of_originalist_method, empirical, 'Epistemic reliability of originalist historical analysis.').

omega_variable(
    suppression_mechanism_originalism,
    'Is the suppression of non-originalist outcomes structural (appointments, precedent, institutional gatekeeping) or internalized (legal profession''s self-policing, law students adopting originalism to get clerkships)?',
    'Track suppression trajectory after political shifts: if a non-originalist president appoints non-originalist judges, does suppression decrease (structural) or persist through professional norms (internalized)? Survey law students/clerkship applicants on methodological conformity pressures.',
    'If internalized, the constraint''s effective suppression exceeds its institutional enforcement — the legal profession carries the suppression internally. This would increase measured suppression and support snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_originalism, empirical, 'Structural vs. internalized suppression in originalist dominance.').

omega_variable(
    originalism_living_constitutionalism_foreclosure,
    'Does originalism logically foreclose living constitutionalism within a single interpretive framework, or do they coexist as competing frameworks held by different judicial coalitions?',
    'Analyze whether any sitting justice or coherent theory integrates both: e.g., ''original meaning for original provisions, evolving standards for open-textured clauses.'' If such hybrid positions are logically stable, the readings coexist; if originalism''s fixation thesis directly contradicts living constitutionalism''s evolution thesis, they foreclose.',
    'If forecloses, the kernel has a genuine logical fracture. If coexists_with, the kernel''s contestation is political/institutional, not logical — different coalitions hold different readings without contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_living_constitutionalism_foreclosure, conceptual, 'Logical relationship between originalist and living constitutionalist readings.').

omega_variable(
    originalism_positivism_relation,
    'Does originalism foreclose positivist reading (validity from enactment procedures) or influence it (originalism as one enactment-grounded methodology among others)?',
    'Examine whether originalists and positivists (e.g., Baude, Sachs) converge on ''law of interpretation'' approach: originalism as the positivistically identified interpretive rule. If yes, influences; if originalism claims moral authority beyond enactment, forecloses.',
    'Clarifies the kernel''s internal structure: a three-way foreclosure (originalism vs living) vs. a two-way coexistence with a third influencer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_positivism_relation, conceptual, 'Structural relation between originalist and positivist readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_tr_t1980, us_constitution_meaning__originalist_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_tr_t1988, us_constitution_meaning__originalist_reading, theater_ratio, 1988, 0.18).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_tr_t1996, us_constitution_meaning__originalist_reading, theater_ratio, 1996, 0.22).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_tr_t2005, us_constitution_meaning__originalist_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_tr_t2010, us_constitution_meaning__originalist_reading, theater_ratio, 2010, 0.31).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_tr_t2016, us_constitution_meaning__originalist_reading, theater_ratio, 2016, 0.34).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_tr_t2020, us_constitution_meaning__originalist_reading, theater_ratio, 2020, 0.36).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_tr_t2024, us_constitution_meaning__originalist_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_be_t1980, us_constitution_meaning__originalist_reading, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_be_t1988, us_constitution_meaning__originalist_reading, base_extractiveness, 1988, 0.32).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_be_t1996, us_constitution_meaning__originalist_reading, base_extractiveness, 1996, 0.41).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_be_t2005, us_constitution_meaning__originalist_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_be_t2010, us_constitution_meaning__originalist_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_be_t2016, us_constitution_meaning__originalist_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_be_t2020, us_constitution_meaning__originalist_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_be_t2024, us_constitution_meaning__originalist_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_su_t1980, us_constitution_meaning__originalist_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_su_t1988, us_constitution_meaning__originalist_reading, suppression_requirement, 1988, 0.45).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_su_t1996, us_constitution_meaning__originalist_reading, suppression_requirement, 1996, 0.55).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_su_t2005, us_constitution_meaning__originalist_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_su_t2010, us_constitution_meaning__originalist_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_su_t2016, us_constitution_meaning__originalist_reading, suppression_requirement, 2016, 0.78).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_su_t2020, us_constitution_meaning__originalist_reading, suppression_requirement, 2020, 0.82).
narrative_ontology:measurement(us_constitution_meaning__originalist_reading_su_t2024, us_constitution_meaning__originalist_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, judicial_review_constraint).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, stare_decisis_constraint).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, federalist_society_pipeline_constraint).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, bruen_history_tradition_test).

% DUAL FORMULATION NOTE:
% Part of us_constitution_meaning kernel family. This reading (originalist) claims fixation at ratification; living_constitutionalist_reading claims evolving application; positivist_reading claims validity from enactment procedures. The three stories share a kernel but instantiate different constraints with different ε, beneficiaries, and victims. Originalism's ε is high (extraction from rights claimants); living constitutionalism's ε is lower but its suppression operates differently; positivism's ε depends on whether enactment procedures are themselves contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__originalist_reading, institutional, 0.25).
constraint_indexing:directionality_override(us_constitution_meaning__originalist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
