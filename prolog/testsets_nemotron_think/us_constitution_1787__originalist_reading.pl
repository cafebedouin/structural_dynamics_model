% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: Originalist Reading: Constitutional Meaning Fixed at Ratification
 *   domain: constitutional_law
 *
 * SUMMARY:
 *   The originalist reading of the U.S. Constitution asserts that
 *   constitutional meaning was fixed at ratification (1787-1788) and that the
 *   framers' intent or original public meaning is binding on all later
 *   interpreters. This reading presents itself as a Mountain — a constraint
 *   emerging naturally from the constitutional text itself, requiring only
 *   faithful application. However, the constraint's operational history shows
 *   rising extractiveness: it excludes modern social rights claims
 *   (reproductive autonomy, LGBTQ+ rights, voting rights expansions) that
 *   lack 1787 analogues, benefits a specific judicial and ideological
 *   coalition (originalist judges, Federalist Society network, conservative
 *   legal movement), and requires active enforcement through judicial
 *   appointments, doctrinal gatekeeping, and law school pipeline control. The
 *   measured suppression (0.78) reflects the institutional machinery that
 *   excludes rival interpretations from authoritative venues. The
 *   theater_ratio (0.45) captures the genuine interpretive labor of
 *   originalist methodology alongside its performative function as a
 *   legitimating device for predetermined outcomes.
 *
 * KEY AGENTS:
 *   - originalist_judges: Primary agenda_setter (institutional/analytical) — sets interpretive methodology, controls doctrinal development
 *   - conservative_legal_movement: Primary beneficiary (organized/biographical) — gains policy victories through judicial doctrine without legislative majorities
 *   - federalist_society_network: Beneficiary/agenda_setter (organized/generational) — builds pipeline, coordinates appointments, defines orthodoxy
 *   - modern_rights_claimants: Primary payer (powerless/biographical) — constitutional claims foreclosed by fixed-meaning boundary
 *   - living_constitutionalist_scholars: Excluded (moderate/biographical) — would object but structurally excluded from authoritative interpretation
 *   - marginalized_groups_excluded_by_original_meaning: Payer (powerless/generational) — bear the costs of rights foreclosure
 *   - positivist_judges: Observer (institutional/biographical) — textualist but not originalist; distinct methodological seat
 *   - democratic_majorities: Excluded (organized/generational) — their policy preferences blocked by judicial originalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.72).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.78).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, mountain).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Originalist Reading: Constitutional Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).
domain_priors:emerges_naturally(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, 'f319ce22-b231-4185-a91f-c9da21e8db72').
narrative_ontology:cs_kernel_codification('f319ce22-b231-4185-a91f-c9da21e8db72', fixed_text).
narrative_ontology:cs_authority_grounding('f319ce22-b231-4185-a91f-c9da21e8db72', lineage).
narrative_ontology:cs_interpretation_layer_present('f319ce22-b231-4185-a91f-c9da21e8db72').
narrative_ontology:cs_reading_relation('f319ce22-b231-4185-a91f-c9da21e8db72', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('f319ce22-b231-4185-a91f-c9da21e8db72', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('f319ce22-b231-4185-a91f-c9da21e8db72', foundational, original_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(original_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('f319ce22-b231-4185-a91f-c9da21e8db72', original_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('f319ce22-b231-4185-a91f-c9da21e8db72', foundational, framers_intent_binding_on_later_generations).
narrative_ontology:cs_axiom_status(framers_intent_binding_on_later_generations, holdable).
narrative_ontology:cs_axiom_grounding('f319ce22-b231-4185-a91f-c9da21e8db72', framers_intent_binding_on_later_generations, conventional).
narrative_ontology:cs_axiom('f319ce22-b231-4185-a91f-c9da21e8db72', secondary, judicial_restraint_requires_historical_fixity).
narrative_ontology:cs_axiom_status(judicial_restraint_requires_historical_fixity, holdable).
narrative_ontology:cs_axiom_grounding('f319ce22-b231-4185-a91f-c9da21e8db72', judicial_restraint_requires_historical_fixity, instrumental).
narrative_ontology:cs_reference_frame('f319ce22-b231-4185-a91f-c9da21e8db72', original_understanding_1787).
narrative_ontology:cs_drift_state('f319ce22-b231-4185-a91f-c9da21e8db72', post_new_originalism_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f319ce22-b231-4185-a91f-c9da21e8db72', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_judges).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, federalist_society_network).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, modern_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, living_constitutionalist_scholars).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, marginalized_groups_excluded_by_original_meaning).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, original_meaning_fixed_at_ratification).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, judicial_restraint_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, democratic_legitimacy_of_enacted_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the authoritative interpretation of the Constitution through judicial opinions. They set the methodological rules (original public meaning, original intent, original methods) and apply them to invalidate or uphold legislation. Their exit is analytical: they could adopt a different methodology but the institutional role and professional identity fuse with originalism.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, analytical, national).

% Gains policy victories (on abortion, gun rights, administrative state, religious liberty) through judicial doctrine that would not pass legislative majorities. They invest in the pipeline (law schools, clerkships, nominations) and can shift resources to other legal strategies if originalism becomes disadvantageous — arbitrage-grade exit.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, conservative_legal_movement, beneficiary,
    organized, generational, arbitrage, national).

% Builds and maintains the personnel pipeline for originalist judges, defines methodological orthodoxy, coordinates amicus strategy. They benefit from the constraint's dominance but are constrained by the need to maintain credibility within the legal profession; exit would mean abandoning a decades-long institutional investment.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, federalist_society_network, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, federalist_society_network, agenda_setter).

% Seek constitutional protection for rights unrecognized in 1787 (reproductive autonomy, same-sex marriage, gender equality, voting rights restoration). Their claims are foreclosed by the originalist boundary. Exit is trapped: constitutional amendment is practically impossible; they must wait for judicial coalition change or seek statutory remedies that originalism may also narrow.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, modern_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Argue that constitutional meaning evolves through precedent, social change, and moral progress. They are structurally excluded from authoritative interpretation — originalist dominance in the federal judiciary and legal academia's hiring networks limits their institutional voice. Exit is constrained: they can publish, teach, and litigate but cannot set binding doctrine.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitutionalist_scholars, excluded,
    moderate, biographical, constrained, national).

% Groups whose rights claims depend on post-1787 constitutional development (e.g., Black Americans whose equal protection claims rely on the 14th Amendment's evolving interpretation, women seeking reproductive autonomy, LGBTQ+ individuals). Their identity is fused with the rights the constraint forecloses; exit is not merely costly but identity-destructive.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, marginalized_groups_excluded_by_original_meaning, payer,
    powerless, generational, identity_locked, national).

% Adhere to textualism and democratic enactment (statutes, amendments) but reject original intent/historical meaning as the exclusive interpretive guide. They occupy a distinct methodological seat: they share the coordination benefit of a fixed text but reject the extraction component of originalism's historical boundary. Their exit is analytical — they evaluate the constraint from outside its methodological commitments.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, positivist_judges, observer,
    institutional, biographical, analytical, national).

% Their policy preferences (enacted through legislation) are invalidated by originalist judicial review. They would object to the constraint's anti-democratic extraction but are excluded from constitutional interpretation by design. Exit is constrained: they can pursue constitutional amendment (Article V threshold prohibitive) or court-packing (politically costly and norm-eroding).
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, democratic_majorities, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, determinate rule of recognition for constitutional law: a fixed historical meaning that limits judicial discretion and binds government to the terms the People ratified. Solves the coordination problem of 'what counts as the Constitution' by anchoring it to a specific historical moment.
% TRANSFER_FUNCTION: Transfers interpretive authority from democratic majorities (acting through legislation and amendment) and from later generations (whose moral insights might expand rights) to the Founding generation's understanding, as recovered and applied by a specific contemporary judicial-ideological coalition. The transfer moves power over rights recognition from the political branches and the living to the dead and their designated interpreters.
% ABSENT_VOICES: The Founding generation itself (cannot speak to modern conditions), future generations (whose interests are fixed by a past they did not choose), and the global community affected by U.S. constitutional doctrine (who have no voice in its interpretation). The excluded stakeholders (living_constitutionalist_scholars, democratic_majorities) would object to the foreclosure of evolutionary interpretation but are kept out by the constraint's institutional enforcement.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight, the Supreme Court would revert to living constitutionalist or positivist methodologies within one appointments cycle. Reproductive rights, voting rights, affirmative action, and administrative state doctrines would shift dramatically. The conservative legal movement's primary jurisprudential asset would be lost. The constitutional order would reorganize around a different interpretive baseline.
% FOUNDING_PROBLEM: The 1787 Constitution was built to solve: (1) creating a federal government strong enough to govern but limited enough to preserve liberty; (2) entrenching fundamental law against temporary majorities; (3) providing a stable rule of recognition for legal validity. The originalist reading claims its specific method (fixed meaning) is necessary to solve these problems.
% FOUNDING_PROBLEM_CORROBORATION: Originalists (Scalia, Barrett, Federalist Society) attest the founding problem requires fixed meaning to prevent judicial legislation. Living constitutionalists (Brennan, Dworkin, modern progressive scholars) and positivist textualists (Kagan, many academics) attest the founding problem is solved by textualism + democratic amendment + precedent, not by historical originalism. The historical record (Madison's notes, ratification debates) shows the Founders themselves disagreed on interpretive method — no consensus corroborates originalism as the exclusive solution.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(us_constitution_1787__originalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(us_constitution_1787__originalist_reading),
    narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers interpretive authority from democratic majorities and later generations to a narrow historical moment, enabling a specific coalition to block rights claims without legislative accountability. Suppression (0.78) is high because the constraint's persistence depends on controlling judicial appointments, law review discourse, and bar admission standards — active exclusion of alternatives. Theater_ratio (0.45) reflects that originalist methodology involves real historical research (coordination function) but increasingly serves as a legitimating veneer for outcome-driven judging (extraction function). Accessibility_collapse (0.82) is near-Mountain level: once the originalist frame is accepted, alternative readings appear illegitimate, not merely mistaken. Resistance (0.68) remains substantial from living constitutionalists, positivist textualists, and progressive legal movements. The claimed_type is Mountain (the reading's self-presentation) but the metrics describe a constraint that actively extracts and suppresses — the engine will compute the divergence.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist_judge seat (agenda_setter, analytical exit), the constraint appears as genuine coordination: a neutral method that prevents judicial willfulness. From the modern_rights_claimant seat (payer, trapped exit), the same structure operates as a Snare: a fixed boundary that forecloses their claims while presenting itself as inevitable. From the conservative_legal_movement seat (beneficiary, arbitrage exit), it functions as a Rope: a coordination mechanism that delivers policy wins. The engine computes these per-seat types from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and the Federalist Society network are structural beneficiaries (d ≈ 0.1-0.2): they control the interpretive apparatus and gain policy outcomes. Modern rights claimants and marginalized groups are structural targets (d ≈ 0.8-0.9): they bear the cost of rights foreclosure with trapped or identity-locked exit (constitutional amendment is practically impossible; emigration is the only full exit). Living constitutionalist scholars are excluded (d ≈ 0.7): they would contest but are kept out of authoritative venues. Democratic majorities are payers with constrained exit: they can amend the Constitution but the threshold is prohibitive. Positivist judges sit near symmetric (d ≈ 0.5): they share the textualist coordination benefit but reject the originalist extraction component.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1787) was creating a stable written constitution that could bind government and survive factional conflict. That problem is live — constitutional stability remains necessary. But the originalist reading's specific claim (meaning fixed at ratification) solves a narrower problem: preventing judicial innovation that the conservative legal movement opposes. The arrangement persists not because the founding problem requires this specific interpretive method, but because the beneficiary coalition has built institutional machinery to enforce it. The mandatrophy is unresolved: the coordination function (stable constitutional meaning) could be served by other methods (positivist textualism, common-law constitutionalism), but the extraction function (blocking disfavored rights claims) depends on originalism's specific boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested us_constitution_1787 kernel, or a free-standing constraint?',
    'The constraint_id and cs_structure.reading_relations declare it as originalist_reading of kernel us_constitution_1787. Sibling readings living_reading and positivist_reading instantiate different constraints from the same kernel.',
    'If treated as free-standing, the ε-invariance principle is violated — the same constitutional text would carry multiple ε values depending on which reading evaluates it. The kernel frame requires decomposition into separate constraint stories linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this story instantiates one reading of a contested kernel, not the kernel itself.').

omega_variable(
    originalism_vs_living_foreclosure,
    'Does the originalist reading''s core premise (meaning fixed at ratification) logically foreclose the living reading''s core premise (meaning evolves) within any single interpretive framework?',
    'Examine whether a single judicial officer or legal system can simultaneously hold that constitutional meaning is both fixed at ratification AND evolves with society. If mutually exclusive, relation = forecloses; if held by different factions simultaneously, relation = coexists_with.',
    'If forecloses, the engine''s cs_foreclosure logic activates: adoption of originalist_reading by an authority structure logically displaces living_reading within that structure. If coexists_with, both remain live options across different coalitions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_vs_living_foreclosure, conceptual, 'Structural relationship between originalist and living readings — foreclosure or coexistence.').

omega_variable(
    original_meaning_epistemic_access,
    'Can the original public meaning of 1787-1788 be reliably recovered with sufficient determinacy to constrain modern cases, or is the historical evidence irreducibly underdetermined?',
    'Empirical assessment of historical linguistics, Founding-era corpus linguistics, and the record of originalist judicial opinions: do they converge on determinate answers for contested modern questions, or do they produce persistent disagreement among originalists themselves?',
    'If underdetermined, the constraint''s claimed low extraction (as Mountain) is false — the interpretive discretion required to apply ''fixed meaning'' to novel circumstances becomes a vector for extraction by the interpreting agents. The engine would reclassify toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_epistemic_access, empirical, 'Whether the epistemic demand of originalism is practically satisfiable or masks judicial discretion.').

omega_variable(
    suppression_mechanism_judicial_appointment,
    'Is the suppression of alternative readings primarily structural (judicial appointment power, stare decisis) or internalized (professional socialization of lawyers to treat originalism as the only legitimate method)?',
    'Track the career trajectories of legal academics and judges: does suppression persist after a judge with living-constitutionalist views is appointed, or does the institutional role reshape their methodology?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the legal profession carries the suppression with it. This would increase the omega-adjusted extraction for analytical and institutional seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_judicial_appointment, empirical, 'Structural vs. internalized suppression in the legal profession''s methodological commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_1787__originalist_reading, theater_ratio, 1787, 0.1).
narrative_ontology:measurement(us_c_tr_t1868, us_constitution_1787__originalist_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_1787__originalist_reading, theater_ratio, 1937, 0.25).
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_1787__originalist_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_1787__originalist_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__originalist_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__originalist_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_1787__originalist_reading, base_extractiveness, 1787, 0.15).
narrative_ontology:measurement(us_c_be_t1868, us_constitution_1787__originalist_reading, base_extractiveness, 1868, 0.25).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_1787__originalist_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(us_c_be_t1970, us_constitution_1787__originalist_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_1787__originalist_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__originalist_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__originalist_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_1787__originalist_reading, suppression_requirement, 1787, 0.2).
narrative_ontology:measurement(us_c_su_t1868, us_constitution_1787__originalist_reading, suppression_requirement, 1868, 0.35).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_1787__originalist_reading, suppression_requirement, 1937, 0.55).
narrative_ontology:measurement(us_c_su_t1970, us_constitution_1787__originalist_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_1787__originalist_reading, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__originalist_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__originalist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the us_constitution_1787 constraint family. The kernel us_constitution_1787 decomposes into three readings with distinct ε values and structural profiles: originalist_reading (high extraction, claims Mountain, computes as tangled_rope/snare), living_reading (moderate extraction, claims scaffold/rope), positivist_reading (low extraction, claims rope). The decomposition follows the ε-invariance principle: the label 'the Constitution' covers structurally distinct claims. Originalist_reading forecloses living_reading within a single authority framework; both coexist_with positivist_reading across different judicial coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__originalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(us_constitution_1787__originalist_reading, organized, 0.2).
constraint_indexing:directionality_override(us_constitution_1787__originalist_reading, powerless, 0.85).
constraint_indexing:directionality_override(us_constitution_1787__originalist_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
