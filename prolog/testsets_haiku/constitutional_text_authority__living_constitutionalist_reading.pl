% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__living_constitutionalist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: constitutional_text_authority__living_constitutionalist_reading
 *   human_readable: Constitutional Text Authority (Living Constitutionalist Reading)
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the living constitutionalist reading of the
 *   contested kernel 'constitutional text authority.' It holds that the
 *   Constitution's meaning evolves with social attitudes and moral
 *   understanding rather than remaining fixed at the moment of ratification.
 *   Authority derives from applying ancient constitutional principles to
 *   contemporary circumstances, not from historical intent alone. This
 *   reading enables courts to recognize unenumerated rights and to adapt
 *   equal protection to modern conditions without formal amendment. The
 *   constraint is CLAIMED as tangled_rope (coordination function for
 *   constitutional adaptability + asymmetric extraction from
 *   predictability-dependent actors and originalist tradition) and the
 *   metrics reflect moderate extractiveness (courts gain interpretive power)
 *   balanced against genuine coordination (constitutional stability through
 *   flexibility). This is ONE of three readings of the same kernel; siblings
 *   are originalism (fixed meaning at ratification) and positivism
 *   (law/morality distinction). The readings do not all occupy the same
 *   jurisprudential framework—they represent genuinely competing authority
 *   structures.
 *
 * KEY AGENTS:
 *   - progressive_judiciary: institutional agenda-setter; establishes and defends living interpretation doctrine
 *   - rights_claimants_new_categories: powerless beneficiaries; gain recognition of unenumerated rights
 *   - originalist_legal_tradition: powerful payer; loses interpretive monopoly and faces constant reversion pressure
 *   - predictability_dependent_actors: organized payers; bear costs of constant doctrinal reinterpretation
 *   - conservative_judiciary: excluded institutional seat; argues for fixed meaning but loses the authoritative voice
 *   - constitutional_legal_scholars: observer seat; contest the legitimacy of the constraint's operation
 *   - legislative_branch: institutional payer; loses formal monopoly on constitutional revision
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, 0.58).
domain_priors:suppression_score(constitutional_text_authority__living_constitutionalist_reading, 0.42).
domain_priors:theater_ratio(constitutional_text_authority__living_constitutionalist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__living_constitutionalist_reading, "Constitutional Text Authority (Living Constitutionalist Reading)").
narrative_ontology:topic_domain(constitutional_text_authority__living_constitutionalist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(constitutional_text_authority__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__living_constitutionalist_reading, '12f89863-5b54-40ef-ae59-a0eb6cf5f4bf').
narrative_ontology:cs_kernel_codification('12f89863-5b54-40ef-ae59-a0eb6cf5f4bf', formalized).
narrative_ontology:cs_authority_grounding('12f89863-5b54-40ef-ae59-a0eb6cf5f4bf', lineage).
narrative_ontology:cs_interpretation_layer_present('12f89863-5b54-40ef-ae59-a0eb6cf5f4bf').
narrative_ontology:cs_reading_relation('12f89863-5b54-40ef-ae59-a0eb6cf5f4bf', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('12f89863-5b54-40ef-ae59-a0eb6cf5f4bf', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('12f89863-5b54-40ef-ae59-a0eb6cf5f4bf', foundational, constitutional_meaning_evolves_with_values).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_values, holdable).
narrative_ontology:cs_axiom_grounding('12f89863-5b54-40ef-ae59-a0eb6cf5f4bf', constitutional_meaning_evolves_with_values, deontological).
narrative_ontology:cs_axiom('12f89863-5b54-40ef-ae59-a0eb6cf5f4bf', foundational, contemporary_moral_principles_bind_interpretation).
narrative_ontology:cs_axiom_status(contemporary_moral_principles_bind_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('12f89863-5b54-40ef-ae59-a0eb6cf5f4bf', contemporary_moral_principles_bind_interpretation, empirically_contingent).
narrative_ontology:cs_reference_frame('12f89863-5b54-40ef-ae59-a0eb6cf5f4bf', constitution_as_living_instrument).
narrative_ontology:cs_drift_state('12f89863-5b54-40ef-ae59-a0eb6cf5f4bf', contemporary_2020s, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('12f89863-5b54-40ef-ae59-a0eb6cf5f4bf', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, judiciary_adaptive_authority).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, rights_claimants_new_categories).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, originalist_legal_tradition).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, predictability_dependent_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, legislative_branch).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, moral_progress_in_constitutional_meaning).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, judicial_legitimacy_through_contemporary_values).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Constitution as a living document adapting to evolving social values and moral understanding. Justifies recognition of unenumerated rights (privacy, dignity, equal protection for historically marginalized groups) by reference to contemporary constitutional principles and changing circumstances. Sets precedent that future courts follow or overturn. Faces constant pressure from originalist judges and legislative attempts to restrict judicial power through statutory and constitutional amendment proposals.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, progressive_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Individuals and groups historically excluded from enumerated constitutional protections: African Americans seeking equal protection in education and employment, LGBTQ+ persons seeking dignity and privacy rights, women seeking full equality under law, immigrants seeking due process. Their constitutional status and legal protection depend entirely on judicial willingness to recognize their rights as constitutionally grounded. Without living constitutionalism, they would lack remedies absent formal constitutional amendment.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, rights_claimants_new_categories, beneficiary,
    powerless, biographical, trapped, national).

% Institutional and scholarly position committed to the proposition that constitutional meaning is fixed at the moment of ratification (or at the moment of statutory enactment for statutory law). Bears the cost of losing interpretive authority and doctrinal influence when courts adopt living constitutionalism: the tradition's judgments are overruled or rewritten, predictive power erodes, and originalist judges lose influence in setting constitutional meaning. Must litigate constantly to restore fixed-meaning interpretation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, originalist_legal_tradition, payer,
    powerful, generational, constrained, national).

% State governments, legislatures, administrative agencies, businesses, and institutional actors who structure legal arrangements around stable constitutional understanding. When courts expand constitutional protection via living interpretation (recognizing new unenumerated rights, broadening existing rights), existing statutes and regulatory frameworks are invalidated without legislative action. Actors cannot rely on established constitutional meaning as fixed; legal certainty declines. They bear the cost of constant doctrinal reinterpretation and statutory replacement.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, predictability_dependent_actors, payer,
    organized, biographical, constrained, national).

% Judges and judicial coalitions committed to originalism or textualism. Excluded from the authority structure that living constitutionalism establishes. They would argue for restored fixed meaning, predictability, fidelity to historical understanding, and limits on unenumerated rights. Their judicial opinions stating these positions are not treated as equally authoritative when progressive coalitions control the Supreme Court; their precedents are overturned; their interpretive framework is marginalized in doctrinal discourse.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, conservative_judiciary, excluded,
    institutional, generational, constrained, national).

% The historical authors and ratifiers of the Constitution. Under living constitutionalism, their expressed intent and understanding at ratification is not binding on meaning; courts adapt text to contemporary values rather than following original public understanding. This abstraction 'bears the cost' in that their intended constraints are overridden by evolving doctrine.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, framers_historical_intent, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_text_authority__living_constitutionalist_reading, framers_historical_intent).

% Academic interpreters of constitutional law who contest the proper interpretive methodology. Living constitutionalists defend the flexibility, moral improvement, and relevance of adaptive interpretation. Originalists warn of judicial overreach, indeterminacy, and loss of textual constraint. Scholars occupy the observational seat, analyzing whether the constraint produces legitimate constitutional governance or illegitimate judicial power seizure.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, constitutional_scholars, observer,
    organized, generational, analytical, national).

% The Article V amendment authority: Congress and state legislatures formally empowered to change the Constitution. Living constitutionalism permits courts to establish new constitutional meaning (especially recognition of unenumerated rights and expansion of existing rights) without legislative amendment. Legislatures bear the cost of losing their monopoly on constitutional change and of having statutes invalidated by judicially reinterpreted constitutional meaning. They cannot easily reclaim the role absent a constitutional amendment restricting judicial power.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, legislative_branch, payer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__living_constitutionalist_reading, progressive_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text_authority__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine governance problem that a fixed historical constitutional meaning would become progressively obsolete: a text written for 1787 governance structures (slavery, property-based suffrage, limited commerce) would fail to address 20th and 21st century conditions (corporate power, digital technology, global trade, rights claims unknown at ratification). Living constitutionalism coordinates on a principle that the Constitution remains binding law and source of legitimacy while permitting its meaning to evolve with application to new circumstances. This preserves the Constitution's role as supreme law without requiring Article V amendment for every adaptation.
% TRANSFER_FUNCTION: Transfers interpretive authority from the historical framers and fixed text-at-ratification to contemporary courts and contemporary moral understanding. Shifts the power to establish binding constitutional meaning from the formal amendment process (requiring supermajority consensus across states and Congress) to judicial decisions (requiring only a majority of the Supreme Court). Moves recognition of new rights from legislative or amendment processes to judicial doctrine. Creates recognition of rights that would require formal amendment under originalism (privacy, dignity, sexual orientation equality) without Article V process. Imposes costs on predictability-dependent actors (businesses, states, originalist judges) while benefiting rights claimants and the judiciary (who gain expanded authority).
% ABSENT_VOICES: Originalist judges, textualist scholars, and the constitutional framers (as historical agents) are structurally excluded from authoritative voice. They would argue that the Constitution's fixed meaning at ratification should bind courts and that constitutional change should occur only through Article V amendment. Conservative judicial coalitions and originalist scholars actively contest living constitutionalism but are marginalized in doctrinal authority when progressive majorities control the bench. This exclusion is maintained by the majority controlling interpretive doctrine and by the framework that treats historical intent as interpretively secondary rather than binding.
% DISAPPEARANCE_RATIONALE: If living constitutionalism disappeared and courts returned to strict originalism, the constitutional landscape would realign dramatically. Decisions like Brown v. Board (which reinterpreted equal protection as prohibiting segregation despite original understanding permitting racial classifications), Obergefell v. Hodges (which recognized a unenumerated dignity right grounding marriage equality), and Griswold v. Connecticut/Roe v. Wade (which located privacy rights in textual penumbras) would be reversed or abandoned as unprincipled extensions beyond original meaning. Rights recognized under living constitutionalism would lose their constitutional foundation unless explicitly amended through Article V. State and federal law would realign around narrow original meanings: equal protection would revert to preventing formal classifications without protecting substantive equality; privacy would not be a recognized constitutional right; recognition of new rights-bearer categories (LGBTQ+ persons) would depend on statutory action, not constitutional guarantee. The judiciary's role in constitutional evolution would transfer back toward the amendment process, which is functionally much slower and requires broader political consensus.
% FOUNDING_PROBLEM: The Constitution is a text written in 1787 for a radically different political and social world, addressing governance of an agrarian republic with slavery, limited commerce, and restricted suffrage. Strict originalism applied to fixed 1787 meaning would render it progressively obsolete: modern communications technology, rights to bodily autonomy and dignity, protection of minorities from majoritarian harm, corporate personhood, and digital surveillance were not contemplated at ratification. A Constitution frozen in original meaning would fail to speak to actual governance problems of subsequent centuries. Either the Constitution would be repeatedly amended (Article V process would become the normal mode of governance), or it would lose binding force as courts and legislatures treated historical meaning as archaic.
% FOUNDING_PROBLEM_CORROBORATION: Living constitutionalists and progressive judges attest the founding problem is still live: the text is ancient and must be read through contemporary understanding to remain relevant and legitimate. The constraint ensures constitutional governance remains possible despite changed circumstances. Originalists and conservative judges attest the founding problem is a false justification for judicial overreach: the Constitution can accommodate modern problems through originalist application of original principles to new circumstances (e.g., Second Amendment protects modern firearms even if framers didn't foresee them); the proper solution for gaps is formal amendment under Article V, which the framers deliberately included. Legislative scholars and comparative constitutionalists note that other constitutional democracies manage constitutional change through formal amendment more frequently than the US does, suggesting Article V is not structurally incapable but politically difficult. Originalist judges offer counter-evidence that strict originalism applied systematically has not produced obsolescence (Heller decision, for example, applied original meaning of Second Amendment to modern handguns). The contest is real and not resolved by independent evidence—it is fundamentally a dispute about proper interpretive authority.
narrative_ontology:disappearance_verdict(constitutional_text_authority__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__living_constitutionalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__living_constitutionalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.58 at interval end because the constraint grants courts interpretive power unchecked by historical text (they can establish new meaning) while imposing costs on actors who depend on stable meaning. The extractiveness rises from 0.38 to 0.58 across the first 32 time points (roughly 1954–1990, from Brown through the sexual orientation rights cases) as living constitutionalism becomes doctrinal consensus, then stabilizes (1990–2024) as the framework ossifies into its own originalism—courts defend precedent settled through living interpretation. Suppression is moderate (0.42): the constraint is enforced by majority coalitions on the bench and by the exclusion of originalist arguments from the authoritative doctrinal frame, but real resistance persists (originalist judges, legislative amendment proposals, predictability-dependent actors who challenge expansion). Theater rises gradually (0.18→0.31) as the constraint becomes routine: courts perform 'constitutional values interpretation' even when the outcomes align with political coalitions rather than textual constraints. The claim/metric divergence is intentional: the constraint is CLAIMED as tangled_rope to emphasize its coordination function (constitutional adaptability solves the obsolescence problem) and its extraction (judicial power concentrated without amendment consent); the metrics show that extraction and suppression are real and growing, not merely rhetorical.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive judiciary seat, living constitutionalism is genuine coordination: the Constitution remains binding and legitimate because it speaks to modern conditions. From the originalist tradition seat, the same constraint is pure extraction: courts arrogate amendment power, unpredictability proliferates, and fixed meaning is overridden by judicial preference. From the predictability-dependent seat, living constitutionalism is a cost structure: laws validated as constitutional today are invalidated tomorrow as meaning evolves. From the rights-claimant seat, it is liberation: unenumerated rights become recognizable. The engine computes these per-seat classifications from power, exit, and beneficiary/victim declarations; the authored claim does NOT reconcile them. Seat divergence is the structural fact living constitutionalism instantiates.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive judiciary: agenda-setter, institutional power, constrained exit (cannot unilaterally change constitutional meaning), but controls interpretation—d near 0.2 (strong beneficiary despite institutional constraint, because they set doctrine). Rights-claimants: powerless, trapped exit, benefit from adaptive doctrine—d near 0.1 (full beneficiary). Originalist tradition: powerful but loses authority, constrained exit (cannot escape the interpretive frame courts have set)—d near 0.75 (high target despite power, because they are systematically excluded from the authoritative seat). Predictability-dependent actors: organized, constrained exit (must operate under whatever Constitution courts interpret)—d near 0.70 (high target; they bear costs of constant reinterpretation). Conservative judiciary: institutional power but excluded from doctrine-setting—d near 0.85 (near-total target; they cannot enforce their interpretation without majority). Legislative branch: institutional but loses amendment monopoly—d near 0.65 (asymmetric extraction; legislatures bear the cost of judicial override). No directionality overrides needed; the derivation from beneficiary/victim and exit options produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   Living constitutionalism's founding problem is genuinely live: the Constitution would become obsolete under strict originalism, failing to speak to modern governance needs. But the manner of solving it—transferring amendment authority from Article V (supermajority consensus) to five-justice majorities (simple majority)—involves extraction that does NOT serve the coordination function. Courts could solve the obsolescence problem through narrower adaptive interpretation (applying fixed principles to new circumstances) without claiming the power to redefine the principles themselves. The constraint's classification as tangled_rope (not pure snare) reflects that genuine coordination exists—modern constitutional governance requires flexibility—but extraction is built into the solution mechanism. Courts would resist reducing extraction by returning to originalism (which would lose the flexibility) or by submitting expansions to Article V (which would require broad consensus). The mandatrophy tension: the founding problem remains live, but the extracted power exceeds what the problem requires to solve. This is textbook tangled rope: coordination and extraction are inseparable in this constraint's structure, not because the problem demands it but because courts have engineered the solution to maximize their authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_progress_vs_judicial_invention,
    'When courts recognize unenumerated rights through living constitutionalism, are they discovering moral principles that were always implied in the Constitution''s structure (moral progress in understanding), or are they inventing new constitutional content not present in the historical or textual record (judicial amendment)?',
    'Originalist vs. living-constitutionalist jurisprudential analysis of whether unenumerated rights (privacy, dignity, sexual orientation) can be derived from the constitutional structure and founding principles or require input from post-ratification moral development. Natural experiment: compare living constitutionalism''s outcomes in different constitutional traditions (US, UK, Canada) to assess whether the same ''constitutional values'' drive comparable expansions of rights.',
    'If moral progress explains the outcomes, living constitutionalism is discovering pre-existing constitutional meaning and the extraction is justifiable coordination cost. If judicial invention explains the outcomes, living constitutionalism permits courts to amend the Constitution without Article V consent, and extraction is unauthorized power seizure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_progress_vs_judicial_invention, conceptual, 'Whether living constitutionalism discovers or invents constitutional meaning.').

omega_variable(
    article_v_bypass_necessity,
    'Could the Constitution be adapted to modern conditions through strict originalism + frequent Article V amendment, or does the political difficulty of Article V require courts to bypass formal amendment to keep the Constitution relevant?',
    'Comparative historical analysis: count how many substantive constitutional adaptations have occurred through Article V amendment vs. through judicial interpretation in the modern era. Test whether Article V is structurally incapable of keeping pace with social change or whether political will (not structural necessity) is the constraint.',
    'If Article V is structurally incapable, living constitutionalism is a necessary workaround for coordination (not pure extraction). If Article V is politically difficult but available, living constitutionalism is judicial preference for power without consensus (extraction dominant over coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_v_bypass_necessity, empirical, 'Whether the constraints on Article V amendment necessitate judicial bypass.').

omega_variable(
    contemporary_values_constraint_scope,
    'When courts cite ''contemporary moral principles'' or ''evolving standards of decency'' in constitutional interpretation, what constrains which contemporary values count as constitutionally authoritative? Is the constraint the values themselves, or the judicial ability to invoke values selectively?',
    'Content analysis of constitutional decisions over time: track which contemporary values courts invoke when they support recognized rights vs. when they reject rights claims despite contemporary support (e.g., affirmative action, campaign finance). Identify the pattern: are contemporary values truly constraining, or does the framework permit courts to invoke ''contemporary values'' when those values align with the desired outcome and to ignore contrary contemporary values?',
    'If contemporary values truly constrain, the suppression and extraction metrics are lower (courts are bound by something outside themselves). If contemporary values are selectively invoked, suppression and extraction are higher (the framework permits courts to present selective values as universal constraints).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_values_constraint_scope, empirical, 'Whether ''contemporary values'' are a real constraint or a rhetorical cover for judicial choice.').

omega_variable(
    originalism_reading_coexistence,
    'Is the relationship between living constitutionalism and originalism best understood as coexistence (two live jurisprudential traditions held by different scholarly and judicial communities) or foreclosure (one reading logically rules out the other)?',
    'Jurisprudential analysis of whether a single framework could hold both readings coherently (mixed originalism with limited living interpretation on clear cases of textual ambiguity, for instance). Test whether the core premises contradict each other or whether they are simply incompatible authority claims.',
    'If coexistence, the constraint is robustly contested and both readings remain live (network influence, not elimination). If foreclosure, accepting living constitutionalism requires rejecting originalism as incoherent, and the engine''s rendering of the relationship changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_reading_coexistence, conceptual, 'Whether living constitutionalism and originalism coexist or foreclose each other.').

omega_variable(
    predictability_cost_allocation,
    'Who bears the cost of reduced predictability when courts reinterpret constitutional meaning? Are the costs distributed equally across all affected actors, or concentrated on specific groups (businesses, states, minorities, citizens in general)?',
    'Empirical analysis of who brings successful and unsuccessful constitutional challenges: count whose rights are expanded vs. whose interests are constrained by living constitutionalism over decades. Track legislative response costs: do states/businesses face higher costs than other actors?',
    'If costs are distributed, living constitutionalism may be more justifiable as general governance adaptation. If costs are concentrated on predictability-dependent actors (businesses, state governments) while benefits accrue to rights claimants, extraction is more asymmetric than the moderate extractiveness metric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predictability_cost_allocation, empirical, 'Whether the costs of constitutional unpredictability are distributed or concentrated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__living_constitutionalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t8, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(cons_tr_t8, observed).
narrative_ontology:measurement(cons_tr_t16, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement_basis(cons_tr_t16, observed).
narrative_ontology:measurement(cons_tr_t24, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(cons_tr_t24, observed).
narrative_ontology:measurement(cons_tr_t32, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement_basis(cons_tr_t32, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(cons_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t8, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement_basis(cons_be_t8, observed).
narrative_ontology:measurement(cons_be_t16, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement_basis(cons_be_t16, observed).
narrative_ontology:measurement(cons_be_t24, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement_basis(cons_be_t24, observed).
narrative_ontology:measurement(cons_be_t32, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 32, 0.59).
narrative_ontology:measurement_basis(cons_be_t32, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(cons_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t8, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement_basis(cons_su_t8, observed).
narrative_ontology:measurement(cons_su_t16, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(cons_su_t16, observed).
narrative_ontology:measurement(cons_su_t24, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(cons_su_t24, observed).
narrative_ontology:measurement(cons_su_t32, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 32, 0.43).
narrative_ontology:measurement_basis(cons_su_t32, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(cons_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__living_constitutionalist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__positivist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, judicial_supremacy_in_rights_recognition).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, article_v_amendment_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'constitutional_text_authority.' The other readings (originalist_reading, positivist_reading) produce substantially different ε values because they solve different coordination problems and create different extraction profiles. All three readings share the same kernel (the Constitution is the binding law) but disagree on where authority comes from. The three constraints form a constraint family; each story declares its siblings in reading_relations and represents a live competing position in constitutional jurisprudence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
