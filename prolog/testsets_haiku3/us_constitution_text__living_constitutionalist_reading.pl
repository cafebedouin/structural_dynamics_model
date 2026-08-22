% ============================================================================
% CONSTRAINT STORY: us_constitution_text__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_living_constitutionalist, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_text__living_constitutionalist_reading
 *   human_readable: Living Constitution Interpretive Doctrine (US)
 *   domain: constitutional_law/interpretive_theory
 *
 * SUMMARY:
 *   This story instantiates the living-constitutionalist reading of the US
 *   Constitution's interpretive authority. The core claim is that
 *   constitutional meaning must evolve with society: the principles embedded
 *   in the Constitution (liberty, equal protection, due process) remain
 *   constant, but their application to new social and technological contexts
 *   must adapt. Judges empowered to declare what the Constitution means for
 *   purposes of live cases are authorized to read principles into new
 *   circumstances. The reading competes with two siblings: originalism
 *   (meaning is fixed at ratification; judges recover historical intent) and
 *   legal positivism (validity derives from formal enactment, not meaning or
 *   intent). This story focuses solely on the living-constitutionalist
 *   reading as an ε-invariant constraint—the reading of the shared kernel (US
 *   Constitution text) that living constitutionalists instantiate.
 *
 * KEY AGENTS:
 *   - Living-constitutionalist judges: set and enforce the interpretive frame by writing opinions that apply constitutional principles adaptively
 *   - Rights claimants in evolved contexts (LGBTQ+ people, reproductive autonomy seekers, digital privacy advocates): benefit from adaptive interpretation that reaches into unenumerated rights
 *   - Originalist and textualist jurists: object that living constitutionalism permits judicial policymaking; their core claim (meaning is fixed) is treated as already decided by the interpretive framework itself
 *   - Democratic legislators: lose authority when judicial interpretation reaches principles legislators have not enacted
 *   - Amendment constituency: see living constitutionalism as an end-run around formal amendment procedures
 *   - International human rights advocates: benefit from the flexibility to align US constitutional meaning with global norms
 *   - Academic jurisprudential community: theorizes and measures the operation of the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_text__living_constitutionalist_reading, 0.34).
domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__living_constitutionalist_reading, "Living Constitution Interpretive Doctrine (US)").
narrative_ontology:topic_domain(us_constitution_text__living_constitutionalist_reading, "constitutional_law/interpretive_theory").

domain_priors:requires_active_enforcement(us_constitution_text__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__living_constitutionalist_reading, 'ab20da89-3a7f-4f07-97ca-8b26f106aedd').
narrative_ontology:cs_kernel_codification('ab20da89-3a7f-4f07-97ca-8b26f106aedd', fixed_text).
narrative_ontology:cs_authority_grounding('ab20da89-3a7f-4f07-97ca-8b26f106aedd', lineage).
narrative_ontology:cs_interpretation_layer_present('ab20da89-3a7f-4f07-97ca-8b26f106aedd').
narrative_ontology:cs_reading_relation('ab20da89-3a7f-4f07-97ca-8b26f106aedd', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab20da89-3a7f-4f07-97ca-8b26f106aedd', us_constitution_text__positivist_reading, influences).
narrative_ontology:cs_axiom('ab20da89-3a7f-4f07-97ca-8b26f106aedd', foundational, constitutional_principles_adapt_to_context).
narrative_ontology:cs_axiom_status(constitutional_principles_adapt_to_context, holdable).
narrative_ontology:cs_axiom_grounding('ab20da89-3a7f-4f07-97ca-8b26f106aedd', constitutional_principles_adapt_to_context, deontological).
narrative_ontology:cs_axiom('ab20da89-3a7f-4f07-97ca-8b26f106aedd', foundational, judicial_interpretation_is_constitution_law).
narrative_ontology:cs_axiom_status(judicial_interpretation_is_constitution_law, holdable).
narrative_ontology:cs_axiom_grounding('ab20da89-3a7f-4f07-97ca-8b26f106aedd', judicial_interpretation_is_constitution_law, conventional).
narrative_ontology:cs_axiom('ab20da89-3a7f-4f07-97ca-8b26f106aedd', secondary, post_ratification_practice_updates_meaning).
narrative_ontology:cs_axiom_status(post_ratification_practice_updates_meaning, holdable).
narrative_ontology:cs_axiom_grounding('ab20da89-3a7f-4f07-97ca-8b26f106aedd', post_ratification_practice_updates_meaning, empirically_contingent).
narrative_ontology:cs_reference_frame('ab20da89-3a7f-4f07-97ca-8b26f106aedd', adaptive_principle_interpretation).
narrative_ontology:cs_drift_state('ab20da89-3a7f-4f07-97ca-8b26f106aedd', contemporary_originalist_backlash_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('ab20da89-3a7f-4f07-97ca-8b26f106aedd', '').
narrative_ontology:cs_kernel_id(us_constitution_text__living_constitutionalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, rights_claimants_in_evolved_contexts).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, claims_to_fixed_meaning_as_constraint).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, international_human_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, originalist_and_textualist_jurists).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, democratic_legislators).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, amendment_constituency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Constitution as a document whose principles must adapt to contemporary circumstances. They exercise discretionary judgment in applying framers' principles to cases the framers could not have foreseen (digital privacy, reproductive autonomy, same-sex marriage). Their authority derives from the judicial function itself—they are empowered to declare what constitutional meaning is for purposes of deciding live cases. The constraint's enforcement is their opinion-writing and binding case law. They set the interpretive frame and control which arguments reach the judicial level.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, living_constitutionalist_judges, agenda_setter,
    institutional, generational, constrained, national).

% People asserting rights (abortion access, same-sex marriage, digital privacy, voting access for marginalized groups) that the Constitution's text does not explicitly enumerate but that living-constitutionalist doctrine reads into the principles of liberty, equal protection, or dignity. They depend entirely on judicial acceptance of the adaptive-interpretation frame to succeed. Their identities are constituted partly through the rights they claim—they cannot exit the constraint (cannot stop being LGBTQ+, cannot stop needing abortion access). Without living constitutionalism, their claims fail entirely; the fixed text simply does not address their situation.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, rights_claimants_in_evolved_contexts, beneficiary,
    powerless, biographical, identity_locked, national).

% Argue that living constitutionalism permits judges to impose their own policy preferences under the guise of constitutional interpretation. They lose definitional authority every time a living-constitutionalist decision overrides what they see as the Constitution's fixed text or original public understanding. They are excluded from the framework's default position—the living-constitutionalist interpretive regime treats their core claim (meaning is fixed) as already decided at the level of jurisprudential theory. They cannot renounce originalism without losing their professional identity and credibility with originalist constituencies, so their exit is identity-locked. Their objection is not that the framework cannot be applied, but that applying it is illegitimate judicial policymaking.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, originalist_and_textualist_jurists, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__living_constitutionalist_reading, originalist_and_textualist_jurists, excluded).

% Face the constraint that judicial interpretations of constitutional principles can override or limit their statutory authority. Under living constitutionalism, judges can read constitutional meaning that did not exist in the text at the time of enactment, which means legislated policy can be struck down retroactively as violating an evolved constitutional principle. They must accept that their interpretation of what the Constitution allows is subordinate to the judicial reading. They retain powerful formal authority (legislative power) but face constrained exit—they cannot opt out of the Constitution's interpretive reach.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, democratic_legislators, payer,
    powerful, generational, constrained, national).

% People who believe constitutional changes should require the formal amendment process (Article V). They see living constitutionalism as a workaround that effectively amends the Constitution without going through the prescribed democratic procedure. The constraint extracts legitimacy from formal amendment authority—interpretation that accomplishes de facto amendment without the constitutional process feels like a democratic loss to this group. They are trapped in the constraint: the only exit from living constitutionalism is either a constitutional amendment (nearly impossible for most issues) or a change in judicial personnel (generational timescale).
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, amendment_constituency, payer,
    moderate, generational, trapped, national).

% Use living-constitutionalist interpretations to argue that US constitutional doctrine converges with international human rights norms. They cite living-constitutionalist decisions (Obergefell, Lawrence) as evidence that constitutional meaning evolves to embrace evolving global standards. They benefit from the flexibility the doctrine provides; they can move their advocacy between jurisdictions (US courts, international forums, foreign legal systems) if living constitutionalism stalls in one venue. Their exit is mobile—they can shift focus to jurisdictions with more favorable interpretive regimes.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, international_human_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Produces scholarship that theorizes and contests the living constitutionalist doctrine. They neither set the constraint's rules nor directly bear its costs; they interpret and measure its operation for the legal and academic communities. Their analysis informs which framings gain traction in judicial opinions and legal culture. They operate from the analytical seat—they study the constraint without being directly extracted from or benefiting from it.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, academic_jurisprudential_community, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__living_constitutionalist_reading, living_constitutionalist_judges).
narrative_ontology:fixing_cost_class(us_constitution_text__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified framework for judicial reasoning that permits constitutional principles to maintain relevance across radically different social and technological contexts—without requiring formal amendment every time circumstances shift. It coordinates the behavior of lower courts by establishing that the Supreme Court's interpretive authority includes adaptive application, and coordinates the interpretation of ambiguous constitutional text across generations.
% TRANSFER_FUNCTION: Transfers legitimate definitional authority over what the Constitution means from the historical intent of the ratifiers (fixed at t=1789, 1868, 1971 etc.) to living judicial interpretation (continuously updated). It moves authority from the past to the present, from fixed meaning to adaptive meaning. It transfers from statutory and democratic legislation any claim to finality—what legislatures enact can be reinterpreted as violating evolved constitutional principles.
% ABSENT_VOICES: Formal originalists and textualists are not absent—they participate in the same Court and publish dissenting opinions—but the framework that living constitutionalism establishes treats their core claim (that meaning is fixed) as already settled by jurisprudential theory. The voices truly excluded are those of the framers themselves (who cannot update their own intent to defend it) and citizens in prior generations whose constitutional moment has passed—their understanding of the Constitution's meaning becomes subject to reinterpretation by judges who never consulted them.
% DISAPPEARANCE_RATIONALE: If living constitutionalism as a doctrine disappeared and strict originalism became the only legitimate interpretive frame, dozens of constitutional rights recognized in the last 50 years would be removed from the constitutional floor—same-sex marriage, modern privacy rights, voting protections recognized under the 15th Amendment, gender equality under the 14th Amendment would all revert to their narrower 1789/1868 framers' understanding. Abortion rights, if recognized at all, would be struck down entirely. The legal, social, and political landscape would reorganize around a fixed-meaning constitution, and millions of people would lose rights they currently hold.
% FOUNDING_PROBLEM: The Constitution was written for an 18th-century agrarian society and amended in the 19th century for post-slavery reconstruction. By the 20th century, new technologies (radio, television, electronic surveillance), new social movements (civil rights, feminism, LGBTQ+ rights), and new economic realities (corporate power, digital platforms, global supply chains) raised legal questions that the Constitution's text could not possibly address. The founding problem was: how can a fixed text remain authoritative as the supreme law when circumstances change so dramatically that the text's application is indeterminate?
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (Randy Barnett, Keith Whittington) concede that the founding problem is real but argue the solution is formal amendment, not judicial reinterpretation. Living-constitutionalist scholars (Jack Balkin, Cass Sunstein) argue the founding problem persists and formal amendment is politically impossible for most issues, making adaptive interpretation necessary. Comparative constitutional scholarship (David Law, Tom Ginsburg) documents that most modern democracies with written constitutions face the same problem and solve it through interpreted evolution (not just amendment). The empirical fact that the founding problem exists is not contested; the remedy is.
narrative_ontology:disappearance_verdict(us_constitution_text__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__living_constitutionalist_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.58 endpoint) because the constraint transfers definitional authority over what counts as constitutional meaning from historical intent (fixed, not updatable) to judicial discretion (continuously adjustable). This is extractive of originalist authority and of strict-amendment-process legitimacy; it benefits rights claimants whose claims could not survive historical-meaning interpretation. Suppression is moderate (0.34) because the constraint does not require coercive enforcement—the framework's legitimacy rests on accepting that judges have interpretive discretion, and while originalists resist this, the doctrine is openly defended, not hidden or coercive. Theater ratio is modest (0.22) because much of the judicial activity is genuine principled reasoning (discerning what constitutional principles entail in new contexts), though some share is defensive—justifying expansive interpretation as authentic principle-application rather than judicial legislating. The measurement series shows extractiveness rising from 0.38 to peak at 0.65 (t=50), then dropping to 0.58 at the projected endpoint (t=60), reflecting growing resistance and political backlash against expansive interpretations (originalist judicial appointments, constitutional amendment proposals) that constrain the living-constitutionalist frame. Suppression requirement declines over the interval (0.42 to 0.31, then slight uptick to 0.34) as the doctrine becomes culturally entrenched and needs less active defense—but the slight uptick at t=60 reflects renewed backlash and the need to defend against originalist counter-interpretations more forcefully.
 *
 * PERSPECTIVAL GAP:
 *   From the living-constitutionalist judicial seat, the constraint is genuine coordination—establishing a unified framework so the Constitution remains relevant across radically different times and enables principled adaptive reasoning. From the originalist seat, it is pure extraction of meaning-setting authority and an illegitimate transfer of constitutional amendment power from the people (Article V) to judges. From the rights-claimant seat (powerless, constrained), it is a coordination benefit—without it, their rights collapse entirely. From the democratic-legislator seat, it is extraction of final authority over what the Constitution allows. The engine computes these divergences from the structural data: judges hold institutional power with generational horizons and constrained exit (they cannot leave the bench); rights claimants are powerless with biographical horizons and identity-locked exit (their identity is constituted in part by the rights they claim); originalist jurists are institutional but their exit is identity-locked (they cannot renounce originalism without losing their jurisprudential identity); legislators are powerful but face constrained exit (they remain bound by whatever the Constitution is interpreted to mean). These differences drive the per-seat type divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights claimants derive d from being beneficiaries with no exit: d ≈ 0.15 (strong beneficiary end). Living-constitutionalist judges derive d from setting the constraint's operation plus benefiting from the authority it grants them: d ≈ 0.25-0.35 (mild beneficiary, mild agenda-setter). Originalist jurists derive d as victims of the transfer of meaning-setting authority: d ≈ 0.70-0.80 (strong target end). Democratic legislators derive d as moderate targets (extract-constrained by constitutional reinterpretation): d ≈ 0.55-0.65. Amendment constituency derives d as targets (their formal-process authority is bypassed): d ≈ 0.65-0.75. The directionality divergence is structural and requires no override: each stakeholder's relationship to the constraint is captured by the beneficiary/victim declarations and exit modulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled rope because it holds BOTH a genuine coordination function (enabling constitutional principles to remain authoritative across changed contexts without continuous amendment) AND asymmetric extraction (judges gain definitional authority; originalists lose it; amendment process is bypassed; some democratic prerogatives are constrained). It requires active enforcement: judicial dissents, originalist appointments, backlash amendments, and academic counter-arguments are all attempts to enforce rival interpretive regimes. The constraint persists because each side believes it is defending legitimate constitutional authority—judges defending adaptive interpretation as fidelity to enduring principles; originalists defending fixed meaning as fidelity to democratic enactment. No party benefits enough to maintain a snare (pure extraction), and no party is hurt enough to fix it unilaterally. The rising extractiveness trajectory (0.38 to 0.65) reflects the gradual accumulation of judicial reinterpretations that expand living-constitutionalist authority (e.g., Obergefell, pending decisions on trans rights). The declining suppression requirement reflects normalization—the doctrine becomes culturally accepted and needs less active defense. The theater ratio rise reflects increasing defensiveness—judges must spend more opinion-text explaining why adaptive interpretation is not judicial legislating, not just doing the interpretation itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_discretion_scope_ambiguity,
    'Where is the boundary between legitimate adaptive interpretation of constitutional principles and illegitimate judicial legislating? How much discretion do judges have before they cross into policymaking?',
    'Empirical observation of judicial behavior: which interpretations gain stable acceptance across partisan cycles, and which trigger constitutional amendment proposals or court-packing movements? Theoretical analysis of what makes interpretation legitimate (coherence with prior precedent, fidelity to principles, reasoned elaboration) vs. arbitrary (treating precedent as irrelevant, changing principles without explanation).',
    'If the boundary is clear and stable, the doctrine''s legitimacy is durable and the constraint remains tangled_rope. If the boundary is indeterminate or constantly shifting, the constraint drifts toward snare (pure authority extraction by judges without principled constraint). If judicial discretion is actually narrow (many cases have determinate answers under principled living constitutionalism), extractiveness drops and the constraint drifts toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_discretion_scope_ambiguity, conceptual, 'Whether living-constitutionalist interpretation has principled boundaries or becomes arbitrary judicial policymaking.').

omega_variable(
    reading_kernel_foreclosure,
    'Does living constitutionalism logically foreclose originalism, or can both readings coexist within a single constitutional framework?',
    'Jurisprudential analysis: can a single judge or court apply both living-constitutionalist reasoning (adapt principles to modern contexts) and originalist reasoning (recover original meaning) to different cases without internal contradiction? Or does accepting one framework require logically rejecting the other?',
    'If they foreclose each other (one framework''s core premise directly contradicts the other), the two readings are structurally incompatible and cannot coexist in a unified jurisprudence—only one can be correct. If they coexist, both readings remain live positions held by different jurists/parties, and the constraint reflects an ongoing contest between incommensurable interpretive regimes. If they influence each other (originalism limits how radically living constitutionalism can adapt), the relation is lateral influence, not foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Whether living constitutionalism and originalism are logically incompatible or merely competitive.').

omega_variable(
    intergenerational_legitimacy_transfer,
    'Is it legitimate for one generation of judges to update the Constitution''s meaning for future generations without their consent? Does living constitutionalism effectively transfer amendment authority from the people (Article V) to judges, or does it preserve constitutional authority by keeping principles alive?',
    'Normative-institutional analysis: compare to other democracies'' approaches (written vs. unwritten constitutions, amendment rates, judicial interpretive scope). Empirical observation of whether judicial reinterpretations that change outcomes (e.g., same-sex marriage from being unconstitutional to being constitutionally required) command public legitimacy over time, or whether they trigger sustained backlash and amendment efforts.',
    'If living constitutionalism is seen as legitimate evolution of constitutional meaning (higher public legitimacy over time), the constraint''s foundational legitimacy is stronger and extractiveness may stabilize or decline. If it is seen as illegitimate end-running of the amendment process (sustained backlash, amendment proposals), the constraint is vulnerable to structural replacement (originalist appointment strategy, constitutional amendment) and extractiveness may peak and then collapse as the constraint is overridden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_legitimacy_transfer, preference, 'Whether intergenerational transfer of meaning-setting authority via judicial interpretation is legitimate constitutional practice or illegitimate policymaking.').

omega_variable(
    sibling_reading_framing_selection,
    'The three sibling readings (living constitutionalist, originalist, positivist) compete to define what the Constitution IS. Is there a fact of the matter about which reading is correct, or does the reading that gains institutional authority (judicial appointments, party control) simply become ''correct'' by institutional fiat?',
    'Epistemological inquiry: are there constitutional facts (truths about what the Constitution means independent of who interprets it)? If yes, empirical resolution through convergence of interpretive methods on the same answer. If no, then constitutional meaning is institutional (whatever the Supreme Court says it is), and the ''correct'' reading is the one that controls judicial power at a given time.',
    'If there are constitutional facts, the living-constitutionalist reading can be true or false—either constitutional meaning genuinely does evolve with society or it doesn''t. If constitutional meaning is institutional, then the contest between readings is a pure power competition, and the constraint is a rent-extraction mechanism that the winning reading uses to lock in its authority. This transforms the type from tangled_rope (mixed coordination and extraction) toward snare (pure extraction with coordination cover story).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_framing_selection, conceptual, 'Whether constitutional meaning is a fact to be discovered or an institutional outcome to be achieved through political power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__living_constitutionalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_const_living_tr_t0, us_constitution_text__living_constitutionalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(us_const_living_tr_t10, us_constitution_text__living_constitutionalist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(us_const_living_tr_t20, us_constitution_text__living_constitutionalist_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(us_const_living_tr_t30, us_constitution_text__living_constitutionalist_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(us_const_living_tr_t40, us_constitution_text__living_constitutionalist_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(us_const_living_tr_t50, us_constitution_text__living_constitutionalist_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(us_const_living_tr_t60, us_constitution_text__living_constitutionalist_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(us_const_living_be_t0, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(us_const_living_be_t10, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(us_const_living_be_t20, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(us_const_living_be_t30, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(us_const_living_be_t40, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(us_const_living_be_t50, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(us_const_living_be_t60, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_const_living_su_t0, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(us_const_living_su_t10, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(us_const_living_su_t20, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(us_const_living_su_t30, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 30, 0.34).
narrative_ontology:measurement(us_const_living_su_t40, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement(us_const_living_su_t50, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 50, 0.31).
narrative_ontology:measurement(us_const_living_su_t60, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 60, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__living_constitutionalist_reading, 0.18).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, supreme_court_appointment_process).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, constitutional_amendment_possibility_window).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'US Constitution text.' The originalist_reading and positivist_reading are sibling constraints instantiating alternative readings of the same kernel. Each reading has a different ε (fixed meaning vs. adaptive meaning vs. formal procedure), different beneficiaries, different victims, and likely different types. The three readings are linked by network.affects_constraints (each influences the others' operation) and are decomposed via OQ-26 ε-invariance: they are distinct constraints sharing a kernel, not one constraint viewed from different angles. A unified analysis of 'the Constitution' requires comparing all three stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__living_constitutionalist_reading, powerless, 0.12).
constraint_indexing:directionality_override(us_constitution_text__living_constitutionalist_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
