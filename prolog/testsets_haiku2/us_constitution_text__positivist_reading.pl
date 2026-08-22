% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__positivist_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: Constitutional Validity as Procedural Constraint (Positivist Reading)
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   The positivist reading of the US Constitution holds that constitutional
 *   validity derives from formal enactment procedures (the text as written,
 *   Article V amendment process) rather than moral content or historical
 *   meaning. Under this reading, judges are bound by source-validity: their
 *   authority is constrained to apply and interpret the formally enacted
 *   text, not to recognize constitutional rights based on evolving moral
 *   principle or on principles they believe implicit in the Constitution's
 *   spirit. The reading benefits institutional stability and judicial
 *   restraint by narrowing the legitimate basis for constitutional
 *   interpretation. It imposes costs on substantive justice advocates and
 *   historically excluded constituencies whose claims lack formal textual
 *   basis. The constraint is CLAIMED as tangled_rope because it coordinates a
 *   genuine institutional problem (preventing outcome-oriented jurisprudence)
 *   while asymmetrically extracting from those whose justice claims the
 *   positivist frame forecloses. The measurement series shows extractiveness
 *   rising sharply in the early interval (0.48 to 0.62 over the first 15 time
 *   points) as the constraint becomes jurisprudentially dominant, then
 *   plateauing as it stabilizes as the ruling interpretive orthodoxy.
 *
 * KEY AGENTS:
 *   - Formalist judges: set and enforce the procedural constraint; benefit from the rule-bound frame
 *   - Substantive justice advocates: constrained by the frame; bear costs when their moral claims lack textual basis
 *   - Excluded constitutional minorities: constrained exit; forced to pursue Article V amendment for recognition
 *   - Article V gatekeepers: benefit from concentration of constitutional change authority in formal amendment process
 *   - Living constitutionalist courts: excluded from the positivist frame; their interpretive moves are ruled per se procedurally illegitimate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.68).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.72).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "Constitutional Validity as Procedural Constraint (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, 'e1cc223f-6a44-43b9-9317-c5a4cd7369ec').
narrative_ontology:cs_kernel_codification('e1cc223f-6a44-43b9-9317-c5a4cd7369ec', formalized).
narrative_ontology:cs_authority_grounding('e1cc223f-6a44-43b9-9317-c5a4cd7369ec', extraction).
narrative_ontology:cs_interpretation_layer_present('e1cc223f-6a44-43b9-9317-c5a4cd7369ec').
narrative_ontology:cs_reading_relation('e1cc223f-6a44-43b9-9317-c5a4cd7369ec', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1cc223f-6a44-43b9-9317-c5a4cd7369ec', us_constitution_text__living_constitutionalist_reading, influences).
narrative_ontology:cs_axiom('e1cc223f-6a44-43b9-9317-c5a4cd7369ec', foundational, validity_from_procedure_not_content).
narrative_ontology:cs_axiom_status(validity_from_procedure_not_content, holdable).
narrative_ontology:cs_axiom_grounding('e1cc223f-6a44-43b9-9317-c5a4cd7369ec', validity_from_procedure_not_content, deontological).
narrative_ontology:cs_axiom('e1cc223f-6a44-43b9-9317-c5a4cd7369ec', foundational, amendment_process_supreme_over_interpretation).
narrative_ontology:cs_axiom_status(amendment_process_supreme_over_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('e1cc223f-6a44-43b9-9317-c5a4cd7369ec', amendment_process_supreme_over_interpretation, conventional).
narrative_ontology:cs_reference_frame('e1cc223f-6a44-43b9-9317-c5a4cd7369ec', article_v_procedural_supremacy).
narrative_ontology:cs_drift_state('e1cc223f-6a44-43b9-9317-c5a4cd7369ec', contemporary_identity_politics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e1cc223f-6a44-43b9-9317-c5a4cd7369ec', '').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, institutional_stability_doctrine).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, judicial_restraint_schools).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, excluded_constitutional_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, article_v_gatekeepers).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, substantive_justice_advocates).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, rule_of_law_as_procedural_regularity).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, separation_of_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Constitution as bound by its formal text and the amendment procedures in Article V. They set the interpretive standard that validity derives from source (enactment procedure), not outcome (moral rightness). Their enforcement of this frame constrains what lower courts and the political branches can argue is constitutionally permissible. Career advancement in formalist schools rewards adherence to procedural validity over substantive justice outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, formalist_judges, agenda_setter,
    institutional, generational, constrained, national).

% Bear the cost when the positivist frame prevents courts from recognizing new or evolving rights grounded in moral principle rather than historical enactment. They argue for outcomes (equal protection of marginalized groups, dignitary interests) that lack textual or originary basis in the Constitution as written. The positivist constraint tells them: 'If your right is not in the text or in the amendment process, it is not constitutionally real.' Exit means ceasing to seek constitutional protection for their claims, which for identity-locked constituencies means abandoning constitutional argument as a frame for justice entirely.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, substantive_justice_advocates, payer,
    powerless, biographical, identity_locked, national).

% Groups whose historical exclusion from the polity meant they could not participate in the original ratification or in prior amendment processes. Under the positivist reading, their claims to constitutional protection lack the historical source-validity formalism requires. They can petition for constitutional amendment, but the amendment process itself has structural barriers (supermajority, veto points) that make remedy through formal enactment extremely costly and time-consuming. The constraint's enforcement keeps their substantive claims formally illegitimate until formally enacted.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, excluded_constitutional_minorities, payer,
    moderate, generational, constrained, national).

% Political actors and institutional positions (Senate, supermajority threshold, state legislatures) whose formal control of the amendment process is vindicated by the positivist reading. The reading makes formal amendment the ONLY legitimate path to constitutional change, which concentrates power in the hands of those who control Article V. They benefit from the constraint because it narrows the avenues through which constitutional meaning can shift to those they control or can block.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, article_v_gatekeepers, beneficiary,
    institutional, generational, analytical, national).

% Courts and judges that hold the living constitutionalist reading would argue for dynamic interpretation of constitutional principles. They are excluded from the conversation conducted under the positivist frame because that frame defines their interpretive moves as illegitimate — not grounded in source-validity. They would contend that constitutional meaning must evolve with contemporary understanding and that judges have legitimate authority to recognize emergent rights. The positivist constraint tells them their reasoning is per se unconstitutional.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, living_constitutionalist_courts, excluded,
    institutional, generational, analytical, national).

% The philosophical tradition grounding the constraint: legitimacy derives from formal source and procedure, not from moral correctness or natural law. Observers (legal theorists, comparative constitutional scholars) can see the constraint's logical structure and its consequences but are not themselves arranged around it as beneficiaries or victims.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legal_positivism_tradition, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(us_constitution_text__positivist_reading, legal_positivism_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__positivist_reading, article_v_gatekeepers).
narrative_ontology:fixing_cost_class(us_constitution_text__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Constrains constitutional interpretation to a verifiable, contentious-outcome-independent standard: what is formally enacted. Solves the coordination problem that if constitutional meaning changes based on judges' moral judgments, every constitutional clause becomes a license for outcome-oriented reasoning, making the Constitution a text that means whatever a sufficient judicial majority votes it means. The coordination is: we agree that the Constitution's authority rests on its source (formal enactment) and its alteration rests on formal amendment, not on judicial reasoning about what justice requires.
% TRANSFER_FUNCTION: Moves authority to change constitutional meaning from courts (and from moral or prudential reasoning) to the political actors and institutions that control the Article V amendment process (Congress, state legislatures, ratification supermajorities). Also transfers interpretive authority from judges deciding what substantive principles the Constitution protects to judges applying the formal text as a constraint on their own reasoning. Transfers the power to recognize new constitutional rights from the judiciary to voters and legislators who can amend the text.
% ABSENT_VOICES: Living constitutionalist judges and scholars, who would argue that constitutional interpretation must track evolving societal understanding and that formal amendment is too rigid a constraint. Substantive justice advocates arguing that moral principle should inform constitutional interpretation. Originalists would add their voice but from a different critical angle — they would argue the positivist reading ignores the historical semantic content of the text, not just its formal validity. Their absence is structural: the positivist frame defines their interpretive moves as procedurally illegitimate regardless of content.
% DISAPPEARANCE_RATIONALE: If the positivist constraint vanished overnight, constitutional courts would immediately resume reasoning from evolving principle, historical principle, and moral content. New constitutional rights would be recognized through common-law constitutional development rather than formal amendment. The Article V amendment process would become less central to constitutional change. Constitutional meaning would become more fluid and contestable. The world would reorganize around a different allocation of authority between courts and political branches, and the amendment process would shift from the gatekeeper of constitutional change to one mechanism among several.
% FOUNDING_PROBLEM: How to prevent the Constitution from becoming a meaningless text whose interpretation is entirely subjective and outcome-determined by judges' personal moral views. How to keep constitutional law rule-bound rather than result-bound.
% FOUNDING_PROBLEM_CORROBORATION: Formalist judges and legal positivists attest the founding problem is live and that the positivist reading solves it. Living constitutionalists and substantive justice advocates attest the founding problem is misconceived — that the real problem is rigidity masquerading as objectivity, and that the positivist frame produces unjust outcomes by precluding courts from recognizing valid constitutional claims. Critical race theorists and scholars of constitutional exclusion attest that the positivist frame is historically weaponized to lock in the constitutional choices of exclusionary majorities.
narrative_ontology:disappearance_verdict(us_constitution_text__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__positivist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the positivist frame systematically excludes entire categories of constitutional claims (those grounded in evolving principle, moral reasoning, or unenumerated substantive values) from the domain of legitimate judicial cognition. Suppression is higher still (0.72) because the constraint's persistence depends actively on judicial self-constraint and the institutional hierarchy enforcing doctrinal conformity — judges who reason from living constitutionalism or natural-law foundations must suppress that reasoning or face institutional sanctions. Theater is moderate (0.41) because a genuine coordination function exists (preventing outcome-oriented jurisprudence) but a growing share of the constraint's operation consists of theatrical adherence to formalism as a professional norm, even as judges find ways to reach desired outcomes through close textual reading. The measurement series shows a steeper rise in the first 15 time points (extractiveness gains 0.14 over this period) coinciding with the Reformation era of originalism and formalism (roughly 2005–2020), then plateau as the doctrine becomes entrenched. The theater ratio rises throughout, suggesting increasing reliance on interpretive theater (finding formal justification for predetermined outcomes) rather than genuine source-validity constraint.
 *
 * PERSPECTIVAL GAP:
 *   The formalist judge and the substantive justice advocate are two seats of radically different power and exit options experiencing the same constraint as opposite types. This is where the tangled_rope structure lives: genuine coordination for the powerful seat, pure extraction for the powerless seat, unified by a single procedural rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Formalist judges sit near the beneficiary end of the directionality spectrum (d~0.1–0.2): the constraint amplifies their institutional authority, constrains junior courts through doctrinal hierarchy, and makes their professional success correlate with adherence to formalism. Substantive justice advocates and excluded minorities sit at the target end (d~0.85–0.95): the constraint forecloses their preferred claims, forces them toward identity-locked choice (abandon constitutional argument or conform to formalism), and makes alternative routes (living constitutionalism, moral reasoning) structurally illegitimate. Article V gatekeepers sit near beneficiary (d~0.15): they benefit from the concentration of constitutional change authority. Living constitutionalist courts are excluded rather than positioned on the directionality axis — they are kept out of the formal conversation the constraint structures. The derivation chain runs: beneficiaries (formalists, stability doctrine) → low d; victims (substantive claimants, excluded minorities) → high d; exit options (constrained for claimants, analytical for judges) → amplifies the target asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing outcome-determined jurisprudence) is live for formalists and fully contested for others. If the problem were dead, the positivist reading would degrade to piton status (theatrical formalism masking outcome-oriented reasoning). Current evidence: the constraint does coordinate against pure outcome jurisprudence — judges do constrain their reasoning to textual and historical sources, and this constraint is not merely theatrical for most cases. However, the theater ratio has risen (0.28 → 0.41 over the interval), suggesting that as the constraint becomes entrenched, more interpretive energy goes to making predetermined outcomes look formally justified rather than discovering constraints from textual sources. Mandatrophy is not yet resolved: the coordination function persists (formalism does constrain reasoning and prevent the Constitution from becoming infinitely malleable), but the question of whether that coordination is worth its extraction cost is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_validity_vs_moral_legitimacy,
    'Can a constitution maintain legitimacy if it is formally valid but morally deeply unjust? Does the positivist reading''s separation of validity from moral content constitute a legitimate constraint, or is it a cover for locking in unjust historical choices?',
    'Historical comparison: do constitutions that abandon formal validity (accepting interpretive flexibility without amendment) experience loss of rule-of-law stability, or do they achieve greater substantive justice with comparable predictability? Empirical study of constitutional flux in democracies that permit non-amendment change.',
    'If formal validity is decoupled from legitimacy, the positivist reading is exposed as a false summit — it appears natural (valid = formal) but actually extracts from those whose justice claims it forecloses. If moral legitimacy is necessary for stability, the extraction cost of the positivist frame is visible and contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_validity_vs_moral_legitimacy, empirical, 'Whether formal validity correlates with constitutional legitimacy or whether legitimacy requires moral content.').

omega_variable(
    article_v_impossibility,
    'Does the Article V amendment process have structural barriers so high that it functions as a permanent foreclosure of constitutional change for powerless groups without elite consensus? If so, is the positivist reading''s insistence on Article V as the sole legitimate path to constitutional change itself extractive?',
    'Analysis of Article V failure rates and supermajority requirements; comparison with amendment rates in other federalist systems; study of which groups have successfully achieved constitutional amendment and which have been permanently blocked.',
    'If Article V is structurally unreachable, then the positivist reading creates a permanent class of constitutionally illegitimate claims from groups that cannot command supermajorities. The constraint would be unambiguously snare-class for those groups (pure extraction with no exit). If Article V is occasionally achievable, the constraint remains tangled_rope (coordination for formalists, extraction for claimants, but theoretically reversible through amendment).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_v_impossibility, empirical, 'Whether Article V amendment is procedurally available to groups advocating substantive change or structurally foreclosed.').

omega_variable(
    positivism_vs_originalism_decomposition,
    'Is the positivist reading logically distinct from originalism, or does it reduce to originalism in practice? Both constrain judges to non-outcome-determined reasoning; does the distinction between ''formal validity'' and ''original public meaning'' maintain coherence, or do formalist judges collapse into searching for original meaning?',
    'Jurisprudential analysis of Supreme Court opinions classified as formalist/positivist vs. originalist: do they differ in methodology and constraint, or do they converge? If they converge, the two are aspects of a single constraint, not two readings.',
    'If they are distinct, two separate constraint stories are warranted. If they collapse, the positivist reading may be analyzable as a variant or interpretive layer of originalism, and the kernel decomposition should be reconsidered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivism_vs_originalism_decomposition, conceptual, 'Whether positivism and originalism are structurally distinct constraints or collapsed into one.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (external enforcement through career incentives, doctrinal hierarchy, institutional sanctions) or internalized (judges accepting the positivist frame as legitimate and suppressing non-conforming reasoning from within)?',
    'Post-exit study: judges who leave the federal bench and publish scholarship; changes in reasoning patterns pre- vs. post-retirement; interviews documenting whether suppression persists after institutional enforcement mechanisms are removed.',
    'If internalized, the constraint''s effective suppression is higher than institutional measurement suggests — targets carry the suppression with them beyond the constraint. If structural, the constraint would relax if institutional enforcement were removed. For identity-locked substantive advocates, the presence of internalized suppression indicates the constraint has deeper roots than institutional coercion alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether judicial suppression of non-positivist reasoning is institutional coercion or internalized norm.').

omega_variable(
    kernel_committer_framing,
    'Does the positivist reading truly instantiate a distinct kernel reading, or does it represent the same kernel assessed under a different evaluative frame (rule-of-law stability vs. substantive justice)? That is, are the three readings alternative interpretations of the Constitution, or are they alternative criteria for evaluating constitutional legitimacy?',
    'Structural analysis: do the three readings disagree on what the Constitution IS (kernel level), or on whether that thing is GOOD (evaluative level)? If the former, they are genuine readings; if the latter, they may be perspectives on a single constraint rather than constraints from different readings.',
    'If they are alternative evaluative frames, the committer framing should be reconsidered and possibly decomposed into separate kernel stories and observer-axis stories. If they are genuine readings (different instantiations of the kernel), the current approach holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_framing, conceptual, 'Whether the three readings are alternative instantiations of the kernel or alternative evaluation frames for the same constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__positivist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(us_c_tr_t5, us_constitution_text__positivist_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_text__positivist_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_text__positivist_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_text__positivist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(us_c_tr_t25, us_constitution_text__positivist_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_text__positivist_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(us_c_tr_t35, us_constitution_text__positivist_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_text__positivist_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__positivist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(us_c_be_t5, us_constitution_text__positivist_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(us_c_be_t10, us_constitution_text__positivist_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(us_c_be_t15, us_constitution_text__positivist_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(us_c_be_t20, us_constitution_text__positivist_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(us_c_be_t25, us_constitution_text__positivist_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(us_c_be_t30, us_constitution_text__positivist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(us_c_be_t35, us_constitution_text__positivist_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(us_c_be_t40, us_constitution_text__positivist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__positivist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(us_c_su_t5, us_constitution_text__positivist_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(us_c_su_t10, us_constitution_text__positivist_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(us_c_su_t15, us_constitution_text__positivist_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(us_c_su_t20, us_constitution_text__positivist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(us_c_su_t25, us_constitution_text__positivist_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(us_c_su_t30, us_constitution_text__positivist_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(us_c_su_t35, us_constitution_text__positivist_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(us_c_su_t40, us_constitution_text__positivist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, article_v_amendment_supermajority_requirement).

% DUAL FORMULATION NOTE:
% The us_constitution_text kernel has three structurally distinct readings: positivist (this story), originalist, and living_constitutionalist. Each reading produces a different constraint with different beneficiaries, victims, and extraction profiles. The positivist reading constrains interpretation to formal source validity and Article V amendment; originalism adds the historical-meaning constraint; living constitutionalism permits evolution without amendment. The three are linked as a kernel family and affect each other: the dominance of one reading in judicial orthodoxy (positivism or originalism) constrains the viability of competing readings. Each reading's network.affects_constraints includes the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__positivist_reading, powerless, 0.92).
constraint_indexing:directionality_override(us_constitution_text__positivist_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
