% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Democratic Contestation Over Judicial Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   Popular constitutionalism is a contested reading of how the U.S.
 *   Constitution's meaning should be determined. It asserts that
 *   constitutional interpretation should not be the exclusive province of
 *   courts (judicial supremacy) but rather should emerge from sustained
 *   contestation among courts, legislatures, executive branches, and popular
 *   movements. The reading challenges the professional legal establishment's
 *   interpretive monopoly and distributes authority to democratic forces.
 *   This constraint story models popular constitutionalism as a framework
 *   that coordinates interpretive authority while asymmetrically extracting
 *   stability and judicial finality from those who depend on
 *   counter-majoritarian constitutional protection. The claimed type is
 *   tangled_rope: it provides a genuine coordination function (multiple
 *   legitimate interpreters) but requires active suppression of the
 *   originalist and judicial-supremacist alternatives, and it extracts from
 *   judicial institutions and minority constituencies who lose the insulation
 *   that hierarchical authority provided.
 *
 * KEY AGENTS:
 *   - popular_movements: claim interpretive co-authority with courts through sustained mobilization
 *   - legislative_majorities: gain authority to interpret the Constitution through statutory and amendment processes
 *   - anti_establishment_coalitions: benefit from decentralized, democratized interpretive authority
 *   - judicial_supremacists: lose institutional monopoly on constitutional interpretation
 *   - minority_rights_constituencies: lose counter-majoritarian protection that judicial supremacy provided
 *   - constitutional_stability_dependents: bear the cost of continuous contestation rather than settled doctrine
 *   - constitutional_courts: remain formal arbiters but must defend authority against ongoing challenge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.68).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.72).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism: Democratic Contestation Over Judicial Interpretation").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, 'a00ea6eb-a55a-46e2-9efa-61ddcf6c371b').
narrative_ontology:cs_kernel_codification('a00ea6eb-a55a-46e2-9efa-61ddcf6c371b', fixed_text).
narrative_ontology:cs_authority_grounding('a00ea6eb-a55a-46e2-9efa-61ddcf6c371b', distributed).
narrative_ontology:cs_reading_relation('a00ea6eb-a55a-46e2-9efa-61ddcf6c371b', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a00ea6eb-a55a-46e2-9efa-61ddcf6c371b', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('a00ea6eb-a55a-46e2-9efa-61ddcf6c371b', foundational, constitutional_meaning_democratically_contestable).
narrative_ontology:cs_axiom_status(constitutional_meaning_democratically_contestable, holdable).
narrative_ontology:cs_axiom_grounding('a00ea6eb-a55a-46e2-9efa-61ddcf6c371b', constitutional_meaning_democratically_contestable, deontological).
narrative_ontology:cs_axiom('a00ea6eb-a55a-46e2-9efa-61ddcf6c371b', foundational, interpretive_authority_distributed_across_branches_and_movements).
narrative_ontology:cs_axiom_status(interpretive_authority_distributed_across_branches_and_movements, holdable).
narrative_ontology:cs_axiom_grounding('a00ea6eb-a55a-46e2-9efa-61ddcf6c371b', interpretive_authority_distributed_across_branches_and_movements, conventional).
narrative_ontology:cs_reference_frame('a00ea6eb-a55a-46e2-9efa-61ddcf6c371b', popular_democratic_constitutionalism).
narrative_ontology:cs_drift_state('a00ea6eb-a55a-46e2-9efa-61ddcf6c371b', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a00ea6eb-a55a-46e2-9efa-61ddcf6c371b', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_establishment_coalitions).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_supremacists).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, minority_rights_constituencies).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_stability_dependents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, minority_rights_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Social movements (civil rights, labor, progressive, conservative, populist) claim interpretive authority over constitutional meaning through sustained political mobilization, constitutional amendment campaigns, and electoral pressure on branches. They assert that constitutional meaning should shift with popular will rather than remain fixed by judicial pronouncements. They gain access to constitutional meaning-making outside the courts and can reshape foundational commitments through sustained struggle.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, beneficiary,
    organized, generational, mobile, national).

% Congress and state legislatures gain interpretive authority in the popular constitutionalist reading: they can enact statutes that embody their own constitutional interpretations, and sustained legislative majorities can reshape the constitutional landscape through law and amendment without deferring entirely to judicial doctrine. They escape the constraint of judicial supremacy.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, biographical, constrained, national).

% Coalitions that oppose entrenched institutional authority structures (judicial elites, constitutional scholars, Washington consensus) gain legitimacy for claiming that 'the people' should directly shape constitutional interpretation through popular action rather than waiting for courts to pronounce settled doctrine. Anti-elitist movements find in popular constitutionalism a theoretical framework that decentralizes interpretive authority.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_establishment_coalitions, beneficiary,
    moderate, biographical, mobile, national).

% Courts, constitutional scholars, and institutional actors who defend the position that judicial pronouncements on constitutionality should be treated as binding constitutional settlement. The popular constitutionalist framework directly challenges their authority: it treats Supreme Court doctrine as one voice in an ongoing contest rather than as the final word. They bear the cost of contested authority and must continuously defend judicial finality.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_supremacists, payer,
    institutional, generational, constrained, national).

% Racial minorities, religious minorities, LGBTQ populations, and other groups that historically depended on counter-majoritarian judicial protection face a structural dilemma under popular constitutionalism: the framework distributes interpretive authority to majorities and popular movements, but those same majorities historically opposed minority rights protections. They pay the cost of losing the insulation that judicial supremacy provided while gaining theoretical access to constitutional contestation that may not benefit them in practice.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, minority_rights_constituencies, payer,
    powerless, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, minority_rights_constituencies, beneficiary).

% Economic actors, property holders, contractual parties, and institutional arrangements that depend on stable, predictable constitutional settlement. They bear the cost of continuous constitutional contestation: if constitutional meaning shifts with popular movements rather than settling through judicial doctrine, long-term planning and property security become more uncertain. Interstate commerce, tax policy, and regulatory schemes all depend on some settled constitutional baseline.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_stability_dependents, payer,
    powerful, civilizational, constrained, national).

% Constitutional scholars and judges who defend original public meaning or original intent are excluded from the popular constitutionalist framework: they would argue that constitutional meaning should be fixed by historical fidelity, not democratized through popular contestation. Their exclusion is deliberate — the reading treats originalism as an elite hermeneutic strategy that resists democratic contestation of meaning.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, originalist_scholars, excluded,
    institutional, generational, constrained, national).

% Courts (especially the Supreme Court) remain the formal arbiters of constitutional disputes and the authors of binding doctrine, even under a popular constitutionalist reading. However, their authority is now contested: they must defend their interpretations against popular movements that claim co-equal authority, and they face pressure from democratic mobilization that treats judicial pronouncements as provisional rather than final. They must actively maintain their interpretive authority against ongoing challenge.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_courts, agenda_setter,
    institutional, generational, trapped, national).

% Legal scholars, political theorists, and historians analyze the popular constitutionalist framework, document instances of popular constitutional interpretation, and theorize the conditions under which popular movements successfully reshape constitutional meaning. They observe the constraint's operation and produce interpretive analysis that feeds back into the contest.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_theorists, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for resolving constitutional contests when branches disagree and courts are contested: rather than treating judicial pronouncements as final, the reading coordinates on the process of ongoing popular contestation and movement politics as legitimate sites of constitutional meaning-making. It coordinates multiple interpretive authorities (courts, legislatures, movements, executive, popular action) around the principle that all are legitimate contributors to constitutional meaning.
% TRANSFER_FUNCTION: Redistributes interpretive authority away from courts and constitutional specialists toward popular movements, legislative majorities, and anti-establishment coalitions. It moves the power to authoritatively declare constitutional meaning from a concentrated institutional seat (federal courts) to a distributed, contentious process (popular struggle, electoral pressure, movement mobilization). The transfer is of authority and legitimacy, not material resources, but it carries material consequences for policy and rights.
% ABSENT_VOICES: Constitutional minorities, non-mobilized constituencies, and those dependent on counter-majoritarian judicial protection have limited voice in popular mobilization processes; their absence from movement politics means the popular constitutionalist framework systematically excludes or deprioritizes their claims. Originalists and defenders of judicial supremacy are deliberately excluded from the framework — they would argue for fixed meaning and institutional hierarchy, but the reading treats their arguments as elite resistance to democratization.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism as a legitimate reading disappeared overnight, constitutional contestation would not stop, but its legitimacy structure would shift: challenges to judicial doctrine would revert to being treated as violations of constitutional settlement rather than as valid democratic contestation; amendment campaigns and legislative resistance would lose theoretical grounding in constitutional authority and would be framed instead as efforts to change (not interpret) the Constitution; the framework that grants legitimacy to popular movements as constitutional interpreters would collapse, returning authority concentration to courts and formal amendment procedures.
% FOUNDING_PROBLEM: Judicial supremacy over constitutional interpretation concentrates interpretive authority in an unelected institution insulated from democratic pressure; this excludes popular constituencies from meaningful participation in determining constitutional meaning and creates a legitimacy gap between who makes constitutional law (courts) and who is governed by it (the people). Popular constitutionalism was developed to resolve this gap by asserting that constitutional meaning should be democratically contestable.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars working in popular constitutionalism traditions (Kramer, Sunstein, Tushnet, Kapur) and historians of social movements documenting instances where popular mobilization reshaped constitutional meaning (civil rights movement's impact on Fourteenth Amendment interpretation, labor movement's challenge to Lochner doctrine) attest the founding problem remains live. Court-watchers and democratic theorists outside the judicial academy confirm that legitimacy contests over who gets to interpret the Constitution persist. Conservative originalists and progressive living-constitutionalists dispute whether popular constitutionalism is a solution or a threat, but none deny the founding problem's reality.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.68) because the framework redistributes authority away from concentrated institutional seats toward popular movements, benefiting anti-establishment coalitions while harming those dependent on judicial finality and minority rights protection. The asymmetry is structural: beneficiaries gain authority; victims lose institutional insulation. Suppression is high (0.72) because maintaining popular constitutionalism as a legitimate framework requires actively suppressing originalist and judicial-supremacist arguments — the reading must defend against the claim that constitutional meaning should be fixed and authoritatively pronounced by courts, and that defense is an active, ongoing enforcement process. Theater is moderate (0.41) because popular constitutionalism includes genuine contestation and movement activity, but a significant portion of the framework's maintenance involves performative invocation of 'the people's will' without actual popular mobilization. The measurement series shows extractiveness and suppression rising from t0 to t32 as the reading became more widely adopted in legal scholarship (Obama era forward, 2010–2020), then plateauing as the reading reached stable adoption in some academic circles. The trajectory reflects the reading's history: early adoption among progressive legal scholars, institutional resistance from originalists and formalists, gradual mainstreaming in constitutional theory, followed by asymptotic stabilization as institutional structures adapted.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (judicial supremacists) experiences the popular constitutionalist reading as a delegitimization of their authority structure and a return to flux in constitutional meaning. From their position, Marbury v. Madison's settlement (courts pronounce constitutional law authoritatively) is being dissolved in favor of a contestatory model that recreates the antebellum politics of constitutional interpretation. The beneficiary seats experience it as liberation from elite judicial control. The minority-rights seat experiences a tragic paradox: they are theoretically elevated to co-interpreters with equal stakes in constitutional meaning-making, but majorities now have a framework that legitimizes their direct contestation of minority-protecting doctrines. This perpectival gap maps to different computed classification outcomes per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular movements and legislative majorities are beneficiaries with moderate to high power and mobile to constrained exit: they can claim interpretive authority without losing access to courts. Their directionality is low, trending toward beneficiary. Judicial supremacists are payers (they lose interpretive monopoly) with institutional power and constrained exit (courts cannot abandon constitutional interpretation): their directionality is high, trending toward target. Minority-rights constituencies are the crux: they are nominally beneficiaries in the popular constitutionalist reading (they gain voice in constitutional contestation), but they are victims in practice (majorities can now directly challenge the constitutional protections courts granted them). This dual positioning creates the suppression asymmetry: suppression is required to keep the minority-rights voice from being drowned out by majoritarian contestation, but that suppression is inconsistently applied in the framework itself. The reading does not solve the paradox — it theorizes interpretive authority democratically while leaving minorities structurally exposed. Directionality overrides were not necessary here: the beneficiary/victim declarations and exit-option analysis capture the structural relationships faithfully.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (concentration of constitutional authority in courts) remains live and is actively contested in contemporary legal scholarship. The popular constitutionalist reading does not resolve it; rather, it reframes it as structural inevitability — constitutional meaning-making cannot be monopolized because popular movements and legislative majorities will always contest court pronouncements. The constraint's mandatrophy status is therefore 'live but intractable': the problem the reading was built to solve (democratic legitimacy of constitutional authority) cannot be solved by the framework itself because that solution would require actually removing courts from constitutional interpretation, which the reading does not do. Courts remain the formal final arbiters even as their authority is theoretically delegitimized. This creates a permanent extraction condition: the framework extracts stability and finality without providing an alternative institutional seat for constitutional settlement. The reading coordinates on contestation itself rather than on resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_capture_risk,
    'Does distributing interpretive authority to popular movements and majorities protect minority constitutional rights, or does it expose them to majoritarian revision?',
    'Historical analysis of instances where popular movements successfully contested constitutional protections for minorities (e.g., the pre-Civil War movement to protect slavery in the Constitution, the mid-20th-century white-supremacist movements contesting civil rights protections). Evaluation of whether the popular constitutionalist framework legitimizes such contestation.',
    'If majoritarian capture is the probable outcome, the constraint is a snare (pure extraction from minorities), not a tangled rope. If the framework successfully insulates minority rights even while democratizing other interpretive authority, the coordination function is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(majoritarian_capture_risk, empirical, 'Whether popular constitutionalism protects minority rights or exposes them.').

omega_variable(
    institutional_boundary_ambiguity,
    'What counts as ''popular constitutional interpretation''? Where is the boundary between legitimate popular contestation and illegitimate violation of constitutional settlement?',
    'Analysis of how popular constitutionalist theorists and practitioners have drawn this boundary in concrete cases. Evaluation of whether the boundary is principled or merely convenience.',
    'If the boundary is ambiguous, suppression is higher (maintaining the reading requires continuous policing of what counts as legitimate contestation). If the boundary is clear, suppression requirements decrease. The reading''s stability depends on resolving this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_boundary_ambiguity, conceptual, 'The scope and legitimacy criteria of popular constitutional interpretation.').

omega_variable(
    judicial_authority_paradox,
    'Is the reading internally consistent? If courts retain final authority to pronounce constitutional doctrine, do they retain ''judicial supremacy'' even if theoretically contested by popular movements?',
    'Clarification of what the reading means by ''popular constitutional interpretation'' when courts still have the institutional power to override it. Evaluation of whether the reading''s core claim (that interpretation is democratically contestable rather than judicially supreme) is genuinely operative or merely rhetorical.',
    'If courts retain de facto supremacy while losing de jure authority, the reading is a theater-heavy piton (performative contestation without actual power redistribution). If courts genuinely lose authority to movements and legislatures, the constraint is as described.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_authority_paradox, conceptual, 'Whether popular constitutionalism actually redistributes authority or merely contests its exercise.').

omega_variable(
    reading_sibling_foreclosure,
    'Does the popular constitutionalist reading logically foreclose originalism and judicial supremacy, or do they coexist as competing but coherent claims?',
    'Philosophical analysis of whether a single constitutional framework can simultaneously hold that meaning is (a) fixed at ratification / evolving with values (originalism/living constitutionalism) AND (b) democratically contestable through popular movements (popular constitutionalism). Evaluation of whether these premises are contradictory or compatible.',
    'If foreclosure is true, the three readings are mutually exclusive frames for the same kernel — only one can be correct. If coexistence is true, the readings are different institutional voices in an unresolved contest. This affects the relation classification between readings in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure, conceptual, 'Whether sibling readings foreclose or coexist within coherent constitutional frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t8, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(us_c_tr_t8, observed).
narrative_ontology:measurement(us_c_tr_t16, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(us_c_tr_t16, observed).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(us_c_tr_t24, observed).
narrative_ontology:measurement(us_c_tr_t32, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(us_c_tr_t32, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(us_c_tr_t40, projected).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(us_c_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t8, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(us_c_be_t8, observed).
narrative_ontology:measurement(us_c_be_t16, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(us_c_be_t16, observed).
narrative_ontology:measurement(us_c_be_t24, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement_basis(us_c_be_t24, observed).
narrative_ontology:measurement(us_c_be_t32, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(us_c_be_t32, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(us_c_be_t40, projected).
narrative_ontology:measurement(us_c_be_t50, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(us_c_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t8, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(us_c_su_t8, observed).
narrative_ontology:measurement(us_c_su_t16, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(us_c_su_t16, observed).
narrative_ontology:measurement(us_c_su_t24, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(us_c_su_t24, observed).
narrative_ontology:measurement(us_c_su_t32, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement_basis(us_c_su_t32, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(us_c_su_t40, projected).
narrative_ontology:measurement(us_c_su_t50, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(us_c_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__popular_constitutionalism_reading, 0.14).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).

% DUAL FORMULATION NOTE:
% The us_constitution_interpretive kernel decomposes into three structurally distinct constraints corresponding to three readings: originalist_reading (meaning fixed at ratification; mountain candidate with high accessibility collapse), living_constitution_reading (meaning evolves with societal values; rope or tangled rope), and this story (popular_constitutionalism_reading — meaning emerges from democratic contestation; tangled rope with moderate extraction and high suppression). All three share the same kernel text (the Constitution) but carry different ε values because they make different empirical and normative claims about authority, settlement, and legitimacy. The network links them because sibling readings causally influence each other: the rise of living constitutionalism as a mainstream reading created space for popular constitutionalism to challenge both originalism and judicial supremacy. Each reading's adoption creates downstream pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
