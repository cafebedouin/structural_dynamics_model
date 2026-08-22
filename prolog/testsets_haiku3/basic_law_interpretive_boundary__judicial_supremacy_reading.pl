% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Basic Laws as Judicially Enforceable Constitutional Constraint (Judicial Supremacy Reading)
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   Under the judicial supremacy reading of the Basic Laws, the Israeli
 *   Supreme Court interprets the Basic Laws as a judicially enforceable
 *   constitutional framework that binds the Knesset. Legislation
 *   contradicting the Court's reading of the Basic Laws can be invalidated.
 *   This reading emerged gradually through landmark decisions (1995 Bank
 *   Hamizrahi, 2000 Adalah, and others) in which the Court asserted its
 *   authority to review the constitutionality of Knesset legislation against
 *   the Basic Laws. The constraint structures the relationship between the
 *   Court (as interpreter and enforcer) and the Knesset (as a legislative
 *   body whose enactments are subject to judicial review). Rights-claimants
 *   benefit by gaining access to constitutional protection through
 *   litigation; the Knesset majority bears extraction in the form of
 *   legislative invalidation and the persistent threat of nullification. This
 *   reading is one of three structurally distinct interpretations of the same
 *   kernel (the Basic Laws as foundational commitments): the judicial
 *   supremacy reading, the parliamentary sovereignty reading (Knesset holds
 *   final authority to interpret and amend the Basic Laws), and the balanced
 *   contestation reading (both institutions hold bounded, legitimate
 *   authority within their respective domains).
 *
 * KEY AGENTS:
 *   - Supreme Court institution: enforces the Basic Laws, invalidates contradictory legislation, interprets the boundary between constitutional and unconstitutional acts
 *   - Knesset legislative majority: enacts legislation subject to judicial review, bears the extraction of legislative nullification and constrained authority
 *   - Rights-claimants (petitioners): gain a veto mechanism against legislation they view as rights-violating, beneficiary seat but dependent on Court agreement
 *   - Parliamentary sovereignty advocates: excluded from institutional effect, would impose alternative reading if institutional balance shifted
 *   - International observers: assess legitimacy by reference to human rights law and comparative practice, do not directly control the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.72).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Basic Laws as Judicially Enforceable Constitutional Constraint (Judicial Supremacy Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, 'e88e293c-08ed-4553-a47f-5810fcac8076').
narrative_ontology:cs_kernel_codification('e88e293c-08ed-4553-a47f-5810fcac8076', fixed_text).
narrative_ontology:cs_authority_grounding('e88e293c-08ed-4553-a47f-5810fcac8076', lineage).
narrative_ontology:cs_interpretation_layer_present('e88e293c-08ed-4553-a47f-5810fcac8076').
narrative_ontology:cs_reading_relation('e88e293c-08ed-4553-a47f-5810fcac8076', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('e88e293c-08ed-4553-a47f-5810fcac8076', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('e88e293c-08ed-4553-a47f-5810fcac8076', foundational, supreme_court_final_interpreter).
narrative_ontology:cs_axiom_status(supreme_court_final_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('e88e293c-08ed-4553-a47f-5810fcac8076', supreme_court_final_interpreter, deontological).
narrative_ontology:cs_axiom('e88e293c-08ed-4553-a47f-5810fcac8076', foundational, basic_laws_judicially_enforceable).
narrative_ontology:cs_axiom_status(basic_laws_judicially_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('e88e293c-08ed-4553-a47f-5810fcac8076', basic_laws_judicially_enforceable, deontological).
narrative_ontology:cs_reference_frame('e88e293c-08ed-4553-a47f-5810fcac8076', judicially_enforceable_constitution).
narrative_ontology:cs_drift_state('e88e293c-08ed-4553-a47f-5810fcac8076', contemporary_parliamentary_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e88e293c-08ed-4553-a47f-5810fcac8076', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_institution).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_majorit).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, legislature_future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Basic Laws as a judicially enforceable constitutional framework and invalidates Knesset legislation that contradicts its reading of those Laws. Controls the boundary between legislation it deems constitutional and legislation it nullifies. Justifies its authority as guardian of foundational rights and constitutional structure. The institution's legitimacy and scope depend on maintaining this interpretive monopoly.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_institution, agenda_setter,
    institutional, generational, mobile, national).

% Gain a veto mechanism against legislation they view as rights-violating by petitioning the Court for judicial review. Their access to constitutional protection depends entirely on the Court's willingness to hear their case and accept their interpretation of the Basic Laws. They benefit from the Court's supremacy only when the Court agrees with their reading.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants, beneficiary,
    powerless, biographical, constrained, national).

% Bears the extraction in the form of legislative invalidation: laws it enacts can be nullified by the Court, limiting the scope of majority rule. The legislature retains the formal authority to legislate but faces the persistent threat that its enactments will be struck down if the Court disagrees with their constitutional status. Amending the Basic Laws themselves faces the same risk: if an amendment contradicts what the Court deems the core of the Basic Laws, the Court may nullify the amendment itself.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_majorit, payer,
    institutional, biographical, constrained, national).

% Future legislatures bear the long-term constraint: once the Court locks in an interpretation of the Basic Laws, changing that interpretation requires not just a new legislative majority but overcoming the Court's accumulated doctrine and resistance to overthrowing precedent. They are excluded from the initial framing of the Basic Laws' meaning.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, legislature_future_generations, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, legislature_future_generations, excluded).

% Would argue for Knesset supremacy in constitutional interpretation but are structurally barred from imposing that reading on the system so long as the Supreme Court maintains its interpretive authority. Their alternative reading of the Basic Laws is excluded from institutional effect unless a crisis or constitutional reform shifts the institutional balance.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, parliamentary_sovereignty_advocates, excluded,
    moderate, biographical, trapped, national).

% Judge the legitimacy of the judicial supremacy arrangement by reference to international human rights law and comparative constitutional practice. Their assessments influence external pressure on Israel's institutions but do not directly control the constraint's operation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_institution).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable interpretive framework for the Basic Laws: one authoritative institution (the Supreme Court) interprets foundational rights and constitutional structure, preventing legislative majorities from fragmenting constitutional meaning through ad-hoc statutory changes. Provides rights-claimants with a predictable, single forum for constitutional claims rather than leaving such disputes to electoral cycles and shifting legislative coalitions.
% TRANSFER_FUNCTION: Moves constitutional authority from the Knesset (as the elected body holding final legislative power) to the Supreme Court (as the institution holding final power to invalidate legislation). The Court collects the authority to nullify legislation in its own name and on its own interpretation; the Knesset loses the ability to enact laws it believes constitutional without facing judicial strike-down; rights-claimants gain the ability to override legislative choices via litigation.
% ABSENT_VOICES: Parliamentary majority members whose legislative agenda is blocked by the Court have no structural recourse except constitutional amendment, which the Court may itself veto if the amendment is deemed to violate the core of the Basic Laws. Unorganized publics who prefer legislative over judicial governance are excluded from institutional effect. Legislatures of future generations are bound by today's Court decisions and have limited ability to revise the Court's constitutional reading.
% DISAPPEARANCE_RATIONALE: If the judicial supremacy constraint vanished—if the Court lost authority to invalidate legislation—the legislative majority would immediately reclaim control over constitutional meaning. Laws the Court had struck down would be re-enacted; Basic Laws could be amended or superseded by simple legislation; rights-claimants would lose their veto mechanism. Constitutional stability would depend entirely on legislative self-restraint rather than judicially enforceable limits. The distribution of political power would shift sharply toward the sitting Knesset majority.
% FOUNDING_PROBLEM: Prior to judicial review of the Basic Laws, the Basic Laws existed as formal documents without institutional enforcement: a legislative majority could effectively rewrite them through ordinary legislation without formal amendment. No constitutional hierarchy existed to constrain majority rule or protect minorities from legislative overreach on foundational rights.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court and rights-advocacy organizations attest the founding problem persists: without judicial enforcement, legislative majorities could unilaterally alter or evade the Basic Laws' protections. Parliamentary sovereignty advocates contest this, arguing the problem is overstated and that the cure (judicial supremacy) creates worse problems. Comparative constitutional scholars outside Israel note that most democracies have adopted some form of constitutional review, supporting the Court's framing that this problem is real and widespread.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the Court's authority to invalidate legislation transfers significant constitutional authority from the elected Knesset to an appointed court. The extraction accumulates over time (0.45 → 0.68) as the Court's doctrine solidifies and legislative majorities internalize the constraint that certain legislation will be struck down. Suppression is high (0.72) because the constraint's persistence depends on the Court's institutional will to maintain its interpretive monopoly and actively defend against legislative attempts to override or circumvent it (e.g., via supermajority amendment clauses or sunset procedures). Theater ratio is relatively low (0.28) because the Court's invalidation authority is genuinely functional—it actually prevents legislation from taking effect—rather than merely performative; however, the theater ratio rises over time (0.12 → 0.28) as the Court increasingly emphasizes the performative aspects of judicial independence and the rhetorical defense of its role against parliamentary pressure. Accessibility collapse is high (0.79) because once the Court claims authority to invalidate legislation, alternatives for a legislative majority seeking to enact constitutionally controversial measures largely collapse: supermajority requirements, amendment procedures, and explicit statutory language all remain subject to judicial review. Resistance is moderate (0.58) because some parliamentary majorities and constitutional scholars contest the constraint's legitimacy, arguing for parliamentary sovereignty, but institutional inertia and international legitimacy support the Court's position.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's institutional perspective, the constraint is genuine coordination solving the foundational-law problem: without judicial enforcement, Basic Laws become advisory rather than binding. From the Knesset majority's perspective (especially when the Court strikes down legislation the majority deems constitutional), the constraint is pure extraction: the Court unilaterally imposes its interpretation and prevents the elected body from governing according to its understanding of the constitution. Rights-claimants experience the constraint asymmetrically: it is coordination when they prevail in litigation (Court strikes down legislation they opposed) and mere exposure to judicial discretion when they lose (Court upholds legislation they challenged). International observers experience it as benign constitutional review; domestic parliamentary advocates experience it as institutional overreach. These perspectival divergences follow from the fact that the constraint consolidates interpretive authority in one institution and makes that institution's reading binding on the others—a structural asymmetry that different seats read very differently.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court as agenda-setter holds d near 0.0 (full beneficiary): it collects the authority to nullify legislation in its own name and on its own reading; its exit is mobile (the Court could theoretically refuse to exercise review authority, but institutional self-interest makes that unlikely). Rights-claimants hold d near 0.3 (partial beneficiary): they gain the veto mechanism but only when the Court agrees with their reading; they are constrained by whether the Court deems their claimed rights cognizable under the Basic Laws. The Knesset majority holds d near 0.8 (substantial target): it bears the extraction of legislative invalidation and operates under the persistent threat of nullification; its exit is constrained (it cannot unilaterally override the Court's constitutional judgment without constitutional amendment, which the Court may itself veto). Future legislatures hold d near 0.95 (near-total target): they inherit the Court's accumulated doctrine and face the hardest barrier to revising the Court's interpretation. Parliamentary sovereignty advocates hold d near 0.85 (high target): their alternative reading is excluded from institutional effect. The directionality profile shows a strong concentration of extraction on legislative authority and a corresponding concentration of benefit in the Court and rights-claimants (when they align with the Court's reading).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not resolved. The founding problem (unenforceable Basic Laws) is live and the disappearance verdict is world_rearranges: the constraint's persistence clearly depends on active maintenance by the Supreme Court, not inertia. The constraint is a tangled_rope, not a zombie arrangement. However, there is a secondary mandatrophy risk: if future legislatures come to believe that the Court's reading of the Basic Laws no longer reflects the actual foundational commitments (that the Court has become a tool for imposing judicial preferences rather than guarding genuine constitutional limits), the constraint's legitimacy would erode and the Knesset might seek to reassert its authority through supermajority amendment or institutional confrontation. The theater ratio's rise (0.12 → 0.28) suggests increasing performative maintenance of the Court's role, which could signal mandate drift; however, extractiveness also rises, indicating the constraint continues to have genuine functional effect. The constraint remains a contested arrangement whose mandate depends on continued belief that the Basic Laws represent genuine foundational commitments requiring judicial protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_contingency,
    'Do the Basic Laws represent genuine foundational commitments that require protection from legislative majorities, or are they codifications of currently-fashionable political commitments that should be revisable by ordinary legislation?',
    'Long-term institutional stability test: if legislatures consistently attempt to override or reinterpret the Basic Laws and their failure to do so generates persistent crisis (attempted constitutional amendment, institutional confrontation, legislative supermajority initiatives), the problem is live and the constraint addresses it. If legislatures accept the Court''s guardianship and the arrangement stabilizes without crisis, the problem may be nominal or the arrangement may be legitimacy-maintaining performance rather than necessity-addressing constraint.',
    'If the founding problem is contingent on current political consensus rather than structural necessity, the constraint''s type would shift from tangled_rope (solving a real coordination problem with extraction) toward snare (pure extraction with coordination story as cover). The difference is whether the Basic Laws genuinely need protection from majorities or whether the Court uses them as a lever for institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_contingency, conceptual, 'Whether the foundational-law problem is structural or contingent on current consensus.').

omega_variable(
    judicial_supremacy_vs_balance,
    'Is the constraint structurally a monopoly on constitutional interpretation (the Court is the sole final arbiter), or does it represent a balanced division of labor where both the Court and Knesset hold legitimate constitutional authority within their respective domains?',
    'Empirical observation of institutional practice: does the Court treat the Knesset''s constitutional judgments as binding within any domain, or does it reserve final say on all constitutional questions? Do legislative supermajorities ever impose constitutional change that the Court accepts as outside its review authority? If yes, the system is balanced; if no, it is monopolistic.',
    'Under this reading, the constraint is authored as judicial supremacy (the Court holds final authority). If the empirical practice is actually balanced (the Knesset retains constitutional authority in certain domains), the constraint''s ε would be lower and its type would shift toward rope rather than tangled_rope. The authored axiom (supreme_court_final_interpreter) would become contested rather than holdable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_supremacy_vs_balance, empirical, 'Whether the Court''s authority is monopolistic or balanced with the Knesset''s.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression structural (the Court has genuine institutional power to nullify legislation and legislatures fear that power) or internalized (legislatures have come to believe the Court''s reading is correct and self-censor accordingly)?',
    'Examine instances where the Court has struck down legislation and how legislatures responded: did they attempt to circumvent or override the Court, or did they accept the ruling as binding even when they disagreed? If circumvention is rare, suppression is partly internalized. If circumvention is common, suppression is primarily structural and actively maintained.',
    'If suppression is primarily internalized, the constraint''s extractiveness is higher than the structural measure suggests—the target (Knesset majority) carries the suppression with it and is unlikely to attempt exit even if institutional barriers weakened. If suppression is primarily structural, exit barriers are real but the constraint''s persistence depends on continued institutional maintenance by the Court. The distinction matters for predicting how the constraint would behave if external conditions changed (e.g., if a supermajority sought to override the Court).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized deference.').

omega_variable(
    kernel_reading_contest_framing,
    'Is the constitutional contest between judicial supremacy and parliamentary sovereignty a legitimate disagreement about how to interpret a shared kernel (the Basic Laws as foundational), or does one side''s reading foreclose the other''s?',
    'Logical analysis of the axioms: if the judicial supremacy axiom (the Court holds final authority on constitutional interpretation) directly contradicts the parliamentary sovereignty axiom (the Knesset holds final authority), then one reading forecloses the other within a single framework. If both readings can be held simultaneously in different institutional domains or by different parties, they coexist. If one reading creates pressure that constrains but does not eliminate the other, it influences rather than forecloses.',
    'This omega documents the committer structure: are the three readings genuinely alternative frameworks (different parties can hold them), or are they logically incompatible such that accepting one requires rejecting the other? If they are logically incompatible, the engine''s foreclosure detection should flag the incompatibility; if they are alternative positions parties hold simultaneously (Israeli institutions and constitutional scholars hold all three views), they coexist. The resolution affects how the constraint family is modeled: as a set of mutually exclusive readings, or as a contested terrain where multiple readings remain live.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, conceptual, 'Whether the kernel readings are alternative frameworks or logically foreclosing positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(basi_tr_t0, observed).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(basi_tr_t5, observed).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(basi_tr_t10, observed).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(basi_tr_t15, observed).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(basi_tr_t20, observed).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(basi_tr_t25, observed).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(basi_tr_t30, observed).
narrative_ontology:measurement(basi_tr_t35, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(basi_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(basi_be_t0, observed).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(basi_be_t5, observed).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(basi_be_t10, observed).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(basi_be_t15, observed).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(basi_be_t20, observed).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(basi_be_t25, observed).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(basi_be_t30, observed).
narrative_ontology:measurement(basi_be_t35, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(basi_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(basi_su_t0, observed).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(basi_su_t5, observed).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(basi_su_t10, observed).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(basi_su_t15, observed).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(basi_su_t20, observed).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(basi_su_t25, observed).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(basi_su_t30, observed).
narrative_ontology:measurement(basi_su_t35, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(basi_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the basic_law_interpretive_boundary kernel. The three readings instantiate structurally distinct constraints with different beneficiaries, victims, and ε values, but share the same referent (the Basic Laws as foundational commitments). Judicial supremacy reading (this file): Court interprets and enforces, high ε for legislation threatening court-protected liberties. Parliamentary sovereignty reading: Knesset holds final authority, ε for legislation is lower (Knesset's own judgment controls). Balanced contestation reading: both institutions hold legitimate authority in different domains, ε is distributed across both institutions and neither can unilaterally nullify the other. The three readings represent genuinely different constraint structures, not different measurements of a single constraint. They are linked via network.affects_constraints to indicate the kernel family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__judicial_supremacy_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
