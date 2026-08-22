% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Constitutional Validity via Formal Procedure (Positivist Reading)
 *   domain: legal/political
 *
 * SUMMARY:
 *   This constraint story instantiates the POSITIVIST READING of the
 *   contested kernel 'us_constitution_meaning.' The reading asserts that
 *   constitutional validity derives entirely from formal enactment procedures
 *   and institutional authority, explicitly excluding external moral
 *   principles from the determination of constitutional meaning. Judges are
 *   constrained by the enacted text and the formal amendment process;
 *   arguments grounded in natural law, moral philosophy, or evolving social
 *   values are treated as extra-constitutional. The reading benefits
 *   institutional authority (courts, legislatures, proceduralist scholars)
 *   and harms substantive-justice claimants whose rights claims lack explicit
 *   textual warrant. The constraint is CLAIMED as tangled_rope (coordination
 *   function real, extraction asymmetric) and the authored metrics describe
 *   substantially extractive, actively enforced operation — the engine
 *   measures that divergence as diagnostic of how procedure can become a
 *   vehicle for power concentration.
 *
 * KEY AGENTS:
 *   - Institutional authority agents: courts and legislatures enforcing the procedural constraint
 *   - Proceduralist jurists: benefit from positivist reasoning and maintain the constraint through doctrine
 *   - Substantive justice claimants: powerless groups unable to argue rights lacking textual warrant
 *   - Extra-textual rights advocates: organized movements whose preferred reasoning mode is excluded
 *   - Amendment process blockage: the structural trap when supermajority consensus fails
 *   - Originalist and living constitutionalist judges: excluded from this reading's framework, coexisting as rivals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.68).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.71).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Constitutional Validity via Formal Procedure (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, '5657f0f5-64d9-492e-becd-7555d9bfbc53').
narrative_ontology:cs_kernel_codification('5657f0f5-64d9-492e-becd-7555d9bfbc53', fixed_text).
narrative_ontology:cs_authority_grounding('5657f0f5-64d9-492e-becd-7555d9bfbc53', extraction).
narrative_ontology:cs_interpretation_layer_present('5657f0f5-64d9-492e-becd-7555d9bfbc53').
narrative_ontology:cs_reading_relation('5657f0f5-64d9-492e-becd-7555d9bfbc53', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5657f0f5-64d9-492e-becd-7555d9bfbc53', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('5657f0f5-64d9-492e-becd-7555d9bfbc53', foundational, constitutional_validity_from_procedure_alone).
narrative_ontology:cs_axiom_status(constitutional_validity_from_procedure_alone, holdable).
narrative_ontology:cs_axiom_grounding('5657f0f5-64d9-492e-becd-7555d9bfbc53', constitutional_validity_from_procedure_alone, conventional).
narrative_ontology:cs_axiom('5657f0f5-64d9-492e-becd-7555d9bfbc53', foundational, moral_reasoning_extra_constitutional).
narrative_ontology:cs_axiom_status(moral_reasoning_extra_constitutional, holdable).
narrative_ontology:cs_axiom_grounding('5657f0f5-64d9-492e-becd-7555d9bfbc53', moral_reasoning_extra_constitutional, deontological).
narrative_ontology:cs_reference_frame('5657f0f5-64d9-492e-becd-7555d9bfbc53', formal_enactment_authority).
narrative_ontology:cs_drift_state('5657f0f5-64d9-492e-becd-7555d9bfbc53', contemporary_amendment_gridlock_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5657f0f5-64d9-492e-becd-7555d9bfbc53', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, institutional_authority_agents).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, proceduralist_jurists).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, extra_textual_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts and legislatures operate under the positivist constraint: constitutional validity flows from formal enactment and amendment procedure, not from external moral evaluation. They set the boundaries of legitimate constitutional argument by accepting only text-grounded and procedure-grounded claims. Their power derives from the constraint itself — they can dismiss rights claims that lack textual warrant, and their enforcement of this rule is what keeps moral reasoning outside the bounds of constitutional legitimacy.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, institutional_authority_agents, agenda_setter,
    institutional, generational, arbitrage, national).

% Jurists committed to legal positivism and formalism benefit from a constitutional order that rests legitimacy on procedure rather than substantive moral content. Their interpretive methods are validated; their authority to adjudicate constitutional meaning is secured by the constraint that moral reasoning is out of bounds. They maintain the constraint through doctrine, teaching, and published jurisprudence. They have the option to switch interpretive schools, but institutional prestige accrues to proceduralist reasoning within this frame.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, proceduralist_jurists, beneficiary,
    institutional, generational, mobile, national).

% Groups seeking recognition of rights grounded in moral principles, human dignity, or social welfare — privacy, bodily autonomy, subsistence, freedom from arbitrary harm — find themselves unable to argue their claim before courts if the claim lacks explicit textual support. The positivist constraint forecloses their preferred argumentative mode. They must either find textual hooks for their claim (reframing), accept judicial dismissal on formalist grounds, or pursue constitutional amendment, which is prohibitively expensive and slow.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, substantive_justice_claimants, payer,
    powerless, biographical, trapped, national).

% Scholars, advocates, and movements that ground constitutional claims in natural law, international human rights, evolving social understanding, or moral philosophy are structurally excluded from legitimate constitutional discourse by the positivist reading. Their arguments are treated as legislative policy proposals, not constitutional claims. They bear the cost of this exclusion — their preferred mode of reasoning is delegitimized, their scholarly authority is marginal in courts, and their ability to shape constitutional meaning through litigation is blocked.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, extra_textual_rights_advocates, payer,
    organized, generational, constrained, national).

% The formal amendment procedure (Article V) is the positivist constraint's only escape valve for substantive change. It is deliberately difficult, requiring supermajority consensus. When the amendment process is gridlocked — which it structurally tends toward — the positivist reading collapses into originalism in practice: the text cannot change, moral arguments are excluded, so the text as originally understood becomes the only available reference point. This dynamic traps substantive claimants with no recourse.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, amendment_process_blockage, excluded,
    powerful, generational, trapped, national).

% Originalist judges and scholars ostensibly share common cause with positivists (both exclude moral reasoning), but the reading describes a constraint on INSTITUTIONAL authority itself — the authority to determine what counts as a valid constitutional claim. Originalism also constrains that authority, but via historical meaning rather than pure formal procedure. Where positivism and originalism diverge (e.g., on whether unwritten principles can be inferred from textual structure), neither has jurisdictional authority to adjudicate the other; they coexist as competing frameworks.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, originalist_judges, excluded,
    institutional, generational, constrained, national).

% Living constitutionalists are explicitly ruled out by the positivist reading: they appeal to evolving social norms, contemporary understanding, and moral principle as sources of constitutional meaning. The positivist constraint treats these appeals as extra-constitutional reasoning. Living constitutionalists have institutional power (they sit on courts, write scholarship), but this reading constrains the legitimacy of their mode of argument within the constitutional discourse.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, living_constitutionalist_judges, excluded,
    institutional, generational, constrained, national).

% Legal scholars, political theorists, and comparative constitutionalists observe the constraint in operation. They analyze how the positivist reading distributes power (institutional authority consolidated, moral reasoning marginalized) and measure its effects on constitutional development (amendment gridlock, textual stasis, documented divergence from public moral sentiment).
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, constitutional_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__positivist_reading, institutional_authority_agents).
narrative_ontology:fixing_cost_class(us_constitution_meaning__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes constitutional meaning by anchoring validity to formal procedure (text, ratification, amendment) rather than to shifting moral consensus. This solves the coordination problem of how a written constitution can bind successive generations: the text is fixed, the procedure is known, so interpretation can claim objectivity and prevent courts from rewriting the constitution to match their moral preferences.
% TRANSFER_FUNCTION: Transfers authority to adjudicate constitutional claims from substantive-moral reasoning (where people disagree widely) to formal-procedural reasoning (where the rules are supposedly neutral). The cost is borne by anyone whose rights claim lacks textual warrant — they lose access to the forum of constitutional litigation. The benefit accrues to institutional authority (courts, legislatures, proceduralist scholars) whose legitimacy is secured by formalism.
% ABSENT_VOICES: Natural-law theorists, international human-rights advocates, substantive moral philosophers, and movements seeking recognition of unenumerated rights on moral grounds are structurally excluded from constitutional discourse under this reading. They would argue that a constitution's validity depends on its alignment with human rights and moral principles, not merely its formal enactment. They are kept out by the same rule that defines constitutional legitimacy.
% DISAPPEARANCE_RATIONALE: If the positivist constraint vanished — if courts suddenly treated moral reasoning as legitimate constitutional argument — the boundaries of constitutional litigation would expand dramatically. Claims for privacy, dignity, subsistence, and other unwritten rights would become justiciable. The amendment process would lose its monopoly on constitutional change. Constitutional doctrine would become more fluid and contested, and institutional authority would fragment across competing frameworks (originalism, living constitutionalism, natural law). The American constitutional order would reorganize around a different legitimacy principle.
% FOUNDING_PROBLEM: Early American constitutional practice showed judges imposing their moral and political preferences under the guise of interpretation, with no principled constraint on their reasoning. The Lochner era exemplifies this: courts struck down economic and labor regulation based on unarticulated moral commitments to liberty of contract. The positivist constraint was designed to prevent judges from using the Constitution as a vehicle for their moral views — to ground constitutional validity in the formal procedure by which the Constitution was adopted and can be amended, excluding extra-constitutional moral reasoning.
% FOUNDING_PROBLEM_CORROBORATION: Proceduralist jurists and conservative scholars attest the Lochner problem is still live, citing instances of what they call judicial moral reasoning (abortion jurisprudence, privacy doctrine, substantive due process). Living constitutionalists and moral-rights advocates attest the founding problem was a historical artifact of unguarded judicial power that is now effectively constrained by professional norms, precedent, and political accountability — they deny that moral reasoning in constitutional law is the same as Lochner-style imposing of judicial preference. Independent scholarly analysis documents the empirical debate: some find systematic bias in judicial reasoning; others find professional constraints effective.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction at 0.68 (endpoint) reflects the asymmetric distribution of authority and argumentative access: institutional actors retain power to interpret and enforce, while substantive claimants bear the cost of exclusion. Suppression at 0.71 is high because the constraint's persistence depends on active enforcement of the boundary between legitimate (procedural) and illegitimate (moral) reasoning — courts must repeatedly dismiss substantive arguments, jurisprudence must police the boundary, and professional norms must discourage moral reasoning in constitutional law. Theater at 0.41 reflects a moderate proportion of performative activity: much of what proceduralist jurists do is genuinely constrained by text (real coordination function), but a growing share is devoted to explaining why moral arguments cannot be admitted (the suppressive machinery). The temporal series shows extraction accumulating from t=0 (0.45) to t=50 (0.68), tracking the amendment process's increasing gridlock (fewer successful amendments, more unwritten claims denied) and the institutionalization of positivist doctrine in law schools and courts. Theater rises from 0.18 to 0.41, marking the increasing rhetorical work required to defend proceduralism against living constitutionalist and moral-rights challenges. One shared time grid: every metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   From the proceduralist jurist's seat, the constraint is genuine coordination: it solves the problem of judicial moral reasoning and stabilizes constitutional meaning. From the substantive claimant's seat, the same constraint is pure extraction: it forecloses their argumentative mode and denies them access to the most authoritative forum for rights recognition. The engine computes these divergences from the structural data — the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are institutional authority agents (institutional power, arbitrage exit — they can navigate multiple interpretive frameworks) and proceduralist jurists (institutional power, mobile exit — they can switch schools). Victims are substantive justice claimants (powerless, trapped — no exit except changing their moral commitments or amending the Constitution) and extra-textual rights advocates (organized but constrained — they can lobby for amendment or shift to legislative advocacy, but their preferred forum, constitutional litigation, is closed). The amendment process blockage is excluded rather than a beneficiary because it is structural machinery, not an agent; it is the mechanism by which substantive claimants are trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Lochner-era judicial imposing of moral preferences) is attested as resolved by proceduralists but contested by moral-rights advocates, who argue that the constraint merely hides rather than eliminates moral reasoning and that professional norms, not formal procedure, provide the real constraint. The theater ratio rising from 0.18 to 0.41 tracks increasing rhetorical work defending proceduralism against this challenge. If the founding problem is dead (moral reasoning is adequately suppressed by professional norms) but the constraint persists (substantive claimants remain excluded), the constraint would be classified as a piton — a degraded coordination mechanism maintained by inertia. However, the current metrics describe active extraction, not theatrical performance; the constraint is actively being enforced and actively distributing benefits to proceduralist authority. The Tangled Rope classification captures this: real coordination function (stabilizing meaning through procedure), asymmetric extraction (benefits institutional authority, harms substantive claimants), active enforcement (the boundary between legitimate and illegitimate reasoning must be constantly defended).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_reasoning_boundary_ambiguity,
    'Can courts apply formal procedure without implicitly applying moral reasoning about what procedure is legitimate, and does that implicit reasoning undermine the positivist claim to exclude moral reasoning?',
    'Analytical philosophy of law: examination of whether procedural reasoning is conceptually independent from substantive moral reasoning, or whether all rule-application involves implicit moral judgment about the rule''s legitimacy.',
    'If procedural reasoning cannot be fully separated from implicit moral reasoning, the positivist reading''s core claim (exclude moral reasoning) is structurally incoherent. The constraint would be reinterpreted as relocating moral reasoning to the procedural level rather than eliminating it, changing the classification from Tangled Rope to Snare (false coordination claim).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_reasoning_boundary_ambiguity, conceptual, 'Whether the boundary between formal procedure and moral reasoning is conceptually clean.').

omega_variable(
    amendment_process_gridlock_structural_inevitability,
    'Is the amendment process''s gridlock a contingent political fact or a structural feature of the supermajority requirement? If structural, does positivism collapse into originalism in practice as a matter of necessity rather than choice?',
    'Political economy analysis of amendment outcomes and comparative constitutionalism (countries with lower amendment thresholds show higher amendment rates). Counterfactual: if the US amendment threshold were lowered, would substantive constitutional change accelerate and would positivism''s exclusionary effect diminish?',
    'If gridlock is structural, positivism''s escape valve is permanently closed and the constraint becomes a one-way ratchet: only original understanding matters, substantive claimants have no recourse. The classification would shift from Tangled Rope (coordination with asymmetric extraction) to Snare (pure extraction with false coordination framing). If gridlock is contingent, positivism could theoretically function as intended, but current conditions trap it in originalism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_process_gridlock_structural_inevitability, empirical, 'Whether amendment gridlock is inevitable or contingent, and whether it collapses positivism into originalism.').

omega_variable(
    professional_norm_vs_formal_constraint,
    'Is the actual constraint on judicial moral reasoning enforced by formal procedure (text + amendment) or by professional norms, institutional culture, and political accountability in the legal profession?',
    'Comparative study of judges'' reasoning patterns before and after institutional shifts (e.g., change in appointment criteria, shift in law school curriculum, changes in appellate review pressure). If judicial restraint persists despite formal procedure change, norms are the real constraint; if judicial reasoning drifts when norms change, norms matter more than procedure.',
    'If professional norms are the real constraint, the positivist reading''s claim that procedure excludes moral reasoning is an attribution error. The actual constraint is not the reading described but rather institutional culture. This would suggest the constraint is a piton (atrophied coordination, maintained theatrically by professional pretense) rather than a Tangled Rope. If procedure is the real constraint, norms reinforce it, and the reading is as claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_norm_vs_formal_constraint, empirical, 'What enforces the constraint on judicial moral reasoning: formal procedure or professional norms?').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the positivist reading logically foreclose living constitutionalism, or do they coexist as different schools held by different institutional actors?',
    'Logical analysis: can a single judge or court hold both positivism (exclude moral reasoning) and living constitutionalism (apply constitutional principles to evolving social circumstances) without contradiction? Or do they entail incompatible commitments about the source of constitutional meaning?',
    'If they logically foreclose each other, the reading_relation is ''forecloses'' and the kernel contest has a triadic logical structure where each reading rules out the others. If they can coexist (different judges, different cases, different jurisdictions holding different readings), the relation is ''coexists_with'' and the kernel is a site of ongoing dispute without resolution. The classification of the sibling readings would differ accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether positivism and living constitutionalism are logically incompatible or merely competing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__positivist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__positivist_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__positivist_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__positivist_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__positivist_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_meaning__positivist_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(us_c_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__positivist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__positivist_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__positivist_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__positivist_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__positivist_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t50, us_constitution_meaning__positivist_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(us_c_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__positivist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__positivist_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__positivist_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__positivist_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__positivist_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(us_c_su_t40, observed).
narrative_ontology:measurement(us_c_su_t50, us_constitution_meaning__positivist_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(us_c_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The kernel 'us_constitution_meaning' decomposes into three structurally distinct constraints, each a reading of the same contested commitment. The positivist reading grounds validity in formal procedure; the originalist reading grounds it in historical meaning; the living constitutionalist reading grounds it in evolutionary principle. Each reading has different ε (extractiveness from whose standpoint), different beneficiary/victim structure, and different type. The three readings share the same kernel but instantiate different constraints because the ε referent (the standing arrangement under contest — what counts as legitimate constitutional reasoning) is read differently by each framework. Sibling readings are linked via network.affects_constraints so the constraint family is tracked as a unit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__positivist_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
