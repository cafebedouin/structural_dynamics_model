% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty in Constitutional Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'basic_law_interpretive_authority': the parliamentary sovereignty reading
 *   holds that elected legislatures, accountable through democratic
 *   elections, retain final authority to interpret the constitution.
 *   Constitutional meaning is not fixed by courts or by diffuse popular
 *   practice but is authoritatively set by legislative pronouncement,
 *   amendment, or override. This reading is opposed by judicial supremacy
 *   (courts as guardians of constitutional limits, insulated from political
 *   pressure) and popular constitutionalism (meaning emerges from
 *   contestation rather than terminal institutional settlement). The
 *   constraint's structure exhibits both genuine coordination (settles the
 *   problem of who speaks with final authority) and asymmetric extraction
 *   (majorities can override protections minorities expect courts to provide;
 *   judicial independence is contingent on legislative sufferance).
 *
 * KEY AGENTS:
 *   - Elected legislature: holds and exercises final interpretive authority; benefits from institutional power and electoral mandate.
 *   - Judicial independence: the institutional capacity of courts to interpret law without legislative override; constrained and contingent under this reading.
 *   - Rights minorities: structurally dependent on legislative goodwill; bear costs when majorities reinterpret the constitution to their detriment.
 *   - Electorate: benefits from accountability and democratic control of constitutional meaning; bears diffuse costs when electoral remedy is delayed.
 *   - Competing interpretive traditions: excluded from the institutional authority structure; their theoretical claims are barred by the parliamentary sovereignty axiom.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.68).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.72).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'e1dec847-d1f1-420a-88b5-65e4d5d37109').
narrative_ontology:cs_kernel_codification('e1dec847-d1f1-420a-88b5-65e4d5d37109', fixed_text).
narrative_ontology:cs_authority_grounding('e1dec847-d1f1-420a-88b5-65e4d5d37109', extraction).
narrative_ontology:cs_interpretation_layer_present('e1dec847-d1f1-420a-88b5-65e4d5d37109').
narrative_ontology:cs_reading_relation('e1dec847-d1f1-420a-88b5-65e4d5d37109', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e1dec847-d1f1-420a-88b5-65e4d5d37109', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('e1dec847-d1f1-420a-88b5-65e4d5d37109', foundational, democratic_electoral_mandate_supremacy).
narrative_ontology:cs_axiom_status(democratic_electoral_mandate_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('e1dec847-d1f1-420a-88b5-65e4d5d37109', democratic_electoral_mandate_supremacy, deontological).
narrative_ontology:cs_axiom('e1dec847-d1f1-420a-88b5-65e4d5d37109', foundational, institutional_finality_requires_hierarchy).
narrative_ontology:cs_axiom_status(institutional_finality_requires_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('e1dec847-d1f1-420a-88b5-65e4d5d37109', institutional_finality_requires_hierarchy, conventional).
narrative_ontology:cs_reference_frame('e1dec847-d1f1-420a-88b5-65e4d5d37109', westminster_legislative_supremacy).
narrative_ontology:cs_drift_state('e1dec847-d1f1-420a-88b5-65e4d5d37109', contemporary_rights_consciousness_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e1dec847-d1f1-420a-88b5-65e4d5d37109', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_review_authority).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, electorate).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to interpret the basic law through constitutional amendment, statutory override, or declaratory legislation. Justifies this authority through direct democratic election and representative accountability to constituents. Collects the institutional power to set the binding interpretation and, when exercised, to override judicial readings it deems inconsistent with electoral will or constitutional intent.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, agenda_setter,
    institutional, generational, analytical, national).

% The institutional capacity of courts to decide cases according to law without legislative override or interference. Under parliamentary sovereignty, this independence is conditional—legislatures can reverse, narrow, or nullify judicial interpretations through amendment or declaratory acts. Courts bear the cost when legislative action repudiates their constitutional readings, delegitimizing their role as law-interpreters and subjecting them to political revision.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence, payer,
    institutional, generational, constrained, national).

% Groups whose rights depend on constitutional protection from majoritarian legislatures—religious minorities, dissenting political factions, non-dominant linguistic or ethnic groups. Under this reading, their only recourse is legislative goodwill or the ballot. If a legislature interprets the basic law to permit restrictions on minority rights, courts are subordinate and cannot impose a counter-interpretation; minorities bear the cost when legislatures override judicial protections or refuse to recognize them in the first place. They retain a diffuse beneficiary stake: legislatures may choose to protect rights through law, and electoral cycles create periodic opportunity for remedy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities, beneficiary).

% The courts' power to interpret the constitution within the bounds set by legislature. Courts benefit from clarity about the scope of their interpretive mandate and, in parliamentary sovereignty contexts, benefit from deference to judicial reasoning on questions the legislature has not yet chosen to override. The legislature's ability to have the final word provides legitimacy to the courts' intermediate role—they act as trusted interpreters precisely because they are not terminal.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_review_authority, beneficiary,
    institutional, generational, constrained, national).

% Citizens who vote and hold legislatures accountable through electoral cycles. They benefit from constitutional meaning being answerable to representative democracy—no unelected court can impose an interpretation they cannot ultimately reject through the ballot. They also bear diffuse costs: if the legislature interprets the basic law in ways majorities now regret, the remedy is electoral and may take years to manifest; in the interim, rights and institutions may be reconfigured.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, electorate, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, electorate, payer).

% Judicial supremacy advocates and popular constitutionalism proponents are structurally barred from claiming final interpretive authority under this reading. Their alternative framings (courts as guardians of constitutional limits, or popular deliberation as the seat of meaning) are foreclosed by the parliamentary sovereignty axiom. They exist as live intellectual and political movements but are excluded from the institutional arrangement itself.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, competing_interpretive_traditions, excluded,
    institutional, generational, trapped, national).

% Movements seeking to alter the basic law itself—through amendment, convention, or wholesale replacement. They operate at the meta-level: they accept or contest parliamentary sovereignty as the rule for interpreting the EXISTING constitution, but seek to change what the constitution SAYS. Their analytical position allows them to examine whether parliamentary sovereignty serves legitimacy and justice.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_reform_movements, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, accountable interpreter of constitutional meaning in a representative body, ending the problem of multiple claims to final authority and creating a clear path for constitutional meaning to shift when the electorate demands it. Solves the coordination problem of 'who gets to say what the constitution means' by anchoring interpretive authority in democratic accountability.
% TRANSFER_FUNCTION: Transfers institutional authority from independent courts to representative legislatures; transfers the power to override judicial constitutional readings to the electoral majority (mediated through legislative delegates); transfers costs of legislative constitutional action (e.g., minority-rights restrictions) onto those minorities and onto judicial prestige.
% ABSENT_VOICES: Judicial supremacy advocates who would argue courts are better guardians of constitutional limits are structurally excluded—their entire framework presupposes that courts should retain final authority. Popular constitutionalism theorists who would argue constitutional meaning emerges from non-institutional democratic contestation are also excluded—their framework rejects terminal institutional resolution altogether. Rights-minorities may nominally participate but have minimal structural leverage under this arrangement.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty in constitutional interpretation disappeared overnight, courts would retain or reassert final interpretive authority, or constitutional meaning would shift to emerging from popular contestation rather than legislative pronouncement. The legislative authority to override would vanish; legislatures would become bound by judicial readings unless they amended the constitution itself through special process. The balance of institutional power would reorganize fundamentally.
% FOUNDING_PROBLEM: Early constitutional orders faced the problem of institutional anarchy: when legislatures and courts disagreed on what the basic law meant, who could authoritatively settle the dispute? Parliamentary sovereignty solving this by making the legislature the final arbiter—democracy demands that elected representatives, accountable to voters, hold the power to say what binds the people.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary sovereigntists (legal scholars, legislators in Westminster and similar systems, some constitutional theorists) attest the founding problem is still live and that legislative supremacy in constitutional interpretation is the only answer consistent with democracy. Rival readings dispute this: judicial supremacy advocates argue courts are necessary because legislatures cannot be trusted with rights-protection; popular constitutionalists argue that depositing final authority in ANY institutional seat (legislature or court) betrays democratic meaning. The corroboration is from within the parliamentary sovereignty tradition itself; external corroboration is disputed across the kernel.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint transfers interpretive authority from courts to majorities, enabling majorities to extract value (reinterpretation in their favor) at the cost of minority protections. Suppression is correspondingly high (0.72): the constraint's persistence depends on actively defending legislative supremacy against judicial reassertion and popular constitutionalist alternatives. Courts must refrain from claiming final authority; minorities must accept that electoral remedy is their recourse. Theater is moderate (0.41): legislatures do genuinely settle constitutional questions, but an increasing share of legislative activity defends parliamentary supremacy itself (through declaratory acts, amendment processes, or constitutional legislation that narrows judicial review) rather than engaging substantive constitutional problems. Accessibility collapse (0.58) is moderate because alternatives (judicial review, popular constitutionalism) remain intellectually and politically live even if institutionally excluded. Resistance (0.69) is substantial because judicial supremacy advocates, rights-minorities, and popular constitutionalists all mount real opposition. The measurement series shows extraction and suppression rising initially as parliamentary sovereignty models consolidate and become embedded in institutional practice, then plateauing as the arrangement stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   From the legislature's seat, parliamentary sovereignty is genuine democratic coordination: an elected body accountable to voters holding final authority matches democratic principle. From the judicial seat, it is subordination of law to politics—courts lose the independence to check majoritarian overreach. From minorities' seat, it is exclusion from reliable constitutional protection. From the electorate's seat, it is empowerment mediated through periodic elections. These divergences are structural, not perspectival bias. The engine computes per-seat classification from the shared structural data: the legislature benefits, courts and minorities bear costs, the electorate sits between (benefits electorally, bears diffuse costs). The claimed type (tangled_rope) reflects the genuine coordination function (settling who interprets) AND the asymmetric extraction (majorities over minorities). A judicial supremacy reading of the same kernel would have different beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from the beneficiary/victim structure. The legislature is the primary beneficiary (d near 0.0): it collects interpretive authority and can use it to vindicate its preferred readings; its exit options are analytical (it is not trapped, it is the authority structure itself). Judicial independence is a clear victim (d near 1.0): it is contingent on legislative sufferance; courts can be overridden and are constrained by legislative action. Rights minorities are targets (d near 1.0): they are powerless, trapped, dependent on legislative goodwill, and bear costs when majorities reinterpret. The electorate is moderate (d around 0.5): they benefit from democratic control but bear diffuse costs from delayed remedy. No directionality override is needed; the derivation chain from beneficiary/victim + power + exit produces appropriate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is vulnerable to mandatrophy: the founding problem (institutional anarchy about constitutional meaning) could be solved without parliamentary sovereignty (courts could solve it through independent expertise, or popular contestation could constitute it). The constraint's persistence depends on continuous legislative assertion that it is the legitimate solution. As legislatures increasingly use parliamentary sovereignty to defend itself (declaratory acts, amendment procedures) rather than to substantively settle constitutional questions, theater rises and the appearance of mandatrophy grows. The measurement series captures this: suppression requirement stays high (legislatures must actively defend against judicial and popular alternatives) while extractiveness plateaus (the constraint's capacity to move goods to majorities hits saturation). A tangled rope classification correctly identifies this: genuine coordination + active extraction, both necessary to the constraint's persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    representative_mandate_scope,
    'Does the electoral mandate grant legislatures authority to interpret the EXISTING constitution, or only to amend it through special constitutional process?',
    'Historical and comparative constitutional law: examination of which democracies have allowed legislatures to override judicial constitutional interpretations through ordinary legislation vs. requiring amendment. Testing the distinction empirically across Westminster, civil law, and hybrid systems.',
    'If legislative override is limited to formal amendment, the constraint''s extractiveness drops significantly—majorities still cannot easily reinterpret via ordinary legislation. If ordinary legislation suffices, the constraint operates at full extractiveness. This omega distinguishes ''parliamentary sovereignty in constitutional interpretation'' from ''parliamentary sovereignty in constitutional amendment only.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(representative_mandate_scope, empirical, 'Whether electoral mandate extends to constitutional interpretation via ordinary legislation or only to formal amendment.').

omega_variable(
    minority_protection_counterfactual,
    'Would rights minorities be better protected under judicial supremacy or popular constitutionalism than under parliamentary sovereignty?',
    'Comparative empirical study of minority protections across constitutional systems organized under each reading. Natural experiments from constitutional transitions. Historical track record of judicial protection vs. legislative protection in systems that have shifted readings.',
    'If minorities are systematically better protected under judicial supremacy, the parliamentary sovereignty reading generates preventable harms, shifting classification toward snare. If legislative protection is equivalent or superior, the tangled rope classification holds. If popular constitutionalism proves less protective still, it validates parliamentary sovereignty as the least-bad option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_counterfactual, empirical, 'Comparative protection of rights minorities across interpretive authority readings.').

omega_variable(
    majoritarian_abuse_frequency,
    'How often do legislatures under parliamentary sovereignty use their interpretive authority to restrict minority rights that courts had recognized?',
    'Historical data: count instances across democracies where legislatures have explicitly overridden or narrowed judicial constitutional protections for minorities. Compare frequency and severity across systems and time periods.',
    'High frequency and severity indicates suppression is structural and persistent, supporting the 0.72 suppression metric and snare-candidate classification. Low frequency suggests the extraction mechanism is rarely triggered, possibly elevating the constraint to rope or even coordination. The pattern informs whether parliamentary sovereignty is a live threat or a latent framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(majoritarian_abuse_frequency, empirical, 'Empirical track record of majoritarian override of minority protections under parliamentary sovereignty.').

omega_variable(
    reading_foreclosure_logic,
    'Does parliamentary sovereignty FORECLOSE judicial supremacy logically, or do they merely COEXIST as competing live frameworks?',
    'Philosophical analysis: can a single legal system hold both premises simultaneously (e.g., ''legislatures have final authority BUT courts can override them under certain conditions'')? Or does holding one premise entail rejecting the other? Test by examining hybrid systems that claim both.',
    'If they foreclose each other (true logical contradiction), the reading_relations should state ''forecloses''. If they can coexist in different institutional seats or hierarchies (legislature final on ordinary law, court final on rights), they ''coexist_with''. This affects the engine''s foreclosure test and the kernel''s competition structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_logic, conceptual, 'Logical foreclosure vs. coexistence between parliamentary sovereignty and judicial supremacy as readings of the kernel.').

omega_variable(
    electoral_cycle_remedy_gap,
    'Can the electoral cycle provide timely remedy for constitutional misinterpretations by legislatures, or is the lag itself an extraction mechanism?',
    'Empirical analysis of duration from legislative constitutional action to electoral remedy across democracies. Study of whether minority groups have managed to reverse majoritarian constitutional reinterpretations through electoral cycles, and how long reversals took.',
    'If electoral cycles are too long (several election cycles needed to reverse a constitutional reinterpretation), the lag itself becomes a suppression/extraction mechanism—minorities pay a long-term cost before remedy arrives. This would elevate the suppression and extractiveness metrics. If reversals are prompt (1–2 cycles), the constraint operates closer to a coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_cycle_remedy_gap, empirical, 'Whether electoral remedy for legislative constitutional misinterpretation is timely enough to limit minority harm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This is one reading of the contested kernel 'basic_law_interpretive_authority.' Two sibling readings exist: judicial_supremacy_reading (courts hold final authority) and popular_constitutionalism_reading (meaning emerges from democratic contestation). All three readings share the same referent (the basic law) but instantiate different constraints because they axiomatize different sources of interpretive legitimacy. The readings are linked via network.affects_constraints as constraint families. Epsilon values differ across readings: parliamentary sovereignty_reading measures epsilon for the standing arrangement of legislative supremacy (~0.68); judicial_supremacy_reading measures epsilon for the standing arrangement of judicial final authority; popular_constitutionalism_reading measures epsilon for the standing arrangement of contestatory constitutional meaning. These are not different measurements of one constraint; they are different constraints emerging from different axiomatizations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
