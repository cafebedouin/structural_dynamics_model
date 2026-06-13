% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__accountability_void_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity: Accountability Void Reading
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   Qualified immunity is a judicially created doctrine that shields law
 *   enforcement officers from civil liability for constitutional violations
 *   unless the officer violated a 'clearly established' right known at the
 *   time of the violation. Under the accountability void reading, this
 *   doctrine functions as systematic extraction: it guarantees officers
 *   impunity for constitutional violations, transfers the right to sue FROM
 *   victims TO officers, and creates a near-absolute bar to holding officers
 *   accountable. This reading emphasizes that the doctrine has no
 *   constitutional or statutory textual basis — it is a pure judicial
 *   creation that subordinates constitutional guarantees to officer
 *   convenience. The founding problem (frivolous litigation) has been
 *   superseded by modern procedural rules, yet the doctrine persists and
 *   expands. The claim is snare; the metrics describe near-complete
 *   extraction and suppression with high theater (the doctrine is defended
 *   using rationalist language about judicial restraint and necessary
 *   protection, when its actual function is impunity).
 *
 * KEY AGENTS:
 *   - law_enforcement_officers: Beneficiary class (organized, biographical horizon, mobile exit). Protected from liability; doctrine shields career and finances from consequences of unlawful action.
 *   - constitutional_violation_survivors: Payer class (powerless, biographical horizon, trapped exit). Bear full consequences with no remedy; no civil system path to redress or accountability.
 *   - federal_courts: Agenda-setter (institutional power, generational horizon, analytical exit). Administer and expand the doctrine through case law; establish impossibly high 'clearly established' bar.
 *   - police_departments: Beneficiary and agenda-setter (institutional, generational, mobile). Avoid budget liability; departments maintain enforcement postures without financial consequence.
 *   - legislators_and_executives: Excluded (institutional, generational, mobile). Would have authority to abolish immunity; politically constrained; outside the doctrinal conversation.
 *   - civil_rights_advocates: Excluded (moderate power, biographical, constrained exit). Systematically outside decision-making; litigate within the framework courts control.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.91).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.88).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity: Accountability Void Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, '29efd01b-3c7b-4212-9268-4df3f8a05b20').
narrative_ontology:cs_kernel_codification('29efd01b-3c7b-4212-9268-4df3f8a05b20', fixed_text).
narrative_ontology:cs_authority_grounding('29efd01b-3c7b-4212-9268-4df3f8a05b20', extraction).
narrative_ontology:cs_interpretation_layer_present('29efd01b-3c7b-4212-9268-4df3f8a05b20').
narrative_ontology:cs_reading_relation('29efd01b-3c7b-4212-9268-4df3f8a05b20', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('29efd01b-3c7b-4212-9268-4df3f8a05b20', qualified_immunity_doctrine__constitutional_fidelity_reading, influences).
narrative_ontology:cs_axiom('29efd01b-3c7b-4212-9268-4df3f8a05b20', foundational, founding_problem_persistence).
narrative_ontology:cs_axiom_status(founding_problem_persistence, overridden).
narrative_ontology:cs_axiom_grounding('29efd01b-3c7b-4212-9268-4df3f8a05b20', founding_problem_persistence, empirically_contingent).
narrative_ontology:cs_axiom('29efd01b-3c7b-4212-9268-4df3f8a05b20', secondary, officer_liability_incompatible_with_enforcement).
narrative_ontology:cs_axiom_status(officer_liability_incompatible_with_enforcement, overridden).
narrative_ontology:cs_axiom_grounding('29efd01b-3c7b-4212-9268-4df3f8a05b20', officer_liability_incompatible_with_enforcement, instrumental).
narrative_ontology:cs_reference_frame('29efd01b-3c7b-4212-9268-4df3f8a05b20', judicial_supremacy_over_constitutional_remedy).
narrative_ontology:cs_drift_state('29efd01b-3c7b-4212-9268-4df3f8a05b20', contemporary_accountability_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('29efd01b-3c7b-4212-9268-4df3f8a05b20', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, police_departments).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, absolute_executive_immunity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from personal liability for constitutional violations except in the rare case where the officer violated a 'clearly established' right at the time of the violation. They can act with near-complete impunity; even if sued, the immunity doctrine provides dismissal in most cases before trial. Their career and personal finances are shielded from the consequences of unlawful searches, excessive force, false arrest, and coerced confessions.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, beneficiary,
    organized, biographical, mobile, national).

% Bear the full consequences of constitutional violations with no meaningful remedy. Even when their rights are violated — unlawful search, excessive force, false imprisonment — they have no path to hold the officer accountable for damages because the immunity doctrine raises the bar to near-impossible levels. The psychological, physical, and financial harms from the violation cannot be redressed through the civil system that would theoretically enforce the Constitution.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_survivors, payer,
    powerless, biographical, trapped, national).

% Administer and expand the qualified immunity doctrine through case law. Courts at every level — including the Supreme Court — have repeatedly reaffirmed and narrowed the pathways to liability, creating an ever-expanding dome of protection. They maintain the doctrine by establishing that rights are 'clearly established' only when prior case law identified the exact same conduct as unlawful, making it nearly impossible for new violations to cross the threshold.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Benefit structurally from the immunity doctrine because it reduces accountability pressure on their personnel and budgets. Departments face minimal liability exposure and can avoid civil settlements and judgments that would otherwise fund compensation for victims. The doctrine enables departments to maintain aggressive enforcement postures without financial consequence.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, police_departments, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, police_departments, agenda_setter).

% Would have authority to abolish or substantially curtail qualified immunity through legislation, but are politically constrained by police unions and law-and-order constituencies. They are excluded from the doctrinal conversation itself; the immunity rule persists through judicial maintenance rather than legislative authorization.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, legislators_and_executives, excluded,
    institutional, generational, mobile, national).

% Seek to hold officers accountable and would argue for eliminating immunity if seated in the doctrinal conversation. They are systematically excluded from the decision-making structure — they litigate within the immunity framework that courts control, but have no mechanism to revise the foundational doctrine itself.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_advocates, excluded,
    moderate, biographical, constrained, national).

% The Fourth Amendment prohibition on 'unreasonable searches' and the Fourteenth Amendment's promise of 'equal protection' exist as written law. Under this reading, the immunity doctrine operates as a judicial override of the Constitution's plain terms — the Constitution guarantees a remedy, but immunity negates it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, constitutional_text, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(qualified_immunity_doctrine__accountability_void_reading, constitutional_text).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__accountability_void_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading holds that the doctrine does not coordinate a genuine collective-action problem. The rationalist claim (protection from frivolous suits) was the stated function; but the founding problem was never empirically validated and has been superseded by modern procedural rules. Under this reading, the doctrine is pure extraction dressed in coordination language.
% TRANSFER_FUNCTION: Transfers the right to sue for constitutional violations FROM violation survivors TO law enforcement officers and police departments. Victims lose access to civil damages and personal accountability; officers gain immunity from personal liability. The transfer is uncompensated and unidirectional — there is no quid pro quo, no coordination benefit that would justify the asymmetry.
% ABSENT_VOICES: Survivors of constitutional violations have no seat in the federal courts that create and maintain the doctrine; they appear only as plaintiffs seeking dismissal. State legislators and Congress are absent — the immunity rule persists through judicial creation and expansion, not democratic authorization. Civil rights advocates, constitutional scholars, and police-reform movements speak as outside critics but are excluded from the doctrinal conversation itself. The Supreme Court has never seated a survivor of unconstitutional search or excessive force in the deliberation over qualified immunity.
% DISAPPEARANCE_RATIONALE: If qualified immunity disappeared, constitutional violations would carry financial and reputational consequences for officers. Departments would face liability exposure, officers would carry insurance, and enforcement practices would immediately shift to prioritize constitutional compliance. Civil rights litigation would become a functional remedy again; survivors would have a path to vindication and damages. The entire incentive structure would reorganize around accountability rather than impunity.
% FOUNDING_PROBLEM: Federal concern in the 1960s–1970s that federal civil rights officers needed protection from excessive litigation and personal liability to enforce federal law effectively. The doctrine was created in Pierson v. Ray (1967) and elaborated in Harlow v. Fitzgerald (1982) as a qualified protection against frivolous suits and harassment.
% FOUNDING_PROBLEM_CORROBORATION: Federal judges including Justices Sotomayor and Gorsuch, civil rights scholars, and the American Civil Liberties Union have attested that the founding problem (frivolous litigation flood) was never empirically demonstrated and that modern discovery rules, summary judgment, and Rule 11 sanctions already provide the protection the doctrine was meant to ensure. No credible external voice — outside the federal judiciary and law enforcement beneficiary set — maintains that frivolous suits remain a live problem requiring constitutional immunity. The American Bar Association's studies on civil litigation found no evidence of a frivolous-suit plague against federal officers. Legislative testimony and law review articles from actors outside the beneficiary set consistently report that the founding justification has been superseded.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.91 at interval end) because the doctrine grants officers near-absolute immunity from consequences — there is virtually no path to hold them accountable for constitutional violations. Suppression is nearly as high (0.88) because the doctrine is enforced through judicial gatekeeping; the 'clearly established right' test is applied by judges to dismiss cases before trial, actively suppressing the civil remedy path. Theater is substantial (0.62) because the doctrine is defended using language of judicial restraint and necessary protection, while its actual function is impunity. The accessibility_collapse metric runs high (0.79) because once the immunity doctrine is understood, the civil remedy avenue is nearly closed off for victims. Resistance increases over time (0.45 to 0.68 individual level) as social awareness of the doctrine's effects grows, but the doctrine persists because it is maintained by judicial gatekeeping and the Supreme Court has repeatedly reaffirmed it. The coercion_grid tracks intensification across all levels: individual victims face rising barriers (from 0.58 to 0.81 accessibility_collapse), organizational victims (civil rights groups) face rising difficulty in establishing 'clearly established' rights (stakes_inflation 0.58 to 0.79), class resistance grows (0.58 to 0.76) but structural resistance remains constrained (0.41 to 0.62) because the doctrine is maintained at the institutional level by federal courts. All metrics share one time grid: 1982 (Harlow v. Fitzgerald foundation) through 2024 (contemporary high extraction).
 *
 * PERSPECTIVAL GAP:
 *   The officer and department seats will compute as rope-adjacent or even beneficiary-neutral (they gain from the doctrine, have low extraction pressure). The victim seat will compute as snare or tangled_rope (asymmetric extraction, active suppression). The court seat will compute as tangled_rope or piton (coordinates officer protection, extracts no direct benefit, but maintains the arrangement through active gatekeeping). The claimed type (snare) reflects the victim perspective and the structural asymmetry of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers benefit without running the system (low d, near 0.0 for arbitrage-equivalent mobility — they can switch jurisdictions but never exit immunity). Victims are the targets and bear the costs (high d, near 1.0 — trapped, powerless, no exit). Federal courts are the agenda-setters who administer the doctrine and could change it (moderate d, ~0.45 institutional, analytical exit — they maintain the system but are not direct extraction recipients). Police departments are secondary beneficiaries (institutional power, mobile exit at jurisdictional level; could theoretically lobby for abolition but benefit from immunity and do not). The asymmetry is extreme: the doctrine concentrates benefits on officers and departments while dispersing costs on powerless victims with no organized voice in the maintenance of the doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (frivolous litigation against federal officers) has been dead for decades — modern discovery rules, summary judgment, and qualified immunity itself already solved the problem by the 1990s. Yet the doctrine persists and expands, with courts narrowing the 'clearly established right' test to make liability even more difficult. This is mandatrophy: the function (protecting officers from frivolous suits) has been supplanted, but the extraction (immunity from all liability, frivolous or not) remains and accelerates. The doctrine has crossed from scaffolding (temporary protection during a real threat) to piton-like inertia (maintains itself through institutional theater while serving no original purpose) but with snare-level extraction (benefits officers, extracts from victims, requires active enforcement). The six-questions mismatch (founding_problem_status=dead, disappearance_verdict=world_rearranges) confirms mandatrophy: the problem has vanished but the arrangement persists because it benefits the court system and law enforcement, not because the problem is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_empirical_status,
    'Was the founding problem (frivolous litigation against federal officers) ever empirically real, and to what extent have modern procedural rules solved it independent of qualified immunity?',
    'Empirical analysis of litigation rates, frivolous suit frequency, and discovery burden before and after Harlow v. Fitzgerald; comparison of jurisdictions with and without immunity protections; deposition of the problem in legislative hearing testimony from the 1980s versus contemporary data.',
    'If the problem was never substantial or was solved by modern discovery rules, the doctrine''s persistence is mandatrophy without any remaining functional justification, moving classification firmly into snare. If the problem was real and unsolved by procedural rules, immunity retains some protective function, potentially shifting toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_empirical_status, empirical, 'Empirical status of the founding problem and its persistence.').

omega_variable(
    constitutional_authorization_ambiguity,
    'Does the Constitution (Fourth Amendment, Fourteenth Amendment, or common-law tradition) authorize qualified immunity, or is it a pure judicial invention without constitutional or statutory basis?',
    'Historical and textual analysis of the Constitution, the Civil Rights Act of 1871 (Section 1983), and English common-law tradition. Supreme Court jurisprudence on whether qualified immunity is constitutionally compelled or merely constitutionally permitted.',
    'If immunity is constitutionally required, its persistence is authorized and potentially rope-adjacent. If it is a pure judicial creation without textual basis, the doctrine is indefensible even on protective grounds, moving toward snare. This resolves the constitutional_fidelity_reading''s core claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_authorization_ambiguity, conceptual, 'Whether qualified immunity has constitutional or statutory authorization.').

omega_variable(
    remedy_necessity_boundary,
    'Are civil damages and personal liability necessary to vindicate constitutional rights, or do equitable remedies (injunctions, declaratory judgments, administrative discipline) sufficiently enforce the Constitution without exposing officers to personal financial liability?',
    'Comparative analysis of deterrent effects, victim vindication, and constitutional enforcement across jurisdictions using different remedy structures; theoretical and empirical assessment of whether injunctions and administrative discipline achieve the enforcement function that damage liability would.',
    'If alternative remedies suffice, immunity could be reformulated to preserve protection from damages while maintaining accountability through other mechanisms, potentially shifting toward tangled_rope or scaffold. If damages are necessary, immunity is indefensible as a systematic denial of constitutional remedy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedy_necessity_boundary, conceptual, 'Whether damages liability is necessary to enforce constitutional rights or whether alternative remedies suffice.').

omega_variable(
    systemic_extraction_intentionality,
    'To what extent is the current expansive application of qualified immunity an intended consequence of the doctrine''s design versus an unintended metastasis driven by judicial gatekeeping and the ''clearly established'' test?',
    'Analysis of Supreme Court decisions expanding immunity; interviews with federal judges and their law clerks about the ''clearly established right'' test''s application; comparison of stated doctrinal rationales with actual litigation outcomes and immunity rates.',
    'If expansion is intentional, the doctrine is a designed snare and the courts are knowing beneficiary-administrators. If expansion is unintended, the doctrine is a scaffold that has metastasized through judicial decision-making, with potential for doctrinal correction. This affects whether federal courts are beneficiary (intentional) or merely agenda-setter (unintended drift).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_extraction_intentionality, empirical, 'Intentionality and design of qualified immunity''s expansive scope.').

omega_variable(
    reading_foreclosure_test,
    'Does the accountability_void_reading logically foreclose the protective_scaffold_reading, or do they coexist as live positions held by different institutional parties?',
    'Logical analysis of the core premises: the protective_scaffold_reading holds that immunity is necessary; the accountability_void_reading holds that the founding problem is dead and immunity is now pure extraction. If the scaffolding claim and the extraction claim are empirically contingent (both depend on whether the problem is live), they coexist — one party can maintain scaffolding is necessary, another can maintain the problem is solved. If one premise logically contradicts the other within a single framework, foreclosure applies.',
    'If they coexist, the readings are both live; if they foreclose, only one can be held in a coherent framework. This determines the reading_relations atom for this constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Logical relationship between accountability_void and protective_scaffold readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1982, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1982, 0.35).
narrative_ontology:measurement_basis(qual_tr_t1982, observed).
narrative_ontology:measurement(qual_tr_t1995, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1995, 0.41).
narrative_ontology:measurement_basis(qual_tr_t1995, observed).
narrative_ontology:measurement(qual_tr_t2005, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2005, 0.49).
narrative_ontology:measurement_basis(qual_tr_t2005, observed).
narrative_ontology:measurement(qual_tr_t2015, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2015, 0.56).
narrative_ontology:measurement_basis(qual_tr_t2015, observed).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2024, 0.62).
narrative_ontology:measurement_basis(qual_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(qual_be_t1982, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1982, 0.65).
narrative_ontology:measurement_basis(qual_be_t1982, observed).
narrative_ontology:measurement(qual_be_t1995, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1995, 0.74).
narrative_ontology:measurement_basis(qual_be_t1995, observed).
narrative_ontology:measurement(qual_be_t2005, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2005, 0.81).
narrative_ontology:measurement_basis(qual_be_t2005, observed).
narrative_ontology:measurement(qual_be_t2015, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2015, 0.87).
narrative_ontology:measurement_basis(qual_be_t2015, observed).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2024, 0.91).
narrative_ontology:measurement_basis(qual_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1982, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1982, 0.72).
narrative_ontology:measurement_basis(qual_su_t1982, observed).
narrative_ontology:measurement(qual_su_t1995, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement_basis(qual_su_t1995, observed).
narrative_ontology:measurement(qual_su_t2005, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2005, 0.81).
narrative_ontology:measurement_basis(qual_su_t2005, observed).
narrative_ontology:measurement(qual_su_t2015, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2015, 0.85).
narrative_ontology:measurement_basis(qual_su_t2015, observed).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2024, 0.88).
narrative_ontology:measurement_basis(qual_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1982, tn=2024
narrative_ontology:measurement(qual_grid_01, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(class), 1982, 0.71).
narrative_ontology:measurement(qual_grid_02, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(class), 2024, 0.79).
narrative_ontology:measurement(qual_grid_03, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(individual), 1982, 0.58).
narrative_ontology:measurement(qual_grid_04, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(individual), 2024, 0.81).
narrative_ontology:measurement(qual_grid_05, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(organizational), 1982, 0.64).
narrative_ontology:measurement(qual_grid_06, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(organizational), 2024, 0.85).
narrative_ontology:measurement(qual_grid_07, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(structural), 1982, 0.75).
narrative_ontology:measurement(qual_grid_08, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(structural), 2024, 0.77).
narrative_ontology:measurement(qual_grid_09, qualified_immunity_doctrine__accountability_void_reading, resistance(class), 1982, 0.58).
narrative_ontology:measurement(qual_grid_10, qualified_immunity_doctrine__accountability_void_reading, resistance(class), 2024, 0.76).
narrative_ontology:measurement(qual_grid_11, qualified_immunity_doctrine__accountability_void_reading, resistance(individual), 1982, 0.45).
narrative_ontology:measurement(qual_grid_12, qualified_immunity_doctrine__accountability_void_reading, resistance(individual), 2024, 0.68).
narrative_ontology:measurement(qual_grid_13, qualified_immunity_doctrine__accountability_void_reading, resistance(organizational), 1982, 0.52).
narrative_ontology:measurement(qual_grid_14, qualified_immunity_doctrine__accountability_void_reading, resistance(organizational), 2024, 0.74).
narrative_ontology:measurement(qual_grid_15, qualified_immunity_doctrine__accountability_void_reading, resistance(structural), 1982, 0.41).
narrative_ontology:measurement(qual_grid_16, qualified_immunity_doctrine__accountability_void_reading, resistance(structural), 2024, 0.62).
narrative_ontology:measurement(qual_grid_17, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(class), 1982, 0.61).
narrative_ontology:measurement(qual_grid_18, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(class), 2024, 0.82).
narrative_ontology:measurement(qual_grid_19, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(individual), 1982, 0.62).
narrative_ontology:measurement(qual_grid_20, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(individual), 2024, 0.88).
narrative_ontology:measurement(qual_grid_21, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(organizational), 1982, 0.58).
narrative_ontology:measurement(qual_grid_22, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(organizational), 2024, 0.79).
narrative_ontology:measurement(qual_grid_23, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(structural), 1982, 0.52).
narrative_ontology:measurement(qual_grid_24, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(structural), 2024, 0.63).
narrative_ontology:measurement(qual_grid_25, qualified_immunity_doctrine__accountability_void_reading, suppression(class), 1982, 0.74).
narrative_ontology:measurement(qual_grid_26, qualified_immunity_doctrine__accountability_void_reading, suppression(class), 2024, 0.86).
narrative_ontology:measurement(qual_grid_27, qualified_immunity_doctrine__accountability_void_reading, suppression(individual), 1982, 0.68).
narrative_ontology:measurement(qual_grid_28, qualified_immunity_doctrine__accountability_void_reading, suppression(individual), 2024, 0.91).
narrative_ontology:measurement(qual_grid_29, qualified_immunity_doctrine__accountability_void_reading, suppression(organizational), 1982, 0.73).
narrative_ontology:measurement(qual_grid_30, qualified_immunity_doctrine__accountability_void_reading, suppression(organizational), 2024, 0.87).
narrative_ontology:measurement(qual_grid_31, qualified_immunity_doctrine__accountability_void_reading, suppression(structural), 1982, 0.71).
narrative_ontology:measurement(qual_grid_32, qualified_immunity_doctrine__accountability_void_reading, suppression(structural), 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__accountability_void_reading, 0.08).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, section_1983_civil_rights_remedy).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, police_department_accountability_mechanisms).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested qualified_immunity_doctrine kernel. The protective_scaffold_reading and constitutional_fidelity_reading are separate constraints instantiating different ε values and claiming different types. All three readings are linked via network.affects_constraints because they contest the same kernel text (Harlow v. Fitzgerald) and the resolution of one reading affects the structural possibility space of the others. The accountability_void_reading argues the doctrine has metastasized from scaffolding to snare; the protective_scaffold_reading argues it remains necessary scaffolding; the constitutional_fidelity_reading argues it was never legitimate. These are not measurement-perspective variants of one constraint — they are three distinct constraints with three distinct ε values, born from decomposing a natural-language kernel concept into structurally precise claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
