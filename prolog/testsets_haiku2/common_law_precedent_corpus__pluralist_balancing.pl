% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__pluralist_balancing, []).

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
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Common Law Precedent Corpus—Pluralist Balancing Reading
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The pluralist balancing reading of common law precedent holds that the
 *   weight precedent carries varies by domain and context. Property law
 *   maintains strict stare decisis because market participants require
 *   predictable legal stability; constitutional law permits greater departure
 *   because new circumstances and normative evolution demand
 *   reinterpretation. Precedent itself is a contested kernel—strict
 *   formalists would bind it universally, evolutionary theorists would permit
 *   systematic reinterpretation, and the pluralist reading attempts to
 *   navigate between by domain-contingent weights. This story instantiates
 *   the pluralist reading: precedent binds domain-by-domain according to
 *   whether that domain's coordination demands require stability or
 *   adaptability. The claim (tangled_rope) reflects that the regime genuinely
 *   coordinates legal practice within domains while extracting from litigants
 *   through unpredictable domain classification and enforcement of
 *   domain-specific precedent weight.
 *
 * KEY AGENTS:
 *   - Appellate judges: set and adjudicate domain-specific precedent weight; their discretion in domain classification is the core institutional authority.
 *   - Established doctrinal schools: benefit from high precedent weight in their domains; property law, settled constitutional doctrine, contract law accumulate authority.
 *   - Litigants seeking departure: pay in legal cost and likelihood of failure; domain classification is ex ante opaque; unpredictable switching costs.
 *   - Lower court practitioners: navigate multi-tier hierarchy; must adapt to appellate court's domain-contingent precedent weights.
 *   - Formalist jurisprudents: excluded—would demand universal rule rather than domain variance.
 *   - Legal innovation advocates: excluded—would argue domain classification is a mechanism of stabilization bias.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.58).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.41).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.58).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.53).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Common Law Precedent Corpus—Pluralist Balancing Reading").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, '8a1b131a-2d52-4d8d-bf9d-408f818e4b52').
narrative_ontology:cs_kernel_codification('8a1b131a-2d52-4d8d-bf9d-408f818e4b52', fixed_text).
narrative_ontology:cs_authority_grounding('8a1b131a-2d52-4d8d-bf9d-408f818e4b52', lineage).
narrative_ontology:cs_interpretation_layer_present('8a1b131a-2d52-4d8d-bf9d-408f818e4b52').
narrative_ontology:cs_reading_relation('8a1b131a-2d52-4d8d-bf9d-408f818e4b52', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('8a1b131a-2d52-4d8d-bf9d-408f818e4b52', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('8a1b131a-2d52-4d8d-bf9d-408f818e4b52', foundational, domain_contingent_precedent_weight).
narrative_ontology:cs_axiom_status(domain_contingent_precedent_weight, holdable).
narrative_ontology:cs_axiom_grounding('8a1b131a-2d52-4d8d-bf9d-408f818e4b52', domain_contingent_precedent_weight, instrumental).
narrative_ontology:cs_axiom('8a1b131a-2d52-4d8d-bf9d-408f818e4b52', foundational, coordinated_departure_via_appellate_discretion).
narrative_ontology:cs_axiom_status(coordinated_departure_via_appellate_discretion, holdable).
narrative_ontology:cs_axiom_grounding('8a1b131a-2d52-4d8d-bf9d-408f818e4b52', coordinated_departure_via_appellate_discretion, deontological).
narrative_ontology:cs_reference_frame('8a1b131a-2d52-4d8d-bf9d-408f818e4b52', balanced_domain_specific_precedent_doctrine).
narrative_ontology:cs_drift_state('8a1b131a-2d52-4d8d-bf9d-408f818e4b52', contemporary_appellate_jurisprudence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8a1b131a-2d52-4d8d-bf9d-408f818e4b52', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_judges).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, established_doctrinal_schools).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, litigants_seeking_departure).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, lower_court_practitioners).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, rule_of_law_principle).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, judicial_restraint_norm).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, precedent_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges of appellate courts set the weight accorded to precedent within their domain. In the pluralist reading, they retain discretion to balance stability (respecting prior holdings) against contextual adaptation (departing when circumstances substantially changed or reasoning failed). They exercise this discretion via doctrine-specific tests: strict stare decisis in property/contract, moderate deference in administrative law, greater flexibility in constitutional interpretation. Their authority rests on the presumed capacity to identify when a domain warrants stability versus adaptation.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_judges, agenda_setter,
    institutional, generational, constrained, national).

% Legal doctrines and interpretive traditions that have accumulated precedent—property law, contract law, administrative procedure, established constitutional readings—benefit from a regime that weights their precedent heavily in their home domains while permitting departure elsewhere. They stabilize around the precedent base and attract scholarship, practice, and professional identity. A judge inclined to preserve established doctrine has a formal justification within the pluralist framework: 'this domain values stability.' Their benefit is indirect: they do not collect rents, but they persist as authoritative frameworks.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, established_doctrinal_schools, beneficiary,
    institutional, generational, arbitrage, national).

% Parties litigating arguments for precedent departure face domain-dependent barriers. In high-stability domains (property, settled constitutional doctrine), their burden is prohibitive—they must show extraordinary circumstances or fundamental error. In lower-stability domains (emerging constitutional questions, novel regulatory contexts), the barrier is moderate. The pluralist reading creates unpredictability at the domain boundary: a litigant cannot know ex ante whether their domain will be treated as stable or adaptive. They pay in legal cost (developing departure arguments that may fail based on domain classification) and in likelihood of losing when their domain is deemed stable.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, litigants_seeking_departure, payer,
    moderate, biographical, constrained, national).

% Trial court judges and litigators must navigate a multi-tier precedent hierarchy where the weight of precedent varies by domain and context. They follow appellate authority, but the appellate frame itself is unstable—a circuit or state court's treatment of precedent in domain A does not signal treatment in domain B. They bear the cost of doctrinal uncertainty: preparing cases for multiple departure scenarios, explaining to clients why precedent weight is domain-contingent, and adapting practice as appellate courts rebalance stability/adaptation within domains.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, lower_court_practitioners, payer,
    moderate, biographical, constrained, regional).

% Jurists and legal philosophers who argue that the rule of law requires determinate, predictable precedent weight (either universally stable or universally adaptive) are excluded from the pluralist conversation. They would advocate for either strict stare decisis (universally stable) or systematic overruling doctrine (universally adaptive), rejecting domain variance as incoherent. The pluralist reading brackets their objection by asserting that balancing is itself coherent jurisprudence, not a violation of rule-of-law principle.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, rule_of_law_conception_formalists, excluded,
    institutional, generational, trapped, national).

% Movements for legal reform, critical legal scholars, and advocates for rights expansion would argue that domain-contingent precedent weight is a mechanism of stabilization bias—the pluralist reading will classify their domains as 'stable' and foreclose departure regardless of changed circumstances. They are excluded in the sense that their voice in the ballpark of accepted jurisprudence is muted by the framework that pre-adjudicates which domains deserve flexibility.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_innovation_advocates, excluded,
    organized, biographical, trapped, national).

% The Supreme Court (or equivalent apex body) observes the pluralist system it has instantiated. It retains the power to reclassify domain stability, to articulate new tests for when departure is warranted, and to overrule prior decisions across all domains if necessary. It is the court of last resort precisely because it stands outside the domain-specific regime and can adjust the system itself.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, highest_appellate_authority, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__pluralist_balancing, appellate_judges).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__pluralist_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Precedent as a coordination device: establishes predictable legal frameworks within domains so parties can plan, contract, and litigate with knowledge of applicable law. Domain-specific variance acknowledges that different legal domains have different coordination demands—property law requires high predictability, constitutional law requires adaptability to new circumstances and values.
% TRANSFER_FUNCTION: Moves legitimacy authority from case-by-case reasoning to precedent-bound reasoning within domains, concentrating the authority to depart (to appellate judges who define domain stability) while diffusing the burden of compliance (on lower courts and litigants who must navigate unpredictable domain classifications). The constraint transfers interpretive burden from rule (does this precedent bind universally?) to jurisdiction (in this domain, does precedent bind tightly or loosely?).
% ABSENT_VOICES: Strict formalists and legal innovation advocates are excluded. Strict formalists would argue the pluralist reading incoherently mixes rule-bound and discretionary reasoning; legal innovators would argue it uses 'domain stability' as a mechanism to foreclose their reform arguments. Both would demand a seat in determining whether a domain warrants stability or adaptation, rather than accepting appellate judges' domain classification as pre-determined.
% DISAPPEARANCE_RATIONALE: If the pluralist balancing regime vanished, legal practice would reorganize around one of two alternatives: either strict stare decisis (precedent binds universally unless overruled at the apex), or systematic overruling doctrine (precedent is always rebuttable). The domain-contingent variance would collapse, and litigants would face a determinate rule instead of a domain-dependent one. Legal institutions have built governance around domain variance; its removal would require recalibration of doctrinal teaching, practice norms, and judicial authority.
% FOUNDING_PROBLEM: Early common law required coherent doctrine across diverse legal domains—property, contract, tort, constitutional—but those domains have evolved differently. Property required stability (land transfers and long-term contracts need predictability); constitutional law required adaptation (new circumstances and normative evolution demand reinterpretation). A universal rule (precedent always binds or never binds) would fail both domains. The pluralist reading was built to solve the need for domain-differentiated precedent weight while preserving overall rule-of-law legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Appellate judges and doctrinal scholars attest the founding problem remains live, citing the damage to each domain if precedent were uniformly strict (ossified constitutional law) or uniformly loose (unpredictable property markets). Legal innovators and rights advocates attest the founding problem is a cover story for stabilization bias—the pluralist reading purports to balance but systematically classifies reform-oriented domains as 'stable.' Independent jurisprudential analysis finds the founding problem genuine but notes that appellate courts have consistently classified innovation-adjacent domains (constitutional law, statutory interpretation) as more flexible, which suggests domain classification itself is contestable.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__pluralist_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__pluralist_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the constraint concentrates authority to depart in appellate hands and requires lower courts and litigants to navigate domain-specific tests. Suppression is moderate (0.41) because the constraint is defended by appeal-court authority and doctrinal consensus, but it faces resistance from those who argue for either stricter stare decisis (domain variance is incoherent) or systematic overruling (all domains should permit departure). Theater ratio is moderate (0.44) and rising slightly: the pluralist frame presents itself as balanced and principled, but a growing share of commentary questions whether domain classification merely rationalizes stabilization bias. The measurement series shows extractiveness rising from t0 to t25, then slightly declining and stabilizing—a pattern consistent with the constraint accumulating institutional entrenchment (higher extraction) as appellate courts repeatedly articulate domain-specific tests, followed by marginal correction as lower courts and litigants push back on domain classifications they perceive as biased. Theater ratio shows a similar pattern: the legitimating narrative ('balanced domain-specific precedent weight') gains rhetorical dominance in the middle interval, then slightly recedes as the gap between the balancing claim and the actual domain classifications becomes harder to ignore.
 *
 * PERSPECTIVAL GAP:
 *   From the appellate judge's seat, the constraint is genuine coordination—it provides a framework for deciding precedent weight that respects both stability demands (in property) and adaptability demands (in constitutional law) while maintaining rule-of-law legitimacy. From the litigant's seat seeking departure, the same structure operates as enforced extraction: domain classification is opaque until appeal, changing precedent weight ex post, and domain boundaries themselves are contestable. The lower court sits in between: it must enforce appellate domain classifications while aware that those classifications are domain-contingent and can shift. The engine computes directionality from these structural asymmetries—appellate judges extract from litigants via domain authority, lower courts are constrained by the multi-tier hierarchy, litigants pay in unpredictable legal cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate judges derive d close to 0.0 (beneficiary position): they set the domain classification, retain discretion to adjust domain stability, and face minimal personal cost from the regime. Established doctrinal schools derive d near 0.15 (secondary beneficiaries): they benefit from high precedent weight in their domains but do not directly set the classification. Litigants seeking departure derive d near 0.85 (strong target position): they bear the cost of domain classification, face domain-contingent barriers to departure, and have constrained exit (they cannot choose a different precedent regime). Lower courts derive d around 0.70 (secondary target): they must enforce domain-specific tests but lack the authority to set them, creating dependence on appellate signals. The override for established_doctrinal_schools moves d from a pure beneficiary position (0.0) to a secondary beneficiary position (0.15) to reflect that they benefit indirectly through doctrinal stabilization rather than directly through rule-setting.
 *
 * MANDATROPHY ANALYSIS:
 *   The pluralist reading faces a mandatrophy risk: the founding problem (need for domain-differentiated precedent weight) may become obsolete if legal domains become less distinct or if technology enables case-by-case reasoning instead of domain-based rules. A future in which artificial intelligence can instantly retrieve all relevant precedents and reweigh them for each case might dissolve the coordination problem the pluralist regime solves—domain variance was necessary when precedent search was expensive; perfect retrieval might permit case-by-case balancing. The constraint would persist through institutional inertia (judges trained in domain-specific tests, doctrinal literature organized around domain classifications) rather than active maintenance of the coordination function. The measured theater_ratio (0.44) suggests the constraint is already partly performative—the domain-specific tests are articulated, but the actual application of precedent weight shows drift and contestation. The measurement trajectory does not show terminal decay, so mandatrophy is not yet resolved; but the arena shows enough friction (resistance at 0.53, rising theater) that mandatrophy observation is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_classification_stability,
    'What determines whether a legal domain is classified as requiring stable or adaptive precedent weight? Is the classification grounded in the domain''s coordination demands, or in appellate preference and doctrinal tradition?',
    'Comparative analysis of how appellate courts classify novel domains (emerging constitutional questions, new statutory regimes). If classification tracks coordination demands, novel domains with high coordination needs should be classified as stable regardless of reform advocates'' preferences. If classification tracks appellate preference, domains with similar coordination needs should show inconsistent classifications across time and jurisdiction.',
    'If domain classification is unstable or arbitrary, litigants face even higher unpredictability; the constraint''s extractiveness would rise and the coordination function would degrade. If classification is stable and coordinate-demand-tracking, the constraint provides genuine coordination with acceptable litigant burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_classification_stability, empirical, 'Whether domain classification is objective or preference-driven.').

omega_variable(
    stabilization_bias_in_domain_variance,
    'Does domain-contingent precedent weight systematically stabilize established doctrinal schools at the cost of foreclosing legal innovation? That is, are domains associated with reform (constitutional rights, statutory interpretation of new regimes) classified as requiring less stability, while domains associated with established interests (property, contract) are classified as requiring more?',
    'Historical analysis of domain classifications and their correlation with innovation/stability framing. Interview appellate judges about their reasoning for domain classification. Compare departure rates across domains to assess whether variance tracks stated coordination demands or actual power distributions.',
    'If stabilization bias is systematic, the constraint is a mechanism of established-doctrine entrenchment, not genuine balancing—extractiveness would be higher and the tangled_rope would shift toward snare. If bias is incidental or correctable, the constraint provides genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stabilization_bias_in_domain_variance, conceptual, 'Whether the pluralist reading masks a stabilization bias favoring established doctrines.').

omega_variable(
    domain_boundary_contestability,
    'When two legal domains overlap or a new question falls between traditional domains (e.g., digital privacy in property/tort/statutory law), who determines which domain''s precedent weight applies? Is the determination jurisdictional (appellate authority decides), categorical (the question''s nature determines it), or contestable (litigants can argue for domain reclassification)?',
    'Review appellate court decisions on domain classification disputes. Assess whether parties successfully argue for alternative domain classifications and whether such arguments create precedent or remain case-by-case.',
    'If domain boundaries are contestable, litigants gain some exit option (argue for a different domain with lower precedent weight) and extractiveness decreases. If boundaries are fixed by appellate authority, litigants are trapped in domain classification and extractiveness increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domain_boundary_contestability, empirical, 'Whether domain boundaries are contestable or fixed by appellate authority.').

omega_variable(
    kernel_reading_contention,
    'Which sibling reading of the common_law_precedent_corpus kernel dominates in contemporary jurisprudence, and is the pluralist reading a stable equilibrium or a temporary accommodation between strict stare decisis and evolutionary framework?',
    'Survey appellate court decisions to assess which reading each court instantiates. Compare departure rates across time and jurisdiction to assess whether the pluralist framing is settling into stable doctrine or shifting toward one of the sibling readings.',
    'If the pluralist reading is settling into stable doctrine, extractiveness and theater_ratio should stabilize or decline. If it is shifting toward strict stare decisis or evolutionary framework, the constraint''s type would reclassify and extractiveness would change accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Whether the pluralist reading is a stable equilibrium or a transient accommodation between incompatible interpretive traditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.32).
narrative_ontology:measurement(comm_tr_t8, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 8, 0.36).
narrative_ontology:measurement(comm_tr_t16, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 16, 0.4).
narrative_ontology:measurement(comm_tr_t25, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 25, 0.44).
narrative_ontology:measurement(comm_tr_t35, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 35, 0.45).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 50, 0.44).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comm_be_t8, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(comm_be_t16, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(comm_be_t25, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(comm_be_t35, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 35, 0.6).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comm_su_t8, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(comm_su_t16, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(comm_su_t25, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(comm_su_t35, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 35, 0.42).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 50, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__pluralist_balancing, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).

% DUAL FORMULATION NOTE:
% The common_law_precedent_corpus is a contested kernel admitting three structurally distinct constraint stories. The pluralist_balancing reading (this file) balances precedent weight by domain; strict_stare_decisis binds precedent universally; evolutionary_framework permits systematic reinterpretation. Each reading instantiates a different ε and beneficiary/victim structure. The pluralist reading influences the sibling readings—it sets the boundary conditions within which strict formalism or evolution can argue for departure. All three stories are linked via network.affects_constraints; together they form the common_law_precedent_corpus constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__pluralist_balancing, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
