% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__living_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__living_constitution_reading, []).

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
 *   constraint_id: us_constitution_interpretive__living_constitution_reading
 *   human_readable: Living Constitution: Evolving Interpretive Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the living-constitution reading of the
 *   contested U.S. constitutional interpretive kernel: the claim that
 *   constitutional meaning legitimately evolves with societal values and that
 *   interpretive authority derives from reasoned adaptation to contemporary
 *   conditions rather than from fixed original meaning or ongoing popular
 *   contestation. This reading has produced real coordination value
 *   (extending protections to previously unprotected groups, allowing federal
 *   regulatory capacity to track industrial and technological change without
 *   the near-impossible Article V amendment process) and real asymmetric
 *   costs (states'-rights displacement, doctrinal instability for
 *   original-meaning adherents, expanded compliance burden for regulated
 *   entities). The ε authored here is for the living-constitution reading's
 *   own operation as the standing interpretive arrangement in periods where
 *   it commands judicial majorities — not for either sibling reading's
 *   alternative, and not averaged across readings.
 *
 * KEY AGENTS:
 *   - civil_rights_expansion_claimants: primary beneficiary of evolving equal protection doctrine
 *   - reproductive_autonomy_advocates: beneficiary whose footing is exposed to interpretive reversal
 *   - lgbtq_rights_claimants: beneficiary secured largely through evolving due process/equal protection reasoning
 *   - federal_regulatory_agencies: institutional beneficiary and co-administrator of the doctrine via litigation
 *   - federal_judiciary: the interpretive authority itself, administering the reading case by case
 *   - states_rights_advocates: primary payer, displaced legislative prerogative
 *   - original_meaning_textualists: primary payer, contest the doctrine's legitimacy directly
 *   - entities_constrained_by_expanded_federal_reach: diffuse payer bearing compliance costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, 0.52).
domain_priors:suppression_score(us_constitution_interpretive__living_constitution_reading, 0.58).
domain_priors:theater_ratio(us_constitution_interpretive__living_constitution_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__living_constitution_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__living_constitution_reading, "Living Constitution: Evolving Interpretive Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__living_constitution_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__living_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__living_constitution_reading, '04d3247b-9150-4066-983c-c80b39326af0').
narrative_ontology:cs_kernel_codification('04d3247b-9150-4066-983c-c80b39326af0', fixed_text).
narrative_ontology:cs_authority_grounding('04d3247b-9150-4066-983c-c80b39326af0', lineage).
narrative_ontology:cs_interpretation_layer_present('04d3247b-9150-4066-983c-c80b39326af0').
narrative_ontology:cs_reading_relation('04d3247b-9150-4066-983c-c80b39326af0', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('04d3247b-9150-4066-983c-c80b39326af0', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('04d3247b-9150-4066-983c-c80b39326af0', foundational, meaning_adapts_to_contemporary_conditions).
narrative_ontology:cs_axiom_status(meaning_adapts_to_contemporary_conditions, holdable).
narrative_ontology:cs_axiom_grounding('04d3247b-9150-4066-983c-c80b39326af0', meaning_adapts_to_contemporary_conditions, instrumental).
narrative_ontology:cs_axiom('04d3247b-9150-4066-983c-c80b39326af0', secondary, unenumerated_rights_derivable_from_structural_principles).
narrative_ontology:cs_axiom_status(unenumerated_rights_derivable_from_structural_principles, holdable).
narrative_ontology:cs_axiom_grounding('04d3247b-9150-4066-983c-c80b39326af0', unenumerated_rights_derivable_from_structural_principles, deontological).
narrative_ontology:cs_reference_frame('04d3247b-9150-4066-983c-c80b39326af0', post_new_deal_evolving_doctrine_settlement).
narrative_ontology:cs_drift_state('04d3247b-9150-4066-983c-c80b39326af0', contemporary_originalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('04d3247b-9150-4066-983c-c80b39326af0', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, federal_regulatory_agencies).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, states_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, entities_constrained_by_expanded_federal_reach).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on courts reading the Fourteenth Amendment's equal protection and due process clauses expansively to secure protections not enumerated at ratification. Their legal standing and remedies depend directly on judges treating constitutional meaning as capable of growth; a fixed-meaning regime would foreclose many of the doctrines they invoke.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants, beneficiary,
    moderate, generational, constrained, national).

% Have historically depended on unenumerated privacy and liberty rights derived from substantive due process. Their legal footing rises and falls with whether the interpretive authority is understood to evolve; recent reversals demonstrate how exposed this beneficiary group is to a change in interpretive reading.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates, beneficiary,
    moderate, biographical, constrained, national).

% Secured marriage equality and anti-discrimination protections substantially through evolving equal protection and due process doctrine rather than text fixed at ratification or by statute. Exit from the constitutional system is not available; their legal status is a direct function of which interpretive reading commands a judicial majority.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants, beneficiary,
    moderate, generational, constrained, national).

% Operate broad regulatory mandates (environmental, labor, financial, health) justified by an expansively read Commerce Clause and implied federal powers doctrine. They both benefit from and actively litigate to preserve the evolving-interpretation framework that grounds their statutory authority.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_regulatory_agencies, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__living_constitution_reading, federal_regulatory_agencies, agenda_setter).

% Federal appellate and supreme courts are the mechanism through which the living-constitution reading is applied — they decide, case by case, whether text extends to new circumstances by reasoned adaptation. They administer the interpretive authority itself and could, by shifting doctrine, contract or expand its scope.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Argue that expansive federal Commerce Clause and implied-powers readings have displaced state legislative prerogatives reserved by the Tenth Amendment. States cannot exit the federal constitutional order; their only recourse is litigation, constitutional amendment (practically foreclosed by supermajority requirements), or waiting for a doctrinal shift via judicial appointments.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, states_rights_advocates, payer,
    organized, generational, constrained, national).

% Judges, scholars, and litigants who hold that fixed original meaning is the only legitimate interpretive method experience the living-constitution reading as a direct usurpation of legitimate constitutional authority. They cannot opt out of the doctrine when it commands a judicial majority; their remedy is confined to appointments, litigation strategy, and public argument.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists, payer,
    organized, civilizational, constrained, national).

% Businesses, individuals, and local governments subject to federal regulation justified by evolving Commerce Clause doctrine (e.g., environmental rules, healthcare mandates, labor standards) that would not exist under a narrower original-meaning reading. They bear compliance costs directly traceable to the interpretive expansion and have limited exit short of relocating outside U.S. jurisdiction entirely.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, entities_constrained_by_expanded_federal_reach, payer,
    moderate, biographical, constrained, national).

% Study and critique the doctrine's coherence, its historical pedigree, and its comparative outcomes against originalist and popular-constitutionalist readings. They do not hold power to adjudicate but shape the intellectual terrain judges draw on.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__living_constitution_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__living_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for constitutional meaning to track changed social facts and moral understandings without requiring the formal Article V amendment process for every adaptation — solving the genuine problem that a document ratified in 1788 cannot anticipate every future circumstance.
% TRANSFER_FUNCTION: Moves interpretive and legislative authority from state legislatures and the constitutional amendment process toward the federal judiciary and federal regulatory agencies; moves substantive legal protections toward previously unprotected groups and moves compliance burdens toward entities newly reached by expanded federal doctrine.
% ABSENT_VOICES: Framers-era drafters cannot speak to what they intended for circumstances they could not foresee, which both sides of the kernel contest invoke selectively. State legislatures displaced by expansive federal Commerce Clause readings are formally represented in Congress but not in the judicial doctrine-setting process itself, where the interpretive reading is actually adjudicated.
% DISAPPEARANCE_RATIONALE: If the living-constitution reading vanished and courts adopted strict original-meaning interpretation exclusively, doctrines protecting privacy, reproductive autonomy, LGBTQ+ rights, and broad federal regulatory authority would lose their primary judicial grounding overnight; entire areas of federal regulation and individual-rights litigation would need new textual or statutory bases or would lapse.
% FOUNDING_PROBLEM: The Constitution's text is terse, was drafted for an agrarian 18th-century society, and does not explicitly address many circumstances (industrial economies, digital privacy, reproductive technology, sexual orientation) that the reasoned-adaptation approach was developed to accommodate without triggering Article V's near-impossible amendment threshold.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative constitutional scholars outside the immediate beneficiary groups (e.g., scholars studying Article V's near-total practical inoperability since 1971) corroborate that the amendment process is functionally closed, which is independent evidence the adaptation problem is structurally real. Original-meaning textualists and states'-rights advocates, from outside the beneficiary set, corroborate that the problem persists but dispute that judicial reinterpretation rather than legislative or amendment action is the legitimate solution — they attest the founding problem is live but contest which institution should resolve it.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__living_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__living_constitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__living_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__living_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__living_constitution_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__living_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__living_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.52 — this reading transfers real interpretive authority from state legislatures and the amendment process to the federal judiciary, and imposes real compliance costs via expanded federal reach, but it also produces genuine coordination value (rights extension, regulatory adaptability) that a pure-extraction snare would not. Suppression (0.58) reflects that alternative interpretive methods (originalism, popular constitutionalism) are not eliminated but are actively contested and periodically displaced by shifting judicial composition — this is coercive in the sense that whichever reading commands a majority binds the losing side without their consent, but it is not total suppression since the doctrinal battle remains genuinely live across generations. Theater ratio is modest (0.28) because the doctrine performs substantive interpretive work in most cases rather than functioning as pure legitimating cover. Accessibility collapse is moderate (0.45): once a case is decided, practical alternatives collapse for the litigants, but the interpretive method itself remains contestable at the level of judicial appointments and future litigation — it has not permanently foreclosed originalism as this reading's own tradition periodically loses ground to the sibling readings. Resistance is high (0.72), reflecting sustained organized opposition (states'-rights advocacy, originalist legal movement, federalism societies) that has, in the current period, achieved significant doctrinal reversals.
 *
 * PERSPECTIVAL GAP:
 *   From the federal judiciary's own seat, this reading is coordination — a principled method for keeping foundational text functional across changing circumstances, avoiding either constitutional ossification or constant amendment churn. From the states'-rights and original-meaning textualist seats, the identical mechanism computes as extraction of interpretive authority they regard as illegitimately seized rather than granted. The engine's per-seat computation should surface this divergence directly from the beneficiary/victim and exit-option data rather than requiring either seat's self-description to be adjudicated.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil rights, reproductive autonomy, and LGBTQ+ rights claimants are structural beneficiaries — the doctrine is the primary vehicle securing their legal protections, so their directionality sits near the full-beneficiary end. Federal regulatory agencies are institutional beneficiaries with mobile exit options since they can pursue alternative statutory or administrative bases if doctrine shifts. States'-rights advocates and original-meaning textualists are structural targets: the doctrine directly displaces their preferred interpretive method and legislative prerogatives, and their exit options are constrained because they cannot leave the constitutional system, only contest it through appointments and litigation over long time horizons. Entities constrained by expanded federal reach are targets bearing diffuse compliance costs traceable to the interpretive expansion, with constrained exit short of relocating outside U.S. jurisdiction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a terse 18th-century text needing to govern circumstances its drafters could not foresee, combined with a functionally closed formal amendment process — remains genuinely live by the corroboration of scholars outside the beneficiary set who study Article V's near-total inoperability. This blocks a facile mandatrophy verdict: the doctrine is not simply an atrophied artifact defended by inertia, because the coordination function it performs (enabling textual adaptation without formal amendment) continues to be exercised, not merely gestured at. What keeps this from being clean coordination is the asymmetric cost structure: the reading's application in any given era systematically favors litigants and institutional actors aligned with the judiciary's contemporary composition, and reverses course when that composition changes, imposing real transition costs on whichever side loses. The tangled_rope classification (rather than pure rope or pure snare) reflects exactly this: real coordination function, real asymmetric extraction, sustained by active judicial enforcement — not a static resolved status but a structure held in permanent tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_vs_usurpation_framing,
    'Is the living-constitution method a legitimate interpretive tool inherent to any workable constitutional order, or is it a judicial usurpation of authority properly reserved to the amendment process and elected legislatures?',
    'No empirical resolution exists — this is a foundational jurisprudential dispute about the source of interpretive legitimacy. Partial evidence: comparative study of peer constitutional democracies with more amendable founding documents, to see whether functional adaptation needs occur there without doctrinal evolution playing the same load-bearing role.',
    'If adaptation is illegitimate usurpation, the coordination function claimed here is largely pretextual and the constraint is better classified nearer snare; if legitimate, the tangled_rope classification (real coordination plus real asymmetric cost) holds as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_vs_usurpation_framing, conceptual, 'Whether evolving interpretation is legitimate constitutional method or judicial overreach — the central kernel dispute.').

omega_variable(
    sibling_reading_structural_delta,
    'What would change structurally if the originalist_reading or popular_constitutionalism_reading became the dominant judicial or political practice instead?',
    'Comparative doctrinal analysis: originalist_reading would narrow federal Commerce Clause scope, eliminate most unenumerated-rights doctrine, and shift interpretive authority toward ratification-era historical inquiry — its beneficiary/victim sets substantially invert relative to this reading (states''-rights advocates and textualists become beneficiaries; civil rights and reproductive autonomy claimants become the payer class). popular_constitutionalism_reading would shift interpretive authority away from courts entirely toward legislatures and social movements, reducing judicial power scope for both this reading and originalism alike.',
    'Confirms the ε-invariance requirement: because the beneficiary and victim sets substantially invert or restructure under each sibling reading, each is authored as its own separate constraint story rather than as an alternative measurement of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents where the kernel disagreement is structurally located, per Rule 2 of the committer frame.').

omega_variable(
    judicial_composition_dependency,
    'Is the reading''s actual dominance a function of reasoned jurisprudential consensus or simply a function of which political coalition controls judicial appointments at a given moment?',
    'Track doctrinal reversal rate against changes in judicial composition versus changes in scholarly or public consensus; a high correlation with appointment changes and low correlation with independent argument would support the composition-dependency reading.',
    'If composition-dependent, the suppression and resistance metrics should be read as measuring a live political contest rather than a settled interpretive method, reinforcing the tangled_rope (contested, enforcement-dependent) rather than rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_composition_dependency, empirical, 'Whether the reading''s dominance tracks argument quality or judicial appointment politics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__living_constitution_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__living_constitution_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__living_constitution_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__living_constitution_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_interpretive__living_constitution_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(us_c_tr_t80, us_constitution_interpretive__living_constitution_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(us_c_tr_t100, us_constitution_interpretive__living_constitution_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(us_c_be_t60, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 60, 0.47).
narrative_ontology:measurement(us_c_be_t80, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 80, 0.5).
narrative_ontology:measurement(us_c_be_t100, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(us_c_su_t60, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(us_c_su_t80, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(us_c_su_t100, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__living_constitution_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three members of the us_constitution_interpretive kernel family. living_constitution_reading (this story), originalist_reading, and popular_constitutionalism_reading each author a distinct ε, beneficiary/victim structure, and classification for the same underlying contested kernel — the source and legitimacy of constitutional interpretive authority. Per the ε-invariance principle, these are not one constraint measured three ways but three structurally distinct constraints whose beneficiary sets substantially diverge (in some cases inverting) across readings. All three should link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
