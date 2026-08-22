% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine: Fifth Amendment Compensation for Severe Value Diminution
 *   domain: constitutional_law/property_rights
 *
 * SUMMARY:
 *   The regulatory takings doctrine holds that regulations that go
 *   sufficiently 'far' in diminishing property value without physical
 *   appropriation may constitute a compensable taking under the Fifth
 *   Amendment. This reading instantiates one interpretation of the contested
 *   takings clause: it expands 'taking' beyond direct physical seizure to
 *   include severe economic diminution through regulation, requiring courts
 *   to apply an ad hoc balancing test (Penn Central factors: the economic
 *   impact of the regulation, its interference with reasonable
 *   investment-backed expectations, and the nature of the government action).
 *   This reading is directly opposed to the physical_appropriation_reading
 *   (which recognizes only direct seizures) and creates a spectrum relative
 *   to categorical_takings_reading (which carves out per se rules for
 *   physical occupations and total value elimination but uses Penn Central
 *   for other cases). The doctrine generates coordination failure: property
 *   owners gain protection against regulatory 'overreach,' but regulatory
 *   agencies must internalize compensation costs, creating a de facto veto
 *   power for property owners over stringent regulation. Environmental and
 *   public-interest constituencies pay through weakened regulatory protection
 *   and exclusion from the balancing test.
 *
 * KEY AGENTS:
 *   - property_owners_with_regulatory_impact: beneficiaries (gain compensation claims for value diminution)
 *   - real_estate_development_interests: beneficiaries + agenda-setters (drive litigation, shape judicial doctrine)
 *   - regulatory_agencies: payers (must compensate or restrict stringency)
 *   - environmental_constituencies: excluded payers (no standing in takings doctrine, absorb regulatory weakening)
 *   - public_interest_constituencies: powerless payers (trapped, non-participating)
 *   - regulatory_restraint_advocates: agenda-setters (litigators and judges extending the doctrine)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.68).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.52).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine: Fifth Amendment Compensation for Severe Value Diminution").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, '920b5bc0-bbf5-430e-bbca-b5e33110ac87').
narrative_ontology:cs_kernel_codification('920b5bc0-bbf5-430e-bbca-b5e33110ac87', fixed_text).
narrative_ontology:cs_authority_grounding('920b5bc0-bbf5-430e-bbca-b5e33110ac87', lineage).
narrative_ontology:cs_interpretation_layer_present('920b5bc0-bbf5-430e-bbca-b5e33110ac87').
narrative_ontology:cs_reading_relation('920b5bc0-bbf5-430e-bbca-b5e33110ac87', takings_clause_boundary__physical_appropriation_reading, forecloses).
narrative_ontology:cs_reading_relation('920b5bc0-bbf5-430e-bbca-b5e33110ac87', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('920b5bc0-bbf5-430e-bbca-b5e33110ac87', foundational, economic_value_is_protected_property_interest).
narrative_ontology:cs_axiom_status(economic_value_is_protected_property_interest, holdable).
narrative_ontology:cs_axiom_grounding('920b5bc0-bbf5-430e-bbca-b5e33110ac87', economic_value_is_protected_property_interest, deontological).
narrative_ontology:cs_axiom('920b5bc0-bbf5-430e-bbca-b5e33110ac87', foundational, ad_hoc_balancing_test_legitimate_takings_measure).
narrative_ontology:cs_axiom_status(ad_hoc_balancing_test_legitimate_takings_measure, holdable).
narrative_ontology:cs_axiom_grounding('920b5bc0-bbf5-430e-bbca-b5e33110ac87', ad_hoc_balancing_test_legitimate_takings_measure, empirically_contingent).
narrative_ontology:cs_reference_frame('920b5bc0-bbf5-430e-bbca-b5e33110ac87', property_rights_including_economic_use_value).
narrative_ontology:cs_drift_state('920b5bc0-bbf5-430e-bbca-b5e33110ac87', contemporary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('920b5bc0-bbf5-430e-bbca-b5e33110ac87', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners_with_regulatory_impact).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, real_estate_development_interests).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, regulatory_agencies).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, environmental_constituencies).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, public_interest_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_rights_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own land or development rights subject to regulations (zoning, environmental protection, coastal access) that substantially diminish economic value without physical appropriation. Under this reading, they gain the right to claim compensation when regulation 'goes too far' — triggering a balancing test rather than facing categorical denial. Their exit is constrained because the property remains under their title but its use is restricted; they cannot sell it for intended purpose but can claim takings damages if the court agrees the regulation exceeds permissible police power.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners_with_regulatory_impact, beneficiary,
    moderate, generational, constrained, national).

% Developers and investment funds that acquire land anticipating regulatory approval or regulatory rollback. The regulatory takings doctrine creates opportunities for valuation arbitrage: purchase restricted land at depressed prices, litigate takings claims, and capture either compensation or regulatory variance. They have the institutional capacity and litigation budget to test the doctrine's boundaries; their exit is arbitrage because they can shift geography or litigation timing.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, real_estate_development_interests, beneficiary,
    organized, generational, arbitrage, national).

% Environmental, land use, and public health agencies that promulgate regulations intended to protect shared resources (wetlands, air quality, habitat, coastal access). Under this reading, agencies must now either pay compensation for regulations that 'go too far' or restrict regulatory ambition to stay within the ad hoc balancing test. Their exit is constrained: they cannot simply prohibit the use without risking a takings judgment; they must internalize compensation costs or adjust regulatory stringency downward.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Environmental organizations and constituencies that depend on regulatory protection: preservation groups, wildlife advocates, clean-water coalitions. The regulatory takings doctrine creates fiscal drag on regulation — agencies must now justify stringency not just on environmental grounds but on cost-benefit grounds that factor in compensation liability. These constituencies are structurally excluded from takings balancing (they are not property owners and have no standing to claim the compensation); their interests are diluted by the fiscal pressure on regulators.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, environmental_constituencies, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__regulatory_takings_reading, environmental_constituencies, excluded).

% Non-property-owning publics that benefit from regulation (neighborhood residents near proposed development, users of public lands and waters, future generations). They bear the cost of weakened regulation through degraded environmental quality and lost public goods but have no seat at the takings table and no way to contest takings judgments. Their exit is trapped: they cannot relocate easily if a regulation they depend on is struck down as a taking.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, public_interest_constituencies, payer,
    powerless, biographical, trapped, national).

% Scholars and jurists who hold that property rights include a presumptive entitlement to economic use, and that regulations that substantially diminish value without compensation violate those rights. They vindicate a normative claim: economic value is a cognizable property interest deserving constitutional protection. Their analytical seat carries no enforcement power but shapes the intellectual framework through which courts interpret the takings clause.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_rights_theorists, beneficiary,
    analytical, generational, analytical, national).

% The institutional coalition (including property-rights litigators, development interests, and judicial skeptics of expansive regulation) that defends and extends the regulatory takings doctrine through litigation and judicial appointments. They set the agenda by: (1) bringing test cases that expand what counts as a compensable 'taking'; (2) building administrative resistance to regulation in anticipation of takings liability; (3) supporting judges who adopt broader constructions of the doctrine. Their arbitrage is temporal: they can defer regulation through litigation risk, reducing near-term development constraints, while building a doctrinal architecture that systematically favors property rights claims.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, regulatory_restraint_advocates, agenda_setter,
    institutional, generational, arbitrage, national).

% Communities that would benefit from stronger regulation but are structurally absent from takings discourse: low-income neighborhoods adjacent to polluting facilities, indigenous communities with claims to ancestral lands, future generations. They would object to the regulatory takings doctrine because it raises the cost of regulations that protect them, but they are not convened in takings litigation and have no institutional machinery to participate in the balancing test.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, regulatory_stringency_constituencies, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__regulatory_takings_reading, real_estate_development_interests).
narrative_ontology:fixing_cost_class(takings_clause_boundary__regulatory_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal test (ad hoc balancing) that allows courts to evaluate when a regulation has crossed the line from legitimate police power into uncompensated appropriation. Without such a test, property owners would face absolute regulatory prohibition (coordinating around a bright-line rule), or property rights would be absolute (no regulation). The doctrine coordinates property owners' expectations about when regulation requires compensation against regulators' need for a workable standard.
% TRANSFER_FUNCTION: Moves fiscal risk and decision authority from property owners toward regulatory agencies: agencies must either compensate for regulations that fail the balancing test, or restrict regulatory ambition. Additionally transfers power to courts (who apply the balancing test and determine 'how far is too far') from legislatures (who set policy). In successful takings claims, moves money from the government treasury to the complaining property owner.
% ABSENT_VOICES: Non-property-owning publics dependent on regulation (renters, users of shared resources, future generations) and environmental constituencies who would argue for regulatory stringency are structurally excluded from the takings balancing test. They have no standing to claim compensation and no seat at the table where 'how far is too far' is negotiated. Regulatory agencies themselves can argue, but they argue from a fiscal-constraint position rather than a public-interest position.
% DISAPPEARANCE_RATIONALE: If the regulatory takings doctrine disappeared overnight, regulatory agencies would immediately expand stringency (no compensation liability to internalize); development incentives would shift dramatically (expected regulatory blocking power increases); litigation dockets would shrink (no takings suits); and property valuations in regulated sectors would reset downward (development projects contingent on takings-claim upside would no longer be viable). The entire institutional apparatus of regulatory restraint that the doctrine anchors would reorganize.
% FOUNDING_PROBLEM: Early takings jurisprudence addressed direct physical seizures and permanent occupations (the government takes your land). As regulation became a primary mechanism of public policy (zoning, environmental protection, historic preservation), the question emerged: does 'taking' apply only to physical possession, or does regulation that demolishes economic value also constitute a taking? The founding problem is the boundary question: where does police power end and compensable appropriation begin?
% FOUNDING_PROBLEM_CORROBORATION: The boundary question remains live and contested. Property-rights litigators and development interests attest it is unresolved and the doctrine continues to evolve. Regulatory agencies and environmental organizations attest the problem is OVER-solved in favor of property — they argue the founding problem of regulatory constraints on property has been excessively relaxed. Economists and property theorists from multiple schools (some defending regulatory takings, others critiquing it as judge-made compensation) attest the boundary remains fundamentally contested.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the doctrine transfers resources from the public (through reduced regulation and compensation payments) to property owners and development interests; the transfer is asymmetric (beneficiaries have litigation capacity, victims are diffuse). Suppression is moderate (0.52): the doctrine suppresses aggressive regulation through compensation liability and litigation risk, but the suppression is not absolute—regulators can still act if they absorb costs. Theater is elevated (0.41): a significant share of doctrinal activity is performative—balancing tests that appear even-handed but systematically weight in favor of compensation, academic debate about legitimate boundaries that masks a structural shift in institutional power. The measurement series shows extractiveness rising steeply in the first 20 time units (doctrine being developed and extended through litigation), then plateauing (doctrine stabilizes around established test). Theater rises similarly but more slowly, reflecting the accumulation of rhetorical infrastructure around the ad hoc balance. This temporal pattern reflects the doctrine's life-cycle: it emerged as a boundary question, expanded through strategic litigation, and has consolidated into an institutional arrangement with stable parameters.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory restraint advocates and development interests sit at institutional power; from their position, the doctrine is a legitimate check on arbitrary regulatory power and a protection for property owners' reasonable expectations. Regulatory agencies also sit at institutional power but with constrained exit: they can restrict regulatory stringency (adapting to the constraint) or absorb compensation costs (changing their budgets and policy priorities), but they cannot ignore the doctrine. The divergence is not primarily about power—it is about interest: institutional players protecting property interests have institutional machinery to advance the regulatory takings reading; institutional players protecting public interests are diffused across agencies without a unified voice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation from beneficiary/victim declarations and exit options: property owners (beneficiaries, constrained exit, moderate power) derive d ~0.25 (beneficiary direction); development interests (beneficiaries, arbitrage exit, organized power) derive d ~0.15 (strong beneficiary position); regulatory agencies (victims, constrained exit, institutional power) derive d ~0.75 (strong payer position with constrained alternatives); environmental constituencies (victims despite organization, constrained exit, moderate power) derive d ~0.65 (payer direction); public-interest constituencies (victims, trapped exit, powerless) derive d ~0.90 (maximum target position). No directionality overrides are necessary—the structural data produces the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The regulatory takings doctrine was founded to solve a real boundary problem: where does legitimate police power end and compensable appropriation begin? The problem is live—courts continue to refine the balancing test and property owners continue to litigate takings claims. However, mandatrophy is NOT present: the doctrine is not an atrophied function maintained by inertia. Instead, the doctrine represents a systematic institutional shift in which property protection has been extended beyond physical appropriation to include economic diminution. The constraint is actively defended and extended by beneficiary interests (development lawyers, property-rights advocates, allied judges). The distribution of costs and benefits is asymmetric by design, not by accident—the doctrine serves the interests it benefits by design. Mandatrophy would apply if the takings clause were a vestigial rule maintained through theater despite having no real function; instead, the doctrine is a working mechanism of property protection that creates real winners (property owners) and real losers (public-interest constituencies).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    how_far_is_too_far_indeterminacy,
    'What constitutes regulation that goes ''too far'' in diminishing value? Does the Penn Central balancing test produce predictable outcomes, or does it systematically advantage property owners through doctrinal evolution?',
    'Empirical study of takings outcomes: do courts grant compensation at higher rates for cases with similar fact patterns over time? Do property-owner claims show higher success rates than regulatory-agency defenses in equivalent circumstances?',
    'If the balancing test proves systematically biased toward property compensation, the constraint reclassifies from tangled_rope (genuine coordination with asymmetric extraction) toward snare (systematic extraction disguised by neutral balancing language). If the test proves genuinely balanced, the constraint remains tangled_rope with legitimate coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(how_far_is_too_far_indeterminacy, empirical, 'Whether the ad hoc balancing test produces predictable, neutral outcomes or systematically favors property-owner claims.').

omega_variable(
    regulatory_chilling_effect_magnitude,
    'How much does takings liability actually chill regulatory innovation? Do regulatory agencies restrict stringency in anticipation of takings claims, or do they proceed with regulation and absorb compensation costs as part of normal operations?',
    'Analysis of regulatory innovation pre- and post-major takings decisions; interviews with agency officials about compensation liability in policy-making; econometric analysis of regulatory stringency relative to takings exposure.',
    'If chilling effect is substantial (agencies restrict stringency in anticipation of liability), the suppression_requirement measurement understates the constraint''s actual suppressive force. If chilling effect is minimal (agencies treat compensation as an operational cost), suppression is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chilling_effect_magnitude, empirical, 'Empirical magnitude of regulatory chilling effect from takings liability.').

omega_variable(
    beneficiary_vs_coordination_boundary,
    'Is the regulatory takings doctrine primarily a coordination mechanism (establishing when property owners'' reasonable expectations are protected), or is it primarily an extractive mechanism (transferring resources from regulators to property owners)?',
    'Analysis of doctrine''s origins and evolution: if it emerged to solve a genuine coordination problem (uncertain boundaries between police power and appropriation), it carries coordination function; if it emerged through strategic litigation by development interests to weaken regulatory constraints, it carries primary extraction function.',
    'If primarily coordination, the constraint''s classification as tangled_rope is correct. If primarily extractive, the constraint reclassifies toward snare. The beneficiary/victim structure suggests extraction dominates, but the coordination function (establishing a workable boundary test) is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_vs_coordination_boundary, conceptual, 'Whether the doctrine''s primary function is coordination or extraction.').

omega_variable(
    standing_and_representation_asymmetry,
    'Is the exclusion of environmental and public-interest constituencies from takings balancing a structural feature of property law (non-owners lack standing), or is it a deployable design choice that could be reformed to broaden participation?',
    'Comparative constitutional analysis: do other legal systems provide standing mechanisms for non-owners to assert public-interest arguments in takings cases? If yes, do those mechanisms produce different balancing outcomes?',
    'If the exclusion is structurally immutable, it is a permanent feature of the constraint and the asymmetry between beneficiary-represented and victim-excluded seats is constitutive. If the exclusion is a design choice, widening standing would alter the balancing test, potentially reducing extractiveness and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(standing_and_representation_asymmetry, conceptual, 'Whether standing limitations in takings doctrine are structural features or reform targets.').

omega_variable(
    kernel_reading_sibling_status,
    'Is the regulatory_takings_reading a currently live position that courts and scholars defend, or has it been substantially superseded by the categorical_takings_reading in actual jurisprudence?',
    'Longitudinal analysis of Supreme Court takings decisions: do recent decisions embrace ad hoc balancing (regulatory takings approach) or categorical rules with Penn Central fallback (categorical approach)? Do scholarly defenses of the regulatory reading remain vigorous?',
    'If the regulatory reading is live and expanding (courts extending takings to more regulatory contexts), the constraint remains active. If the reading is being narrowed (recent decisions categorizing more cases as non-takings per se), the constraint may be attenuating and headed toward piton status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_status, empirical, 'Whether the regulatory takings reading remains a live, expanding doctrine or is being narrowed by recent jurisprudence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(taki_tr_t0, observed).
narrative_ontology:measurement(taki_tr_t5, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(taki_tr_t5, observed).
narrative_ontology:measurement(taki_tr_t10, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(taki_tr_t10, observed).
narrative_ontology:measurement(taki_tr_t15, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(taki_tr_t15, observed).
narrative_ontology:measurement(taki_tr_t20, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(taki_tr_t20, observed).
narrative_ontology:measurement(taki_tr_t25, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(taki_tr_t25, observed).
narrative_ontology:measurement(taki_tr_t30, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(taki_tr_t30, observed).
narrative_ontology:measurement(taki_tr_t35, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(taki_tr_t35, projected).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(taki_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(taki_be_t0, observed).
narrative_ontology:measurement(taki_be_t5, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(taki_be_t5, observed).
narrative_ontology:measurement(taki_be_t10, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(taki_be_t10, observed).
narrative_ontology:measurement(taki_be_t15, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(taki_be_t15, observed).
narrative_ontology:measurement(taki_be_t20, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(taki_be_t20, observed).
narrative_ontology:measurement(taki_be_t25, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(taki_be_t25, observed).
narrative_ontology:measurement(taki_be_t30, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(taki_be_t30, observed).
narrative_ontology:measurement(taki_be_t35, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(taki_be_t35, projected).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(taki_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(taki_su_t0, observed).
narrative_ontology:measurement(taki_su_t5, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(taki_su_t5, observed).
narrative_ontology:measurement(taki_su_t10, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(taki_su_t10, observed).
narrative_ontology:measurement(taki_su_t15, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(taki_su_t15, observed).
narrative_ontology:measurement(taki_su_t20, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(taki_su_t20, observed).
narrative_ontology:measurement(taki_su_t25, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement_basis(taki_su_t25, observed).
narrative_ontology:measurement(taki_su_t30, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(taki_su_t30, observed).
narrative_ontology:measurement(taki_su_t35, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement_basis(taki_su_t35, projected).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(taki_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__regulatory_takings_reading, 0.18).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__categorical_takings_reading).

% DUAL FORMULATION NOTE:
% The regulatory_takings_reading is one interpretation of the contested takings_clause_boundary kernel. The physical_appropriation_reading restricts takings to direct physical seizure (narrow victim set, low extractiveness, no active enforcement). The categorical_takings_reading carves out per se rules for physical occupations and total value elimination, then applies Penn Central balancing to other cases (intermediate victim set, moderate extractiveness). The regulatory_takings_reading extends takings to all regulations with severe value diminution, using ad hoc balancing (broad victim set, high extractiveness, active enforcement required). These three readings are structurally distinct constraints with different ε values, beneficiary/victim sets, and institutional consequences. They compete in actual jurisprudence: the Supreme Court has at different times weighted toward each reading, and recent decisions show movement between them. Each story is a reading of the same kernel; the three together form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
