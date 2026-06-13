% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__regulatory_recognition_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: Digital Money Origin: Regulatory Recognition Reading
 *   domain: monetary_history/institutional_economics/technology_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the regulatory_recognition_reading of
 *   the digital_money_origin kernel. It asserts that digital money emerged as
 *   a constraint — an institutionalized arrangement with beneficiaries and
 *   victims — at the moment monetary authorities formally incorporated
 *   digital instruments into statistical aggregates and regulatory
 *   frameworks, not when the technical systems were invented or first used by
 *   individuals. This reading frames the origin event as an act of
 *   institutional recognition that subordinated unregulated innovation to
 *   formal regulatory control. It is distinct from the
 *   became_thinkable_reading (which dates the origin earlier, to when the
 *   concept became technically conceivable) and the first_held_reading (which
 *   dates it to when individuals first held digital monetary instruments).
 *   The three readings of the kernel have substantially different ε values
 *   and different beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - Central banks — the institutional authority that formally incorporates digital instruments into monetary aggregates and policy frameworks
 *   - Regulatory authorities (SEC, CFTC, FinCEN, FATF) — the actors who issue binding definitions and enforcement rules
 *   - Incumbent financial institutions — beneficiaries who gain exclusive right to operate in the regulated space
 *   - Unregulated innovators and cryptocurrency networks — victims who are subordinated or excluded by the recognition act
 *   - State actors — beneficiaries who gain surveillance and tax collection reach into digital systems
 *   - Ordinary account holders — beneficiaries of safety and insurance, but victims of surveillance and reduced privacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.68).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.71).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Digital Money Origin: Regulatory Recognition Reading").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/institutional_economics/technology_studies").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, 'b5c821a9-3c85-4ac3-87df-8414732ea204').
narrative_ontology:cs_kernel_codification('b5c821a9-3c85-4ac3-87df-8414732ea204', fixed_text).
narrative_ontology:cs_authority_grounding('b5c821a9-3c85-4ac3-87df-8414732ea204', extraction).
narrative_ontology:cs_interpretation_layer_present('b5c821a9-3c85-4ac3-87df-8414732ea204').
narrative_ontology:cs_reading_relation('b5c821a9-3c85-4ac3-87df-8414732ea204', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('b5c821a9-3c85-4ac3-87df-8414732ea204', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_axiom('b5c821a9-3c85-4ac3-87df-8414732ea204', foundational, institutional_recognition_constitutes_emergence).
narrative_ontology:cs_axiom_status(institutional_recognition_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('b5c821a9-3c85-4ac3-87df-8414732ea204', institutional_recognition_constitutes_emergence, conventional).
narrative_ontology:cs_axiom('b5c821a9-3c85-4ac3-87df-8414732ea204', foundational, regulatory_definition_is_binding_ontology).
narrative_ontology:cs_axiom_status(regulatory_definition_is_binding_ontology, holdable).
narrative_ontology:cs_axiom_grounding('b5c821a9-3c85-4ac3-87df-8414732ea204', regulatory_definition_is_binding_ontology, conventional).
narrative_ontology:cs_reference_frame('b5c821a9-3c85-4ac3-87df-8414732ea204', unregulated_innovation_phase).
narrative_ontology:cs_drift_state('b5c821a9-3c85-4ac3-87df-8414732ea204', mature_regulatory_framework_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b5c821a9-3c85-4ac3-87df-8414732ea204', '2026-06-12T14:32:17Z').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, central_banks).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, regulatory_authorities).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_innovators).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, cryptocurrency_networks).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, alternative_payment_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, state_actors).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, ordinary_account_holders).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, ordinary_account_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks define what counts as 'money' by incorporating digital instruments into official monetary aggregates (M0, M1, M2) and regulatory frameworks. They set the terms by which digital monetary claims are recognized, taxed, tracked, and included in official statistics. This act of recognition is the moment they treat digital money as 'having emerged' within their domain. Once incorporated, they enforce compliance through banking regulations, capital requirements, and mandatory reporting.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, central_banks, agenda_setter,
    institutional, generational, analytical, national).

% Commercial banks, payment processors, and licensed money transmitters benefit from regulatory recognition because it establishes a formal category they can occupy and control. The act of regulatory recognition converts unregulated innovation into a regulated service requiring banking licenses, capital reserves, and compliance infrastructure — barriers that existing financial institutions can meet but unregulated innovators cannot. They pay compliance costs but capture the legitimate digital money market as a result.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, payer).

% Financial regulators (SEC, CFTC, FinCEN, FATF, national banking commissions) issue rules defining what digital instruments qualify as money, securities, commodities, or other regulated assets. By declaring certain digital systems 'money' and others 'not money,' they enact the boundary that determines when digital money 'emerged' as a recognized phenomenon. This is the enforcement surface: they prosecute unregulated systems, mandate reporting, impose anti-money-laundering requirements, and issue guidance.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Cryptocurrency networks, fintech startups, alternative payment platforms, and experimental monetary systems operate outside formal regulatory frameworks. Once regulatory recognition occurs, they face a choice: cease operation, seek licensing (often impossible for non-traditional models), or continue operating in legal limbo, subject to enforcement action, asset freezes, and exclusion from banking infrastructure. The regulatory recognition act is the moment they are subordinated to the framework.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_innovators, payer,
    organized, biographical, trapped, global).

% Blockchain-based monetary systems (Bitcoin, Ethereum) operate as alternatives to regulatory money. Regulatory recognition treats them as either assets, commodities, or illicit instruments rather than money proper. They can choose to operate internationally, fork their protocols, or resist classification, but regulatory recognition in major jurisdictions constrains their use in legitimate commerce and financial infrastructure.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, cryptocurrency_networks, payer,
    powerful, generational, arbitrage, global).

% Governments benefit from regulatory recognition because it extends their monetary surveillance, control, and tax collection into digital systems. Recognition makes digital money taxable, traceable, and subject to capital controls — extending the state's reach into forms of value storage that might otherwise escape tracking. This is why states coordinated through international bodies (FATF, BIS, IMF) to ensure digital money systems were brought into regulatory frameworks.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, state_actors, beneficiary,
    institutional, generational, analytical, national).

% Individual users of digital money benefit from the safety, insurance protections, and infrastructure that regulatory recognition brings: FDIC insurance, clear property rights, consumer protection rules, and integration with payroll, lending, and government benefit systems. They also pay through surveillance, reporting requirements, capital controls, and reduced financial privacy. Their identity as 'account holders' is constituted through regulatory status — switching to unregulated alternatives means losing the bundle of benefits and becoming subjects of enforcement.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, ordinary_account_holders, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, ordinary_account_holders, payer).

% The Financial Action Task Force (FATF), Bank for International Settlements (BIS), International Monetary Fund (IMF), and similar bodies coordinate regulatory frameworks across nations. They issue recommendations and standards (like the 2019 FATF Guidance on Digital Assets) that define how member states should classify and regulate digital money. They set the global agenda for what counts as money in the formal system.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, international_coordination_bodies, agenda_setter,
    institutional, generational, analytical, global).

% National legislatures enact laws defining monetary systems, payment instruments, and financial services. Their formal recognition of digital money — through statutes defining stablecoins, CBDC frameworks, or amendments to banking laws — is the moment digital money 'emerges' as law. Legislative recognition is distinct from central bank or regulatory recognition, though often coordinated with them.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, legislative_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Economic scholars document and analyze the moment regulatory recognition occurred, providing external corroboration of the origin claim. They examine historical records (when central banks first included digital instruments in M1, when regulators issued first framework documents) and evaluate whether recognition was genuine versus performative.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, academic_economists, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:fixing_cost_class(digital_money_origin__regulatory_recognition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regulatory recognition of digital money solves the coordination problem of how to integrate new monetary instruments into existing financial infrastructure, payment systems, tax collection, and monetary policy transmission. Without a shared formal definition, digital instruments would exist in multiple incompatible regulatory regimes, making cross-border transactions, banking integration, and macroeconomic measurement impossible. Recognition creates a single coherent framework.
% TRANSFER_FUNCTION: Regulatory recognition transfers control over monetary innovation from technical/entrepreneurial actors to institutional/regulatory actors. It moves the power to define what counts as money from those who build new systems to those who govern existing ones. It also transfers legitimacy: unrecognized systems lose access to banking infrastructure, payment rails, and official credibility; recognized systems gain deposit insurance, legal certainty, and integration with government services. The transfer flow is from unregulated innovators to incumbent institutions.
% ABSENT_VOICES: Unregulated innovators and decentralized network participants who built digital money systems before regulatory recognition occurred are structurally excluded from the process of defining what counts as money. They would argue that regulatory recognition imposed retroactive legitimacy criteria designed to subordinate their systems to institutional control. They are not at the table when regulators set the definition. Cryptocurrency advocates argue that recognition was a belated attempt to capture an innovation that succeeded outside regulatory permission.
% DISAPPEARANCE_RATIONALE: If regulatory recognition of digital money suddenly vanished, the world would partially rearrange: unregulated systems would lose their legal status and banking access, but the digital technical infrastructure would persist. The coordination problem would re-emerge because states and financial institutions would need to redefine the boundary between money and non-money. However, the digital systems themselves (blockchains, payment networks) would continue operating, just outside recognized frameworks. The interdependence is real but not absolute — individuals and organizations already using unregulated digital money would continue; those dependent on regulatory integration (employers, government benefits, banking deposits) would face disruption.
% FOUNDING_PROBLEM: Digital monetary systems emerged faster than regulatory frameworks could accommodate them. Early cryptocurrencies, digital payment platforms, and experimental monetary instruments operated in jurisdictional gaps where regulators lacked authority or clarity. The founding problem was regulatory opacity: innovators did not know what rules applied, and regulators did not know what authority they held. The solution was to formally recognize digital instruments within regulatory frameworks, establishing clear rules and enforcement mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and regulators attested the problem was urgent, issuing framework documents (ECB, Federal Reserve, Bank of England, FATF) from 2015 onward. Cryptocurrency advocates and fintech researchers attest that regulatory uncertainty was manufactured retroactively to control innovation that had already succeeded. Academic economists outside the regulatory beneficiary class (Rogoff, Tucker, Goodhart, and independent scholars) document the historical timing: major regulatory policy frameworks emerged 10-15 years after digital payment systems and cryptocurrencies were already in operational use — supporting the reading that regulation followed innovation, not preceded it. The Legislative histories (EU payment services directive amendments, US regulatory guidance) show authorities scrambling to catch up to technology, not setting initial terms.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, contested).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__regulatory_recognition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__regulatory_recognition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because regulatory recognition creates and enforces a boundary that excludes alternatives and concentrates legitimate digital money operations among incumbent institutions. The metric rises steeply from 2008 (0.15, when digital money was unregulated) to 2024 (0.68, when regulatory frameworks are mature and enforcement is routine). Suppression is high (0.71) because the constraint's persistence depends on actively enforcing the boundary between recognized and unrecognized systems — regulators prosecute unregulated systems, impose compliance costs that unregulated actors cannot bear, and exclude them from banking infrastructure. Theater is moderate (0.42) because some regulatory activity genuinely solves coordination problems (integrating digital payment systems into macroeconomic measurement), but a substantial share defends the regulatory exclusivity rather than users. The measurement series track one shared time grid (2008, 2012, 2016, 2020, 2024), showing how the constraint intensified as regulatory recognition matured.
 *
 * PERSPECTIVAL GAP:
 *   Agenda-setter and incumbent institutional seats should compute as tangled_rope (real coordination + asymmetric extraction); unregulated innovator seats should compute as snare (pure extraction, subordination, excluded alternatives). The divergence is structural: the constraint is tangled at the coordination level but extractive at the innovation-subordination level.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and regulatory authorities sit at d ≈ 0.05-0.15 (beneficiaries): they set the rules, enforce them, and face no direct cost. Incumbent financial institutions sit at d ≈ 0.25-0.35 (net beneficiaries): they gain market control and exclusivity, paying compliance costs but capturing the regulated market. Unregulated innovators sit at d ≈ 0.85-0.95 (full targets): they are excluded, subordinated, or forced to cease operation. State actors sit at d ≈ 0.10-0.20 (beneficiaries): they gain surveillance and tax reach. Ordinary account holders sit at d ≈ 0.50-0.60 (symmetric): they gain safety and integration (beneficiary side) but lose privacy and choice (payer side). The directionality for account holders is identity-locked because their ability to access digital money is constituted through their regulatory status — switching to unregulated alternatives means loss of insurance, employer integration, and government benefits, making exit costly beyond pure economic calculation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mislabeling by explicitly declaring both beneficiaries (incumbent institutions, state actors) and victims (unregulated innovators, cryptocurrency networks). Tangled Rope classification requires both: a genuine coordination function (integrating digital systems into monetary policy, macroeconomic measurement, banking infrastructure) AND asymmetric extraction (excluding alternatives, concentrating legitimate operations). The constraint satisfies both criteria. Mandatrophy is not present: the founding problem (regulatory uncertainty around digital systems) is still contested, not dead — some parties argue it was solved, others argue it was manufactured retroactively to control innovation. The constraint remains functional because the coordination function persists: states and financial institutions continue to depend on regulatory recognition for macroeconomic control and financial stability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_vs_emergence,
    'Is ''emergence'' an objective event (when the thing came into being) or a performative event (when an authority declared it existed)? Does regulatory recognition constitute the emergence of digital money, or merely formally acknowledge what had already emerged technically and socially?',
    'Historical analysis comparing technical capability (when digital payment systems were functional), individual adoption (when people held and used digital instruments), and institutional recognition (when authorities issued frameworks). If recognition lags technical/social emergence by years or decades, recognition is acknowledgment, not constitution. If recognition precedes adoption (authority creates the category before it is used), then recognition constitutes emergence in an institutional sense.',
    'If recognition is constitution, this reading''s origin date is correct (2015–2020). If recognition is acknowledgment, the origin date should be earlier (when technical or social emergence occurred). The classification hinges on whether ''emergence'' is ontological (the thing came into being) or institutional (the authority declared it exists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognition_vs_emergence, conceptual, 'Whether regulatory recognition constitutes or acknowledges digital money''s emergence.').

omega_variable(
    intentionality_of_exclusion,
    'Did regulatory authorities knowingly design recognition frameworks to exclude and subordinate unregulated innovators, or did exclusion occur as an incidental effect of applying existing regulatory categories to new technologies?',
    'Regulatory history and internal documents: did authorities discuss exclusion as a goal? Did they design frameworks with flexibility for alternative systems, or deliberately erected barriers? Did they coordinate internationally with the explicit intent to prevent unregulated alternatives from scaling, or did they focus on incorporating existing systems?',
    'If exclusion was intentional, the constraint is purely extractive (snare-like) at the innovation level. If exclusion was incidental, the constraint is tangled_rope with unintended victims. The intentionality question affects whether the constraint is properly characterized as beneficiary-driven (institutional actors chose to exclude) versus structure-driven (regulatory logic naturally excludes alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_exclusion, empirical, 'Whether regulatory recognition excluded alternatives by design or as a side effect.').

omega_variable(
    sibling_reading_ontology,
    'Can all three sibling readings coexist as live institutional claims, or does one reading''s truth imply the falsity of another?',
    'Institutional analysis: can an authority simultaneously hold that digital money emerged when technically conceivable (became_thinkable), when individuals first held it (first_held), and when authorities formally recognized it (regulatory_recognition)? Or does each reading require denying the validity of the others? The question is whether the three readings represent genuine alternative framings of the same contested phenomenon (coexist_with) or whether they logically foreclose each other within a single institutional framework (forecloses).',
    'If the readings coexist, each is a legitimate reading held by different parties. If one forecloses another, the kernel dispute is resolvable by examining which reading is actually operative in current institutional practice. The resolution affects how the constraint family is structured: do we model three independent readings or a hierarchy of readings where one renders others obsolete?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_ontology, conceptual, 'Whether the three sibling readings can simultaneously hold as institutional truths or whether they logically foreclose each other.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (legal barriers, banking exclusion, enforcement action) or internalized (unregulated systems voluntarily comply, seeking legitimacy and access even without coercion)?',
    'Behavioral and compliance data: do unregulated systems attempt to meet regulatory requirements even before enforcement action? Do they voluntarily adopt identity-verification, reporting, and compliance infrastructure seeking regulatory recognition? If yes, suppression is partly internalized. If unregulated systems only comply under threat of enforcement or exclusion, suppression is primarily structural.',
    'If suppression is internalized, the constraint''s effective extractiveness is higher than the structural measure suggests — the targets carry the suppression with them psychologically and operationally even absent external enforcement. If suppression is structural, the measured suppression accurately reflects the coercive apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether regulatory suppression operates through external barriers or internalized compliance-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regulatory_recognition_theater_2008, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement_basis(regulatory_recognition_theater_2008, projected).
narrative_ontology:measurement(regulatory_recognition_theater_2012, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2012, 0.28).
narrative_ontology:measurement_basis(regulatory_recognition_theater_2012, observed).
narrative_ontology:measurement(regulatory_recognition_theater_2016, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2016, 0.36).
narrative_ontology:measurement_basis(regulatory_recognition_theater_2016, observed).
narrative_ontology:measurement(regulatory_recognition_theater_2020, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(regulatory_recognition_theater_2020, observed).
narrative_ontology:measurement(regulatory_recognition_theater_2024, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(regulatory_recognition_theater_2024, observed).

% Extraction over time
narrative_ontology:measurement(regulatory_recognition_extractiveness_2008, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2008, 0.15).
narrative_ontology:measurement_basis(regulatory_recognition_extractiveness_2008, projected).
narrative_ontology:measurement(regulatory_recognition_extractiveness_2012, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2012, 0.32).
narrative_ontology:measurement_basis(regulatory_recognition_extractiveness_2012, observed).
narrative_ontology:measurement(regulatory_recognition_extractiveness_2016, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2016, 0.54).
narrative_ontology:measurement_basis(regulatory_recognition_extractiveness_2016, observed).
narrative_ontology:measurement(regulatory_recognition_extractiveness_2020, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement_basis(regulatory_recognition_extractiveness_2020, observed).
narrative_ontology:measurement(regulatory_recognition_extractiveness_2024, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(regulatory_recognition_extractiveness_2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(regulatory_recognition_suppression_2008, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2008, 0.25).
narrative_ontology:measurement_basis(regulatory_recognition_suppression_2008, projected).
narrative_ontology:measurement(regulatory_recognition_suppression_2012, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2012, 0.42).
narrative_ontology:measurement_basis(regulatory_recognition_suppression_2012, observed).
narrative_ontology:measurement(regulatory_recognition_suppression_2016, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2016, 0.58).
narrative_ontology:measurement_basis(regulatory_recognition_suppression_2016, observed).
narrative_ontology:measurement(regulatory_recognition_suppression_2020, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement_basis(regulatory_recognition_suppression_2020, observed).
narrative_ontology:measurement(regulatory_recognition_suppression_2024, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(regulatory_recognition_suppression_2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(digital_money_origin__regulatory_recognition_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).

% DUAL FORMULATION NOTE:
% The digital_money_origin kernel has three readings with substantially different origin dates and structural properties. This reading (regulatory_recognition) is downstream of the other two: regulatory recognition presupposes that digital money became technically thinkable and was held by individuals before it could be formally recognized by authorities. However, the three readings are not a strict causal sequence — they represent competing institutional framings of when 'emergence' occurred, not a historical sequence of events. Each reading instantiates a different constraint with different ε values and different beneficiary/victim configurations. The readings coexist in institutional practice: authorities use the regulatory_recognition frame for their own decisions, technical communities use the became_thinkable frame, and individual users use the first_held frame. The constraint family models the contest over which reading is operative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__regulatory_recognition_reading, organized, 0.88).
constraint_indexing:directionality_override(digital_money_origin__regulatory_recognition_reading, powerless, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
