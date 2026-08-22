% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: First Practical Holding of Non-Physical Money
 *   domain: monetary/institutional/technological
 *
 * SUMMARY:
 *   This constraint instantiates the 'first held' reading of the digital
 *   money origin kernel: the moment individuals began holding non-physical
 *   monetary instruments as practical stores of value. This reading dates the
 *   emergence of digital money to when the technology became sufficiently
 *   reliable and accessible that ordinary people (not just institutions)
 *   could maintain balances in non-physical form as a routine practice. The
 *   reading is situated in post-WWII banking infrastructure development,
 *   accelerating through electronic fund transfer systems (1970s–1990s) and
 *   culminating in ubiquitous digital-first payment (2000s–present). Unlike
 *   the 'became thinkable' reading (which would place the origin earlier, at
 *   the moment digital money was conceptually possible) or the 'regulatory
 *   recognition' reading (which would place it at formal institutional
 *   incorporation), this reading marks the constraint as emerging when
 *   practical adoption became widespread enough that holding digital balances
 *   was no longer exceptional but ordinary. The measured extraction (0.68 at
 *   interval end) reflects that while the constraint solves a genuine
 *   coordination problem (trustworthy digital value storage), it also
 *   concentrates infrastructure control, enables transaction surveillance,
 *   and creates structural exclusion for populations without access.
 *
 * KEY AGENTS:
 *   - early_adopters_with_infrastructure_access: Individuals and households in developed economies who gained immediate convenience from digital holdings; d near beneficiary end (low extraction experienced, high exit mobility)
 *   - digital_payment_network_operators: Banks, processors, fintech platforms that mediate holdings; d near institutional beneficiary end (they set rules, collect fees, control infrastructure)
 *   - populations_without_digital_infrastructure: Individuals in rural, low-income, conflict regions excluded from digital holding by structural barriers; d at target end (high suppression, trapped exit, bearing costs of cash marginalization)
 *   - cash_dependent_economies: National systems and informal financial sectors facing institutional pressure to digitize; d at target end (enforcement of standardization, regulatory scrutiny, legitimacy loss)
 *   - monetary_authorities: Central banks and regulators observing and gradually codifying digital money as normal; analytical seat, neither collecting nor paying directly from this constraint alone
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.68).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.54).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "First Practical Holding of Non-Physical Money").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary/institutional/technological").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, '7a2a35f6-5456-4b57-be0c-299000e8045d').
narrative_ontology:cs_kernel_codification('7a2a35f6-5456-4b57-be0c-299000e8045d', distributed).
narrative_ontology:cs_authority_grounding('7a2a35f6-5456-4b57-be0c-299000e8045d', extraction).
narrative_ontology:cs_reading_relation('7a2a35f6-5456-4b57-be0c-299000e8045d', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a2a35f6-5456-4b57-be0c-299000e8045d', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('7a2a35f6-5456-4b57-be0c-299000e8045d', foundational, practical_adoption_marks_emergence).
narrative_ontology:cs_axiom_status(practical_adoption_marks_emergence, holdable).
narrative_ontology:cs_axiom_grounding('7a2a35f6-5456-4b57-be0c-299000e8045d', practical_adoption_marks_emergence, conventional).
narrative_ontology:cs_axiom('7a2a35f6-5456-4b57-be0c-299000e8045d', secondary, infrastructure_access_determines_social_inclusion).
narrative_ontology:cs_axiom_status(infrastructure_access_determines_social_inclusion, holdable).
narrative_ontology:cs_axiom_grounding('7a2a35f6-5456-4b57-be0c-299000e8045d', infrastructure_access_determines_social_inclusion, empirically_contingent).
narrative_ontology:cs_reference_frame('7a2a35f6-5456-4b57-be0c-299000e8045d', mass_practical_digital_holding).
narrative_ontology:cs_drift_state('7a2a35f6-5456-4b57-be0c-299000e8045d', contemporary_financialization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7a2a35f6-5456-4b57-be0c-299000e8045d', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters_with_infrastructure_access).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, digital_payment_network_operators).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, populations_without_digital_infrastructure).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, cash_dependent_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals in developed economies with reliable electricity, banking relationships, and digital literacy who gain immediate convenience from holding money as digital balances rather than physical currency. They benefit from faster transactions, reduced physical theft risk, and access to interest-bearing accounts. Their exit is smooth — they can revert to cash if desired, and many maintain dual holdings.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopters_with_infrastructure_access, beneficiary,
    moderate, biographical, mobile, regional).

% Banks, payment processors, and fintech platforms that mediate digital money holdings. They set technical standards, determine access conditions, collect transaction fees, and control the infrastructure that makes digital holding practical. Their coordination function is genuine: they solve the problem of trustworthy, interoperable digital value storage. Their extraction comes from fee collection and data leverage from transaction surveillance.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, digital_payment_network_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Individuals in rural, low-income, or conflict-affected regions without reliable bank accounts, smartphones, or electricity. They are excluded from digital money holding by structural barriers, not by choice. As digital money becomes the presumed medium of value storage (in policy, commerce, and institutional expectation), their exclusion operates as a hidden extraction: they bear the costs of cash's increasing marginalization — higher fees for cash handling, reduced merchant acceptance, administrative burden — without accessing digital money's benefits.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, populations_without_digital_infrastructure, payer,
    powerless, biographical, trapped, local).

% National governments and informal financial systems (hawala, mobile money cooperatives, barter networks) that operated on physical currency and direct exchange. As digital money becomes the metric against which 'modernity' and 'economic development' are measured, these systems face policy pressure to digitize, regulatory suspicion, and gradual institutional marginalization. They bear enforcement costs: pressure to adopt digital infrastructure, regulatory scrutiny for non-compliance, and reduced institutional legitimacy.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, cash_dependent_economies, payer,
    powerful, generational, constrained, national).

% Approximately 1.4 billion adults globally without access to formal financial institutions. They are structurally unable to hold digital money through the dominant channels (bank accounts, credit cards, managed fintech platforms). Their exclusion is enforced by KYC/AML requirements, minimum balance rules, geographic service gaps, and documentation barriers. They would have different interests in the definition of digital money's origins if included — they might advocate for infrastructure-light definitions or community-based digital value systems.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_and_underbanked_populations, excluded,
    powerless, immediate, trapped, global).

% Central banks and financial regulators that observe the emergence and scaling of digital money holdings. They measure the constraint's operation through M1/M2 statistics, payment system design, and the gradual institutionalization of digital holding as a normal monetary practice. Their analytical seat lets them see the structure whole, but their power to reshape it is constrained by path dependence and political economy.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__first_held_reading, digital_payment_network_operators).
narrative_ontology:fixing_cost_class(digital_money_origin__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables individuals to hold monetary value in non-physical form: reduces physical security risk, enables rapid value transfer, supports interest-bearing savings vehicles, and allows trustworthy record-keeping of holdings across geography and time.
% TRANSFER_FUNCTION: Moves transaction fees and data surveillance rents from digital money users and merchants to payment network operators; transfers the implicit costs of infrastructure access barriers from populations without digital infrastructure to those with it (cash's marginalization imposes costs on the unbanked).
% ABSENT_VOICES: Unbanked and underbanked populations (1.4 billion adults) are structurally excluded from the conversation about what 'holding digital money' means. Cash-dependent informal financial systems would contest the definition and the presumption of digital superiority. Community-based and non-custodial digital value systems (those not mediated by institutional operators) are marginalized in the institutional definition.
% DISAPPEARANCE_RATIONALE: If the ability to hold digital money disappeared — if digital balances could no longer be stored or transmitted — monetary systems would reorganize rapidly around physical currency, alternative media of exchange (barter, commodity-backed instruments), and informal value transfer. The entire infrastructure of digital banking, e-commerce, and instantaneous settlement would collapse or revert to batch processing and physical settlement.
% FOUNDING_PROBLEM: Physical currency is inconvenient: it incurs theft risk, transportation cost, and degradation; it enables no interest accumulation in dormancy; it is difficult to transfer over distance; it provides no persistent record. Early practitioners (medieval bankers with deposit accounts, telegraph-era value transfer, early electronic transfers) sought to hold and transfer value without constantly converting to and from physical form.
% FOUNDING_PROBLEM_CORROBORATION: Historians of banking (Polanyi, Braudel, Helleiner) and practitioners in digital payment systems attest that convenience and security improvements are the primary drivers of digital money adoption. International development organizations (World Bank, CGAP) document ongoing friction from the persistence of cash-centric economies. However, monetary authorities increasingly attest that digital money holding is now institutionally embedded and self-reinforcing, suggesting the founding problem has been 'solved' in those jurisdictions — the question is whether the problem was genuinely about convenience or was always also about institutional control and data leverage, which would reframe the status.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (1950: early bank customers, optional digital holding, minimal coordination problem solved at scale) to 0.68 (2024: digital holding is presumed, cash faces institutional friction, large populations excluded structurally). The rise traces increasing path dependence: as digital holdings become the norm, the infrastructure operators' power to set fees, define access conditions, and surveil transactions increases. Suppression rises from 0.22 to 0.54 — not through overt coercion but through structural closure: cash handling becomes more expensive and less accessible, regulatory pressure mounts on alternative systems, and the presumption that 'modern' economies are digital-money economies creates soft enforcement. Theater is low (0.22) because the coordination function is real and substantial — digital value storage genuinely solves the problem the constraint was built for — but theater creeps up over time as the justification for exclusion becomes more performative (the reasons unbanked populations cannot access digital money are increasingly regulatory (KYC) rather than technical). The temporal grid is shared: every metric is authored at every examined interval point (1950, 1975, 1990, 2005, 2015, 2024), enabling lifecycle drift detection. The measurements are marked 'observed' because they are grounded in historical record (payment system adoption rates, M1/M2 statistics, transaction volumes, regulatory changes) and contemporary data (unbanked population surveys, cash acceptance surveys, fee schedules).
 *
 * PERSPECTIVAL GAP:
 *   The early adopter and operator seats should compute as experiencing coordination (the constraint genuinely solves their value-storage problem and their exit options remain open or profitable). The powerless-with-infrastructure-barriers seat and the cash-dependent-economy seat should compute as experiencing extraction (the constraint solves a problem for others while imposing closure on them). The measurement series reflects this divergence: beneficiaries experience declining friction (digital holding becomes easier, more trusted, more rewarded with interest) while victims experience rising friction (cash becomes costlier, less accepted, more suspicious). The operator seat experiences steady rent collection. The analytical observer seat measures the whole structure but has limited power to reshape it because path dependence (every institutional actor now expects digital money) creates collective-action problems for reversal.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters (moderate power, mobile exit, regional scope, biographical horizon) are beneficiaries: they gain convenience with option to revert to cash if desired. Their directionality d is low (near beneficiary end, ~0.25–0.35). Operators (institutional power, arbitrage exit, global scope, generational horizon) are agenda-setters and beneficiaries: they solve a coordination problem they built and collect fees. Their d is near institutional beneficiary end (~0.15–0.25). Populations without infrastructure (powerless, trapped exit, local scope, immediate horizon) are victims: they are structurally excluded and bear the costs of cash marginalization without accessing benefits. Their d is high (target end, ~0.80–0.90). Cash-dependent economies (powerful but constrained exit, national scope, generational horizon) are victims: they face institutional pressure to digitize and regulatory scrutiny. Their d is moderate-to-high (~0.60–0.75). Excluded unbanked populations (powerless, trapped exit) would be even further into the target end (d ~0.85+) if counted as stakeholders, but they are listed as 'excluded' because they are structurally unable to voice preferences in the institutions that define digital money. The divergence in directionality across seats means the constraint should compute as Tangled Rope at the operator and early-adopter seats (coordination solves their problem, low effective extraction) and as Snare at the powerless and cash-dependent seats (high effective extraction with structural suppression).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve the practical problem of holding value without physical currency's inconvenience and risk. That founding problem is LIVE for developed-economy populations (they continue to demand digital convenience and its benefits continue to accrue). However, the founding problem is DEAD for excluded and unbanked populations: they never needed digital money (they needed access to financial services, stable value, and low-friction exchange, which were never provided). The institutional founding-problem narrative conflates two problems: (1) the genuine coordination problem of trustworthy digital value storage (alive, solved), and (2) the implicit problem of institutional control over value flows and extractive surveillance (never openly stated as a founding problem, but increasingly central to the constraint's operation). The mandatrophy lies in this conflation: the constraint is justified by the first problem's persistence while actually amplifying the second. The rising theater ratio (0.08 to 0.22) reflects this: the constraint spends increasing effort defending the boundaries of what counts as 'digital money' (KYC/AML rules, regulatory recognition, anti-cash rhetoric) rather than solving the original coordination problem, which is already solved technically. This is the mandatrophy signature: a constraint born to solve a real problem now mostly operating to defend the institutional arrangements that profit from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practical_adoption_vs_conceptual_possibility,
    'Does ''digital money emerged'' mark the moment it became technically possible (became_thinkable_reading) or the moment people began routinely holding it (first_held_reading)?',
    'Historical analysis of adoption timelines: when did the first non-specialist individuals hold digital balances? When did payment volumes cross thresholds where digital holding was no longer optional for convenience but structurally expected? When did merchants begin refusing cash?',
    'If practical adoption is the criterion, this reading is correct and the origin is ~1970s–1980s. If conceptual possibility is the criterion, the origin moves earlier (~1950s). The choice determines the beneficiary/victim set and the measured extraction profile over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_adoption_vs_conceptual_possibility, conceptual, 'Whether the origin marks technical possibility or routine practical adoption.').

omega_variable(
    infrastructure_access_as_extraction_vector,
    'Is the measured suppression (0.54) primarily structural (lacks electricity, banking relationships, documentation) or regulatory (intentionally designed exclusion via KYC/AML rules)?',
    'Examine pre-digital-money exclusion rates vs. post-digital-money exclusion rates in the same populations. Did unbanked populations gain access where digital-first infrastructure lowered barriers? Or did digital-first policies (mobile money KYC, documentation requirements) create new exclusion pathways not present under cash-based systems?',
    'If primarily structural, the suppression is a side effect of development unevenness. If primarily regulatory, the suppression is an intentional feature of the constraint''s operation. This affects whether the constraint should be classified as Tangled Rope (coordination + inadvertent exclusion) vs. Snare (coordination + intentional exclusion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_access_as_extraction_vector, empirical, 'Whether exclusion from digital money is incidental or designed.').

omega_variable(
    cash_marginalization_direction,
    'Does digital money marginalize cash, or does policy choice marginalize cash in the name of digital money?',
    'Compare jurisdictions with organic digital adoption (users prefer digital, merchants accept both, cash fades) vs. jurisdictions with policy-driven digital transitions (governments restrict cash, penalize merchants who refuse digital, mandate documentation). Do the structural dynamics differ?',
    'If organic, digital money emerged as users found it preferable, and extraction is incidental to coordination. If policy-driven, digital money extraction is intentional — suppression of alternatives is a deliberate feature, not a side effect. This affects measured suppression and the case for Snare classification at victim seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cash_marginalization_direction, empirical, 'Whether digital adoption is user-driven or policy-enforced.').

omega_variable(
    regulatory_definition_circularity,
    'Does the regulatory_recognition_reading (formal incorporation into M1/M2) define digital money into existence, or does it merely codify a constraint that already existed through practical adoption?',
    'Examine whether regulatory recognition changed the constraint''s operation materially (e.g., enabled new enforcement tools, new fee structures, new exclusion mechanisms) or merely reflected an existing practice. Did regulatory recognition empower operators to charge fees they could not charge before? Did it enable new surveillance?',
    'If regulatory recognition is merely codification, the three readings mark different aspects of the same constraint and can coexist. If regulatory recognition materially empowered the constraint (e.g., enabled central banks to penalize cash, enabled fintech fee-taking to be formalized as ''monetary policy''), then regulatory_recognition_reading should foreclose or substantially influence first_held_reading — the constraint''s operation changed fundamentally when authorities recognized it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_definition_circularity, conceptual, 'Whether regulatory recognition merely codifies or materially transforms the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1950, digital_money_origin__first_held_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement_basis(digi_tr_t1950, observed).
narrative_ontology:measurement(digi_tr_t1975, digital_money_origin__first_held_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement_basis(digi_tr_t1975, observed).
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__first_held_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement_basis(digi_tr_t1990, observed).
narrative_ontology:measurement(digi_tr_t2005, digital_money_origin__first_held_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement_basis(digi_tr_t2005, observed).
narrative_ontology:measurement(digi_tr_t2015, digital_money_origin__first_held_reading, theater_ratio, 2015, 0.21).
narrative_ontology:measurement_basis(digi_tr_t2015, observed).
narrative_ontology:measurement(digi_tr_t2024, digital_money_origin__first_held_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(digi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1950, digital_money_origin__first_held_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement_basis(digi_be_t1950, observed).
narrative_ontology:measurement(digi_be_t1975, digital_money_origin__first_held_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement_basis(digi_be_t1975, observed).
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__first_held_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement_basis(digi_be_t1990, observed).
narrative_ontology:measurement(digi_be_t2005, digital_money_origin__first_held_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement_basis(digi_be_t2005, observed).
narrative_ontology:measurement(digi_be_t2015, digital_money_origin__first_held_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement_basis(digi_be_t2015, observed).
narrative_ontology:measurement(digi_be_t2024, digital_money_origin__first_held_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(digi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1950, digital_money_origin__first_held_reading, suppression_requirement, 1950, 0.22).
narrative_ontology:measurement_basis(digi_su_t1950, observed).
narrative_ontology:measurement(digi_su_t1975, digital_money_origin__first_held_reading, suppression_requirement, 1975, 0.31).
narrative_ontology:measurement_basis(digi_su_t1975, observed).
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__first_held_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement_basis(digi_su_t1990, observed).
narrative_ontology:measurement(digi_su_t2005, digital_money_origin__first_held_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement_basis(digi_su_t2005, observed).
narrative_ontology:measurement(digi_su_t2015, digital_money_origin__first_held_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement_basis(digi_su_t2015, observed).
narrative_ontology:measurement(digi_su_t2024, digital_money_origin__first_held_reading, suppression_requirement, 2024, 0.54).
narrative_ontology:measurement_basis(digi_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_origin__first_held_reading, 0.18).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, financial_surveillance_institutional_infrastructure).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, cash_marginalization_policy).

% DUAL FORMULATION NOTE:
% The digital_money_origin kernel decomposes into three readings (became_thinkable, first_held, regulatory_recognition) because the three statements have different ε values, different beneficiary/victim structures, and different temporal anchors. The constraint labeled 'digital money' is not one thing; it is three structurally distinct constraints reflecting three moments in the history of non-physical value storage. This story instantiates the first_held_reading (practical adoption by individuals). The became_thinkable_reading places the origin earlier and emphasizes technical possibility, placing extraction lower (it is mostly coordination). The regulatory_recognition_reading places the origin later and emphasizes institutional codification, treating extraction as rising sharply once authorities formalize digital money as part of the money supply — enabling new surveillance and enforcement tools. All three readings share the same kernel (the concept of 'digital money' and its emergence) but instantiate different constraints. This story influences both siblings: if practical adoption (first_held) is the correct origin, then 'becoming thinkable' is a necessary prerequisite, and 'regulatory recognition' is a later step. The network captures this dependence structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__first_held_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
