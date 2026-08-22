% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: Regulatory Recognition of Digital Money
 *   domain: monetary_history/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the regulatory-recognition reading of
 *   the digital-money kernel. Under this reading, digital money 'emerges'
 *   when monetary authorities formally incorporate it into statistical
 *   aggregates and regulatory frameworks—not when the technology is first
 *   conceivable, nor when individuals first hold non-physical instruments,
 *   but when the state recognizes and codifies the form. This reading
 *   emphasizes the institutional lag: digital money functioned as money (was
 *   used, valued, exchanged) long before regulatory recognition, but the
 *   recognition act itself creates a new boundary—between officially
 *   legitimate and unregulated forms—and this boundary becomes extractive.
 *   Incumbent financial institutions benefit from their pre-positioned access
 *   to regulatory processes; unregulated innovators lose legitimacy and
 *   market standing. The constraint is CLAIMED as tangled_rope because it
 *   coordinates a coherent definition of money (genuine coordination
 *   function) while simultaneously extracting from those outside the
 *   regulated perimeter (asymmetric extraction via exclusion). The authored
 *   metrics describe substantial active suppression (licensing requirements,
 *   reserve rules, prohibition on certain use cases) and rising
 *   extractiveness as regulatory frameworks mature and close alternative
 *   routes.
 *
 * KEY AGENTS:
 *   - Central banks: set and enforce the recognition decision; arbitrage access to regulatory authority
 *   - Incumbent financial institutions: benefit from early licensing and regulatory relationship advantage; positioned as first intermediaries of officially recognized digital money
 *   - Regulatory agencies: implement and enforce the recognition framework; agents of the recognition gate
 *   - Unregulated digital innovators: loss of legitimacy and market access when recognition occurs; identity-locked to non-regulated models
 *   - Alternative payment networks: face compliance costs and operational prohibition; constrained but still-powerful incumbents in their own right
 *   - Retail users: gain official standing and legal recourse for recognized digital money; lose access to pre-recognition alternatives
 *   - Analytical observer: tracks the institutionalization of digital money and notes the origin-date depends on reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.68).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.72).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Regulatory Recognition of Digital Money").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, 'd61c88b3-f80e-4c79-b5a6-1f276610d4ad').
narrative_ontology:cs_kernel_codification('d61c88b3-f80e-4c79-b5a6-1f276610d4ad', formalized).
narrative_ontology:cs_authority_grounding('d61c88b3-f80e-4c79-b5a6-1f276610d4ad', extraction).
narrative_ontology:cs_interpretation_layer_present('d61c88b3-f80e-4c79-b5a6-1f276610d4ad').
narrative_ontology:cs_reading_relation('d61c88b3-f80e-4c79-b5a6-1f276610d4ad', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('d61c88b3-f80e-4c79-b5a6-1f276610d4ad', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_axiom('d61c88b3-f80e-4c79-b5a6-1f276610d4ad', foundational, regulatory_recognition_constitutive).
narrative_ontology:cs_axiom_status(regulatory_recognition_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('d61c88b3-f80e-4c79-b5a6-1f276610d4ad', regulatory_recognition_constitutive, conventional).
narrative_ontology:cs_axiom('d61c88b3-f80e-4c79-b5a6-1f276610d4ad', foundational, authority_monopoly_on_money_definition).
narrative_ontology:cs_axiom_status(authority_monopoly_on_money_definition, holdable).
narrative_ontology:cs_axiom_grounding('d61c88b3-f80e-4c79-b5a6-1f276610d4ad', authority_monopoly_on_money_definition, empirically_contingent).
narrative_ontology:cs_reference_frame('d61c88b3-f80e-4c79-b5a6-1f276610d4ad', monetary_authority_exclusive_recognition).
narrative_ontology:cs_drift_state('d61c88b3-f80e-4c79-b5a6-1f276610d4ad', contemporary_digital_asset_proliferation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d61c88b3-f80e-4c79-b5a6-1f276610d4ad', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, central_banks).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, regulatory_agencies).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_digital_innovators).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, alternative_payment_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, retail_users).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, retail_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formally designate which monetary instruments count as money within national accounting frameworks. They decide when digital tokens, electronic balances, and cryptographic representations enter M1/M2/M3 aggregates and become subject to monetary policy. Their recognition act confers official status and legitimacy, triggering downstream regulatory jurisdiction.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, central_banks, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the regulatory recognition gate because existing banks have pre-positioned relationships with regulators and compliance infrastructure already deployed. When a new form of money is formally recognized, incumbent institutions are first to be licensed to hold, issue, or intermediate it. They extract rents from this timing advantage and from exclusionary compliance costs that bar unregulated competitors.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, agenda_setter).

% Issue the binding interpretations of what digital instruments qualify as money, what oversight is required, what issuance is permitted. Their regulatory frameworks codify the recognition decision, create licensing requirements, and enforce entry barriers that exclude innovators operating outside the regulatory perimeter.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Developed functional digital monetary instruments (peer-to-peer transfers, cryptographic currencies, decentralized ledgers) before regulatory recognition. Their innovation preceded the regulatory framework; when recognition comes, they face a choice: restructure to comply (costly, identity-erasing), migrate to unrecognized status (marginalized), or litigate. Many are identity-locked to the non-regulated model their entire technical and ideological commitment rests on.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_digital_innovators, payer,
    moderate, biographical, identity_locked, global).

% Operate large-scale digital transfer systems (cross-border remittance networks, stablecoin systems, blockchain-based payment rails) that function as money but resist or predate formal regulatory recognition. When central banks move to recognize and regulate digital money, these networks face licensing barriers, reserve requirements, and explicit prohibition of certain use cases. They bear the extraction as capital requirements and operational constraints.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, alternative_payment_networks, payer,
    powerful, biographical, constrained, global).

% Gain access to officially recognized digital monetary instruments (central bank digital currency, regulated stablecoins, electronic bank money) with consumer protection, legal recourse, and integration into the financial system's official payment infrastructure. They also lose access to unregulated alternatives that operated outside the recognition framework, and face compliance costs (KYC/AML) embedded in the regulatory recognition structure.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, retail_users, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, retail_users, payer).

% Tracks the institutional history of how monetary authorities decide what counts as money, how recognition has shifted with technology, and how the recognition act itself becomes an apparatus of incumbency protection. Notes that the emergence of digital money is distinct depending on which reading one adopts—technical feasibility, first use, or regulatory blessing each gives a different origin date.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:fixing_cost_class(digital_money_origin__regulatory_recognition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, authoritative determination of what constitutes money within a jurisdiction, replacing competing de facto monetary instruments with one codified standard. This solves the double-coincidence-of-wants problem at institutional scale: if many rival digital tokens circulate, merchants and users face exchange-rate risk and coordination friction; central bank recognition of one digital form eliminates that friction.
% TRANSFER_FUNCTION: Moves regulatory legitimacy and legal standing from unregulated innovators to incumbent institutions by conditioning digital-money status on formal licensing, reserve requirements, and compliance infrastructure that incumbents have already built or can build at lower cost. The transfer is of position in the financial hierarchy, not of physical resources.
% ABSENT_VOICES: Unregulated digital-currency developers and users of alternative networks are structurally excluded from the recognition decision—they have no seat at the central bank's table and no voting power in regulatory rulemaking. They would argue that digital money already exists (is already held, exchanged, and valued) independent of recognition, and that regulatory recognition is a post-hoc capture mechanism. Decentralized-finance communities and libertarian monetary theorists are kept outside the conversation.
% DISAPPEARANCE_RATIONALE: If central banks and regulators suddenly ceased formalizing digital money into statistical aggregates and regulatory frameworks, unregulated digital instruments would remain operational—they would not vanish. But the landscape would reorganize: absent regulatory recognition, digital money would persist in an informal, lower-trust tier, excluded from integration with banking infrastructure, subject to higher exchange-rate volatility, and unable to settle institutional payments. Regulated financial institutions would lose the ability to capture digital-money markets through licensing, and innovation would accelerate outside the regulatory perimeter. The financial system's stability architecture would fragment.
% FOUNDING_PROBLEM: Early digital instruments (electronic bank balances, cryptographic tokens) functioned as money but existed outside official monetary aggregates and regulatory purview. Central banks needed a coherent way to monitor money supply, conduct monetary policy, and manage systemic risk as transactions increasingly moved from cash to digital form. Regulatory authorities needed a framework to prevent money-like financial instruments from operating without oversight.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and financial regulators attest the founding problem remains live—digital money proliferation creates systemic risks, AML/CFT gaps, and monetary policy blind spots if left unrecognized. Incumbent financial institutions corroborate that the problem justifies regulation. However, decentralized-finance researchers and monetary historians contest this: they argue digital money was already functioning as money before recognition (problem solved through use, not through regulatory declaration), and that the 'founding problem' is a retrospective framing that serves regulatory interests. Independent academic sources (monetary history, financial anthropology) document that digital payment systems worked effectively in unregulated spaces prior to formal recognition.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.42 to 0.68 over the interval because regulatory recognition accumulates compliance costs and exclusionary barriers that did not exist before. Early in the interval (time 0), digital money is unrecognized but functional—extractiveness is moderate because there is no monopoly rent on legitimacy yet. As recognition happens (time 5–15), compliance requirements and licensing barriers rise sharply; extractiveness climbs. By the end of the interval (time 25), the regulatory framework is mature, alternative routes are closed off, and incumbent financial institutions have fully captured the market for legitimate digital money—extractiveness is high. Theater rises from 0.22 to 0.41 because regulatory agencies spend increasing effort justifying the recognition framework (consumer protection, systemic stability, AML/CFT compliance) even as the framework's primary function becomes incumbent protection. Suppression rises from 0.58 to 0.72 because enforcement of the regulatory perimeter requires active suppression: licensing denials, operational prohibitions on unregulated networks, legal action against alternative currencies. This measured suppression is not structural (like exclusion from a natural resource) but institutional—it requires continuous enforcement to maintain.
 *
 * PERSPECTIVAL GAP:
 *   From the central bank and incumbent institution seats, this is genuine coordination: establishing a single authoritative definition of money solves real monetary-policy and systemic-risk problems. From the unregulated-innovator and alternative-network seats, the same structure is extraction with a coordination cover story: the definition was chosen to benefit incumbents, alternatives that worked as money are delegitimized for regulatory rather than functional reasons, and the 'stability' narrative masks market capture. The engine computes this divergence from the structural data: the beneficiary seat (incumbent_financial_institutions, role=beneficiary with secondary_role=agenda_setter) and the payer seats (unregulated_digital_innovators, alternative_payment_networks, role=payer) should produce different type classifications when the engine seats them at different power/exit positions. The analytical_observer seat sits neutral but sees both framings.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and regulatory agencies have high power and analytical exit (they set the terms and answer to other institutional actors, not to market forces)—directionality toward this constraint skews beneficiary-ward (low d). Incumbent financial institutions have institutional power and arbitrage-grade exit (they can adapt to new regulatory regimes or relocate to friendlier jurisdictions)—their directionality is also beneficiary-ward. Unregulated digital innovators have moderate power and identity_locked exit (their entire technical and ideological stance rests on non-regulated operation; restructuring to comply means abandoning their founding mission)—directionality toward this constraint skews target-ward (high d). Alternative payment networks have powerful institutional position but constrained exit (they cannot relocate or restructure without abandoning their business model)—mixed directionality, but tilting toward the payer end. Retail users are powerless but have constrained exit (forced into the recognized system by network effects and lack of alternatives)—moderate target directionality. The committer content (which reading applies) sits in an omega variable; the structural data (who benefits, who pays, what power and exit each holds) is what drives the per-seat classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'contested' because central banks and regulators attest the problem (digital money proliferation outside official oversight) is live and ongoing, while monetary historians and DeFi researchers attest it was already solved through use before recognition. The disappearance_verdict is 'world_rearranges' because if regulatory recognition ceased, digital money would not vanish—it would reorganize into informal and parallel systems. This mismatch (contested status + world_rearranges verdict) signals mandatrophy: the founding problem may be dead (digital money already existed and functioned as money prior to recognition) but the constraint persists because regulatory apparatus has made incumbency capture an end in itself. Theater rising from 0.22 to 0.41 supports the mandatrophy signal: as the founding problem becomes less live, regulatory agencies spend more effort on performative justification (consumer protection theater, systemic-risk narrative) rather than solving an actual coordination problem. The constraint does not satisfy mandatrophy-resolved status because the problem is contested—some parties still attest it is live. If empirical evidence conclusively showed that unregulated digital money posed no systemic risk and functioned reliably before recognition, the constraint would be reclassified as resolving-stage mandatrophy (problem dead, constraint persisting for institutional inertia).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_recognition_vs_functional_emergence,
    'Is the point at which monetary authorities formally recognize digital money the same as the point at which digital money actually emerges as a social technology? Or does functional emergence (when it is actually used, valued, and exchanged) precede and logically ground regulatory recognition?',
    'Historical-genealogical analysis: trace the timeline of digital money use, adoption, and functional integration in payment systems, then compare to the date of formal regulatory recognition. If functional use predates recognition by years or decades, the readings are temporally distinct. If regulatory recognition precedes or coincides with functional adoption, the readings may converge.',
    'If functional emergence clearly precedes recognition, this reading''s origin date is later than the ''true'' emergence (as defined by the other readings), and the constraint this reading instantiates is a post-hoc regulatory capture apparatus, not the emergence of digital money itself. If recognition precedes function, this reading may be the most accurate. The classification of the constraint as tangled_rope (coordination + extraction) depends on this resolution: if the coordination problem predates recognition, the extraction is secondary; if recognition creates the coordination problem, the extraction is structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_recognition_vs_functional_emergence, empirical, 'Temporal relationship between functional emergence and regulatory recognition of digital money.').

omega_variable(
    regulatory_capture_vs_prudent_oversight,
    'Are the licensing requirements and regulatory barriers that rise over the interval genuine prudential safeguards (preventing systemic risk, protecting consumers), or are they incumbent-protection mechanisms that happen to use prudential justification?',
    'Comparative-regulatory analysis: compare jurisdictions with high regulatory barriers to digital money versus jurisdictions with lighter regulation, and assess systemic risk, consumer harm, and innovation rates in each. If high-barrier jurisdictions show superior stability without corresponding innovation suppression in low-barrier jurisdictions, the barriers are justified as prudential. If low-barrier jurisdictions show equivalent or lower systemic risk with higher innovation, the barriers are suspect as capture.',
    'If the barriers are genuinely prudential, the rising suppression_requirement is the necessary cost of coordination; the constraint is correctly classified as tangled_rope (real coordination, necessary extraction cost). If the barriers are primarily capture, the suppression is extractive overhead; the constraint should be reclassified toward snare. This omega addresses the boundary between justified regulatory extraction and unjustified incumbent capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_vs_prudent_oversight, empirical, 'Whether regulatory barriers serve prudential systemic-risk objectives or incumbent-protection objectives.').

omega_variable(
    committer_frame__regulatory_recognition_reading_identity,
    'This constraint instantiates the ''regulatory_recognition_reading'' of the digital_money_origin kernel. What makes this reading structurally distinct from the ''became_thinkable_reading'' and ''first_held_reading'' of the same kernel?',
    'The three readings attach the origin of digital money to three different institutional moments: (1) regulatory_recognition_reading: when authorities formally incorporated it into aggregates/frameworks (latest date); (2) became_thinkable_reading: when the concept was technically/institutionally conceivable (earlier); (3) first_held_reading: when individuals first held non-physical instruments as practical stores of value (earliest). Each reading produces a different constraint with different victims/beneficiaries. The regulatory_recognition_reading is dominated by legal/regulatory barriers and benefits incumbent financial institutions at the expense of unregulated innovators. This omega documents the kernel contest itself: no single constraint can resolve which reading is ''true,'' so the framework instantiates all three as separate stories linked by network.affects_constraints.',
    'The instantiation of this reading as a tangled_rope (coordination + extraction) depends on accepting that regulatory recognition is a meaningful social event distinct from technical feasibility and initial adoption. If regulatory recognition is merely documentation of what already existed (purely documentary function, zero extraction), the constraint should be reclassified as rope or mountain. If recognition is actively constitutive (creates new boundaries, new legitimacy tiers, new extraction routes), the constraint is correctly classified as tangled_rope. The committer choice (which reading applies) is a conceptual decision, not an empirical one, but the structural consequences (who benefits, who pays) follow from the reading choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame__regulatory_recognition_reading_identity, conceptual, 'Identity and structural distinctness of the regulatory_recognition_reading within the digital_money_origin kernel contest.').

omega_variable(
    identity_lock_mechanism__unregulated_innovators,
    'The unregulated_digital_innovators stakeholder is marked identity_locked. What specific identity fusion binds them to the non-regulated model? Is it ideological (conviction that financial services should not be regulated), technical (their entire architecture rests on decentralized operation), or relational (their community and social standing depends on maintaining independence)?',
    'Qualitative interviews and ethnographic observation of digital-currency development communities; document the reasons innovators give for refusing to restructure for regulatory compliance. If the reasons are primarily ideological, the lock is normative; if technical, architectural; if relational, social. The resolution determines the likely stability of the lock under exit pressure: ideological locks can shift with argument, technical locks are more durable, relational locks are highly durable.',
    'If the identity lock is primarily ideological, some fraction of unregulated innovators may restructure for regulatory recognition (reducing the victim population). If the lock is technical and relational, exit suppression will be higher—innovators will remain locked in non-recognized space and the constraint''s suppression_requirement will persist. This shapes the long-term trajectory: a tangible_rope with some identity-lock dissolution might evolve toward rope (pure coordination) or snare (pure extraction), depending on how many innovators accept the regulatory framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism__unregulated_innovators, empirical, 'Nature and durability of identity-lock mechanism binding unregulated digital innovators to non-regulated model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__regulatory_recognition_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(digi_tr_t5, digital_money_origin__regulatory_recognition_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(digi_tr_t10, digital_money_origin__regulatory_recognition_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(digi_tr_t15, digital_money_origin__regulatory_recognition_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(digi_tr_t20, digital_money_origin__regulatory_recognition_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(digi_tr_t25, digital_money_origin__regulatory_recognition_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(digi_be_t5, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(digi_be_t10, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(digi_be_t15, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(digi_be_t20, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(digi_be_t25, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(digi_su_t5, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(digi_su_t10, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(digi_su_t15, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(digi_su_t20, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(digi_su_t25, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(digital_money_origin__regulatory_recognition_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).

% DUAL FORMULATION NOTE:
% The digital_money_origin kernel contests three origin dates for digital money. This story instantiates the regulatory_recognition_reading (latest date, regulatory frameworks as the constitutive event). The became_thinkable_reading dates emergence to technical/institutional conceivability (earlier); the first_held_reading dates it to initial adoption as a store of value (earliest). All three are live readings held by different analytical communities—the readings coexist and influence each other (regulatory frameworks constrain what is technically thinkable, and first adoption drives toward regulatory recognition) but do not foreclose each other. Each reading produces a separate constraint story with distinct beneficiaries, victims, and type classifications. Link all three via network.affects_constraints to model the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__regulatory_recognition_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
