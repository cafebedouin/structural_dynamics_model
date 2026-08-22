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
 *   human_readable: Regulatory Recognition of Digital Money (Institutional Reading)
 *   domain: monetary/institutional/legal
 *
 * SUMMARY:
 *   This story instantiates the REGULATORY_RECOGNITION reading of the
 *   contested kernel 'digital_money_origin.' It asserts that digital money
 *   emerged as a constraint when monetary authorities formally incorporated
 *   it into statistical aggregates, regulatory frameworks, and supervisory
 *   categories — not when the technology became conceivable
 *   (became_thinkable_reading) nor when individuals first held digital money
 *   as a store of value (first_held_reading). From this reading's
 *   perspective, the origin date is the date regulatory recognition occurred:
 *   when central banks formally classified digital money in M-aggregates,
 *   when banking regulators issued capital and reserve rules for digital
 *   assets, when international standard-setters (FSB, BIS) issued guidance.
 *   The recognition act is simultaneously descriptive (identifying a
 *   phenomenon that exists) and prescriptive (deciding which phenomena
 *   deserve money status). This reading emphasizes the institutional and
 *   legal character of the origin — money emerged as a regulatory category,
 *   not as a technological achievement or a social practice. The claimed type
 *   is tangled_rope: it coordinates the classification problem (which digital
 *   phenomena deserve money status) AND asymmetrically extracts from
 *   unregulated innovators, who lose the freedom to operate outside the
 *   defined regulatory perimeter.
 *
 * KEY AGENTS:
 *   - Monetary Authorities (central banks, national regulators): agenda setters and enforcers of regulatory definition
 *   - Incumbent Financial Institutions (commercial banks, payment processors): primary beneficiaries capturing monopoly privileges and barriers against competition
 *   - Unregulated Digital Innovators (startups, protocol developers): victims bearing compliance costs and operational constraints
 *   - Peer-to-Peer Payment Networks (cryptocurrency, mesh networks): victims facing identity-lock because autonomy depends on remaining unregulated
 *   - Decentralized Finance Protocols (algorithmic systems): victims trapped with no compliant operation path
 *   - Retail Users (citizens, payment users): incidental beneficiaries gaining consumer protection but losing payment choice
 *   - International Financial Regulators (FSB, BIS, IMF): global agenda setters coordinating recognition definitions
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
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Regulatory Recognition of Digital Money (Institutional Reading)").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary/institutional/legal").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, '7753008b-22b1-49c1-b055-a3823905a481').
narrative_ontology:cs_kernel_codification('7753008b-22b1-49c1-b055-a3823905a481', formalized).
narrative_ontology:cs_authority_grounding('7753008b-22b1-49c1-b055-a3823905a481', extraction).
narrative_ontology:cs_interpretation_layer_present('7753008b-22b1-49c1-b055-a3823905a481').
narrative_ontology:cs_reading_relation('7753008b-22b1-49c1-b055-a3823905a481', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('7753008b-22b1-49c1-b055-a3823905a481', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_axiom('7753008b-22b1-49c1-b055-a3823905a481', foundational, regulatory_recognition_constitutive_of_money_status).
narrative_ontology:cs_axiom_status(regulatory_recognition_constitutive_of_money_status, holdable).
narrative_ontology:cs_axiom_grounding('7753008b-22b1-49c1-b055-a3823905a481', regulatory_recognition_constitutive_of_money_status, conventional).
narrative_ontology:cs_axiom('7753008b-22b1-49c1-b055-a3823905a481', foundational, institutional_definition_prior_to_social_practice).
narrative_ontology:cs_axiom_status(institutional_definition_prior_to_social_practice, holdable).
narrative_ontology:cs_axiom_grounding('7753008b-22b1-49c1-b055-a3823905a481', institutional_definition_prior_to_social_practice, conventional).
narrative_ontology:cs_reference_frame('7753008b-22b1-49c1-b055-a3823905a481', regulatory_money_definition_authority).
narrative_ontology:cs_drift_state('7753008b-22b1-49c1-b055-a3823905a481', contemporary_post_2020_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7753008b-22b1-49c1-b055-a3823905a481', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, banking_regulators).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_digital_innovators).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, peer_to_peer_payment_networks).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, decentralized_finance_protocols).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, retail_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks and national financial regulators formally recognize and define digital money through regulatory orders, statistical classifications, and supervisory frameworks. They decide which digital assets count as money for accounting, reserve requirement, and monetary policy purposes. Their recognition is performative — it both describes what exists and prescriptively establishes what can operate as money within the regulated financial system.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Commercial banks and incumbent payment processors gain regulatory legitimacy and legal monopoly status through the formal recognition regime. Once digital money is defined and regulated, their charter privileges (ability to hold deposits, operate clearing systems, access central bank facilities) become enforceable barriers against unregulated competitors. They directly influence the definition process through industry associations, policy consultation, and regulatory capture.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, agenda_setter).

% Startups, open-source developers, and decentralized network operators who created digital payment mechanisms before regulatory definition. Once regulatory frameworks materialize, they are either forced to obtain licenses (creating compliance cost and bureaucratic dependency) or shut out from the formal financial system. Their innovation path becomes constrained to either adaptation under regulatory supervision or operation in the shadow economy.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_digital_innovators, payer,
    moderate, biographical, constrained, global).

% Communities built around specific digital payment protocols (cryptocurrency networks, mesh payment systems) face identity-locked exit: their existence as distinct payment systems depends on operating outside regulatory definition. Regulatory recognition creates a choice between accepting institutional incorporation (losing protocol independence) or remaining functionally marginal. Their exit options collapse because the identity of the protocol IS its operational autonomy.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, peer_to_peer_payment_networks, payer,
    moderate, biographical, identity_locked, global).

% Algorithmic and smart-contract-based payment and credit systems that depend on remaining unregulated to function (no human counterparty for KYC, no operator to license, no centralized entity to supervise). Regulatory recognition regimes treat them as either unlicensed money services or unacceptable financial infrastructure, making their core mechanism incompatible with legal operation. They cannot pivot to compliance without losing protocol properties.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, decentralized_finance_protocols, payer,
    powerless, immediate, trapped, global).

% Citizens and retail payment users gain regulatory consumer protections, deposit insurance, and fraud remedies when digital money is formally recognized. They also gain certainty about what counts as legal tender and what assets they can safely hold. However, their payment choice set is simultaneously narrowed to what regulators permit to operate legally.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, retail_users, beneficiary,
    organized, biographical, constrained, national).

% International bodies (FSB, BIS, IMF) coordinate regulatory definitions of digital money across jurisdictions. They establish the framework within which national authorities move, creating path dependency and lock-in effects. International standards bodies become the enforcers of recognition definitions at the global level.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, financial_regulators_international, agenda_setter,
    institutional, generational, analytical, global).

% Informal payment networks, remittance systems, and illicit finance actors who operated digital money transfers before formal recognition. Regulatory definition explicitly excludes them and criminalizes their operation. They are structurally outside the recognition conversation but are the objects of the enforcement that gives regulatory definition its force.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, shadow_economy_actors, excluded,
    powerless, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:fixing_cost_class(digital_money_origin__regulatory_recognition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, authoritative classification of what counts as money for accounting, reserve requirements, monetary policy transmission, and financial stability purposes. Solves the institutional coordination problem of which digital assets deserve the privilege of being treated as money within the formal financial system, and which payment mechanisms qualify for access to central bank facilities.
% TRANSFER_FUNCTION: Transfers institutional monopoly privilege and regulatory approval from traditional banking infrastructure to formal entities, while extracting compliance costs and operational constraints from unregulated innovators. The recognition itself is the transfer — it grants or denies the power to operate legally in the regulated financial space. Incumbent institutions gain entry barriers against competition; unregulated innovators lose the ability to operate outside the defined regulatory perimeter.
% ABSENT_VOICES: Decentralized protocol communities (whose design assumes regulatory non-recognition) and shadow-economy operators (whose participation would be illegitimated by definition) are excluded from the recognition negotiation. They would argue for open-ended definitions, pluralistic money concepts, or no formal definition at all — but they are not seated at regulatory tables. Their exclusion is structural to how regulatory recognition operates.
% DISAPPEARANCE_RATIONALE: If regulatory recognition of digital money disappeared — if central banks reverted to only recognizing physical currency and bank deposits as official money — the digital payment landscape would fragment. Unregulated networks could expand without compliance burden; incumbent institutions would lose regulatory-backed barriers; the authority to define what counts as money would dissolve to market competition and multiple local standards. The formal financial system would contract.
% FOUNDING_PROBLEM: By the early 2010s, digital payment mechanisms (electronic transfers, peer-to-peer networks, cryptocurrency) had achieved practical scale without institutional legitimacy. Central banks and regulators needed authoritative criteria to determine which digital instruments deserved treatment as money (reserve assets, regulatory assets, counted in M1/M2 aggregates) versus payment technologies (not money, separately regulated). The problem was institutional coherence: which digital phenomena fit within existing monetary frameworks and which required new regulatory categories.
% FOUNDING_PROBLEM_CORROBORATION: Central banks (ECB, Federal Reserve, Bank of England) attest the problem is continuously live, as new digital payment forms emerge and require regulatory classification. Regulatory consultations and policy papers from 2016–present describe this ongoing classification challenge. Independent monetary economists and financial technology analysts (outside the regulatory system and not primarily motivated by the established banking industry) confirm that defining digital money remains an active institutional problem requiring periodic regulatory reassessment.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness starts at 0.35 and rises to 0.68 over the interval, driven by progressive strengthening of regulatory definition and enforcement machinery. Early extractiveness is low because regulatory recognition was incomplete and unevenly enforced across jurisdictions; unregulated innovators retained operational freedom in gray zones. By the interval end, comprehensive regulatory frameworks exist in major jurisdictions, making the extraction complete and persistent. Suppression is high (0.71) and rises monotonically because it is structural: regulatory recognition literally bars unregulated operators from the formal financial system. The mechanism is legal (licensing, capital requirements, central bank access gates) not primarily force, but the barrier is absolute. Theater ratio is moderate (0.42) and rises to this level as regulatory sophistication increases — early regulatory definitions were genuinely focused on classification and consumer protection, but over time an increasing share of regulatory activity defends incumbents' competitive position against innovation. The measurement series use one shared time grid (every metric at every point), capturing the progressive lock-in of regulatory advantage over 40 time units. Extraction accelerates until regulatory frameworks stabilize (around t=25), after which extractiveness plateaus — further tightening adds marginal constraint but the core extraction is established.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats compute this constraint as pure coordination (or low-extraction rope): they see regulatory definition as necessary institutional coherence, consumer protection, and financial stability infrastructure. The payer seats (unregulated innovators, decentralized protocols) compute it as snare: they see the same definition as a legal barrier designed to protect incumbents and eliminate competition. This divergence is not reconcilable — it follows from structural position. The engine should compute tangled_rope from the beneficiary/victim and enforcement data, but the per-seat type divergence should be stark: agenda-setter ~rope, payer ~snare. The authored claimed_type (tangled_rope) describes the structural reality (both coordination and extraction present), not any single seat's perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities sit at d=0.0 (analytical seat, setting the rules — they are beneficiaries of the coordination function and controllers of the definition). Incumbent financial institutions sit at d~0.1-0.2 (powerful institutional agents with arbitrage options, yet primary beneficiaries of regulatory barriers — they prefer the constrained constraint to a more open competitive environment, so their directionality is beneficiary-leaning despite institutional power). Unregulated innovators sit at d~0.85-0.95 (moderate power but trapped or identity-locked exit, facing systematic extraction through exclusion from regulatory legitimacy — full targets). Decentralized finance protocols sit at d=1.0 (powerless, trapped, no compliant operation path — the constraint extracts their entire operational space). Retail users sit at d~0.5-0.6 (incidental beneficiaries with constrained exit — they gain protections but lose choice, net near-symmetric). This per-seat divergence is the core perspectival gap: from the monetary authority seat, recognition is neutral classification; from the unregulated innovator seat, it is coercive extraction of operational freedom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (institutional classification of digital money) is LIVE: central banks continue issuing new guidance, regulators reopen questions as technology evolves, and international bodies update standards. The disappearance_verdict is WORLD_REARRANGES: if regulatory recognition vanished, the digital payment landscape would fragment and incumbent institutions would lose competitive barriers. This pairing (live founding_problem + world_rearranges verdict) indicates no mandatrophy — the constraint is not a zombie maintaining theatrical functions after its function died. However, the rising theater_ratio (0.18 to 0.42 over the interval) and plateau in extractiveness after t=25 suggest that regulatory activity is increasingly performative: early regulation was genuinely needed classification work; later regulation is defending competitive position. This is a sign of incipient mandatrophy (the coordination function is satisfied, but the extraction persists through bureaucratic inertia), but the constraint has not yet crossed into full piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_recognition_temporal_ambiguity,
    'At what specific institutional moment did ''regulatory recognition'' occur? Was it the first national central bank classification? The first international coordination? The formalization in statute vs. guidance?',
    'Historical audit of central bank policy statements, regulatory publications, and standard-setting body minutes from 2008–2020. Identify the first moment a central bank formally added a digital money category to its statistical aggregates or issued binding supervisory guidance.',
    'Different origin dates yield different constraint timelines and different periodization of when the extraction began. An earlier date (e.g., 2013) makes extraction earlier and longer; a later date (e.g., 2018) compresses the extraction timeline. The classification is robust to this ambiguity, but the measured time-to-plateau differs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_recognition_temporal_ambiguity, empirical, 'Temporal referent ambiguity: which regulatory event marks the origin of recognition?').

omega_variable(
    beneficiary_capture_in_definition,
    'To what degree did incumbent financial institutions shape the regulatory definitions of digital money? Was regulatory recognition a capture event or an independent regulatory judgment?',
    'Analysis of regulatory consultation records, industry association lobbying data, and regulatory agency meeting minutes. Trace the path from industry proposals to regulatory definitions.',
    'If capture is substantial, the constraint''s classification as tangled_rope (hybrid coordination + extraction) should shift toward snare (pure extraction using coordination as cover). If regulatory definition was substantially independent, tangled_rope holds. The directionality of incumbent institutions might also shift (from d~0.15 to d~0.35 under higher capture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_in_definition, empirical, 'Degree of institutional capture in the regulatory definition process.').

omega_variable(
    innovation_suppression_mechanism,
    'Is the suppression of unregulated innovators structural (legal barriers to operation, enforcement) or internalized (innovators have internalized the belief that they cannot operate outside regulatory frameworks)?',
    'Post-recognition behavior of digital payment innovators: do they persist in developing regulation-evasive technologies, or do they pivot to compliant operational models? Post-barrier suppression trajectories reveal mechanism.',
    'If suppression is structural only, the barrier is permeable over time as enforcement decays or workarounds develop. If substantially internalized, innovators carry the suppression even if formal barriers relax — the extraction is more durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_suppression_mechanism, empirical, 'Whether the suppression of unregulated operators is structural or internalized.').

omega_variable(
    kernel_reading_foreclosure,
    'Does this reading (regulatory_recognition) logically foreclose one of its sibling readings (became_thinkable, first_held), or do all three readings coexist as live alternative framings of the same events?',
    'Logical analysis: can an agent hold ''digital money originated when the concept was thinkable'' (became_thinkable) AND ''digital money originated when monetary authorities formally recognized it'' (this reading) in a single coherent framework? Or do these premises directly contradict?',
    'If foreclosure exists, the network relation should be ''forecloses'' (rare). If they coexist, ''coexists_with'' (more common). The categorization affects how the engine models the kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether regulatory_recognition and sibling readings logically foreclose each other or coexist.').

omega_variable(
    international_coordination_vs_capture,
    'Did international financial standard-setters (FSB, BIS) independently converge on digital-money definitions, or did they coordinate to harmonize definitions that served incumbent institutions'' interests?',
    'Comparative analysis of FSB/BIS guidance documents with independent monetary economics literature. Do the international standards track academic recommendations, or do they uniquely benefit incumbent interests?',
    'If independent convergence, the international layer is coordination efficiency. If coordinated capture, the extraction mechanism is global and harder to resist locally. This affects spatial_scope and the persistence of the constraint across jurisdictions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_coordination_vs_capture, empirical, 'Whether international monetary coordination reflects independent convergence or coordinated institutional capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__regulatory_recognition_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(digi_tr_t5, digital_money_origin__regulatory_recognition_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(digi_tr_t10, digital_money_origin__regulatory_recognition_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(digi_tr_t15, digital_money_origin__regulatory_recognition_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(digi_tr_t20, digital_money_origin__regulatory_recognition_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(digi_tr_t25, digital_money_origin__regulatory_recognition_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(digi_tr_t30, digital_money_origin__regulatory_recognition_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__regulatory_recognition_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(digi_be_t5, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(digi_be_t10, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(digi_be_t15, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(digi_be_t20, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(digi_be_t25, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(digi_be_t30, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(digi_su_t5, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(digi_su_t10, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(digi_su_t15, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(digi_su_t20, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(digi_su_t25, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(digi_su_t30, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(digital_money_origin__regulatory_recognition_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, central_bank_digital_currency_authority).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, banking_regulation_capital_requirements_digital_assets).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, cryptocurrency_regulatory_classification).

% DUAL FORMULATION NOTE:
% This constraint (regulatory_recognition_reading) is part of the three-reading decomposition of the contested kernel 'digital_money_origin.' The three readings share the referent (the digital payment landscape) but disagree on the origin date. This reading privileges the institutional/legal moment (regulatory recognition) over technological conception or user practice. See constraint_digital_money_origin__became_thinkable_reading and constraint_digital_money_origin__first_held_reading for the competing temporal framings. All three readings are linked via network.affects_constraints to indicate their membership in the same constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__regulatory_recognition_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
