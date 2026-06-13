% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__maximalist_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__maximalist_reading
 *   human_readable: Bitcoin Whitepaper Covenant (Maximalist Reading)
 *   domain: cryptoeconomics/monetary-policy
 *
 * SUMMARY:
 *   The Bitcoin whitepaper (published October 2008) describes a peer-to-peer
 *   electronic cash system with a fixed 21-million-coin supply and
 *   proof-of-work consensus. The MAXIMALIST READING interprets this
 *   whitepaper as an immutable foundational covenant: the emission schedule,
 *   consensus rules, and scarcity commitment are sacred and cannot be altered
 *   without betraying Bitcoin's founding mission. Under this reading, ANY
 *   protocol modification—even efficiency improvements, privacy enhancements,
 *   or scalability solutions—is evaluated against the covenant: 'Does this
 *   violate the whitepaper commitment?' If the answer is yes (where yes is
 *   defined broadly), the proposal is rejected and its proponents are labeled
 *   protocol-splitters or altcoiners. This reading benefits early adopters
 *   and hodlers (immutability = scarcity preservation = wealth security) and
 *   vests protocol-change authority in the maximalist developer coalition
 *   (they control the interpretation and gate proposals). It victimizes
 *   layer-2 teams, innovation researchers, and pragmatic protocol advocates
 *   who propose improvements and encounter the covenant frame as a rejection
 *   mechanism and active suppression. The constraint is CLAIMED as snare
 *   (pure extraction using immutable-law framing as cover). The authored
 *   metrics (0.68 extractiveness, 0.71 suppression, rising theater ratio)
 *   reflect this interpretation. Note: SIBLING READINGS exist—the
 *   pragmatic_synthesis reading permits base-layer immutability while
 *   allowing upper-layer innovation; the utility_reading treats the
 *   whitepaper as a minimum viable spec enabling improvement. All three
 *   readings contest the same kernel (Bitcoin's whitepaper and consensus
 *   rules). Each reading is ε-distinct and forms a separate constraint story,
 *   linked via the network.
 *
 * KEY AGENTS:
 *   - early_adopters_holders: Benefit from immutability narrative and scarcity commitment; mobile exit (can move to other assets). Their wealth depends partly on the covenant reading persisting.
 *   - maximalist_developer_coalition: Agenda-setter; controls the interpretation of immutability; gates protocol proposals; exercises veto authority via code review and narrative framing. Organized power, mobile exit but exit is costly to their authority.
 *   - layer2_scalability_projects: Constrained by immutable base layer; propose settlement improvements and encounter covenant rejection. Powerful actors but constrained exit (network splinter is costly).
 *   - innovation_layer_developers: Propose opcodes, privacy schemes, state commitments; encounter covenant frame as rejection. Moderate power, constrained exit.
 *   - protocol_change_advocates: Identity-locked to Bitcoin; propose improvements based on network conditions; face suppression via covenant framing and social pressure. Moderate power, identity_locked exit—the most trapped seat.
 *   - casual_users: Benefit from hard-money narrative; pay via poor utility (throughput, finality, fees). Powerless but mobile exit.
 *   - core_consensus_researchers: Analytical seat; measure whether the covenant is structural or constructed narrative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.68).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.71).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, snare).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Whitepaper Covenant (Maximalist Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary-policy").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '5229e092-d4ec-4912-b788-b8512a0c592a').
narrative_ontology:cs_kernel_codification('5229e092-d4ec-4912-b788-b8512a0c592a', fixed_text).
narrative_ontology:cs_authority_grounding('5229e092-d4ec-4912-b788-b8512a0c592a', extraction).
narrative_ontology:cs_interpretation_layer_present('5229e092-d4ec-4912-b788-b8512a0c592a').
narrative_ontology:cs_reading_relation('5229e092-d4ec-4912-b788-b8512a0c592a', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_reading_relation('5229e092-d4ec-4912-b788-b8512a0c592a', bitcoin_consensus_kernel__utility_reading, coexists_with).
narrative_ontology:cs_axiom('5229e092-d4ec-4912-b788-b8512a0c592a', foundational, whitepaper_immutable_covenant).
narrative_ontology:cs_axiom_status(whitepaper_immutable_covenant, holdable).
narrative_ontology:cs_axiom_grounding('5229e092-d4ec-4912-b788-b8512a0c592a', whitepaper_immutable_covenant, deontological).
narrative_ontology:cs_axiom('5229e092-d4ec-4912-b788-b8512a0c592a', foundational, emission_schedule_sacred).
narrative_ontology:cs_axiom_status(emission_schedule_sacred, holdable).
narrative_ontology:cs_axiom_grounding('5229e092-d4ec-4912-b788-b8512a0c592a', emission_schedule_sacred, deontological).
narrative_ontology:cs_reference_frame('5229e092-d4ec-4912-b788-b8512a0c592a', immutable_whitepaper_authority).
narrative_ontology:cs_drift_state('5229e092-d4ec-4912-b788-b8512a0c592a', contemporary_scalability_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5229e092-d4ec-4912-b788-b8512a0c592a', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, maximalist_developer_coalition).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, layer2_scalability_projects).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, innovation_layer_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_change_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) and rises over the interval (0.48 → 0.68) because the constraint extracts protocol-change authority from those who would improve Bitcoin and concentrates it in the maximalist coalition. The measurement trajectory shows the constraint hardening over time: early on, some debate occurred and the constraint was less enforced; as pragmatic pressure increased (more proposals, more users asking for scalability), the coalition enforced the covenant more actively, raising extractiveness. By year 12–16 the rate of extraction plateaus (0.68) because the regime stabilizes: pragmatists have learned they cannot win via debate and mostly divert effort to layer-2/altcoins, or become fully identity-locked to Bitcoin's constraints. Suppression is high (0.71) and rises through the interval (0.55 → 0.71) because maintaining the covenant reading requires active gatekeeping: rejecting proposals, labeling pragmatists as splitters, exercising commit authority to block PRs. The suppression trajectory shows increasing enforcement effort—the coalition must work harder to suppress alternatives as the pragmatic pressure mounts. Theater ratio is moderate-high (0.42) and rises over the interval (0.25 → 0.42) because the constraint's function transitions from background law (early on, immutability seemed natural and requiring little defense) to actively maintained narrative (as pragmatic alternatives emerged and scaled, the coalition had to perform more discourse work to defend the covenant reading). Accessibility collapse is high (0.79) because once the covenant frame is accepted, alternatives become nearly invisible—forking means splitting the network, moving to altcoins means abandoning Bitcoin credibility, and staying means accepting the constraints. No path forward is cheap. Resistance is moderate (0.61) because pragmatists form a coherent opposition (layer-2 teams, researchers, protocol advocates), but their diffuse nature and identity-lock prevent them from mounting sustained coordination to overthrow the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The maximalist developer coalition (agenda-setter seat) should compute a MUCH lower extractiveness, near-zero suppression, and low theater—they perceive themselves as defending Bitcoin's founding mission against dilution, coordinating consensus around immutability, and doing essential maintenance work. Computed type from their seat: mountain or rope (we are defending natural law / genuine coordination). From the payer seats (protocol advocates, layer-2 teams): high extractiveness, high suppression, rising theater—they perceive pure gatekeeping that blocks their innovations. Computed type from their seat: snare (extraction disguised as immutable law). From the beneficiary seats (hodlers, early adopters): moderate extractiveness (they benefit without bearing the cost, but they do bear reputational/narrative risk if the constraint breaks), low suppression, low theater—they perceive the constraint as protecting their wealth without much active work. Computed type from their seat: tangled_rope (coordination that benefits them, extraction that falls on others). From the casual-user seat: moderate extractiveness (poor utility), low suppression (they can exit easily), low theater (they don't engage with the narrative). Computed type from their seat: rope (genuine coordination they could take or leave). The divergence of per-seat types is the diagnostic target: a constraint with stable authored metrics but divergent per-seat classifications is a canonical case of asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint extracts protocol-change authority from developers and concentrates it in the maximalist coalition. Beneficiaries (early adopters, hodlers, maximalists) gain from immutability narrative and authority concentration without bearing costs; victims (pragmatists, layer-2, innovation researchers) bear costs (deferred improvements, innovation suppression) without controlling the constraint. Identity-lock on protocol advocates intensifies the trap: they remain in Bitcoin because they identify with it, but the constraint prevents them from improving it. Directionality for each seat: early_adopters_holders (~0.25, beneficiary-side, mobile exit), maximalist_developer_coalition (~0.25–0.35, beneficiary-side, they control the constraint), layer2_scalability_projects (~0.75, target-side, constrained exit), innovation_layer_developers (~0.75, target-side, constrained exit), protocol_change_advocates (~0.85, target-side, identity-locked exit—the most trapped), casual_users (~0.5, symmetric, mobile exit, dual benefit/cost). The directionality derivation (beneficiary/victim + exit + power → d) does not require overrides here; the structural data is transparent and derived d values are accurate. No directionality_overrides are needed because the beneficiary/victim declarations and exit options directly yield the correct d.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_reading_vs_technical_document,
    'Is the whitepaper a foundational immutable covenant (the maximalist reading), a technical design document subject to improvement (the pragmatic reading), or a minimum viable consensus specification (the utility reading)?',
    'Historical analysis of Satoshi''s original intent (email archives, source code comments, early forum posts); comparative analysis of how other technical protocols treat their founding specifications (are they treated as sacred or as improvable baselines?); assessment of whether the constraint''s persistence tracks the technical facts or the beneficiary interests of the coalition controlling the interpretation.',
    'If the whitepaper is a covenant, the maximalist reading is structurally accurate and the constraint is mountain-like (inevitable from the rules themselves). If it is a technical document or minimum spec, the reading is constructed narrative, the constraint is snare (extraction via false naturalization), and forking or alternative readings become legitimate. If readings genuinely coexist (different parties hold different interpretations), the constraint is tangled_rope (coordination + asymmetric interpretation-power).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_reading_vs_technical_document, conceptual, 'Whether the constraint''s structure rests on technical immutability or on a constructed reading of immutability.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression structural (technical barriers to forking or protocol change, economic cost of network splinter) or internalized (protocol pragmatists have adopted the covenant reading as their own identity, making them reluctant to propose changes even when technically feasible)?',
    'Post-fork analysis: if a pragmatic fork succeeds and former Bitcoin partisans migrate, suppression was primarily structural; if suppression persists even with low-cost alternatives available, it is internalized. Survey or ethnographic research on why pragmatists stay despite frustration: do they cite lock-in costs (structural) or identification with Bitcoin''s immutability (internalized)?',
    'If suppression is internalized, the constraint''s effective suppression is higher than the structural measure suggests (victims carry the suppression with them even after exit options open); if structural, the suppression could collapse quickly if exit costs drop or alternatives emerge. This affects the stability prediction for the constraint: internalized suppression stabilizes it; structural suppression makes it vulnerable to fork or regulatory change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in the covenant frame.').

omega_variable(
    hodler_benefit_quantification,
    'What fraction of early adopters'' wealth depends on the maximalist reading (immutability narrative), and what fraction depends on network utility and organic adoption?',
    'Economic analysis of Bitcoin price correlations with covenant-defending discourse vs. utility-improving developments; survey of hodler motivation (store-of-value narrative vs. medium-of-exchange utility); counterfactual: if pragmatic improvements were adopted and Bitcoin retained network effects but lost the immutability-maximalism framing, how much would hodler valuations decline?',
    'If hodler wealth is heavily dependent on the covenant narrative (>60%), their beneficiary role is secured by the constraint and they have strong incentive to suppress pragmatic readings. If hodler wealth is mostly driven by network effects and utility, the covenant is performative theater and could be abandoned without major wealth loss. This affects whether the constraint is a genuine snare or a piton (mostly inertial maintenance of a narrative that no longer solves the founding problem).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hodler_benefit_quantification, empirical, 'Degree to which early-adopter benefit depends on immutability narrative vs. network utility.').

omega_variable(
    kernel_reading_contest_structure,
    'Is the maximalist reading one defensible interpretation among multiple coexisting readings, or does it foreclose the others within Bitcoin''s governance framework?',
    'Examine whether pragmatic and utility readings are debated as live alternatives in core Bitcoin development forums and code review, or whether they are excluded before debate via narrative gatekeeping (dismissed as ''not Bitcoin,'' ''protocol splitters,'' ''altcoin thinking''). Document the ratio of substantive technical debate to rhetorical exclusion in protocol discussions.',
    'If the readings coexist, the constraint is tangled_rope (coordination + contested interpretation). If the maximalist reading actively forecloses pragmatic readings via narrative authority, the constraint is snare (extraction via interpretive gatekeeping). The reading_relations field in cs_structure will record whether this reading forecloses or coexists with pragmatic and utility readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Whether the maximalist reading forecloses or coexists with pragmatic and utility readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcm_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bcm_tr_t2, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 2, 0.31).
narrative_ontology:measurement(bcm_tr_t4, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement(bcm_tr_t8, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 8, 0.41).
narrative_ontology:measurement(bcm_tr_t12, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(bcm_tr_t16, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 16, 0.42).

% Extraction over time
narrative_ontology:measurement(bcm_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(bcm_be_t2, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 2, 0.54).
narrative_ontology:measurement(bcm_be_t4, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(bcm_be_t8, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(bcm_be_t12, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(bcm_be_t16, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 16, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bcm_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bcm_su_t2, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 2, 0.62).
narrative_ontology:measurement(bcm_su_t4, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 4, 0.67).
narrative_ontology:measurement(bcm_su_t8, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(bcm_su_t12, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(bcm_su_t16, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 16, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__maximalist_reading, 0.25).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).

% DUAL FORMULATION NOTE:
% The bitcoin_consensus_kernel constraint family consists of three readings of the same contested kernel: (1) maximalist_reading treats the whitepaper as immutable covenant, extracting protocol-change authority; (2) pragmatic_synthesis permits base-layer immutability while allowing upper-layer innovation, decomposing extraction; (3) utility_reading treats the whitepaper as minimum viable spec enabling iterative improvement, reducing the constraint's extractiveness substantially. The three readings are ε-distinct: they differ in what counts as a 'protocol change that violates the covenant' (narrow in pragmatic, none in utility, strict in maximalist), and thus in how much developer labor is suppressed. They form a constraint family because they contest the interpretation of the same kernel (Bitcoin's whitepaper and consensus rules). Each story must declare its own ε, beneficiary/victim structure, and claimed type independently, without reconciliation. The maximalist reading is the highest-extraction variant; it forecloses or coexists with the siblings, as declared in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__maximalist_reading, organized, 0.25).
constraint_indexing:directionality_override(bitcoin_consensus_kernel__maximalist_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
