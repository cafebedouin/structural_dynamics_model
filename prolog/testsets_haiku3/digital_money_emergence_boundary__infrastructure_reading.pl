% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__infrastructure_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Boundary: Infrastructure Emergence (1967-1977)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   The infrastructure reading defines digital money by the technical
 *   capacity to move it electronically through cleared channels, dating its
 *   emergence to 1967 (first ATMs), 1972 (ACH fully operational), and
 *   crystallizing at 1977 (SWIFT operational globally). This reading
 *   privileges the perspective of banking infrastructure operators and the
 *   clearing houses that control the rails. It makes money a category defined
 *   not by what consumers can hold or even what theorists think, but by what
 *   the infrastructure can move. The coordination problem is real: scaling
 *   settlement required electronic clearing. But the reading entangles
 *   genuine coordination (fast, accurate interbank settlement) with
 *   extraction (control over the boundary definition, gatekeeping of
 *   non-integrated institutions, fee revenue to infrastructure operators).
 *   Beneficiaries (infrastructure operators and incumbent banks) have
 *   institutional incentive to freeze this reading as the canonical one
 *   because it legitimates their role and locks alternatives out. The
 *   constraint is claimed as tangled_rope because it serves both coordination
 *   (real problem solved) and extraction (real value captured by
 *   gatekeepers). The metrics show rising extractiveness over the decade as
 *   the infrastructure solidifies and alternative clearing paths are
 *   foreclosed; rising theater as the functional narrative ('we solve
 *   settlement') gradually carries less of the constraint's actual operation
 *   (gatekeeping and fee collection carry more). The constraint is one
 *   reading of a contested kernel: where and how digital money emerges is a
 *   question three different communities answer differently.
 *
 * KEY AGENTS:
 *   - banking_infrastructure_operators: Institutional power, global reach; define and control the rails (ACH, SWIFT); benefit from exclusive control of the boundary; set fees unilaterally.
 *   - consumers: Powerless, identity_locked into banking system; cannot access infrastructure directly; existence as monetary agents becomes dependent on bank mediation.
 *   - commercial_banks: Organized power, constrained exit (must use infrastructure to remain competitive); benefit from mediation role; locked into paying fees to infrastructure operators.
 *   - non_integrated_financial_institutions: Powerful in isolation but trapped; excluded by technical and regulatory standards; cannot compete on the definition of money itself.
 *   - central_banks: Institutional power; benefit from infrastructure (enables monetary policy) but are themselves coordinate users, not controllers.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.68).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.42).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Boundary: Infrastructure Emergence (1967-1977)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '919c7236-c5c2-4a12-8a3b-dd9770eece71').
narrative_ontology:cs_kernel_codification('919c7236-c5c2-4a12-8a3b-dd9770eece71', formalized).
narrative_ontology:cs_authority_grounding('919c7236-c5c2-4a12-8a3b-dd9770eece71', extraction).
narrative_ontology:cs_interpretation_layer_present('919c7236-c5c2-4a12-8a3b-dd9770eece71').
narrative_ontology:cs_reading_relation('919c7236-c5c2-4a12-8a3b-dd9770eece71', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('919c7236-c5c2-4a12-8a3b-dd9770eece71', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('919c7236-c5c2-4a12-8a3b-dd9770eece71', foundational, infrastructure_defines_monetary_category).
narrative_ontology:cs_axiom_status(infrastructure_defines_monetary_category, holdable).
narrative_ontology:cs_axiom_grounding('919c7236-c5c2-4a12-8a3b-dd9770eece71', infrastructure_defines_monetary_category, conventional).
narrative_ontology:cs_axiom('919c7236-c5c2-4a12-8a3b-dd9770eece71', foundational, clearing_capacity_precedes_consumer_access).
narrative_ontology:cs_axiom_status(clearing_capacity_precedes_consumer_access, holdable).
narrative_ontology:cs_axiom_grounding('919c7236-c5c2-4a12-8a3b-dd9770eece71', clearing_capacity_precedes_consumer_access, instrumental).
narrative_ontology:cs_reference_frame('919c7236-c5c2-4a12-8a3b-dd9770eece71', infrastructure_controlled_monetary_definition).
narrative_ontology:cs_drift_state('919c7236-c5c2-4a12-8a3b-dd9770eece71', post_1977_entrenchment, gap(codification_collapse, minor, false)).
narrative_ontology:cs_created_at('919c7236-c5c2-4a12-8a3b-dd9770eece71', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, consumers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, non_integrated_financial_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, central_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, monetary_sovereignty_requires_clearing_infrastructure).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, settlement_infrastructure_precedes_consumer_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the clearing houses, ACH networks, and SWIFT messaging systems that enable electronic fund transfer between banks. They define what counts as 'money' within the system: transfers that can be executed electronically through their infrastructure. They benefit from monopolistic control of the rails and from the definition-power that comes with technical gatekeeping. They set interbank fees, settlement standards, and participation rules.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the infrastructure's existence (can clear payments electronically, reducing settlement friction) but must pay access fees and comply with technical standards set by infrastructure operators. They become key intermediaries in the new definition: deposits held at banks are now 'digital money' because banks can move them electronically. They capture value by charging consumers for account services and playing the spread.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, payer).

% Hold deposits at banks that increasingly define money as electronically transferable balances rather than physical currency. They cannot directly access the infrastructure; their access is mediated by their bank. They bear the cost of infrastructure fees embedded in account maintenance charges, and they are locked into the definition: if your money is 'digital' only because a bank holds it and can move it electronically, you cannot exercise monetary autonomy outside the banking system. Your money exists in the infrastructure; you exist at the periphery.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, consumers, payer,
    powerless, biographical, identity_locked, global).

% Credit unions, savings associations, and non-bank lenders are excluded from full participation in SWIFT and ACH networks or face high participation costs. They must either join the infrastructure at high cost (locking them into the rules) or remain outside and offer inferior clearing services. Their exclusion is not accidental; it is maintained by regulatory structure and technical gatekeeping. The definition of digital money as 'what moves through our infrastructure' freezes them out of the category.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, non_integrated_financial_institutions, payer,
    powerful, biographical, trapped, global).

% Benefit from the infrastructure's existence because it allows them to conduct monetary policy by moving reserves through the same clearing systems (eventually: Fed Wire, TARGET2). They coordinate with infrastructure operators on regulatory standards and can mandate participation by supervised institutions. They also exercise some gatekeeping authority but are themselves dependent on the infrastructure existing.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter).

% Alternative clearing concepts (decentralized ledgers, private networks, alternative settlement models) are structurally barred from claiming the status 'digital money' because the definition is locked to infrastructure that already exists and is controlled by entrenched actors. They would argue that digital money should be definable in multiple ways (peer-to-peer, decentralized, non-intermediated) but cannot compete on the definition itself.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, payment_network_alternatives, excluded,
    moderate, biographical, trapped, global).

% Examine what counts as money and on what grounds. They measure whether the infrastructure reading (money = what moves through cleared electronic systems) is economically coherent and whether it serves public purposes or primarily benefits infrastructure operators and incumbent banks.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_theorists_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The infrastructure enables high-volume, low-error interbank settlement: payments that previously required physical transport of value or expensive telegraphic instructions can now be routed through standardized, fast clearing systems. This solves the coordination problem of how to settle millions of transactions daily without collapse or error.
% TRANSFER_FUNCTION: Moves control of the boundary between monetary and non-monetary assets from consumers and alternative definitions to infrastructure operators and banking incumbents. Moves fees from clearing operations to banks and infrastructure operators. Moves the narrative authority over 'what is money' from theorists and governments to the technical gatekeepers of the rails.
% ABSENT_VOICES: Decentralized settlement advocates, non-bank financial institutions locked out of SWIFT, consumers who would prefer monetary systems not intermediated through commercial banks, alternative clearing house designs, and the broader public interest in whether this particular infrastructure boundary should define a fundamental economic category.
% DISAPPEARANCE_RATIONALE: If this infrastructure and its definitional claim disappeared, central banks would either build alternative clearing systems or revert to physical settlement for major transactions. The category 'digital money' would either cease to exist or be redefined by whatever replacement infrastructure arose. Banks would lose the fee revenue and operational advantage that come from being the only route to electronic settlement. The definition of money would again be contestable rather than locked to a particular technical path.
% FOUNDING_PROBLEM: By the mid-1960s, paper-based clearing could not scale to post-war transaction volumes. Banks faced exponential growth in checks and payments but no way to move value electronically. Settlement took weeks; errors cascaded. A technical solution was needed: how to verify, route, and settle payments without physical transport.
% FOUNDING_PROBLEM_CORROBORATION: Banking historians and payment systems engineers (outside the benefiting parties) document the clearing crisis of the 1960s and the technical imperatives that drove ACH and SWIFT development. Infrastructure operators attest the problem is continuous (they must keep clearing working). Central banks also attest: they depend on fast clearing for monetary control. The problem remains live but is now buried under the layer of operational success — it no longer looks like a problem because the infrastructure works.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured as 0.68 at 1977 because the infrastructure operators have captured three layers of value: (1) the definition-power (money is what we can move), (2) mandatory participation fees (banks must join and stay in the network), (3) the exclusive gatekeeping function (non-integrated institutions and alternative clearing models are locked out). The measurement trajectory from 0.35 to 0.68 shows rising extractiveness as the infrastructure goes from novel to indispensable; as it becomes indispensable, the operators' bargaining power rises. Suppression is lower (0.42 at 1977) than extractiveness because participants are not actively resisting the system—they are moving to it voluntarily because it solves a real problem. The suppression that exists is structural (regulatory gatekeeping, network-lock, technical barriers to entry) rather than coercive. Theater is moderate-low (0.28) because the coordination narrative is genuinely true and carries most of the story, but a growing share of operational effort is gatekeeping rather than settlement. The accessibility_collapse is high (0.71) because once the infrastructure exists and is locked in place, alternatives collapse: any competitor would need to build equivalent clearing infrastructure, which is prohibitively expensive and faces regulatory barriers. Resistance is moderate (0.38) because some actors (central banks, consumer advocates, alternative clearing advocates) do resist this reading, but they are outweighed by the alignment of incumbent banks and infrastructure operators on this definition.
 *
 * PERSPECTIVAL GAP:
 *   The infrastructure reading generates sharp divergence between seats. Infrastructure operators and central banks perceive this reading as natural—of course digital money is what the infrastructure can move; what else would it be? Non-integrated institutions and consumers perceive this reading as a deliberate gatekeeping construction that benefits entrenched players. Monetary theorists may perceive it as parochial—defining money by infrastructure rather than by function or chartality. The engine computes these divergences from the power/exit/role data; the narrative gap is not a bug but the constraint's structural signature. A reading that diverges sharply between seats is precisely what reveals extraction: if everyone perceived the same type, the constraint would look like pure coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   From the infrastructure operator's seat: full beneficiary (d ≈ 0.0). They define the boundary, collect the fees, control participation, and bear no cost. From the consumer's seat: moderate-to-high target (d ≈ 0.75). They pay indirectly through bank fees; their monetary autonomy is locked into the system; they cannot hold or transact with money outside of mediation. From the commercial bank's seat: near-symmetric (d ≈ 0.5). They benefit from the infrastructure (can clear efficiently) but pay for access and are constrained by the operators' rules. From the non-integrated institution's seat: high target (d ≈ 0.85). They are excluded from full participation; they cannot claim the status 'digital money' for their services; they lose market share to integrated competitors. These directional divergences are computed by the engine from the structural data (beneficiary/victim declarations, exit options, power atoms). The payer seats should compute quite differently from the agenda-setter seat, which is exactly the divergence the infrastructure reading instantiates.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT show mandatrophy. The founding problem (scaling settlement) is live—the infrastructure solves it continuously. The constraint persists because it works and because its beneficiaries defend it actively. This is not an atrophied mandate wearing theatrical maintenance; it is an active, extractive constraint. The measurement trajectory (rising extractiveness, sustained suppression, modest theater) confirms: the constraint's primary function is not theater; it is gatekeeping and rent collection, which remain functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_authority_source,
    'Who has the legitimate authority to define what counts as ''digital money''—the technical operators of clearing infrastructure, monetary theorists and regulators, consumers and their preferences, or some combination?',
    'Regulatory debate and jurisprudence: when courts, central banks, and legislatures rule on what qualifies as a monetary instrument, they are resolving where definition authority lives. Also: international monetary coordination and whether different jurisdictions adopt different boundaries (if SWIFT+ACH definition becomes universal, infrastructure operators have won; if alternatives claim money status, the boundary is contested).',
    'If regulators reassign definition authority to a different seat (consumers, central banks, theorists), the constraint''s beneficiary structure would shift and the type might reclassify from tangled_rope toward piton (infrastructure becomes one functional layer rather than the gatekeeper) or toward snare (if the gatekeeping is reassigned to a different actor with the same extractive posture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_authority_source, conceptual, 'Whether definition authority over money is rightly held by infrastructure operators or belongs elsewhere.').

omega_variable(
    infrastructure_coordination_inseparability,
    'Is the coordination function (fast, reliable interbank settlement) inseparable from the gatekeeping function (exclusive control of the boundary definition and exclusion of non-integrated institutions)?',
    'Thought experiment: could a different institutional arrangement provide the same settlement coordination while allowing alternative clearing paths and competing definitions of digital money? Regulatory experiment: could an open-access clearing standard (like TCP/IP but for payment settlement) achieve the coordination without the gatekeeping?',
    'If separable, the constraint could be decomposed into two stories: (1) a genuine rope for interbank settlement coordination, and (2) a snare for the gatekeeping that rides on top of it. If inseparable, the gatekeeping is a structural cost of the coordination and the tangled_rope type is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(infrastructure_coordination_inseparability, conceptual, 'Whether settlement coordination requires the gatekeeping that currently accompanies it.').

omega_variable(
    reading_boundary_contest,
    'Is the infrastructure reading''s boundary (money = what infrastructure can move) the canonical boundary, or is it one among three equally plausible readings that happen to be held by different institutional communities?',
    'Historical analysis: which reading won the institutional power struggle (if any)? Which reading persists in regulatory codification? Futures analysis: as new technologies emerge (decentralized ledgers, programmable money, central bank digital currencies), do they realign the readings or do they create fourth boundaries?',
    'If the infrastructure reading is canonical and the others are marginal, the constraint classification stands. If all three readings persist as live alternatives (conceptualization in academic economics, consumer-holdings in regulatory consumer protection, infrastructure in banking operations), the kernel remains genuinely contested and no single reading has won the boundary definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_contest, conceptual, 'Whether this reading is canonical or one among coexisting alternatives.').

omega_variable(
    consumer_identity_lock_mechanism,
    'Is the consumer''s identity_locked exit status (trapped in bank-mediated money) a structural feature of digital infrastructure or an internalized identity fusion where consumers have come to accept bank mediation as inevitable?',
    'Counterfactual: if an alternative clearing system emerged that allowed direct consumer-to-consumer settlement without bank mediation, would consumers flock to it (structural lock) or stay in banks (internalized lock)? Post-exit measurement: if consumers left the banking infrastructure for an alternative, would their sense of monetary autonomy change (internalized suppression vs. structural)?',
    'If structural lock, the suppression_requirement (0.42) is accurate and consumers have no real alternative. If internalized lock (consumers believe bank mediation is inevitable even though technical alternatives exist), the effective suppression is higher than measured and the constraint is more extractive than the metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_identity_lock_mechanism, empirical, 'Whether consumer exit-locking is structural or internalized identity fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 1977).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement_basis(digi_tr_t1967, observed).
narrative_ontology:measurement(digi_tr_t1970, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement_basis(digi_tr_t1970, observed).
narrative_ontology:measurement(digi_tr_t1972, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1972, 0.23).
narrative_ontology:measurement_basis(digi_tr_t1972, observed).
narrative_ontology:measurement(digi_tr_t1975, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1975, 0.26).
narrative_ontology:measurement_basis(digi_tr_t1975, observed).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.28).
narrative_ontology:measurement_basis(digi_tr_t1977, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement_basis(digi_be_t1967, observed).
narrative_ontology:measurement(digi_be_t1970, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement_basis(digi_be_t1970, observed).
narrative_ontology:measurement(digi_be_t1972, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1972, 0.56).
narrative_ontology:measurement_basis(digi_be_t1972, observed).
narrative_ontology:measurement(digi_be_t1975, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1975, 0.62).
narrative_ontology:measurement_basis(digi_be_t1975, observed).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.68).
narrative_ontology:measurement_basis(digi_be_t1977, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.25).
narrative_ontology:measurement_basis(digi_su_t1967, observed).
narrative_ontology:measurement(digi_su_t1970, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1970, 0.32).
narrative_ontology:measurement_basis(digi_su_t1970, observed).
narrative_ontology:measurement(digi_su_t1972, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1972, 0.38).
narrative_ontology:measurement_basis(digi_su_t1972, observed).
narrative_ontology:measurement(digi_su_t1975, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement_basis(digi_su_t1975, observed).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.42).
narrative_ontology:measurement_basis(digi_su_t1977, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__infrastructure_reading, 0.18).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, swift_network_gatekeeping).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, ach_participation_cartel).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the digital_money_emergence_boundary kernel. The infrastructure reading privileges the perspective of clearing operators and defines emergence by technical capacity to move money electronically. It coexists_with the conceptualization_reading (emergence by theoretical possibility) and the consumer_holdings_reading (emergence by direct consumer access). Each reading has a different beneficiary set, different victim set, and different type: infrastructure reading is tangled_rope, conceptualization reading is rope/mountain (depending on whether theory binds), consumer_holdings reading is snare/piton (infrastructure operators extract by withholding consumer access). The three readings form a constraint family—link all three via network.affects_constraints to enable corpus analysis of how definition contests propagate through the monetary system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
