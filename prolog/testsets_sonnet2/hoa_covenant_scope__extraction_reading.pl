% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__extraction_reading, []).

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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant as Fine-Proliferation Revenue and Board Power Mechanism
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This story instantiates the extraction reading of the contested HOA
 *   covenant-scope kernel: the covenant text is the same document a
 *   coordination reading and a behavioral-control reading would also
 *   describe, but this reading holds that the covenant's operative function
 *   has become revenue generation and board power consolidation via fine
 *   proliferation, expedited lien processes, and attorney-fee extraction
 *   under fee-shifting clauses. The referent for extractiveness is the
 *   standing enforcement arrangement as this reading sees it — a
 *   fine-and-lien apparatus that has grown well past any plausible
 *   coordination cost — not the reformed, coordination-only covenant this
 *   reading would prefer. Two other readings of the same kernel
 *   (coordination_reading, behavioral_control_reading) are separate
 *   constraint files; they are not folded into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.64).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.71).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant as Fine-Proliferation Revenue and Board Power Mechanism").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, '21bc92e4-487a-4dee-b69b-99f69040e9de').
narrative_ontology:cs_kernel_codification('21bc92e4-487a-4dee-b69b-99f69040e9de', formalized).
narrative_ontology:cs_authority_grounding('21bc92e4-487a-4dee-b69b-99f69040e9de', extraction).
narrative_ontology:cs_interpretation_layer_present('21bc92e4-487a-4dee-b69b-99f69040e9de').
narrative_ontology:cs_reading_relation('21bc92e4-487a-4dee-b69b-99f69040e9de', hoa_covenant_scope__coordination_reading, influences).
narrative_ontology:cs_reading_relation('21bc92e4-487a-4dee-b69b-99f69040e9de', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('21bc92e4-487a-4dee-b69b-99f69040e9de', foundational, enforcement_revenue_decoupled_from_cost).
narrative_ontology:cs_axiom_status(enforcement_revenue_decoupled_from_cost, holdable).
narrative_ontology:cs_axiom_grounding('21bc92e4-487a-4dee-b69b-99f69040e9de', enforcement_revenue_decoupled_from_cost, empirically_contingent).
narrative_ontology:cs_axiom('21bc92e4-487a-4dee-b69b-99f69040e9de', foundational, fee_shifting_disproportionate_to_violation_severity).
narrative_ontology:cs_axiom_status(fee_shifting_disproportionate_to_violation_severity, holdable).
narrative_ontology:cs_axiom_grounding('21bc92e4-487a-4dee-b69b-99f69040e9de', fee_shifting_disproportionate_to_violation_severity, empirically_contingent).
narrative_ontology:cs_reference_frame('21bc92e4-487a-4dee-b69b-99f69040e9de', cost_recovery_enforcement_baseline).
narrative_ontology:cs_drift_state('21bc92e4-487a-4dee-b69b-99f69040e9de', contemporary_fee_shifting_regime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('21bc92e4-487a-4dee-b69b-99f69040e9de', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, collections_legal_counsel).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, well_resourced_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, well_resourced_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and amends the covenant's rules-enforcement schedule, votes to hire the management firm and legal counsel, and controls which violations get cited. Board seats often rotate among a small clique with low turnout elections; individual board members are frequently shielded from personal liability while directing fine schedules and lien referrals that generate revenue and cement their control of the association's discretionary budget.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, board_members, agenda_setter,
    organized, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, board_members, beneficiary).

% Contracted by the board to administer inspections, issue violation notices, and process fines; frequently paid a percentage of collected fines and administrative fees on top of a flat management fee, giving a direct financial incentive to maximize citation volume rather than resolve underlying issues. Can be replaced by the board but often shapes the covenant amendments it will later enforce.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    institutional, generational, arbitrage, regional).

% Retained to send demand letters, file liens, and pursue foreclosure on delinquent fine balances; recovers attorney's fees from the homeowner under the covenant's fee-shifting clause regardless of the underlying fine's size, so a fifty-dollar violation can generate thousands in recoverable legal costs. Has no incentive to resolve disputes informally since litigation and lien processing are the revenue event.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, collections_legal_counsel, beneficiary,
    institutional, biographical, arbitrage, regional).

% Own units subject to the covenant and cannot exit without selling, which the accumulating lien itself can block or devalue. A missed payment, disputed fine, or unresolved aesthetic citation compounds through late fees, attorney fees, and interest until the total owed can exceed the original violation by an order of magnitude, occasionally leading to foreclosure over sums far smaller than the home's value.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, biographical, trapped, local).

% Do not sit on the board and cannot vote, but bear the covenant's costs indirectly as landlords pass through HOA fine exposure, special assessments, and rising dues in rent. Can move at lease end but have no voice in the rules generating the cost, and short-term leases mean the cost repeats with each unit turnover.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, mobile, local).

% Can absorb occasional fines without financial distress and often serve on or have informal access to the board, giving them influence over which rules get written and which violations get selectively enforced against neighbors. Pay dues and occasional fines but do not face the compounding lien spiral that threatens vulnerable owners.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, well_resourced_homeowners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, well_resourced_homeowners, payer).

% Some states have begun requiring dispute-resolution steps before liens or capping attorney-fee recovery on small violations, but enforcement of these caps is uneven and most HOA governance remains largely self-regulated with minimal external audit of fine schedules or fee arrangements.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, state_real_estate_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__extraction_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, coordinates shared costs (landscaping, insurance, common-area repair) and internalizes externalities between adjacent owners — a genuine collective-action problem that motivated the original covenant structure.
% TRANSFER_FUNCTION: Moves money from cited homeowners (and, through rent pass-through, from renters) to the management firm's fine-collection share, to collections counsel's recoverable attorney's fees, and to the discretionary budget the board controls — well beyond what shared-infrastructure coordination requires.
% ABSENT_VOICES: Renters have no vote despite bearing pass-through costs. Homeowners facing foreclosure over compounding fines rarely have the resources to contest a lien in court and are absent from the board meetings where fine schedules are set and amended.
% DISAPPEARANCE_RATIONALE: If covenant enforcement vanished, the management firm's per-fine revenue stream and collections counsel's fee-shifting pipeline would disappear immediately; board members would lose the primary discretionary lever they currently use to reward allies and punish dissenters; genuine shared-infrastructure costs would need a different funding mechanism, but the fine-and-lien apparatus itself is not what funds landscaping or insurance.
% FOUNDING_PROBLEM: Original covenants were adopted to solve real coordination problems: shared maintenance costs, insurance pooling, and preventing free-riding on common-area upkeep.
% FOUNDING_PROBLEM_CORROBORATION: Board members and management firms attest the fine schedule remains necessary for property-value protection and compliance. Independent evidence — state regulator complaint data, foreclosure-over-small-lien case reporting, and homeowner litigation records — corroborates from outside the benefiting parties that fine proliferation and fee-shifting substantially exceed what the original coordination problem requires, and several state legislatures have moved to cap fee recovery in response.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__extraction_reading, 0.64, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hoa_covenant_scope__extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.38 to 0.64) tracking the compounding pattern the reading identifies: initial fine schedules that were plausibly cost-recovery oriented drift toward escalating late fees, mandatory attorney referral at low delinquency thresholds, and lien filing timelines shortened to accelerate fee-generating events. Suppression is high and rising (0.45 to 0.71) because homeowners are structurally trapped — a lien can block sale, and disputing a fine risks compounding attorney fees regardless of the fine's merit, which chills contestation independent of any legal remedy's formal availability. Theater ratio is moderate and rising (0.42 at end) — some genuine property-standards enforcement occurs, but a growing share of activity is procedural: notice cycles and hearing rituals that exist mainly to satisfy due-process minimums before the fee-generating lien step, not to resolve the underlying issue.
 *
 * PERSPECTIVAL GAP:
 *   From the board's seat, aggressive fine enforcement is protecting property values and covenant integrity through legitimate governance discretion. From the trapped homeowner's seat, the identical fine schedule and lien timeline is a mechanism that converts a minor rules infraction into a foreclosure-scale liability via compounding fees the homeowner has no practical way to contest before the fees exceed the original violation. The engine computes these as different seat classifications from the same structural data; this reading does not adjudicate between them but authors the structural facts (fee-shifting, lien acceleration, trapped exit) that produce the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Board members, the management firm, and collections counsel are declared beneficiaries because the fine-and-fee structure is the direct source of their discretionary control and revenue — this reading places them near the beneficiary end of directionality. Financially vulnerable homeowners are declared victims: trapped exit options (a lien can block or devalue sale) push their effective directionality toward full target regardless of nominal power parity with well-resourced owners. Renters bear costs through pass-through without any board vote, which this reading treats as extraction without even nominal coordination input on their part — their mobile exit (moving at lease end) does not offset the recurring cost imposed on each new tenancy.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is deliberate: this reading does not deny that shared-infrastructure coordination is a real function the covenant could serve (avoiding a pure-snare mislabel that would erase the genuine externality problem), but it holds that the SAME structure now also runs an asymmetric extraction channel through fine proliferation and fee-shifting that the coordination function does not require. Collapsing this into either 'pure coordination, ignore the fines' or 'pure extraction, no coordination ever existed' would each be a mandatrophy failure — the classification requires both a real beneficiary/coordination structure and a real victim/enforcement structure to be true simultaneously, which is exactly what tangled_rope demands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_severability,
    'Is the fine-and-fee enforcement apparatus severable from the covenant''s genuine shared-infrastructure coordination function, or are they structurally fused such that any enforcement mechanism capable of coordinating maintenance costs necessarily also enables this extraction pattern?',
    'Comparative study of HOAs that cap attorney-fee recovery and mandate pre-lien mediation against those with uncapped fee-shifting: if coordination outcomes (maintenance funding, dues collection) hold steady while extraction indicators (average fine-to-original-violation ratio, foreclosure-over-small-lien incidence) drop, the functions are severable.',
    'If severable, the extraction component is purely additive rent-seeking layered onto a legitimate coordination core, supporting reform via caps and mandated dispute resolution rather than covenant abolition. If fused, reform would require restructuring the coordination mechanism itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_severability, empirical, 'Whether extraction and coordination functions can be structurally separated by policy intervention.').

omega_variable(
    kernel_framing_choice_extraction_vs_coordination,
    'Given that the same covenant text supports a coordination reading, a behavioral-control reading, and this extraction reading, what evidence justifies foregrounding fine proliferation and fee-shifting as the operative function rather than treating them as enforcement overhead on a genuinely coordinating instrument?',
    'This reading was selected because the fee-shifting and lien-acceleration pattern is documented as growing over time (rising base_extractiveness series) independent of any corresponding growth in actual shared-infrastructure costs, and because state regulatory responses (fee caps, mandated mediation) specifically target the fine/fee mechanism rather than the underlying maintenance-funding function — signaling that external observers also locate the problem there.',
    'Under the coordination_reading, the same covenant text would carry low ε and classify as rope or scaffold; under this reading it classifies as tangled_rope with high ε. The classification is reading-indexed, not a single fact about the covenant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_extraction_vs_coordination, conceptual, 'Alternative framing documentation: why this reading was selected as the extraction-foregrounding account of the kernel.').

omega_variable(
    selective_enforcement_evidentiary_basis,
    'Is selective enforcement (citing dissenting or less-connected owners more aggressively than allied or well-resourced owners) a documented empirical pattern in this specific association, or an inference from structural incentive alone?',
    'Audit of citation records against board voting records and social proximity to board members; comparison of fine dismissal rates across owner categories.',
    'If selective enforcement is documented, it strengthens the board-power-consolidation half of this reading considerably; if fine issuance is in fact uniform, this reading''s power-consolidation claim would need to rest solely on the revenue-extraction mechanism rather than discretionary targeting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selective_enforcement_evidentiary_basis, empirical, 'Whether selective/discretionary enforcement is empirically substantiated or inferred from incentive structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hoa__tr_t4, hoa_covenant_scope__extraction_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(hoa__tr_t8, hoa_covenant_scope__extraction_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__extraction_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(hoa__tr_t16, hoa_covenant_scope__extraction_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__extraction_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hoa__be_t4, hoa_covenant_scope__extraction_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__extraction_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__extraction_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__extraction_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__extraction_reading, base_extractiveness, 24, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(hoa__su_t4, hoa_covenant_scope__extraction_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(hoa__su_t8, hoa_covenant_scope__extraction_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(hoa__su_t12, hoa_covenant_scope__extraction_reading, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(hoa__su_t16, hoa_covenant_scope__extraction_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__extraction_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(hoa__su_t24, hoa_covenant_scope__extraction_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__extraction_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the hoa_covenant_scope kernel decomposed per the ε-invariance principle: coordination_reading (low ε, rope/scaffold, genuine externality-internalization function), behavioral_control_reading (moderate ε, aesthetic/behavioral conformity enforcement as value-maximization), and this extraction_reading (high ε 0.55-0.70, tangled_rope, fine-proliferation and fee-shifting as revenue/power mechanism). Each reading authors its own ε, beneficiary/victim structure, and classification from the same underlying covenant text; they are linked here rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
