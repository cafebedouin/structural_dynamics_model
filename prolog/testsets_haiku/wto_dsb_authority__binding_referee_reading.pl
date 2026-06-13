% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO Dispute Settlement Body Binding Authority (Binding Referee Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   The World Trade Organization's Dispute Settlement Body issues binding
 *   rulings on member state compliance with WTO covered agreements. Once a
 *   panel or Appellate Body concludes a measure violates treaty obligations,
 *   compliance becomes mandatory: the member state must modify or withdraw
 *   the measure within a specified period. Non-compliance authorizes
 *   retaliatory trade sanctions. This reading instantiates the constraint as
 *   binding adjudication with sovereignty trade-off: member states
 *   surrendered policy discretion in trade-covered domains in exchange for
 *   market access and enforcement mechanisms. The constraint persists because
 *   export-dependent economies benefit from locked-in market access,
 *   multinational trading blocs dominate dispute outcomes and use DSB
 *   authority to constrain competitor policies, and the dispute settlement
 *   apparatus itself has institutional incentives to maintain and expand its
 *   reach. Domestic constituencies displaced by trade rulings (farmers,
 *   workers, environmental advocates) are left without policy recourse. The
 *   claim is tangled rope; the metrics reflect substantial extraction (0.68)
 *   justified by coordination function but enforced through suppression and
 *   retaliation authorization. This is ONE reading of a contested kernel;
 *   sibling readings (advisory coordination and judicial activism) constitute
 *   different structural claims about the same WTO treaty commitment.
 *
 * KEY AGENTS:
 *   - Dispute Settlement Body: Institutional agenda-setter that interprets treaty and issues binding rulings; derives authority and legitimacy from binding enforcement authority.
 *   - Export-dependent economies: Beneficiaries who gain market access and legal recourse against protectionism; benefit from locked-in arrangements they could not secure unilaterally.
 *   - Multinational trading blocs: Beneficiaries with superior legal resources and political leverage; dominate dispute filing and win at high rates; use DSB authority to constrain rival bloc policies.
 *   - Sovereignty-constrained nations: Payers who surrendered policy discretion; face retaliation if they legislate outside the agreement's bounds; trapped exit option.
 *   - Policy-displaced domestic constituencies: Powerless payers with identity-locked exit; lose labor, environmental, and agricultural protections when DSB strikes down domestic legislation.
 *   - Democratic legislatures: Observers whose formal authority is transformed into a costly choice between compliance and retaliation.
 *   - Non-member states: Excluded from governance and DSB jurisdiction but subject to de facto pressure from WTO-member trading blocs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.68).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.72).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO Dispute Settlement Body Binding Authority (Binding Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, '1cd29a66-1bcb-476c-8da5-0ce62bed1c20').
narrative_ontology:cs_kernel_codification('1cd29a66-1bcb-476c-8da5-0ce62bed1c20', fixed_text).
narrative_ontology:cs_authority_grounding('1cd29a66-1bcb-476c-8da5-0ce62bed1c20', extraction).
narrative_ontology:cs_interpretation_layer_present('1cd29a66-1bcb-476c-8da5-0ce62bed1c20').
narrative_ontology:cs_reading_relation('1cd29a66-1bcb-476c-8da5-0ce62bed1c20', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('1cd29a66-1bcb-476c-8da5-0ce62bed1c20', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('1cd29a66-1bcb-476c-8da5-0ce62bed1c20', foundational, member_state_discretion_surrendered).
narrative_ontology:cs_axiom_status(member_state_discretion_surrendered, holdable).
narrative_ontology:cs_axiom_grounding('1cd29a66-1bcb-476c-8da5-0ce62bed1c20', member_state_discretion_surrendered, deontological).
narrative_ontology:cs_axiom('1cd29a66-1bcb-476c-8da5-0ce62bed1c20', foundational, binding_compliance_obligation_from_treaty_text).
narrative_ontology:cs_axiom_status(binding_compliance_obligation_from_treaty_text, holdable).
narrative_ontology:cs_axiom_grounding('1cd29a66-1bcb-476c-8da5-0ce62bed1c20', binding_compliance_obligation_from_treaty_text, empirically_contingent).
narrative_ontology:cs_reference_frame('1cd29a66-1bcb-476c-8da5-0ce62bed1c20', member_state_negotiated_consensus).
narrative_ontology:cs_drift_state('1cd29a66-1bcb-476c-8da5-0ce62bed1c20', contemporary_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1cd29a66-1bcb-476c-8da5-0ce62bed1c20', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, export_dependent_economies).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, multinational_trading_blocs).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, dispute_settlement_apparatus).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, sovereignty_constrained_nations).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, policy_displaced_domestic_constituencies).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, non_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, sovereignty_constrained_nations).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, treaty_supremacy_doctrine).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, retaliation_authorization_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets WTO treaty language and issues binding rulings on member state compliance with covered agreements. Administers the appellate process and determines which disputes are within its jurisdiction. Enforces its own authority by certifying non-compliance and authorizing retaliatory measures. The DSB functions as both interpreter and arbiter of the scope of its own mandate.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).

% Surrendered policy discretion in trade-covered domains by acceding to the WTO agreement. They benefit from market access and predictable trading rules but face binding compliance obligations they cannot unilaterally revise. Non-compliance triggers retaliation: authorized tariffs, suspension of concessions, or other trade restrictions imposed by trading partners. Policy choices in labor standards, environmental regulation, intellectual property, and domestic agricultural support are now subject to DSB review and revision.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, sovereignty_constrained_nations, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, sovereignty_constrained_nations, beneficiary).

% Gain market access and binding legal recourse against competitor protectionism. Can file disputes and rely on DSB enforcement to open foreign markets and prevent policy backsliding by trading partners. The retaliation authorization mechanism protects their market-access commitments. They experience the binding DSB authority as a coordination solution that locks in access that unilateral negotiations could not secure. For these actors, exit (leaving the WTO) carries catastrophic cost — they depend on the agreement's enforcement to hold their market share.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, export_dependent_economies, beneficiary,
    organized, generational, mobile, global).

% Dominate dispute filing and win disputes at high rates due to superior legal resources and political leverage. The DSB's binding authority allows them to lock in favorable market conditions and constrain rival bloc policies through disputes. They can credibly threaten retaliation, which makes their policy preferences sticky across the system. Exit option: they can invoke emergency safeguards or modify their commitments under Article XXI; they are not truly trapped, though retaliation costs are substantial.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, multinational_trading_blocs, beneficiary,
    institutional, generational, arbitrage, global).

% Lose policy protection when DSB panels strike down domestic labor laws, environmental regulations, or agricultural subsidies as WTO-inconsistent. Farmers, manufacturers, and labor unions in democracies cannot exit the constraint — their country's government negotiated the treaty they now face as binding law. They can lobby domestically but lack formal standing in DSB proceedings. The identity lock is profound: their economic security is fused to a sovereignty choice they did not make and cannot reverse unilaterally.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, policy_displaced_domestic_constituencies, payer,
    powerless, biographical, identity_locked, local).

% Administers the panel/appellate system, staffs legal infrastructure, interprets precedent, and certifies compliance. Expands its institutional reach and influence as dispute volume increases and as disputes involve more complex interpretive questions. Panel members and Appellate Body judges derive professional authority and career standing from their role in WTO adjudication. The apparatus benefits from the binding authority framework and has incentives to interpret broadly to maintain relevance and justify institutional maintenance.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, dispute_settlement_apparatus, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, dispute_settlement_apparatus, beneficiary).

% Are structurally excluded from WTO governance and DSB jurisdiction but are subject to de facto pressure from WTO-member trading blocs that use DSB rulings to reshape global supply chains and investment rules. They have no formal recourse; they can be harmed by DSB rulings that affect third-party trade flows or regulatory standards, but they have no seat at the table. Their exclusion is maintained by the consensus rule for new membership and the requirement that accession entails full acceptance of WTO law.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, non_member_states, excluded,
    moderate, generational, trapped, global).

% Formally retain authority to legislate but face binding review by a supranational body with power to authorize retaliation if legislation is judged WTO-inconsistent. They can choose to incur retaliation costs, but that choice is now expensive and visible. The DSB's authority transforms legislative discretion from a binary right into a cost-benefit calculation constrained by treaty enforcement. Democratic mandates for labor protection, environmental standards, or agricultural support can be overridden by binding panels staffed by appointed judges, not elected representatives.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, democratic_legislatures, observer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__binding_referee_reading, multinational_trading_blocs).
narrative_ontology:fixing_cost_class(wto_dsb_authority__binding_referee_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates bilateral dispute resolution and opportunistic retaliation: instead of unilateral responses to perceived violations, a centralized neutral panel interprets treaty language and authorizes proportional remedies. Solves a mutual credibility problem: smaller states can challenge larger states' violations without fear of unilateral retaliation, and larger states gain predictable recourse rather than ad hoc power politics.
% TRANSFER_FUNCTION: Moves policy discretion from elected legislatures to appointed panels and arbiters, and reallocates market access from protectionists to exporters. Authorizes retaliatory tariffs as enforcement, which transfers wealth from consumers (who pay higher prices) to governments (which collect tariff revenue and gain negotiating leverage).
% ABSENT_VOICES: Domestic constituencies harmed by trade policy (workers displaced by import competition, farmers subject to subsidy rollbacks, environmental advocates facing weakened standards) have no formal voice in DSB proceedings. Non-member states affected by DSB rulings are entirely excluded. Small and medium-sized nations often lack legal resources to participate effectively in disputes, rendering them silent in practice despite formal membership.
% DISAPPEARANCE_RATIONALE: If the binding DSB authority vanished overnight, member states would revert to bilateral negotiation and unilateral retaliation; trade agreements would be renegotiated annually or abandoned; export-dependent economies would face higher uncertainty and likely reduced market access; multinational trading blocs would use political and military leverage rather than legal enforcement; protectionism would spike as domestic constituencies regained policy protection; smaller states would lose legal recourse against larger economies.
% FOUNDING_PROBLEM: Mid-20th century GATT bilateral dispute resolution was slow, subject to power politics, and allowed stronger nations to block complaint procedures. The founding WTO treaty incorporated a binding panel system to solve credible commitment: member states needed a mechanism that would bind even powerful partners and constrain opportunistic policy changes.
% FOUNDING_PROBLEM_CORROBORATION: WTO institutional histories and trade economists attest the founding problem was genuine: pre-WTO bilateral disputes were frequently unresolved and power-asymmetric. Contemporary critics — environmental lawyers, labor advocates, and democratic theory scholars outside the benefiting bloc — attest the founding problem has been eclipsed: the real problem now is that binding authority enables overconstrained governance and unaccountable interpretation. Member states themselves are divided: export-dependent nations attest the system works; domestic-protection constituencies attest it strips legitimate policy capacity.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.42 to 0.68) as dispute volume increases and DSB authority expands into new interpretive domains (intellectual property, services, regulatory standards). The founding coordination problem—bilateral disputes subject to power politics—is genuinely solved by binding panels; this is the coordination function. However, the same mechanism that solves bilateral dispute escalation creates asymmetric extraction: larger economies and multinational blocs exploit superior legal capacity to lock in favorable market conditions. Suppression rises (0.48 to 0.72) as the retaliation authorization mechanism hardens—member states face increasingly credible threats of retaliatory tariffs if they deviate from DSB rulings. Theater (0.12 to 0.28) is moderate-low: panels do legitimate work interpreting treaty language, but growing shares of interpretive activity defend the expansion of DSB authority itself rather than clarifying treaty meaning. The three-metric time grid is aligned: every metric is authored at every examined time point, enabling temporal analysis of lifecycle drift.
 *
 * PERSPECTIVAL GAP:
 *   From the export-dependent economy seat, the DSB's binding authority is coordination protection—the only mechanism that credibly locks in market access against protectionist backsliding. From the sovereignty-constrained nation seat, especially when that nation faces a DSB ruling that strikes down democratically-mandated labor or environmental protections, the same authority is enforced extraction: a supranational body overriding legitimate domestic governance. From the multinational bloc seat with legal resources to dominate disputes, the binding authority is a tool of structural advantage. From the domestic constituency seat (farmer, worker, environmental advocate), the mechanism is pure suppression—policy protection they democratically demanded has been stripped by appointed judges with no accountability to them. The engine computes directionality per seat from the structural data; these divergent experiences follow from the asymmetric power, exit options, and beneficiary/victim positioning.
 *
 * DIRECTIONALITY LOGIC:
 *   Export-dependent economies (organized power, mobile exit via market strategies, beneficiaries) sit near d=0.2: they benefit from locked-in access and retain arbitrage options within the system. Multinational trading blocs (institutional power, arbitrage exit, beneficiaries) sit near d=0.15: they dominate outcomes and can navigate DSB disputes strategically. Sovereignty-constrained nations (moderate power, constrained exit, payers) sit near d=0.6: they bear the cost of surrendered discretion and face retaliation if they deviate, but they also benefit from market access (hence not purely targeted). Policy-displaced constituencies (powerless, identity-locked exit, payers) sit at d=0.85: they bear costs they did not choose, have no exit, and gain nothing visible in return. The DSB itself (institutional power, analytical exit, agenda-setter) sits near d=0.5 for computational purposes but is the constraint's beneficiary in terms of institutional maintenance and mandate expansion. Directionality_overrides are not needed: the structural derivation from beneficiary/victim + exit captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by maintaining the founding coordination function: the DSB's binding authority genuinely solves the pre-WTO problem of unilateral dispute escalation and power-asymmetric resolution. However, the constraint exhibits extraction accumulation: extractiveness rises over 30 years as the DSB's interpretive scope expands into intellectual property, services, regulatory harmonization, and other domains beyond the original GATT trade-goods mandate. The constraint is not a zombie (mandatrophy-resolved) because the core coordination function persists; it is a case of coordination function providing cover for extraction expansion. This is precisely the tangled rope structure: real coordination function + asymmetric extraction + active enforcement (retaliation authorization) that must be actively maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_authority_legitimacy,
    'Does the DSB''s binding authority rest on explicit treaty language that member states knowingly accepted, or on interpretive expansion beyond what treaty signers understood they were conceding?',
    'Genealogical analysis of treaty negotiation records, statements of intent at WTO founding, accession negotiations for later-joining members, and comparison between original GATT dispute procedures and WTO innovations. Contemporary member state testimony about what they believed they were accepting.',
    'If binding authority was explicit and knowingly accepted, the constraint is legitimate tangled rope: real coordination function, explicit extraction trade-off. If binding authority expanded via interpretation beyond what was understood at signing, the constraint approaches judicial activism reading and reclassification as snare becomes possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_authority_legitimacy, empirical, 'Whether binding DSB authority was the agreed-upon trade-off or interpretive expansion beyond negotiated mandate.').

omega_variable(
    sovereignty_exit_asymmetry,
    'Do all WTO member states face equivalent exit costs from the DSB binding authority, or is exit predominantly costly for export-dependent economies while less costly (or even beneficial) for domestically-protected economies?',
    'Comparative analysis of exit costs across member states by development level, export dependence, and trading-bloc position. Economic modeling of what each member state would gain/lose from unilateral WTO withdrawal.',
    'If exit costs are symmetric, the constraint approaches pure coordination rope. If exit is asymmetrically costly (locked in the export-dependent nations, optional for the protected economies), the constraint becomes more snare-like for the trapped parties even as it remains coordinative for the beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_exit_asymmetry, empirical, 'Whether exit costs from DSB binding authority are symmetric across member states or lock in subordinate economies.').

omega_variable(
    domestic_constituency_suppression_mechanism,
    'Is the suppression of policy-displaced domestic constituencies (farmers, workers, environmental advocates) structural (economic dependency on trade, lack of legal standing in DSB) or internalized (these constituencies have accepted the trade governance frame and see DSB authority as legitimate)?',
    'Post-exit suppression trajectory: if domestic movements successfully overturn DSB rulings or legislate contrary policies after high-profile DSB strikes, assess whether suppression persists or collapses. Track whether domestic constituencies frame DSB rulings as legitimate or as illegitimate imposition.',
    'If structural suppression, the constraint''s measured suppression (0.72) understates effective suppression for powerless agents. If internalized, the constraint persists via acceptance rather than coercion. If mixed, the suppression_requirement measurements need recalibration by seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_constituency_suppression_mechanism, empirical, 'Whether suppression of policy-displaced constituencies is structural or internalized.').

omega_variable(
    reading_contest_empirical_arbiter,
    'Is the binding_referee_reading or the advisory_coordination_reading the better fit to actual DSB practice and member state behavior?',
    'Empirical inventory of DSB outcomes: what proportion of cases result in (a) binding compliance with modified policy, vs. (b) negotiated side settlements, vs. (c) non-compliance with authorized retaliation? Track member state statements about whether they view DSB rulings as binding or advisory. Examine compliance rates and dispute patterns.',
    'If compliance rates are high and member states treat rulings as binding, this reading''s framing holds. If compliance is rare or negotiated around, the advisory_coordination_reading may better describe the system. Different readings produce different χ (effective extraction) computations: binding produces higher χ, advisory produces lower χ.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_empirical_arbiter, empirical, 'Whether the DSB actually functions as binding authority or as advisory coordination.').

omega_variable(
    retaliation_authorization_extraction_mechanism,
    'Does the retaliation authorization mechanism primarily enforce coordination (credible commitment to treaty compliance) or primarily extract through threat (coercion that shapes policy beyond treaty text)?',
    'Analyze threat patterns: which member states authorize retaliation threats, which pay the threats, whether retaliation is proportional to actual treaty violation or operates as political leverage beyond the dispute. Track whether retaliation threats are used to enforce unwritten expectations or treaty language itself.',
    'If retaliation is primarily coordination (enforcing agreed terms), suppression is justified cost of the coordination. If retaliation is primarily leverage (enforcing unwritten expectations or power relationships), the constraint becomes more snare-like and suppression becomes coercion rather than coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_authorization_extraction_mechanism, empirical, 'Whether retaliation authorization enforces coordination or operates as political extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__binding_referee_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement_basis(wto__tr_t1995, observed).
narrative_ontology:measurement(wto__tr_t2002, wto_dsb_authority__binding_referee_reading, theater_ratio, 2002, 0.16).
narrative_ontology:measurement_basis(wto__tr_t2002, observed).
narrative_ontology:measurement(wto__tr_t2009, wto_dsb_authority__binding_referee_reading, theater_ratio, 2009, 0.2).
narrative_ontology:measurement_basis(wto__tr_t2009, observed).
narrative_ontology:measurement(wto__tr_t2016, wto_dsb_authority__binding_referee_reading, theater_ratio, 2016, 0.25).
narrative_ontology:measurement_basis(wto__tr_t2016, observed).
narrative_ontology:measurement(wto__tr_t2021, wto_dsb_authority__binding_referee_reading, theater_ratio, 2021, 0.28).
narrative_ontology:measurement_basis(wto__tr_t2021, observed).
narrative_ontology:measurement(wto__tr_t2025, wto_dsb_authority__binding_referee_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(wto__tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__binding_referee_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement_basis(wto__be_t1995, observed).
narrative_ontology:measurement(wto__be_t2002, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2002, 0.51).
narrative_ontology:measurement_basis(wto__be_t2002, observed).
narrative_ontology:measurement(wto__be_t2009, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2009, 0.59).
narrative_ontology:measurement_basis(wto__be_t2009, observed).
narrative_ontology:measurement(wto__be_t2016, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2016, 0.64).
narrative_ontology:measurement_basis(wto__be_t2016, observed).
narrative_ontology:measurement(wto__be_t2021, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2021, 0.68).
narrative_ontology:measurement_basis(wto__be_t2021, observed).
narrative_ontology:measurement(wto__be_t2025, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(wto__be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__binding_referee_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement_basis(wto__su_t1995, observed).
narrative_ontology:measurement(wto__su_t2002, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2002, 0.56).
narrative_ontology:measurement_basis(wto__su_t2002, observed).
narrative_ontology:measurement(wto__su_t2009, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2009, 0.64).
narrative_ontology:measurement_basis(wto__su_t2009, observed).
narrative_ontology:measurement(wto__su_t2016, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement_basis(wto__su_t2016, observed).
narrative_ontology:measurement(wto__su_t2021, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2021, 0.72).
narrative_ontology:measurement_basis(wto__su_t2021, observed).
narrative_ontology:measurement(wto__su_t2025, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(wto__su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__binding_referee_reading, 0.14).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, national_trade_policy_discretion).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, domestic_regulatory_protection).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, international_investment_arbitration).

% DUAL FORMULATION NOTE:
% The WTO DSB authority decomposes into three structurally distinct constraint stories sharing the same kernel (the treaty commitment to centralized dispute resolution) but instantiating different readings of what that commitment entails. The binding_referee_reading claims member states surrendered policy discretion; the advisory_coordination_reading claims member states retain ultimate authority; the judicial_activism_reading claims the DSB exceeded its mandate. Each reading produces different ε values and different stakeholder directionalities. The three stories are linked by their common kernel and their mutual contest over the nature of that kernel. This story (binding_referee_reading) posits the highest extractiveness (0.68) because it assumes binding compliance obligations and retaliation authorization. The advisory_coordination_reading posits lower extractiveness because it assumes member state discretion persists. The judicial_activism_reading posits different victim sets (it identifies judges and activist interpreters as extractors rather than enforcers). All three stories are necessary to model the actual institutional contest within the WTO.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
