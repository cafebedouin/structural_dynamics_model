% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member State Welfare-Bounded Free Movement (Sovereignty Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The member-sovereignty reading of the federation-membership kernel
 *   asserts that member states retain legitimate authority to bound free
 *   movement rights by reference to national welfare-state capacity and
 *   labor-market protection. This reading emphasizes that EU citizens do not
 *   form a single undifferentiated polity; rather, member states are separate
 *   welfare communities whose fiscal and employment stability depends on
 *   calibrating migration inflows to labor-market demand and welfare
 *   capacity. Supranational integration authority (ECJ expansive
 *   jurisprudence) is treated as illegitimate overreach. The constraint
 *   operates as tangled rope: it coordinates receiving-state welfare
 *   protection and indigenous labor-market security while extracting
 *   opportunity cost from migrants and sending-state populations. The
 *   author's seat is this reading's own framing; the engine computes how each
 *   stakeholder seat perceives the constraint differently.
 *
 * KEY AGENTS:
 *   - Receiving-state welfare administrations (institutional, agenda-setter + beneficiary): set and enforce the restriction rules; protect fiscal boundaries; collect the rents of constrained competition.
 *   - Indigenous labor constituencies (organized, beneficiary): benefit from wage/employment protection and from the reading's framing that prioritizes their welfare-security.
 *   - Economically inactive migrants (powerless, payer): face exclusion or welfare-access restriction; bear extraction most visibly.
 *   - Lower-skilled sending-state workers (moderate, payer): face constrained labor-market access; cannot arbitrage wage differentials; remain trapped in lower-wage markets.
 *   - High-skilled sending-state populations (powerful, payer): experience brain drain and reduced legitimate exit, intensifying fiscal/demographic strain on sending states.
 *   - Supranational integration authorities (institutional, excluded): would argue free movement is constitutive of citizenship; their voice is excluded by this reading.
 *   - Sending-state governments (institutional, observer): must account for emigration pressure and remittance loss while constrained by receiving-state restrictions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.71).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member State Welfare-Bounded Free Movement (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, 'e4f17829-5931-40c9-8562-e3588d8ee0a2').
narrative_ontology:cs_kernel_codification('e4f17829-5931-40c9-8562-e3588d8ee0a2', fixed_text).
narrative_ontology:cs_authority_grounding('e4f17829-5931-40c9-8562-e3588d8ee0a2', extraction).
narrative_ontology:cs_interpretation_layer_present('e4f17829-5931-40c9-8562-e3588d8ee0a2').
narrative_ontology:cs_reading_relation('e4f17829-5931-40c9-8562-e3588d8ee0a2', federation_membership_kernel__integration_reading, forecloses).
narrative_ontology:cs_reading_relation('e4f17829-5931-40c9-8562-e3588d8ee0a2', federation_membership_kernel__welfare_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('e4f17829-5931-40c9-8562-e3588d8ee0a2', foundational, member_state_welfare_sovereignty).
narrative_ontology:cs_axiom_status(member_state_welfare_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('e4f17829-5931-40c9-8562-e3588d8ee0a2', member_state_welfare_sovereignty, deontological).
narrative_ontology:cs_axiom('e4f17829-5931-40c9-8562-e3588d8ee0a2', secondary, labor_market_protection_prerogative).
narrative_ontology:cs_axiom_status(labor_market_protection_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('e4f17829-5931-40c9-8562-e3588d8ee0a2', labor_market_protection_prerogative, empirically_contingent).
narrative_ontology:cs_reference_frame('e4f17829-5931-40c9-8562-e3588d8ee0a2', member_state_bounded_mobility_framework).
narrative_ontology:cs_drift_state('e4f17829-5931-40c9-8562-e3588d8ee0a2', post_2015_migration_crisis_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e4f17829-5931-40c9-8562-e3588d8ee0a2', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_administrations).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, indigenous_labor_constituency).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, lower_skilled_sending_state_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, high_skilled_sending_state_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, high_skilled_sending_state_populations).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, member_state_welfare_sovereignty).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, labor_market_protection_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer national welfare systems (healthcare, pensions, unemployment insurance, housing support) funded by tax bases of current residents and citizens. They justify restrictions on economically inactive migrants as protecting fiscal sustainability and the intergenerational social contract. They define residency requirements, prior-contribution thresholds, and eligibility rules. They benefit directly from constrained inflows reducing welfare claims and from protected priority for citizen and resident claimants.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_administrations, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_administrations, beneficiary).

% Native-born and long-established resident workers protected by restrictions on competing labor inflows. They benefit from lower job competition, potentially higher wage floors, and preference in hiring. Their political coalition (labor unions, worker parties, community organizations) sustains electoral support for the sovereignty reading and enforcement of mobility restrictions.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, indigenous_labor_constituency, beneficiary,
    organized, biographical, constrained, national).

% Non-working EU citizens including retirees, students on limited budgets, caregivers, and unemployed persons. They face direct exclusion from receiving states or restrictions on welfare access even if present. They have no labor-market arbitrage option (they are not seeking work) and limited recourse to appeal restrictions. They are the most visible victims: barred from claiming support they would access under an integration reading, with no way to satisfy labor-market productivity tests.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, national).

% Lower-skilled workers in lower-income EU member states (Southern and Eastern Europe) face restricted labor-market access in wealthier receiving states. They cannot arbitrage wage differentials as freely; nominal wages and job quality improve less through mobility than in an integration reading. They remain trapped in lower-wage, higher-unemployment labor markets. The restriction hits hardest on this group because their wage gap to receiving states is largest and their alternative options are most limited.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, lower_skilled_sending_state_workers, payer,
    moderate, biographical, constrained, national).

% Higher-educated and entrepreneurial workers from sending states experience brain drain and reduced legitimate migration opportunities within the EU. However, they retain arbitrage options (third-country work visas, international professional mobility, global labor markets) that lower-skilled cohorts lack. The bounded reading extracts from sending states through demographic loss (lost talent, lost future taxpayers) and from high-skilled individuals through reduced primary-choice options, but the extraction is not total because they retain global escape routes. From the sending-state perspective, this group experiences opportunity cost; from the receiving-state perspective, they are partial beneficiaries of selective acceptance.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, high_skilled_sending_state_populations, payer,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, high_skilled_sending_state_populations, beneficiary).

% The European Court of Justice, EU Commission, and supranational integration institutions are structurally excluded from voice in the member-sovereignty reading. Under this reading, ECJ expansive free-movement jurisprudence is treated as illegitimate overreach and member-state vetoes constrain supranational authority. The excluded voice is the integration argument that free movement is constitutive of EU citizenship and that welfare states must coordinate rather than exclude. This exclusion is deliberate—it is the reading's core claim against the integration reading.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, supranational_integration_authority, excluded,
    institutional, generational, trapped, global).

% Governments of sending states must account for emigration pressure, remittance dependence for household incomes, and fiscal costs of brain drain (lost tax base, aging population, higher per-capita welfare costs). The bounded reading protects their workforce from permanent exit but also pressures domestic fiscal stability and political legitimacy (citizens blame them for restricted opportunity). They have limited recourse: they cannot unilaterally relax receiving-state restrictions and face political pressure from their own populations to restore exit options.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_governments, observer,
    institutional, generational, constrained, national).

% National labor ministries, social-partnership bodies (employer/union/government councils), and employment authorities enforce the sovereignty reading by administering labor-market tests, residency-duration requirements, sectoral exclusions, and preferential hiring for citizens. They coordinate with welfare administrations to align migration policy with labor-market and fiscal protection objectives. They exercise gatekeeping power over who enters the labor market.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_labor_market_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_administrations).
narrative_ontology:fixing_cost_class(federation_membership_kernel__member_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates receiving-state welfare-system fiscal sustainability with domestic labor-market stability and employment security. By bounding free movement, receiving states calibrate migrant inflows to labor-market demand, prevent welfare access claims from outpacing contribution, and preserve the social contract binding tax contributors to welfare recipients. For sending states, the constraint provides clarity about exit constraints and allows domestic labor-market planning.
% TRANSFER_FUNCTION: Transfers opportunity cost from receiving-state welfare administrations and indigenous labor constituencies (who benefit from constrained competition) to economically inactive migrants and sending-state workers (who lose access or face reduced wage arbitrage). The constraint moves fiscal and employment security from the mobile/vulnerable populations to the settled/organized populations.
% ABSENT_VOICES: Supranational integration authorities (European Court of Justice, EU Commission) and free-movement advocacy organizations are structurally excluded by this reading. They would argue that free movement is constitutive of EU citizenship, that welfare coordination rather than exclusion is the legitimate response, and that the social contract must be renegotiated at the EU level, not defended at the member-state level. Migrants' own political organizations and civil-society advocacy for free movement are under-resourced in policymaking; their voice enters through court litigation and NGO testimony but carries less weight than member-state governments' sovereign prerogatives.
% DISAPPEARANCE_RATIONALE: If the member-sovereignty reading and its enforcement mechanisms disappeared overnight, receiving states would face immediate pressure to absorb economically inactive EU migrants into welfare systems; labor markets would receive larger inflows of lower-skilled workers with consequent wage pressure on competing native cohorts; sending states would face accelerated brain drain, lost remittances, and changing demographic composition; the EU political equilibrium would shift decisively toward either full integration (ECJ expansive reading ascendant) or comprehensive welfare-system coordination and harmonization. The constraint is actively maintained by member-state governments and bureaucracies; its removal would force institutional reorganization across both receiving and sending states.
% FOUNDING_PROBLEM: Post-1945 welfare states were constructed as national social contracts: residents paid taxes over a lifecycle, received services tied to contribution and need, and participated in collective risk-pooling by skill, health, and age cohort. EU free-movement law created a challenge: would welfare systems absorb migrant contributors and claimants equally with citizens? Would labor markets open to cross-border competition? Early European integration (1950s–1970s) assumed wealthy member states could absorb labor mobility without strain and that complementarity would dominate. By the 1990s–2000s, evidence of fiscal pressure from welfare-intensive migration (retirees, low-income families seeking benefits) and labor-market effects on native workers (wage depression in specific sectors, jobs taken before native workers hired) led member states to reaffirm welfare-bounded movement as legitimate.
% FOUNDING_PROBLEM_CORROBORATION: Member states and labor unions attest the founding problem is live: migration continues to create fiscal pressure on welfare systems and wage pressure on vulnerable native workers, requiring maintained restrictions. EU institutions and integration advocates attest the founding problem has been reframed rather than solved—that labor-market effects are modest and variable, welfare capacity is genuinely sufficient if systems are redesigned, so continued restrictions reflect political capture by anti-immigration constituencies, not economic necessity. Independent academic research (CEPR, IZA, World Bank studies) shows modest negative wage effects on some native cohorts (workers without secondary education show ~0.3–0.5% wage reduction per 1% increase in migrant stock in some European labor markets) and moderate fiscal effects (varying by receiving state, from minor surplus to ~1–2% of local government budgets in high-immigration areas); research does not support unambiguous finding that restrictions are economically necessary to prevent welfare or labor-market destabilization.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects that the constraint systematically transfers opportunity from mobile populations to settled populations; this transfer is substantial and decoupled from marginal welfare-service cost in receiving states. Suppression (0.71) is high because enforcement depends on actively excluding migrants, not on incentive alignment—receiving states must maintain residency tests, contribution thresholds, and sectoral exclusions; without active enforcement, migrants would move and claim. Theater (0.42) is moderate: welfare-capacity justification is partly real (welfare systems are funded on national bases and face genuine fiscal constraints) but the measurement series shows rising theater over the interval as restrictions persist despite stabilizing sending-state populations and labor-market complementarities becoming more apparent. The floor values represent earlier EU expansion (2004–2007) when wage effects were feared; the plateau at t35–40 reflects the post-2015 period when restrictions hardened politically despite moderating economic justification. Accessibility collapse (0.62) reflects that alternative pathways exist (individual asylum claims, third-country work visas, some sectoral exceptions) but are narrowed significantly; resistance (0.74) reflects sustained pressure from integration advocates, migrants' rights organizations, and EU institutions, though member states maintain enforcement through political will and control of national police/welfare gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   From the receiving-state welfare-administration seat, the constraint solves a genuine coordination problem (integrating welfare systems with labor markets requires boundary-setting) and the extraction is justified as the price of fiscal sustainability. From the economically inactive migrant seat, the same structure operates as coercive exclusion enforced by legal barriers with no genuine justification. From the indigenous labor seat, the constraint protects employment security. From the sending-state-worker seat, it imposes opportunity cost. From the supranational integration seat, the constraint is illegitimate restraint on a constitutive EU right. The engine computes each seat's directionality and type from the structural data; the perspectival gap is the diagnostic finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state welfare administrations are structural beneficiaries (d ≈ 0.1–0.2): they control the rules, administer the system, and collect the rents of exclusion. Indigenous labor constituencies are beneficiaries (d ≈ 0.2–0.3): they benefit from constrained competition but do not set the rules. Economically inactive migrants are full targets (d ≈ 0.9): they are excluded, have trapped exit options, and no arbitrage. Lower-skilled sending-state workers are targets (d ≈ 0.75): constrained exit, cannot arbitrage wage gaps, organized only within sending states. High-skilled sending-state workers are partial targets (d ≈ 0.55): they face opportunity cost and brain-drain extraction but retain arbitrage options (third-country mobility, professional networks). Supranational integration authorities would compute as targets (d ≈ 0.85) if seated: their preferred reading is excluded; their influence is suppressed by member-state veto power. Sending-state governments are near-symmetric (d ≈ 0.5): they benefit from reduced emigration pressure but lose remittances and face fiscal strain from brain-drain costs. The directionality overrides are unnecessary here; the structural derivation from beneficiary/victim + exit options + power captures the relationships correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war welfare state fiscal sustainability + labor-market stability) is contested in its contemporary form. Member states claim it remains live; integration advocates claim it has been reframed rather than solved. The disappearance verdict (world_rearranges) is unambiguous: if the member-sovereignty reading and its enforcement disappeared, receiving states would immediately face welfare-access claims from migrants, labor markets would receive larger flows, and the EU political settlement would shift. The constraint is not a natural law; it requires active enforcement. The theater-ratio series shows rising performance (justification rhetoric outpacing real welfare-capacity strain) over the interval, which is consistent with mandatrophy—the original problem has partially attenuated but the constraint persists through political will and institutional inertia. However, the constraint is NOT piton-classified because receiving-state administrations continue to actively benefit and actively enforce; if the beneficiary abandoned enforcement, the constraint would vanish. The classification stands as tangled rope because the coordination function (welfare-bounded labor markets) is real, the enforcement is active, and the asymmetric extraction is visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_capacity_measurement_ambiguity,
    'What empirically constitutes ''welfare state capacity''? Is it fiscal sustainability, demographic balance, service-quality maintenance, or some weighting across these dimensions?',
    'Systematic cost accounting of migrant inflows (welfare claims by type), labor-market impact studies (wage and employment effects by skill level and sector), and comparative welfare-system modeling across receiving states.',
    'If welfare capacity is measured purely by fiscal balance, migration might be contained; if measured by service quality or demographic sustainability, the capacity constraint might be looser. Different measurements would justify different restriction levels and potentially shift the constraint''s classification toward rope if capacity proves more resilient than asserted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_capacity_measurement_ambiguity, empirical, 'Whether welfare-capacity constraints are real or constructed as political cover for protectionism.').

omega_variable(
    labor_market_protection_necessity,
    'Do bounded free-movement restrictions genuinely protect native labor-market outcomes (wages, employment, job quality), or is the protection marginal and the restrictions primarily driven by xenophobic political pressure?',
    'Econometric studies comparing wage and employment outcomes in jurisdictions with restrictive vs. permissive free-movement policies, controlling for skill level, sector, and labor-market cycle. Natural experiments from regional restrictions or sectoral exemptions.',
    'If labor-market effects are substantial and concentrated on vulnerable native workers, the constraint''s coordination function is genuine and extraction justified as part of coordination cost. If effects are marginal or concentrated on employers'' wage-bill protection, the constraint is primarily extractive and the labor-protection narrative is cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_protection_necessity, empirical, 'Whether labor-market protection is a real coordination problem or political cover.').

omega_variable(
    brain_drain_extraction_mechanism,
    'To what extent does restricting high-skilled migration constitute extraction FROM sending states? Is it intentional or an incidental effect?',
    'Analysis of remittance flows and fiscal costs to sending states; tracking of sending-state government preferences on mobility; evidence of receiving states deliberately restricting mobility to prevent talent drain from receiving states (intent) vs. incidental effect.',
    'If brain-drain restriction is intentional extraction by receiving states, the constraint is snare-like for sending states, not tangled rope. If incidental, it remains tangled rope but with a different victim structure (high-skilled populations experience externality, not primary targeting). This affects how the constraint should be reframed or decomposed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brain_drain_extraction_mechanism, conceptual, 'Whether brain-drain constraint is intentional extraction or incidental effect of welfare-bounded mobility.').

omega_variable(
    supranational_authority_legitimacy,
    'Is the ECJ''s supranational free-movement interpretation legitimately grounded in the founding treaty or is it activist overreach of the member-sovereignty reading''s understanding?',
    'Textual analysis of founding treaties and subsequent amendments; comparison of member-state intent at each expansion of ECJ free-movement scope; historical record of explicit member-state objections or acceptance; referendum/legislative ratification of supranational authority shifts.',
    'If ECJ interpretation is legitimate evolution of founding authority, the member-sovereignty reading is conservative resistance to legitimate supranational authority and might reclassify toward snare (protecting member-state rents against justified supranational oversight). If ECJ interpretation is activist overreach, the member-sovereignty reading is legitimate defense of treaty bounds and classification as tangled rope holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supranational_authority_legitimacy, conceptual, 'Whether supranational free-movement authority is legitimate or activist overreach.').

omega_variable(
    kernel_framing_stability,
    'Is the federation-membership kernel stable—do all three readings genuinely interpret one shared commitment—or does the reading divergence reflect different kernels being conflated under one label?',
    'Analysis of whether all three readings share a common interpretation of ''member state authority over free movement'' or whether the integration reading and member-sovereignty reading are describing different authority structures entirely. If the readings disagree on what the kernel IS (not just how to interpret it), they may not be siblings but different constraints.',
    'If the readings are siblings of one kernel, the committer frame holds and constraint families are properly decomposed. If they describe different kernels, the decomposition is incomplete and the constraint family needs restructuring (likely: two kernels, not one).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_stability, conceptual, 'Whether the three readings interpret one kernel or describe different kernels.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) structural (legal barriers, documentation requirements, welfare gatekeeping rules) or internalized (migrants believe they do not belong, have internalized the sovereignty claim, comply voluntarily)?',
    'Post-restriction-removal observation: if migrants face barriers and remove them, do migrants flow immediately or is there persistent suppression after barriers fall? Natural experiments from jurisdictions that lifted restrictions (intra-Schengen, bilateral open-labor agreements). Survey evidence on migrant perceptions of legitimacy vs. legal coercion.',
    'If suppression is primarily structural, enforcement capacity is the binding constraint and removal of legal barriers would enable flows. If suppression is partially internalized, the constraint''s effective suppression exceeds the institutional measure and persists even after formal restrictions lift. This affects the longevity and removal cost of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in migrant self-concept.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(fede_tr_t25, observed).
narrative_ontology:measurement(fede_tr_t35, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(fede_tr_t35, observed).
narrative_ontology:measurement(fede_tr_t40, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(fede_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(fede_be_t25, observed).
narrative_ontology:measurement(fede_be_t35, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(fede_be_t35, observed).
narrative_ontology:measurement(fede_be_t40, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(fede_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(fede_su_t25, observed).
narrative_ontology:measurement(fede_su_t35, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(fede_su_t35, observed).
narrative_ontology:measurement(fede_su_t40, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(fede_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__member_sovereignty_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (member_sovereignty_reading) of the contested federation-membership kernel. The kernel is the shared commitment: 'EU member states share authority over free movement and welfare rights.' Three constraint stories instantiate this kernel with different ε values and beneficiary/victim structures: (1) integration_reading: supranational authority expansive, ε ≈ 0.15–0.25 (coordination, low extraction); (2) member_sovereignty_reading (this story): member states bound movement by welfare capacity, ε ≈ 0.68 (substantial extraction); (3) welfare_coordination_reading: coordination of national welfare systems, ε ≈ 0.35–0.45 (moderate extraction, different from both). The ε divergence reflects genuine difference in structure, not measurement basis ambiguity—each reading instantiates a different constraint because each makes a different referent claim about what the federation-membership commitment entails. Links via network.affects_constraints record the kernel family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
