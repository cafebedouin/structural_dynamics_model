% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement as Expansive Supranational Authority (Integration Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint instantiates the INTEGRATION READING of the
 *   federation_membership_kernel. The reading asserts that free movement is a
 *   fundamental right constitutive of EU citizenship, that supranational
 *   authority (the European Court of Justice) should interpret its scope
 *   expansively to maximize labor mobility and equal treatment, and that
 *   member state labor market and welfare protections are secondary to the
 *   integration mandate. Under this reading, displaced local labor,
 *   receiving-state welfare systems, and sending-state public investment bear
 *   externalized costs. The structural delta from sibling readings is
 *   precisely this: the integration reading treats costs to local labor and
 *   welfare sustainability as acceptable prices for supranational market
 *   completion, whereas the member_sovereignty_reading would bound free
 *   movement by national welfare capacity, and the
 *   welfare_coordination_reading would enforce coordination of welfare
 *   systems rather than supranational authority expansion. The claim
 *   (tangled_rope) and the metrics are authored independently: the constraint
 *   coordinates real labor market integration (coordination function) while
 *   concentrating extraction on powerless domestic labor and welfare systems
 *   that cannot exit. Extractiveness has accumulated over the interval as ECJ
 *   jurisprudence has closed off welfare restrictions and as in-migration
 *   into high-cost welfare states has intensified.
 *
 * KEY AGENTS:
 *   - ECJ and supranational institutions: Agenda-setter, institutional power, interpretive authority, no exit; sets the scope of free movement through binding rulings; collects legitimacy and authority accumulation.
 *   - Mobile EU workers: Beneficiary, moderate power (organized by skill/profession), arbitrage exit; gain wage and opportunity arbitrage, welfare access across borders, labor market choice.
 *   - Receiving-state employers: Beneficiary, powerful, mobile exit; access expanded labor supply, wage moderation, reduced training burden.
 *   - Displaced local labor: Payer, powerless, constrained exit; face direct labor market competition, wage depression, reduced bargaining power, geographic immobility.
 *   - Receiving-state welfare systems: Payer, institutional, constrained exit; must provide means-tested benefits to mobile residents without corresponding fiscal inflow; no power to condition access.
 *   - Sending-state governments: Payer, institutional, trapped exit; lose public investment, cannot tax diaspora income, must fund education systems whose beneficiaries exit.
 *   - Member state labor regulators: Excluded, institutional, constrained exit; prevented from enacting labor market protections that would apply asymmetrically to EU-mobile workers.
 *   - Analytical observer: Observes the distributional consequences and the constitutional tension between supranational authority expansion and member state welfare legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.71).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement as Expansive Supranational Authority (Integration Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, 'ff53352b-4f1a-4a06-9294-838139fd646e').
narrative_ontology:cs_kernel_codification('ff53352b-4f1a-4a06-9294-838139fd646e', fixed_text).
narrative_ontology:cs_authority_grounding('ff53352b-4f1a-4a06-9294-838139fd646e', extraction).
narrative_ontology:cs_interpretation_layer_present('ff53352b-4f1a-4a06-9294-838139fd646e').
narrative_ontology:cs_reading_relation('ff53352b-4f1a-4a06-9294-838139fd646e', federation_membership_kernel__member_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('ff53352b-4f1a-4a06-9294-838139fd646e', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('ff53352b-4f1a-4a06-9294-838139fd646e', foundational, individual_free_movement_right_fundamental).
narrative_ontology:cs_axiom_status(individual_free_movement_right_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('ff53352b-4f1a-4a06-9294-838139fd646e', individual_free_movement_right_fundamental, deontological).
narrative_ontology:cs_axiom('ff53352b-4f1a-4a06-9294-838139fd646e', foundational, supranational_authority_expands_to_protect_rights).
narrative_ontology:cs_axiom_status(supranational_authority_expands_to_protect_rights, holdable).
narrative_ontology:cs_axiom_grounding('ff53352b-4f1a-4a06-9294-838139fd646e', supranational_authority_expands_to_protect_rights, instrumental).
narrative_ontology:cs_reference_frame('ff53352b-4f1a-4a06-9294-838139fd646e', expansive_free_movement_rights).
narrative_ontology:cs_drift_state('ff53352b-4f1a-4a06-9294-838139fd646e', contemporary_welfare_state_retrenchment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff53352b-4f1a-4a06-9294-838139fd646e', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, supranational_institutional_authority).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, receiving_state_employers).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_public_investment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_governments).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_low_skilled_workers).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, supranational_authority_supremacy).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, labor_market_integration_as_market_completion).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, individual_rights_over_collective_welfare).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets EU treaties to expand the scope of free movement rights. Issues binding rulings that override national labor market regulations, social benefit restrictions, and welfare eligibility criteria. Justifies expansive interpretation as constitutive of EU citizenship and market completion. Enforces through infringement procedures and preliminary ruling mechanism. Collects institutional legitimacy and supranational authority accumulation from being the sole authoritative interpreter of free movement's boundaries.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, ecj_and_supranational_institutions, agenda_setter,
    institutional, generational, analytical, universal).

% Gain the right to live, work, and claim social benefits in any EU member state. Can arbitrage wage differentials, escape local labor market saturation, and access destination-state welfare benefits without residency penalties. Face minimal barriers to movement or family reunion. Their exit options are enhanced by the constraint — they can always move to a better-positioned jurisdiction.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_workers, beneficiary,
    moderate, biographical, arbitrage, global).

% Access a larger, mobile labor supply constrained only by wage and working conditions, not by citizenship or residency status. Can hire EU-mobile workers at lower wages than would clear local labor markets under closed borders. Are not obligated to train or develop local workforce. Benefit from wage moderation effects and labor supply elasticity that the constraint's enforcement creates.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_employers, beneficiary,
    powerful, biographical, mobile, national).

% Faces increased labor supply in their occupational categories, wage depression from the expanded pool of qualified workers willing to work at lower rates, and reduced bargaining power. Cannot easily relocate or retrain without bearing the full individual cost. Skill-level groups (especially low-skilled, construction, hospitality, social care) experience direct labor market competition from workers with arbitrage advantages. Their national governments are prevented from restricting labor inflow even when local unemployment is high.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_labor, payer,
    powerless, biographical, constrained, local).

% Must provide means-tested benefits, housing assistance, healthcare, and family benefits to EU-mobile residents on equal terms with citizens, despite not having designed the funding mechanism or tax base to absorb demand shocks. Court rulings prevent welfare access restrictions that would otherwise limit the cost. Receive no fiscal compensation from EU institutions or sending states for the cost of providing welfare to in-migrants. Bear the externality of receiving states absorbing welfare costs that would otherwise be costs to sending states.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Lose public investment in education and health infrastructure when skilled workers emigrate; cannot exclude outflows or tax their income in destination states. Face remittance dependence in some regions and skill loss in technical sectors. Must fund education systems whose beneficiaries exit without fiscal return. Are forbidden from restricting freedom of movement as a response to labor drain.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_governments, payer,
    institutional, generational, trapped, national).

% Compete for work with equally-mobile low-skilled workers from other member states; outflow of peers may reduce local opportunity density while labor supply in receiving states expands. Face wage moderation from expanded pools in destination labor markets. Often work in informal or precarious arrangements in destination states without full labor protections.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_low_skilled_workers, payer,
    powerless, biographical, mobile, regional).

% Are statutorily prevented from enacting labor market protections (sectoral wage floors, training requirements, apprenticeship mandates, occupational licensing) that would apply differently to EU-mobile workers versus citizens. ECJ rulings strike down protective measures on grounds of discriminatory effect. Their authority to manage local labor market adjustment is overridden by supranational free movement enforcement. Would argue for the right to pilot labor market controls, protect sectoral training ecosystems, or implement gradual transition policies.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, member_state_labor_market_regulators, excluded,
    institutional, biographical, constrained, national).

% Are prevented from setting welfare eligibility criteria that would condition benefits on citizenship, long-term residence, or contribution history in ways that would reduce the in-migrant welfare draw. Court rulings eliminate 'chilling effects' on welfare access. Cannot use social housing or subsidized childcare to prioritize citizens. Would argue that welfare state legitimacy depends on reciprocal contribution norms and that unlimited access without corresponding fiscal inflow undermines political support for universal welfare.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, member_state_welfare_governments, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__integration_reading, ecj_and_supranational_institutions).
narrative_ontology:fixing_cost_class(federation_membership_kernel__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes barriers to labor market arbitrage across member states, allowing workers to exploit wage and opportunity differentials and employers to access larger labor supply pools. Solves the collective-action problem of member states' incentive to restrict labor inflow unilaterally while collectively benefiting from larger integrated labor markets. Coordinates EU-wide labor mobility through supranational legal enforcement, preventing beggar-thy-neighbor restrictions.
% TRANSFER_FUNCTION: Transfers labor supply from sending to receiving states without corresponding fiscal transfers. Moves welfare cost from origin-state budgets (where the worker trained) to destination-state budgets (where they now reside). Distributes gains from labor market integration (lower wages for employers, expanded choice for mobile workers) while concentrating costs on displaced local labor and receiving-state welfare systems.
% ABSENT_VOICES: Displaced local workers in receiving states have weak institutional representation in EU policymaking; receiving-state welfare administrators are not parties to ECJ interpretation; sending-state labor market regulators cannot block outflows. Member state governments that would restrict free movement for labor market protection or welfare sustainability are structurally excluded from the enforcement mechanism — the ECJ's authority to interpret free movement supersedes their voice. Workers in precarious or informal status in destination states are largely invisible to official welfare systems and bear costs without claim to benefits.
% DISAPPEARANCE_RATIONALE: If free movement enforcement vanished and member states re-established labor market and welfare borders, receiving states would restrict inflows and reducing wage competition; sending states would lose the outflow pressure and regain public investment return; local labor markets would tighten; welfare systems would reduce non-citizen access. The EU labor market would fragment into national compartments; wage divergence across members would widen. Supranational institutional authority would contract, and member states would regain regulatory autonomy. The single labor market as constructed would dissolve.
% FOUNDING_PROBLEM: Pre-EU member states had fragmented labor markets protected by national borders; the founding problem was to create a common market for labor, capital, and goods by removing artificial barriers and creating conditions for competitive integration across jurisdictions.
% FOUNDING_PROBLEM_CORROBORATION: EU institutional actors and economic integration advocates attest that the founding problem remains live and requires expansive interpretation to sustain market integration. Member state governments and labor representatives attest that the founding problem (lack of market access) is substantially solved and that continued expansive interpretation now creates NEW problems (displaced labor, welfare burden, fiscal externality) that the constraint does not internalize. Economic research literature documents both labor market gains from mobility and distributional losses to low-skilled and displaced workers.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 at interval end, rising from 0.35 over 32 years. The constraint extracts from displaced labor and welfare systems (high d for powerless receiving-state labor and institutional welfare systems unable to gate access) and from sending states (institutional power but trapped exit — no legal mechanism to restrict outflows or tax diaspora). The beneficiaries are mobile workers (moderate power, arbitrage exit — low d) and institutional employers (powerful, mobile exit). The supranational agenda-setter extracts institutional authority and legitimacy expansion (institutional power, analytical exit — d near symmetric or slightly beneficiary, but the authority itself is the extracted quantity). Suppression is 0.71: the constraint's persistence depends on actively suppressing member state labor market regulations, welfare access restrictions, and sending-state controls on emigration. Theater is low-moderate (0.28): the integration narrative is genuine and has real coordination content (labor market access, capital mobility, equal treatment rights), but a growing share of the enforcement machinery serves to prevent member states from reimposing protections rather than to deepen labor market integration itself. Accessibility collapse is 0.62: alternatives (national labor markets with borders, selective welfare eligibility) are understood as real options but are structurally closed off by ECJ enforcement and infringement procedure risk. The measurements document extraction accumulation over the interval: as the EU expanded to lower-wage members (2004, 2007) and as ECJ jurisprudence closed off welfare restrictions (Citizens Union rights, derived rights, non-discrimination doctrine), the effective extraction from displaced labor and welfare systems intensified. Suppression requirement rose in parallel: member state attempts to impose residency conditions, contribution requirements, or labor market entry restrictions were struck down, requiring stronger enforcement. Theater rose modestly: the integration narrative remains legitimate but is increasingly used to defend specific extraction mechanisms rather than general labor market opening.
 *
 * PERSPECTIVAL GAP:
 *   From the ECJ and supranational institutional seat, the constraint is genuine coordination: establishing a common labor market, securing individual rights against member state discrimination, deepening integration. From the displaced local labor seat (powerless, constrained, local scope), it is an extraction mechanism: expanded competition from mobile workers, wage depression, no path to block inflows or exit to better conditions. From the receiving-state welfare seat (institutional, constrained), it is an externality: costs imposed without fiscal compensation. From the sending-state seat (institutional, trapped), it is a brain drain and public investment externality. The member state labor regulator seat (excluded) sees it as supranational overreach: the ECJ's expansive interpretation strips member states of the authority to manage labor market transitions, protect sectoral training ecosystems, and calibrate welfare generosity to political sustainability. The engine should compute these divergences from the structural data — the same institutional design produces incompatible classifications from different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   ECJ and supranational institutions (institutional power, analytical exit): d ~ 0.4–0.5, slight beneficiary bias because the constraint expands their authority and interpretive scope; they are not targets of extraction. Mobile EU workers (moderate power, arbitrage exit): d ~ 0.15–0.25, beneficiary bias because they exploit arbitrage opportunities, gain welfare access, and have exit to better jurisdictions. Receiving-state employers (powerful, mobile exit): d ~ 0.25–0.35, beneficiary bias because they access expanded labor supply and wage moderation; they have mobile exit if the constraint shifts against them. Displaced local labor (powerless, constrained exit): d ~ 0.85–0.95, full target bias because they face direct competition, wage depression, no exit options. Receiving-state welfare systems (institutional, constrained exit): d ~ 0.75–0.85, target bias because they bear uncompensated costs and cannot gate access. Sending-state governments (institutional, trapped exit): d ~ 0.80–0.90, target bias because they lose public investment, cannot restrict outflows, have no fiscal compensation mechanism. Member state labor regulators (institutional, constrained): d ~ 0.70–0.80, target bias because their authority is actively overridden. The directionality reflects the structural asymmetry: supranational expansion of authority extracts from member states (by reducing their regulatory autonomy) and from domestic labor (by preventing labor market protection), while benefiting mobile workers and employers who exploit the expanded opportunity set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented pre-EU labor markets, trade barriers, lack of labor market access for mobile workers) is substantially solved: by 2024, worker mobility is widespread, labor markets are integrated in major sectors, and capital flows across borders without friction. Yet the constraint persists and has intensified. The founding mandate (create a common labor market) has outlived its primary function (remove artificial barriers to market access); the constraint now operates primarily to prevent member states from reimposing protections. The constraint has accumulated extraction as it shifted from enabling new access (1990s–2000s) to preventing state retrenchment (2010s–2020s). The theater ratio documents this: enforcement activity increasingly defends access rights against state attempts to condition welfare, restrict labor inflows during unemployment spikes, or impose training requirements. This is the classic mandatrophy signature: the institutional arrangement persists and expands its enforcement machinery even as the coordinate problem it solved has been addressed. The contradiction is contained by reframing free movement as a fundamental RIGHT (rights do not sunset) rather than as an instrumental solution to a market access problem (which could, in principle, be satisfied and closed). The constraint is vulnerable to a member_sovereignty_reading that would rebound the mandate to fit current conditions: welfare state capacity rather than labor market access. The welfare_coordination_reading offers a middle path: enforce anti-dumping rules and worker protections while allowing member states to manage their own welfare designs. This story is a tangled_rope because it delivers genuine coordination (labor market integration) alongside significant extraction (from domestic labor and welfare systems); the extraction is structural, not incidental, and the constraint requires active ECJ enforcement to prevent member states from repairing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_interpretation_boundary,
    'Is ''free movement'' a fundamental right grounded in individual non-discrimination (integration reading) or a conditional coordination right grounded in reciprocal contribution and welfare system capacity (sovereignty reading)?',
    'Treaty amendment or ECJ reversal of jurisprudential course through a major ruling that reframes free movement as bounded by welfare capacity. Alternatively, member state constitutional courts could challenge the supremacy of the free-movement interpretation through a constitutional pluralism dispute.',
    'If the reading is revised to bound free movement by receiving-state welfare capacity, displacement victims and welfare systems would exit the victim set; the constraint would shift from tangled_rope to rope or even mountain. ECJ authority would be bounded rather than expansive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_interpretation_boundary, conceptual, 'Whether free movement is a bounded coordination right or an expansive individual right.').

omega_variable(
    extraction_vs_coordination_magnitude,
    'What share of the measured extractiveness (0.68) represents genuine labor market coordination benefit (solving the original collective-action problem) versus extraction that accumulated as ECJ jurisprudence shifted from enabling access to preventing state retrenchment?',
    'Counterfactual comparative analysis: simulate labor market outcomes under the integration reading versus member_sovereignty_reading assumptions; compute the welfare distribution across displaced labor, mobile workers, employers, and welfare systems. Timeline analysis: separate pre-2008 (barrier removal phase) from post-2008 (retrenchment prevention phase) extractiveness.',
    'If extraction accumulated primarily in the retrenchment-prevention phase, the constraint is a clear case of institutional mandate drift and might be subject to reform under mandatrophy reasoning. If coordination benefits and extraction are inseparable (coordination always requires some redistribution), the constraint remains justified as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_magnitude, empirical, 'Decomposition of coordination function from accumulated extraction over the interval.').

omega_variable(
    welfare_legitimacy_constraint_compatibility,
    'Is unlimited free movement with equal welfare access structurally compatible with national welfare state legitimacy, which depends on reciprocal contribution norms and bounded redistribution?',
    'Survey research on welfare support conditionality and nationality; analysis of political responses to in-migrant welfare use (e.g., backlash and welfare retrenchment); examination of whether welfare systems with high non-citizen access show declining public support or political fragmentation.',
    'If unlimited access erodes welfare legitimacy and triggers retrenchment that harms both citizens and migrants, the integration reading''s assumption that individual rights can override welfare design is falsified. Reform would need to decouple welfare from free movement or establish fiscal compensation mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_legitimacy_constraint_compatibility, empirical, 'Whether the integration reading''s premise that welfare access and free movement can coexist indefinitely is sustainable.').

omega_variable(
    committer_frame_alternative_reading,
    'Is the integration_reading''s expansive authority interpretation a genuine constitutional commitment, or does it represent regulatory capture by supranational institutions expanding their mandate and authority (extraction disguised as principle)?',
    'Historical analysis of ECJ institutional incentives and jurisprudential trajectory; comparison with original EU treaty language on free movement (was expansive interpretation a foreseen implication or a drift?); examination of whether ECJ rulings track the integration principle or the institutional expansion interest.',
    'If the reading is a case of institutional expansion (extraction), then the constraint should be reclassified as snare with ECJ as the captor, and the appropriate remedy is constraining ECJ authority rather than accepting the integration mandate. If the reading is a genuine principle, reform should work through treaty amendment or treaty interpretation, not institutional restructuring.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_alternative_reading, conceptual, 'Whether the integration reading serves a genuine constitutional principle or masks ECJ institutional expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1992, federation_membership_kernel__integration_reading, theater_ratio, 1992, 0.08).
narrative_ontology:measurement_basis(fede_tr_t1992, observed).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_kernel__integration_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement_basis(fede_tr_t2000, observed).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_kernel__integration_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement_basis(fede_tr_t2008, observed).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_kernel__integration_reading, theater_ratio, 2016, 0.24).
narrative_ontology:measurement_basis(fede_tr_t2016, observed).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_kernel__integration_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement_basis(fede_tr_t2020, observed).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__integration_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(fede_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_kernel__integration_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement_basis(fede_be_t1992, observed).
narrative_ontology:measurement(fede_be_t2000, federation_membership_kernel__integration_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement_basis(fede_be_t2000, observed).
narrative_ontology:measurement(fede_be_t2008, federation_membership_kernel__integration_reading, base_extractiveness, 2008, 0.51).
narrative_ontology:measurement_basis(fede_be_t2008, observed).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__integration_reading, base_extractiveness, 2016, 0.63).
narrative_ontology:measurement_basis(fede_be_t2016, observed).
narrative_ontology:measurement(fede_be_t2020, federation_membership_kernel__integration_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(fede_be_t2020, observed).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__integration_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(fede_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_kernel__integration_reading, suppression_requirement, 1992, 0.45).
narrative_ontology:measurement_basis(fede_su_t1992, observed).
narrative_ontology:measurement(fede_su_t2000, federation_membership_kernel__integration_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement_basis(fede_su_t2000, observed).
narrative_ontology:measurement(fede_su_t2008, federation_membership_kernel__integration_reading, suppression_requirement, 2008, 0.61).
narrative_ontology:measurement_basis(fede_su_t2008, observed).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__integration_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement_basis(fede_su_t2016, observed).
narrative_ontology:measurement(fede_su_t2020, federation_membership_kernel__integration_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(fede_su_t2020, observed).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__integration_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(fede_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__integration_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint and its sibling readings (member_sovereignty_reading, welfare_coordination_reading) form a constraint family instantiating the contested federation_membership_kernel. All three are readings of the same underlying commitment — what 'free movement' means for EU citizenship. They have the same empirical domain (EU labor mobility, welfare access) but differ in their fundamental interpretation of authority and obligation. Each has its own constraint_id and separate JSON file; they are linked via network.affects_constraints to enable contention and drift analysis. The integration_reading (this file) treats free movement as expansive supranational authority; the member_sovereignty_reading bounds it by member state welfare capacity; the welfare_coordination_reading enforces coordination without supranational expansion. Together they model how a single contested kernel generates different constraints depending on which reading adjudicates its scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__integration_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
