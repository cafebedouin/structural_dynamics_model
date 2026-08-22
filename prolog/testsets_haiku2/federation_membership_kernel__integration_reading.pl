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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement Supranational Authority (Integration Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The European Court of Justice's expansive interpretation of free movement
 *   as a fundamental right constitutive of EU citizenship creates a
 *   supranational constraint that removes member states' ability to restrict
 *   labor inflows or condition welfare access on residence duration. This
 *   reading instantiates free movement as a supremacy doctrine: the ECJ's
 *   authority overrides national legislatures on labor market and welfare
 *   policy. The constraint redistributes entry opportunities and welfare
 *   access to mobile workers and destination employers while externalizing
 *   brain drain and adjustment costs onto sending states and displaced local
 *   workers. The beneficiaries (mobile workers, destination employers, the
 *   ECJ's institutional authority) gain mobility and market access; the
 *   victims (displaced workers in receiving states, receiving-state welfare
 *   systems, sending states) bear uncompensated costs. The supranational
 *   authority architecture prevents democratic remedy through national
 *   legislative action.
 *
 * KEY AGENTS:
 *   - European Court of Justice: institutional agenda-setter enforcing expansive free-movement doctrine, benefiting from expanded jurisdiction
 *   - Mobile workers (EU citizens exercising free movement rights): beneficiaries with arbitrage-grade exit options, gain equal employment and welfare access
 *   - Receiving-state employers: beneficiaries accessing larger labor pools without immigration restrictions, organized institutional power
 *   - Displaced local workers: powerless victims facing wage pressure and job displacement, trapped without recourse to national protection
 *   - Receiving-state welfare systems: institutional victims bearing fiscal costs without compensatory redistribution
 *   - Sending states: institutional victims losing human capital and tax base via brain drain, constrained exit from the EU framework
 *   - National legislatures: excluded parties whose authority over labor and welfare policy is overridden by ECJ rulings
 *   - Union confederations: organized parties whose bargaining power is undercut by migrant labor, excluded from rulemaking
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.71).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement Supranational Authority (Integration Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '14554f50-00de-4905-adf6-f5e208d663b6').
narrative_ontology:cs_kernel_codification('14554f50-00de-4905-adf6-f5e208d663b6', formalized).
narrative_ontology:cs_authority_grounding('14554f50-00de-4905-adf6-f5e208d663b6', extraction).
narrative_ontology:cs_interpretation_layer_present('14554f50-00de-4905-adf6-f5e208d663b6').
narrative_ontology:cs_reading_relation('14554f50-00de-4905-adf6-f5e208d663b6', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('14554f50-00de-4905-adf6-f5e208d663b6', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('14554f50-00de-4905-adf6-f5e208d663b6', foundational, free_movement_constitutive_of_supranational_citizenship).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_supranational_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('14554f50-00de-4905-adf6-f5e208d663b6', free_movement_constitutive_of_supranational_citizenship, deontological).
narrative_ontology:cs_axiom('14554f50-00de-4905-adf6-f5e208d663b6', foundational, supranational_court_supremacy_over_national_labor_policy).
narrative_ontology:cs_axiom_status(supranational_court_supremacy_over_national_labor_policy, holdable).
narrative_ontology:cs_axiom_grounding('14554f50-00de-4905-adf6-f5e208d663b6', supranational_court_supremacy_over_national_labor_policy, deontological).
narrative_ontology:cs_reference_frame('14554f50-00de-4905-adf6-f5e208d663b6', supranational_citizenship_supremacy).
narrative_ontology:cs_drift_state('14554f50-00de-4905-adf6-f5e208d663b6', contemporary_welfare_fiscal_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('14554f50-00de-4905-adf6-f5e208d663b6', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, receiving_state_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, ecj_institutional_authority).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, union_confederations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets EU treaty language on free movement and enforces it through case law that progressively expands the scope of non-discrimination and mobility. No member state can unilaterally override ECJ rulings without facing enforcement proceedings and treaty violation findings. The court's authority grows as its interpretations override national courts and legislatures on labor and welfare policy.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, european_court_of_justice, agenda_setter,
    institutional, generational, analytical, continental).

% Gain access to labor markets across the EU on equal terms with citizens of the destination state. Can seek employment, reside, and claim equal social benefits without nationality restrictions. Comprise both high-skill professionals and lower-skill workers entering destination labor markets. The supranational rule removes formal barriers and enables arbitrary movement between member states.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_workers, beneficiary,
    moderate, biographical, arbitrage, continental).

% Access a larger, mobile labor pool without needing to navigate national immigration procedures or sponsor visas. Can fill positions at lower negotiating cost than they would face with only local workers available. Union wage floors become vulnerable to undercutting by mobile workers willing to work at lower rates. Directly benefit from the supranational rule that prevents labor market segmentation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_employers, beneficiary,
    powerful, biographical, mobile, national).

% Gains expanded jurisdiction and doctrinal authority as it enforces supranational free-movement doctrine. Each ruling that strikes down national labor protections as discriminatory vindicates the supremacy of supranational authority and establishes the ECJ as the final arbiter of citizenship rights. The institution benefits from the expansion of its interpretive reach into domains previously reserved to national legislatures.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, ecj_institutional_authority, beneficiary,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, ecj_institutional_authority, agenda_setter).

% Face direct labor market competition from intra-EU migrants without the reciprocal right to work in sending states at equivalent scale. Experience wage pressure from abundant supply of mobile workers, job displacement, and reduced union bargaining power. National labor market protections (apprenticeship requirements, union-negotiated wage floors) are struck down as discriminatory under ECJ doctrine. Cannot credibly emigrate to sending states where the jobs went; cannot exit the labor market entirely. Have no recourse to democratic remedy because ECJ authority supersedes national legislative action.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_workers, payer,
    powerless, biographical, trapped, local).

% Bear fiscal costs of expanded free movement without compensatory EU-level redistribution. Migrants claim unemployment benefits, housing allowances, child allowances, and healthcare without contributing long enough to fund their share. The ECJ rules that member states cannot condition benefit access on prior residence duration or contribution levels, treating EU citizens identically to nationals. The fiscal burden concentrates on destination-state treasuries while EU rules prevent member states from adjusting welfare generosity in response to demographic pressure.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Lose human capital and tax base to outmigration. Workers trained at public expense leave; remittances flow back but are insufficient to offset fiscal losses. Cannot tax citizens working abroad without cooperation from destination states (which have disincentive to cooperate). The supranational framework treats mobility as a pure individual right, not a collective resource, so no burden-sharing mechanism exists. High-skill brain drain weakens sending-state labor markets and institutional capacity over time.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_states, payer,
    institutional, generational, constrained, national).

% Cannot restrict free movement or amend labor market protections that the ECJ deems discriminatory, even when constituents demand protection against job displacement or welfare-cost pressures. Legislation passed to protect local workers or manage fiscal sustainability is routinely struck down on equal-treatment grounds. Excluded from rulemaking despite having the only directly democratic mandate; their authority is overridden by supranational judicial authority.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, national_legislatures, excluded,
    institutional, generational, constrained, national).

% Lose bargaining power as their ability to negotiate wage floors and working conditions is undercut by abundant migrant labor competing for the same positions. Cannot legally restrict membership or bargaining scope by nationality. Union-negotiated standards become vulnerable to legal challenge under equal-treatment doctrine. Their institutional interest (labor market segmentation, bargaining power) conflicts with the supranational rule, but they are excluded from formal rulemaking authority despite representing displaced workers.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, union_confederations, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, union_confederations, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__integration_reading, ecj_institutional_authority).
narrative_ontology:fixing_cost_class(federation_membership_kernel__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a continental labor market by removing legal barriers to employment, residence, and social benefits across member states. Solves the collective-action problem of labor market fragmentation: absent the rule, each state would restrict entry to protect local wages, reducing overall continental efficiency and trapping workers in low-opportunity regions. Enables workers to arbitrage wage and opportunity differences and employers to access larger talent pools.
% TRANSFER_FUNCTION: Redistributes labor market entry opportunities from restricted (local workers in receiving states) to unrestricted (mobile workers and employers seeking to hire them). Transfers welfare access from selective (residence-based, contribution-based) to universal (equal across EU citizens). Externalizes costs (brain drain, wage pressure, welfare fiscal stress) from beneficiaries to victim parties. The supranational court enforces this redistribution by voiding member-state protections.
% ABSENT_VOICES: National legislatures, whose authority is overridden by ECJ rulings despite democratic mandate to represent constituent interests in labor and welfare policy. Displaced local workers, who are affected directly but have no institutional standing in supranational proceedings. Unions, which represent workers but are excluded from formal rulemaking. Sending-state publics, who bear brain-drain costs but cannot veto free movement. These absent voices would argue for bounded free movement and member-state welfare autonomy.
% DISAPPEARANCE_RATIONALE: If supranational enforcement of free movement vanished overnight, member states would quickly reimpose labor market restrictions, apprenticeship requirements would become enforceable, union wage floors would recover bargaining power, and welfare eligibility would tighten. Destination states would impose transition periods on new mobility. Sending states would recover tax base and institutional capacity. The EU single market would fragment along labor-market lines, creating bilateral and regional sub-agreements. Continental labor mobility would drop sharply.
% FOUNDING_PROBLEM: Post-WWII European integration required removing the mobility restrictions that warring nation-states had imposed on citizens of former enemies. Free movement was framed as preventing future conflict by binding economies through trade and labor exchange. The ECJ's doctrine extends this founding rationale: mobility itself, not merely the legal absence of restrictions, is constitutive of supranational citizenship and prevents national re-nationalism.
% FOUNDING_PROBLEM_CORROBORATION: The ECJ and EU institutional leadership affirm the founding problem remains live: integration requires expanding free movement to deepen supranational bonds and prevent member-state re-nationalism. Major corporations and destination-state employer associations affirm the problem (labor scarcity, talent mobility) remains live. Member-state legislatures in higher-unemployment regions and union confederations attest the founding problem has been solved (peace is established, economic integration is deep, nationalism is not resurging through labor policy). Economic analysis from OECD, IMF, and independent labor-economist testimony documents that large-scale labor inflows generate local displacement without fiscal compensation, suggesting the founding rationale no longer justifies the measured extraction and that alternative coordination mechanisms (managed migration, burden-sharing funds) could achieve the integration goal at lower extraction cost.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is 0.68 because the constraint channels labor market entry and welfare access from restricted (local workers, member states) to unrestricted parties (mobile workers, destination employers, supranational authority) in ways decoupled from their willingness to bear adjustment costs. The extraction rises over the interval (0.45 to 0.68) as ECJ jurisprudence expands the scope and member states exhaust domestic political remedies. Suppression is 0.71 because the constraint persists through active enforcement (ECJ rulings against member-state protections) and through preventing national democratic remedy—national legislatures cannot amend labor policy without ECJ challenge. Theater is moderate (0.42) because the legitimating narrative (free movement is constitutive of EU citizenship) is genuine from the integration reading's perspective, but a growing share of enforcement activity targets labor market fragmentation rather than citizenship rights per se. Accessibility_collapse is 0.64 because alternatives (national labor market protection, welfare autonomy, bilateral labor agreements) exist in principle but are legally blocked; once the ECJ doctrine is understood, alternatives narrow unless the EU framework itself is contested. Resistance is 0.73 because substantial organized opposition exists (unions, displaced workers, some member states) and has measurable political force, even though it has failed to overturn the supranational rule.
 *
 * PERSPECTIVAL GAP:
 *   The integration reading (this constraint) interprets free movement as a fundamental right and supranational authority as legitimate because it protects the right against national interference. From this seat, extraction is not the frame—coordination and citizenship are. The member_sovereignty_reading reinterprets the same institutional arrangement: free movement bounded by national welfare capacity and labor-market protection. From that seat, supranational authority is institutional overreach and extraction is the frame. The welfare_coordination_reading splits the difference: free movement yes, but coordinated through national welfare systems rather than supranational harmonization. Each reading instantiates a different constraint with a different beneficiary/victim structure and different ε. This story (integration_reading) authors ε for the supranational supremacy arrangement; the sovereignty reading would author ε for the bounded-movement arrangement as a different constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECJ sits at d near 0.95 as the strongest target in disguise: the court benefits from institutional expansion and has no external constraint, but the expansion depends on constant enforcement against member-state resistance. Its interests diverge from the coordination it claims to enable. Mobile workers sit at d near 0.05 (pure beneficiaries: gate access, no enforcement burden). Displaced local workers sit at d near 0.95 (targets: bear all costs, no reciprocal benefit, identity-locked in place). Receiving-state welfare systems sit at d near 0.85 (targets: bear fiscal costs, constrained exit through EU fiscal rules). Sending states sit at d near 0.85 (targets: lose human capital, constrained exit through treaty obligation). A directionality_override for the ECJ is not warranted—institutional power does not lower the directionality when the benefit accrues through enforcement of asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (removing mobility restrictions between warring nation-states) was live in 1957 and credibly justified the constraint. By the 1990s, the problem is substantially solved—peace is stable, economic integration is deep. Yet the constraint persists and expands. The ECJ continues to justify expansive free movement as constitutive of EU citizenship, but this is largely theatrical maintenance: the real coordination problem is solved, and the additional expansion serves institutional (ECJ authority growth) and distributional (mobile workers' interests) goals rather than collective coordination. The measured extraction rising from 0.45 to 0.68 while theater_ratio remains moderate (0.42) suggests the constraint is transitioning toward piton territory—but is not yet a pure piton because active enforcement remains necessary (suppression 0.71) to overcome organized resistance. If resistance erodes and theater rises, the constraint would fully pitonize. The mandatrophy frame applies: the ECJ must continue to articulate why supranational authority over labor and welfare is necessary, even as the original justification (preventing future war through binding mobility) weakens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_supremacy_basis,
    'Does the ECJ''s expansive interpretation of free movement rest on legitimate supranational constitutional authority, or does it represent institutional overreach justified retrospectively by integration ideology?',
    'Doctrinal analysis of treaty language and alternative interpretations; examination of ECJ reasoning patterns; comparison to member-state original intent and treaty debates; assessment of whether the scope expansion reflects evolving constitutional consensus or unilateral institutional expansion.',
    'If the expansion is doctrine-rooted, the constraint''s legitimacy stands from the integration reading''s perspective. If it is institutional overreach, the constraint is better understood as an institutional snare (beneficiary ECJ, victims member states) disguised as coordination. The classification hinges on the authority grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_supremacy_basis, conceptual, 'The legitimacy of supranational ECJ authority over member-state labor and welfare policy.').

omega_variable(
    coordination_extraction_boundary_in_free_movement,
    'Is the extraction measured in this constraint (ε=0.68) the necessary cost of enabling labor mobility coordination, or is it surplus rent collected by beneficiaries above the coordination cost?',
    'Empirical: decompose the measured extraction into (1) coordination costs (verification, enforcement, transaction costs of matching labor supply and demand across borders) and (2) surplus rents (gains to mobile workers, employers, and ECJ beyond what coordination requires). If coordination costs << extraction, the gap is rent.',
    'If coordination costs are high (e.g., mutual recognition of professional credentials requires substantial certification infrastructure), extraction might be justified as necessary. If surplus rents are large, the constraint is more snare than tangled_rope—redistribution without reciprocal collective benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_in_free_movement, empirical, 'Decomposition of measured extraction into necessary coordination cost and surplus rent.').

omega_variable(
    reading_contingency_on_supranational_authority_strength,
    'How dependent is the integration reading''s institutional viability on the continued strength of supranational authority relative to member-state resistance?',
    'Temporal: track member-state defections, legislative resistance, and ECJ rulings struck down or circumvented; if member-state power grows relative to ECJ authority, the reading''s institutional substrate weakens, potentially foreclosing it.',
    'If supranational authority erodes significantly, the integration reading becomes institutionally untenable, and the member_sovereignty_reading gains traction. This is not a disagreement about values; it is a shift in what institutional configuration can sustain the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contingency_on_supranational_authority_strength, empirical, 'The institutional conditions for the integration reading''s persistence.').

omega_variable(
    axiom_overriding_supranational_supremacy,
    'Has the foundational axiom of supranational supremacy over national labor and welfare policy been substantively challenged by empirical evidence of harm (brain drain, job displacement, welfare fiscal crises) such that it is moving from holdable to overridden status?',
    'Track policy discourse, ECJ ruling patterns, and member-state resistance; assess whether challenges to the axiom are marginal (holdable) or represent substantial institutional pressure to reframe (overridden).',
    'If the axiom is overridden, the integration reading''s foundational claim no longer holds within its own tradition. The constraint would reclassify toward snare or require fundamental doctrine revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_overriding_supranational_supremacy, empirical, 'Whether supranational supremacy remains a holdable axiom or is being overridden by evidence and institutional pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__integration_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership_kernel__integration_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__integration_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__integration_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__integration_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership_kernel__integration_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(fede_tr_t25, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership_kernel__integration_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(fede_tr_t30, observed).
narrative_ontology:measurement(fede_tr_t35, federation_membership_kernel__integration_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(fede_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__integration_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership_kernel__integration_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__integration_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__integration_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__integration_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership_kernel__integration_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(fede_be_t25, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership_kernel__integration_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(fede_be_t30, observed).
narrative_ontology:measurement(fede_be_t35, federation_membership_kernel__integration_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(fede_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__integration_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership_kernel__integration_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__integration_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__integration_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__integration_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership_kernel__integration_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(fede_su_t25, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership_kernel__integration_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(fede_su_t30, observed).
narrative_ontology:measurement(fede_su_t35, federation_membership_kernel__integration_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(fede_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__integration_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested federation_membership_kernel. The integration reading instantiates free movement as a supranational supremacy doctrine enforced by the ECJ over national labor and welfare policy. Sibling readings (member_sovereignty and welfare_coordination) instantiate the same kernel under different institutional framings and produce different beneficiary/victim structures and extraction values. Each reading is a separate constraint with its own ε-invariant structure. Network edges link the reading family so that institutional decisions affecting one reading (e.g., ECJ authority erosion) propagate to influence siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
