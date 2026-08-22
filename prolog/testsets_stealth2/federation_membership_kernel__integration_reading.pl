% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Free Movement as Constitutive EU Citizenship Right — Integration Reading
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   This story instantiates the integration reading of the
 *   federation-membership kernel: free movement treated as a fundamental
 *   right constitutive of EU citizenship and single-market completion, with
 *   supranational adjudication interpreting scope expansively to maximize
 *   labor mobility and equal treatment. The standing arrangement under
 *   contest — and therefore the referent of epsilon — is the mobility regime
 *   as it actually operates under that expansive interpretation, assessed by
 *   this reading's own lights; the endorsed alternatives of sibling readings
 *   are different constraints, not parts of this one. The colloquial label
 *   'EU free movement' decomposes, per the epsilon-invariance principle, into
 *   three structurally distinct constraint stories (this reading,
 *   member_sovereignty_reading, welfare_coordination_reading) with different
 *   epsilon ranges, victim sets, and enforcement logics; they are linked
 *   through network.affects_constraints. Claim and metrics are authored
 *   independently: the claimed type reflects the structure visible from this
 *   reading's seat (genuine coordination carrying real asymmetric costs),
 *   while the metric series describe the arrangement's actual operation
 *   across the interval.
 *
 * KEY AGENTS:
 *   - ecj_supranational_court: agenda-setting interpreter (institutional/constrained) — its expansive case law defines the constraint's operative scope and overrides national labor-market protections
 *   - european_commission: agenda-setter with secondary beneficiary position (institutional/constrained) — enforcement initiator and competence-holder
 *   - mobile_eu_workers: primary intended beneficiary (moderate/mobile)
 *   - multinational_service_employers: concentrated beneficiary with arbitrage-grade exit (powerful/arbitrage)
 *   - displaced_local_labor: principal dispersed payer (powerless/constrained) — bears competitive pressure in exposed trades
 *   - receiving_state_fiscal_authorities: institutional payer (institutional/constrained) — absorbs unfunded service loads
 *   - host_sector_trade_unions: organized payer with identity-fused exit (organized/identity_locked) — collective-bargaining coverage eroded at the edges
 *   - sending_state_governments: dual-positioned payer-beneficiary (institutional/constrained) — human-capital loss against remittances and labor-valve relief
 *   - sending_state_service_dependents: trapped payer (powerless/trapped) — services thinned by clinician departures
 *   - third_country_nationals: excluded seat (powerless/trapped) — would contest membership-graded mobility rights
 *   - federal_polity_analysts: analytical observer — tracks the distributional ledger from outside the advocacy coalitions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.48).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.17).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.17).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "Free Movement as Constitutive EU Citizenship Right — Integration Reading").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '7941e722-bf34-4c2c-91c0-3222ff6c3f5b').
narrative_ontology:cs_kernel_codification('7941e722-bf34-4c2c-91c0-3222ff6c3f5b', fixed_text).
narrative_ontology:cs_authority_grounding('7941e722-bf34-4c2c-91c0-3222ff6c3f5b', lineage).
narrative_ontology:cs_interpretation_layer_present('7941e722-bf34-4c2c-91c0-3222ff6c3f5b').
narrative_ontology:cs_reading_relation('7941e722-bf34-4c2c-91c0-3222ff6c3f5b', federation_membership_kernel__member_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('7941e722-bf34-4c2c-91c0-3222ff6c3f5b', federation_membership_kernel__welfare_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('7941e722-bf34-4c2c-91c0-3222ff6c3f5b', foundational, free_movement_constitutive_right).
narrative_ontology:cs_axiom_status(free_movement_constitutive_right, holdable).
narrative_ontology:cs_axiom_grounding('7941e722-bf34-4c2c-91c0-3222ff6c3f5b', free_movement_constitutive_right, deontological).
narrative_ontology:cs_axiom('7941e722-bf34-4c2c-91c0-3222ff6c3f5b', foundational, expansive_teleological_interpretation).
narrative_ontology:cs_axiom_status(expansive_teleological_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('7941e722-bf34-4c2c-91c0-3222ff6c3f5b', expansive_teleological_interpretation, instrumental).
narrative_ontology:cs_reference_frame('7941e722-bf34-4c2c-91c0-3222ff6c3f5b', ever_closer_union_completion).
narrative_ontology:cs_drift_state('7941e722-bf34-4c2c-91c0-3222ff6c3f5b', post_brexit_constrained_integration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7941e722-bf34-4c2c-91c0-3222ff6c3f5b', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, multinational_service_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, sending_state_governments).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_fiscal_authorities).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, host_sector_trade_unions).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_service_dependents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, european_commission).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, eu_citizenship_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, supranational_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the treaty provisions on movement of persons and equal treatment. Its case law — extending market-access reasoning to economically inactive residents, limiting national benefit distinctions, and reviewing strikes against posted-work arrangements — sets the operative scope of mobility rights, and national rules conflicting with its readings are struck down. It sits above the national legal orders it reviews, answers to no electoral constituency, and its docket and doctrinal reach grow with every dispute routed to it.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, ecj_supranational_court, agenda_setter,
    institutional, generational, constrained, continental).

% Initiates infringement actions against member states that restrict movement, proposes the mobility-related legislation (posted workers, social-security coordination), and administers cohesion funds that partially offset adjustment costs in receiving and sending regions. Each widening of enforceable mobility enlarges the body of rules it polices and the portfolio it manages.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, european_commission, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, european_commission, beneficiary).

% Citizens who relocate across member states for work, study, or retirement. They gain access to other members' labor markets on equal-treatment terms, portable pension and social-security records, and secure residence. Their practical experience depends on qualification recognition and host-state administrative cooperation; they owe the communities they leave little that national tax rules do not already reach.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Firms in construction, care, agriculture, logistics, and hospitality staff operations from a continent-wide labor pool. Posted-work arrangements let them execute host-state contracts on home-country payrolls; the elastic labor supply disciplines wage demands in exposed sectors. They coordinate politically through employer federations seated in Brussels.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, multinational_service_employers, beneficiary,
    powerful, biographical, arbitrage, global).

% Lower-wage workers in receiving regions compete with incoming labor in construction, food processing, care work, and seasonal agriculture. Wage and hours effects concentrate in specific trades and towns. Moving themselves is possible but costly, anchored by housing, family, and local networks. Their political voice runs through national channels that the supranational adjudicative layer does not directly weigh.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_labor, payer,
    powerless, biographical, constrained, regional).

% Budget authorities in destination states fund schooling, health registration, housing support, and social assistance for arriving households ahead of and beyond what those households contribute in taxes; coordination regulations allocate liability between systems but no fiscal transfer follows population automatically. Tightening eligibility triggers infringement proceedings rather than negotiation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_fiscal_authorities, payer,
    institutional, generational, constrained, national).

% Sectoral unions in high-coverage bargaining systems (construction, transport, cleaning) watched posted-work arrangements undercut negotiated rates after the 2004 enlargements, and adverse rulings held that strike action against such arrangements could violate establishment freedoms. Their bargaining model presumes national coverage that erodes at the edges; their organizational identity is fused with that model and with the Ghent-style financing systems built on it.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, host_sector_trade_unions, payer,
    organized, biographical, identity_locked, national).

% Governments of newer member states financed the education and training of clinicians, engineers, and tradespeople who then relocated westward. They receive remittances, relief from short-term labor-surplus pressure, and an emigration valve, while regional health systems operate below safe staffing densities. They negotiate transitional safeguard periods at each enlargement round and lobby for cohesion transfers that only partly follow the departed.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, sending_state_governments, beneficiary).

% Patients, elderly residents, and rural communities in high-emigration regions depend on public services thinned by clinician and specialist departures. They hold no seat in mobility or enlargement negotiations at either national or Union level; their situation surfaces mainly through domestic health-crisis reporting.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_service_dependents, payer,
    powerless, immediate, trapped, regional).

% Non-EU nationals resident in the Union are governed by far narrower mobility and equal-treatment rules than EU citizens performing identical work alongside them. They would contest a regime that allocates movement rights by membership category, but the debates that maintain the regime proceed without their participation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, third_country_nationals, excluded,
    powerless, biographical, trapped, continental).

% Scholars of federalism, migration economics, and welfare-state theory track the distributional record of intra-EU mobility — fiscal-incidence studies, displacement estimates, health-staffing data — publishing outside the advocacy coalitions of any seat.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, federal_polity_analysts, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__integration_reading, multinational_service_employers).
narrative_ontology:fixing_cost_class(federation_membership_kernel__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches labor to demand across twenty-seven national labor markets without internal borders; makes qualifications and social-security records portable; and gives citizens an enforceable right to take up residence and work in any member state on equal-treatment terms — a continent-scale allocation problem no single national system could solve alone.
% TRANSFER_FUNCTION: Moves workers, skills, and attached welfare entitlements from sending to receiving regions; moves wage-bargaining leverage in exposed sectors from host-state collective institutions toward mobile employers; and moves adjudicative authority over labor-market rules from national parliaments and courts to Luxembourg. The associated costs — competitive pressure on incumbents, unfunded service loads, depleted clinical staffing — land on host-state workers and budgets and on sending-state services, with little automatic compensation in either direction.
% ABSENT_VOICES: Third-country nationals would contest membership-graded mobility rights but are outside the conversation entirely. Sending-state service dependents have no procedural seat in enlargement or mobility negotiations. Incumbent low-wage workers in receiving regions speak only through national channels that the supranational adjudicative layer is not obliged to weigh before striking national rules down.
% DISAPPEARANCE_RATIONALE: Destination-state health systems that staff large fractions of clinical workforces from intra-EU migration, remittance-dependent households, cross-border service supply chains, and the legal status of several million residents would all require immediate rearrangement; border posts, permits, and bilateral quota negotiations would re-emerge within months, as the Brexit withdrawal process previewed in miniature.
% FOUNDING_PROBLEM: After two continental wars, the founders sought to make another war materially impossible by fusing Europe's economies: the 1957 Treaty of Rome made free movement of workers one of the four foundational freedoms, both to complete the common market and to dissolve the nationalist mobilization potential of closed-off populations.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: the treaty text and the Spaak Report record the founding rationale; OECD analyses and national statistical offices document persistent sectoral shortages and demographic aging that keep the labor-allocation problem live; host-state trade unions and receiving-state audit offices independently attest the cost side of the ledger. No part of this attestation relies solely on the Commission or on mobile-worker advocacy organizations.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.58 reflects a regime whose coordination is real but whose costs are asymmetrically placed: four declared payer groups (competitive pressure on incumbent low-wage labor, uncompensated service loads on receiving budgets, eroded bargaining coverage, depleted sending-state services), with costs transmitted through market channels and unfunded mandates rather than by design — no seat engineered the regime to fleece anyone, which keeps it below snare territory despite substantial extraction. Suppression 0.48 is authored as a raw structural property (unscaled; the engine scales only extractiveness): treaty primacy removes national legislative exit, individuals hold no exit at all, and the one demonstrated state exit (withdrawal) was feasible but ruinously costly — coercive enough to sustain enforcement, short of overt repression. Theater ratio 0.17: enforcement activity is overwhelmingly functional (infringement actions, mutual-recognition administration); the mild theatrical component is citizenship rhetoric outrunning the social dimension actually delivered. Accessibility collapse 0.38: alternatives remain legible after full understanding of the regime — EEA-style association, Swiss-style bilateralism, safeguard clauses, and coordination redesigns — so understanding does not collapse exits. Resistance 0.62: sustained and organized — a member-state exit, permanent opt-outs, referenda on quotas, transitional-clause fights at every enlargement, and constitutional-court challenges to primacy. The temporal series run on one shared seven-point grid so every tracked metric is authored at every examined time point; the suppression_requirement hump (peak near t=48) traces the enforcement ratchet around the 2004 enlargement and the adverse bargaining-rights rulings, easing slightly as proportionality review softened in later case law. Coalition note: the powerless displaced-local-labor seat has a theoretical coalition partner in host-sector unions, but the insider/outsider split — unions defended covered incumbents while posted workers fell outside coverage — has kept that coalition fragile.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (Court, Commission) experience the arrangement as a rights-completion project: each expansive ruling extends a constitutive entitlement and enlarges the docket that justifies the institution. The payer seats compute the same structure as enforced cost-bearing: displaced labor faces competition it did not consent to, fiscal authorities absorb liabilities they cannot renegotiate, unions defend a coverage model the rulings erode, and sending-state dependents lose clinicians to a market they never entered. The dual-positioned sending-state governments straddle the gap — paying in human capital while collecting remittances and pressure-relief. Even at the same nominal institutional power level, receiving-state fiscal authorities and sending-state governments diverge sharply because their exit options and net positions differ; the engine computes these per-seat classifications from the declared structural data, and the divergence between the integration seat's rights-framing and the payer seats' cost-bearing is the measurement this story exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers sit near the beneficiary end (subsidized by the arrangement, mobile exit); multinational employers sit nearest it (beneficiary plus arbitrage-grade relocation of production and payroll). Payers sit near the target end, amplified by constrained or trapped exit — trapped sending-state dependents furthest of all. Sending-state governments resolve mid-range through their dual declaration (payer with secondary beneficiary position), capturing the remittance-and-valve offset against human-capital loss. No directionality_overrides are used: the derivation chain separates the seats adequately from declared roles plus exit atoms, and the override surface keys on power atoms rather than agent names — a correction aimed at, say, the Court would misapply to receiving-state fiscal authorities sharing the institutional atom, so structural differentiation rides on role, secondary role, and exit options instead. The Court's authority-accrual tilt is expressed through its agenda_setter role rather than a numeric override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — dissolving the conditions of inter-state war through economic fusion — largely succeeded and is now historical; the arrangement persists on a successor function (continent-scale labor allocation under demographic aging) that is live and externally corroborated, so the mandate has not been outlived and no mandatrophy-resolved flag is declared. The classification discipline cuts both ways here. Reading the regime as pure rights-coordination (rope) would erase the four payer seats and the uncompensated-cost structure; reading it as pure extraction (snare) would erase the genuine allocation function that no alternative currently performs and that disappearance analysis shows the world depends on. The tangled_rope claim holds both facts in view: enforcement is required (treaty primacy, infringement machinery), beneficiaries and payers are both named, and the extraction rides on top of coordination rather than replacing it. No sunset clause exists — the regime claims permanence, which blocks scaffold classification despite its visibly transitional early decades (the 1957-1993 phase operated under explicit transitional controls that lapsed rather than being sunsetted). Low theater blocks piton: the enforcement work is load-bearing, not performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_membership_reading_contest,
    'This constraint is the integration reading of the federation_membership_kernel; what structural differences would consolidation of the member_sovereignty_reading or welfare_coordination_reading produce?',
    'Track which reading''s premises consolidate in subsequent treaty revision, accession conditionality, and ECJ proportionality doctrine; the reading whose victim set the operative rules come to track becomes the operative constraint.',
    'Under the member_sovereignty reading the victim set expands to national welfare institutions themselves and the arrangement computes as actively enforced extraction against state capacity; under the welfare_coordination reading the uncompensated-cost payers drop out (coordination transfers replace unfunded mandates) and the arrangement trends toward pure coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_membership_reading_contest, conceptual, 'Committer structure: which reading of the membership kernel governs determines victim set, epsilon, and classification.').

omega_variable(
    net_fiscal_incidence_of_intra_eu_migrants,
    'Do receiving-state welfare systems bear net uncompensated costs from intra-EU migrants after counting their tax contributions and the age-selective composition of flows?',
    'Longitudinal administrative-data fiscal-incidence studies replicated across destination states (of the kind pioneered for the United Kingdom), distinguishing working-age contributors from inactive movers and children.',
    'If migrants are net contributors, the receiving_state_fiscal_authorities payer seat collapses and effective extraction falls toward the coordination floor; if net recipients, the payer seat strengthens and the arrangement''s extractive component deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_fiscal_incidence_of_intra_eu_migrants, empirical, 'Whether the uncompensated-welfare-cost victim claim survives fiscal-incidence measurement.').

omega_variable(
    brain_drain_net_externality,
    'Does outflow of trained clinicians and specialists harm sending states net of remittances, return migration, circular movement, and training-cost recovery?',
    'Staffing-density and health-outcome panels comparing high-emigration and matched low-emigration sending regions, controlling for pre-trend; remittance-flow accounting against public training expenditure.',
    'Determines whether sending_state_governments and sending_state_service_dependents hold genuine payer positions (externalized cost) or whether mobility functions as compensated exchange — shifting their directionality and the arrangement''s overall extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brain_drain_net_externality, empirical, 'Whether the brain-drain component of the victim structure is a net externality or an internally compensated flow.').

omega_variable(
    native_displacement_vs_complementarity,
    'Do intra-EU labor inflows displace incumbent low-wage workers in receiving regions or complement them through demand effects and task specialization?',
    'Meta-analytic reconciliation of area-based wage studies with occupational-cell panel designs and enlargement natural experiments (the 2004 accession as quasi-random variation across receiving regions).',
    'If complementarity dominates, displaced_local_labor''s payer position weakens sharply, its directionality falls, and measured resistance recodes as distributive-perception error rather than extraction response; if displacement is real and localized, the payer seat is confirmed and the coalition question with unions becomes live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(native_displacement_vs_complementarity, empirical, 'The displacement literature is genuinely unsettled; the strongest single payer claim in the story rests on it.').

omega_variable(
    cs_authority_framing_ambiguity,
    'Is the operative authority the ECJ as declared, or the primacy-and-effectiveness doctrine layered above the Court — a doctrine the Court itself created (Van Gend en Loos, Costa v ENEL) and that makes its rulings binding on national legal orders?',
    'Trace whether member-state constitutional-court counter-limit doctrines (ultra vires review) discipline the primacy doctrine or are absorbed by it; observe whether the Court can revise the doctrine that constitutes its own authority.',
    'Under the doctrine-as-authority framing, the constraint reads as a self-sealing authority structure whose extraction attribution shifts toward doctrine-maintenance itself, and the agenda-setting seat relocates from the Court to the doctrinal claim; the commitment-system pattern classification changes accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_framing_ambiguity, conceptual, 'Two coherent framings of the same authority structure produce different cs_pattern classifications.').

omega_variable(
    founding_motive_obsolescence,
    'Does the regime persist because the founding war-dissolution logic still binds elite commitments, or because the live labor-allocation function sustains it — and what happens to maintenance effort if the founding motive is fully retired from discourse?',
    'Elite interview and treaty-negotiation records tracking invocations of the founding motive, contrasted with demographic-shortfall projections that quantify the allocation need independent of it.',
    'If the founding logic is dead and allocation is live, the constraint stands on current function (consistent with the authored classification); if both fade, the theatrical component rises toward piton symptoms and the regime becomes maintained by inertia plus rhetorical performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_motive_obsolescence, conceptual, 'Genealogy of persistence: founding motive versus successor function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 0, 68).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__integration_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t12, federation_membership_kernel__integration_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement_basis(fede_tr_t12, observed).
narrative_ontology:measurement(fede_tr_t24, federation_membership_kernel__integration_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement_basis(fede_tr_t24, observed).
narrative_ontology:measurement(fede_tr_t36, federation_membership_kernel__integration_reading, theater_ratio, 36, 0.15).
narrative_ontology:measurement_basis(fede_tr_t36, observed).
narrative_ontology:measurement(fede_tr_t48, federation_membership_kernel__integration_reading, theater_ratio, 48, 0.16).
narrative_ontology:measurement_basis(fede_tr_t48, observed).
narrative_ontology:measurement(fede_tr_t60, federation_membership_kernel__integration_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement_basis(fede_tr_t60, observed).
narrative_ontology:measurement(fede_tr_t68, federation_membership_kernel__integration_reading, theater_ratio, 68, 0.17).
narrative_ontology:measurement_basis(fede_tr_t68, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__integration_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t12, federation_membership_kernel__integration_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement_basis(fede_be_t12, observed).
narrative_ontology:measurement(fede_be_t24, federation_membership_kernel__integration_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement_basis(fede_be_t24, observed).
narrative_ontology:measurement(fede_be_t36, federation_membership_kernel__integration_reading, base_extractiveness, 36, 0.44).
narrative_ontology:measurement_basis(fede_be_t36, observed).
narrative_ontology:measurement(fede_be_t48, federation_membership_kernel__integration_reading, base_extractiveness, 48, 0.56).
narrative_ontology:measurement_basis(fede_be_t48, observed).
narrative_ontology:measurement(fede_be_t60, federation_membership_kernel__integration_reading, base_extractiveness, 60, 0.57).
narrative_ontology:measurement_basis(fede_be_t60, observed).
narrative_ontology:measurement(fede_be_t68, federation_membership_kernel__integration_reading, base_extractiveness, 68, 0.58).
narrative_ontology:measurement_basis(fede_be_t68, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__integration_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t12, federation_membership_kernel__integration_reading, suppression_requirement, 12, 0.26).
narrative_ontology:measurement_basis(fede_su_t12, observed).
narrative_ontology:measurement(fede_su_t24, federation_membership_kernel__integration_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement_basis(fede_su_t24, observed).
narrative_ontology:measurement(fede_su_t36, federation_membership_kernel__integration_reading, suppression_requirement, 36, 0.45).
narrative_ontology:measurement_basis(fede_su_t36, observed).
narrative_ontology:measurement(fede_su_t48, federation_membership_kernel__integration_reading, suppression_requirement, 48, 0.53).
narrative_ontology:measurement_basis(fede_su_t48, observed).
narrative_ontology:measurement(fede_su_t60, federation_membership_kernel__integration_reading, suppression_requirement, 60, 0.51).
narrative_ontology:measurement_basis(fede_su_t60, observed).
narrative_ontology:measurement(fede_su_t68, federation_membership_kernel__integration_reading, suppression_requirement, 68, 0.48).
narrative_ontology:measurement_basis(fede_su_t68, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'EU free movement' covers three structurally distinct constraints — readings of the federation_membership_kernel — that must not share one epsilon: the integration_reading (this file; movement as constitutive right, expansively adjudicated; moderate extraction riding on genuine coordination), the member_sovereignty_reading (movement bounded by national welfare capacity; higher extraction attributed to the supranational layer from the state-capacity seat), and the welfare_coordination_reading (coordination-with-autonomy; lowest extraction, victim set thinned by compensating transfers). The integration reading is upstream: its case law creates the mobility fact-pattern and cost distribution to which the sibling readings respond, which is why this file declares influence-bearing edges to both siblings. Each sibling file reciprocates with its own edges and documents how its epsilon differs and why.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
