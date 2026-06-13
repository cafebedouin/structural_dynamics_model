% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Federation Free Movement Doctrine (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The European Union's free-movement doctrine, in its integration-primary
 *   reading, treats labor mobility across member states as constitutive of
 *   the single market project: restrictions are presumptively illegitimate
 *   unless narrowly justified by public policy, public security, or public
 *   health. This reading prioritizes capital and skilled-labor mobility,
 *   economic efficiency, and deepening integration as the ultimate good. It
 *   benefits mobile workers, multinational firms, and cross-border service
 *   providers by guaranteeing labor-market access; it extracts from local
 *   labor markets in high-wage areas, welfare systems facing per-capita
 *   obligation regardless of prior contribution, and workers in declining
 *   regions who lack mobility capital and cannot compete with EU-wide wage
 *   arbitrage. The constraint operates through Court doctrine, treaty
 *   obligations, and national-law harmonization. Member states nominally
 *   consent to this reading through treaty accession but face political
 *   pressure from domestic constituencies bearing its costs, creating a
 *   structural gap between the integration-primary framing (free movement is
 *   constitutive) and the lived experience of place-bound workers
 *   (labor-market disadvantage is mandatory). This is ONE of three contested
 *   readings of the same underlying kernel; the sibling readings
 *   (sovereignty-primary and subsidiarity-balance) would reconfigure the
 *   beneficiary/victim structure and constraint type entirely.
 *
 * KEY AGENTS:
 *   - mobile_workers_eu: Beneficiaries with moderate power and biographical horizon; can exercise the mobility guarantee for career advantage; extract direct benefit without enforcement cost.
 *   - multinational_corporations: Institutional beneficiaries with arbitrage-grade exit; set corporate strategy to exploit continental labor arbitrage; collect cost savings and efficiency gains.
 *   - local_labor_markets: Victims with moderate power, generational horizon, constrained exit; face wage compression and employment displacement from inbound mobile workers; cannot restrict entry.
 *   - welfare_system_administrators: Victims with organized power, generational horizon, constrained exit; must provide equal access to mobile workers while funding systems from national tax bases; cannot gate by contribution history or residency.
 *   - workers_in_declining_regions: Powerless victims with biographical horizon, trapped exit; face labor-market competition from mobile workers but lack mobility capital to relocate; structurally excluded from policy voice.
 *   - member_state_governments: Institutional agenda-setters nominally enforcing the integration-primary reading; face political pressure from domestic constituencies; attempt work-arounds (posting directives, welfare-contribution gating) while formally honoring treaty obligations.
 *   - european_court: Institutional agenda-setter and authoritative interpreter; actively defends the integration-primary reading against national limitation attempts; narrows member-state derogation space.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.68).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.72).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Federation Free Movement Doctrine (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '67ea9389-0a2a-4480-90df-25b56354d78e').
narrative_ontology:cs_kernel_codification('67ea9389-0a2a-4480-90df-25b56354d78e', fixed_text).
narrative_ontology:cs_authority_grounding('67ea9389-0a2a-4480-90df-25b56354d78e', lineage).
narrative_ontology:cs_interpretation_layer_present('67ea9389-0a2a-4480-90df-25b56354d78e').
narrative_ontology:cs_reading_relation('67ea9389-0a2a-4480-90df-25b56354d78e', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('67ea9389-0a2a-4480-90df-25b56354d78e', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('67ea9389-0a2a-4480-90df-25b56354d78e', foundational, free_movement_constitutive_federation).
narrative_ontology:cs_axiom_status(free_movement_constitutive_federation, holdable).
narrative_ontology:cs_axiom_grounding('67ea9389-0a2a-4480-90df-25b56354d78e', free_movement_constitutive_federation, deontological).
narrative_ontology:cs_axiom('67ea9389-0a2a-4480-90df-25b56354d78e', foundational, restriction_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(restriction_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('67ea9389-0a2a-4480-90df-25b56354d78e', restriction_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('67ea9389-0a2a-4480-90df-25b56354d78e', unrestricted_continental_labor_market).
narrative_ontology:cs_drift_state('67ea9389-0a2a-4480-90df-25b56354d78e', contemporary_post_2015, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('67ea9389-0a2a-4480-90df-25b56354d78e', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers_eu).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, multinational_corporations).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, cross_border_service_providers).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, welfare_system_administrators).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, workers_in_declining_regions).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, economic_integration_theory).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, capital_mobility_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers with education, skills, or professional credentials who benefit from access to labor markets across member states without national discrimination. Can relocate for better wages, working conditions, or career advancement. Their mobility is guaranteed by the constraint and they collect direct benefit (wage arbitrage, career optionality, professional network expansion) without bearing enforcement costs.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers_eu, beneficiary,
    moderate, biographical, mobile, continental).

% Can locate production, services, and talent freely across borders. Benefits from labor cost differentials, supply-chain optimization, and access to skilled workers without relocation restrictions. Collects efficiency gains and cost advantages directly; sets corporate policy to exploit the mobility guarantee.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, continental).

% Professional service firms (consulting, law, accounting, engineering) that operate across borders and relocate talent to serve clients. Benefit from a unified professional labor market without licensing or residence barriers. Collect premium fees for cross-border expertise and operate as institutional agents of labor mobility.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, cross_border_service_providers, beneficiary,
    powerful, generational, arbitrage, continental).

% Regional and local labor markets in higher-wage or skill-intensive areas experience inward migration that compresses wages for low-skilled natives and changes employment composition. Areas with declining industries lose workers and tax base. Cannot restrict entry based on national labor-market protection; must accept the competitive pressure from unrestricted supply of EU-mobile workers as the cost of federation membership.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_markets, payer,
    moderate, generational, constrained, regional).

% Operate redistributive systems (unemployment insurance, family benefits, housing support, healthcare) funded by national tax bases. Must provide equal access to mobile EU workers on the same terms as nationals, absent narrow derogations for public-policy grounds. The constraint limits their ability to gate welfare access by contribution history or residency, thus facing fiscal pressure from population mobility they cannot control.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, welfare_system_administrators, payer,
    organized, generational, constrained, national).

% Low-skilled workers in regions experiencing economic decline, deindustrialization, or agricultural contraction. Face wage competition from mobile EU workers entering their labor markets, cannot restrict that competition via national policy, and lack the mobility capital (language, credentials, networks) to exercise the same freedom to relocate that mobile workers enjoy. Often lack voice in the policy debates about free movement; their interests are structurally excluded from the beneficiary framing.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, workers_in_declining_regions, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, workers_in_declining_regions, excluded).

% Formally commit to the integration-primary reading through treaty accession and must enforce it through national law and policy. In practice, many attempt to work around the constraint (posting directives for temporary workers, welfare-contribution gating, housing restrictions) to protect domestic labor markets and welfare systems, but cannot overtly reverse the treaty commitment without renegotiation or exit. Administer the enforcement infrastructure while bearing political pressure from domestic workers and regional governments.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Enforces the integration-primary reading through case law, striking down national restrictions unless they meet narrow derogation tests (public policy, public security, public health). Declares the scope of free-movement guarantees and narrows member-state flexibility. Functions as the authoritative interpreter of the constraint and actively defends the mobility guarantee against national limitation attempts.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, european_court, agenda_setter,
    institutional, generational, analytical, continental).

% Elected representatives responsive to local constituencies, many of which bear costs (labor-market pressure, welfare-system strain, cultural-integration concerns) from unrestricted free movement. Cannot legislate protective restrictions for their constituencies without breaching treaty obligations, leaving them structurally excluded from the policy space where the constraint's asymmetries could be rebalanced. Their constituents face the constraint; the parliaments cannot respond to grievances within the institutional architecture.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_parliaments, excluded,
    organized, generational, constrained, national).

% The body of legal doctrine, institutional practice, and political narrative that treats free movement as constitutive of federation legitimacy. Benefits from the constraint's persistence because the constraint vindicates the doctrine (circular: the doctrine legitimizes the constraint, the constraint's operation proves the doctrine right). Not an actor, but a framework the integration-primary reading depends on.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, integration_doctrine_apparatus, beneficiary,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(federation_membership_treaty__integration_primary, integration_doctrine_apparatus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__integration_primary, multinational_corporations).
narrative_ontology:fixing_cost_class(federation_membership_treaty__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified labor market across member states: workers can locate where their skills are most valued, firms can source talent without immigration bureaucracy, and professional services can cross borders. Solves the collective problem of fragmented labor supply and demand by removing transactional barriers to matching.
% TRANSFER_FUNCTION: Transfers labor-market advantage and welfare access from workers trapped in low-opportunity regions and from local labor-market protections (capacity to restrict wage competition) to mobile workers and multinational firms. Moves opportunity from place-bound populations to skill-mobile or capital-backed actors. Moves fiscal burden of welfare systems from national gatekeeping to per-capita obligation regardless of contribution history.
% ABSENT_VOICES: Low-skilled workers in declining regions who lack mobility capital; local trade unions in higher-wage areas experiencing inward wage pressure; regional governments bearing the welfare and housing costs of mobility; national parliaments representing constituencies that bear the constraint's costs but cannot legislate remedies. These voices exist but are structurally excluded from the beneficiary coalition and have limited effective voice in treaty renegotiation.
% DISAPPEARANCE_RATIONALE: If the free-movement constraint vanished and member states recovered control over labor immigration, national labor markets would reorganize around new borders, wage structures would reflect national supply/demand rather than EU-wide arbitrage, welfare systems could implement residency-based access controls, and regional economic divergence might accelerate as some areas lose the inflow of mobile capital and skilled workers that the constraint drives.
% FOUNDING_PROBLEM: Post-WWII European integration required breaking down national economic barriers to reduce interstate conflict and increase prosperity. Fragmented labor markets left workers trapped in low-opportunity regions and firms unable to access continental talent pools. Free movement was adopted as the mechanism to create a unified economic space where human capital could allocate efficiently, deepening economic integration as a political hedge against nationalism.
% FOUNDING_PROBLEM_CORROBORATION: The Court and integration-advocacy institutions attest the founding problem remains live: continued divergence in regional opportunity and the risk that protectionism would fragment the market. Member states with declining labor markets and unions in high-wage areas attest the founding problem is substantially solved and free movement persists as a constraint on their ability to protect domestic workers and welfare systems. Independent economic research confirms wage-compression effects in mobile-receiving countries and labor-market dislocation in declining regions, supporting the contested reading.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.15 in 1957 (when free movement existed but labor mobility was low due to language barriers, credential non-recognition, and low welfare-magnet effects) to 0.68 by 2024 (after Eastern expansion, professionalization of service sectors, and integration of welfare systems made the constraint operationally high-consequence). Theater_ratio remains modest (0.28) because the constraint's function—allocating labor efficiently across borders—is real and operationally continuous; the theatrical element is the framing of necessity and inevitability (the claim that free movement is CONSTITUTIVE rather than contingent). Suppression grows from 0.22 to 0.72, reflecting the Court's intensifying enforcement against national restrictions and member states' inability to respond to domestic labor-market pressure. Accessibility_collapse reaches 0.81 by 2024: once workers understand free movement is guaranteed, they can treat the EU as a single labor market, collapsing the alternative of staying in low-opportunity regions (the cost of leaving rises as a relocation move, not as restriction). Resistance remains substantial (0.59 at interval end) because populist movements, regional governments, and trade unions in declining areas mount continuous political pressure against the constraint's effects, even though their formal ability to change it is blocked by treaty obligation. The grid tracks that suppression is highest at the structural level (0.82 by 2024)—the treaty architecture itself blocks national remedies—while resistance is distributed across individual, organizational, and class levels, indicating lived grievance despite institutional inability to remedy.
 *
 * PERSPECTIVAL GAP:
 *   The integration-primary reading computes as tangled_rope from the Court and institutional perspective (real coordination function + asymmetric extraction maintained by enforcement), but computes as snare from the victim seats (labor-market protection is suppressed, the coordination story is cover for capital/skill mobility advantage, resistance is muted by treaty architecture). The beneficiary seats compute as rope or even mountain (inevitable, natural coordination). This divergence is structural, not observational: the constraint is the same, but the seats' structural relationships to it differ radically. The payer seats cannot exit, cannot legislate remedies, and bear distributed costs (wage pressure, welfare strain, regional decline). The beneficiary seats collect concentrated gains (wage arbitrage, talent access, cost savings) and can invoke the coordination story to delegitimize protective restriction. This is the mandatrophy test: the constraint claims coordination (rope), achieves it partially, but the extraction—labor-market advantage for mobile actors, welfare burden for immobile systems—is active and structural, not incidental. The claim/metric gap is intentional: the claimed type (tangled_rope) reflects the constraint's actual structure (genuine coordination + active extraction); the authored metrics (e.g., suppression 0.72) document the extraction intensity; the divergence between claim and beneficiary positions (who experience it as rope or even mountain) is exactly what the engine should detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Mobile_workers are declared beneficiaries with mobile exit (arbitrage-grade, can relocate to any EU labor market): derivation puts d~0.15 (near-beneficiary, structure subsidizes them). Multinational_corporations are beneficiaries with arbitrage exit (can source labor from any member state or subsidiary): d~0.08 (institutional arbitrage, minimal structural constraint). Cross_border_service_providers are beneficiaries with arbitrage exit: d~0.10. Local_labor_markets are declared victims with constrained exit (cannot restrict labor inflow): d~0.84. Welfare_system_administrators are victims with constrained exit (must provide equal access): d~0.76. Workers_in_declining_regions are victims with trapped exit (lack mobility capital, cannot relocate despite opportunity): d~0.91 (deepest target because trapped + victim). Member_state_governments are agenda-setters with constrained exit (can formally comply with treaty but face political pressure): d~0.62 (neither pure beneficiary nor victim; they administer the constraint but cannot escape domestic cost). No directionality overrides are warranted; the derivation captures the true structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—fragmented labor markets limiting European prosperity and deepening integration—was live in 1957 (extractiveness 0.15, low because few workers actually migrated). By 2024, the problem's solution is embedded (EU labor market is substantially integrated). But the constraint persists with increased extraction (0.68, +0.53 over 67 years) and intensified suppression (0.72, +0.50). This pattern is classic mandatrophy: the original coordination problem is largely solved, but the constraint has become a tool for extracting advantage (skilled-labor mobility, capital arbitrage) from place-bound actors whose interests the founding problem never centered. The theater_ratio rise (0.08→0.28) reflects the constraint's increasing reliance on framing—treating economic inevitability and integration-doctrine legitimacy as natural law, rather than defending the actual coordination benefit (which is real but modest). Member states nominally accept the constraint as foundational commitment, but the rise in suppression_requirement (0.22→0.72) indicates they spend increasing enforcement effort defending the constraint against national-level protective restrictions. The grid's rising suppression at all levels (especially structural level, 0.22→0.82) documents that the constraint requires increasing active defense, not passive acceptance—a hallmark of extraction-laden mandatrophy. The founding problem could be re-solved by open-skilled-labor policy with greater national autonomy over low-skilled-labor and welfare protection; the constraint persists because the extraction (capital/skill mobility advantage) is valuable to beneficiary interests and embedded in institutional (Court) authority. This is not false-summit—it is genuine mandatrophy: the constraint solved something real, but the solution now serves extraction more than coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_contingent,
    'Is free movement constitutive of federation (such that any significant restriction dissolves the federation''s legitimacy), or is it contingent on ongoing agreement among member states (such that negotiated restrictions are compatible with federation)?',
    'Institutional event: a formal renegotiation of free-movement terms that member states accept as legitimate federation-preserving modification (or conversely, a major Court ruling that permits significant member-state restrictions and the integration apparatus affirms this does not undermine federation). Empirical test: if the founding problem (labor-market fragmentation reducing prosperity) could be re-solved with greater national autonomy, does accepting that solution preserve federation legitimacy, or does the integration-primary doctrine treat any reversal as structural betrayal?',
    'If free movement is truly constitutive, the integration-primary reading is a mountain (unchangeable, natural to federation). If contingent, it is a tangled_rope (genuine coordination + extraction, renegotiable). This omega determines whether the constraint is immutable doctrine or negotiated balance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutive_vs_contingent, conceptual, 'Whether free movement is an intrinsic feature of federation or a negotiated institutional choice.').

omega_variable(
    labor_market_integration_necessity,
    'Is unrestricted labor mobility structurally necessary to solve the founding problem (labor-market fragmentation limiting prosperity), or could the founding problem be addressed via partial labor mobility (skilled workers, service providers) with protected local labor markets and welfare systems?',
    'Natural experiment or modeling: a jurisdiction that permits skilled-labor mobility but restricts low-skilled immigration and examines whether it preserves the efficiency gains of integration while reducing labor-market pressure on low-skilled workers. Econometric decomposition of the welfare gain from EU integration attributable to labor mobility vs. other channels (goods trade, capital mobility, knowledge spillover).',
    'If partial mobility solves the founding problem, the full free-movement constraint is extractive above coordination necessity; if full mobility is required, extraction and coordination cannot be separated. This affects whether the subsidiarity-balance reading (negotiated bounds) could functionally replace the integration-primary reading without loss of coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_integration_necessity, empirical, 'Whether unrestricted labor mobility is necessary for integration benefits or achievable with negotiated restrictions.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (member states prevented from legislating protective restrictions by Court doctrine and treaty obligation) or internalized (member states have internalized the integration-primary framing and no longer *want* to restrict, treating any restriction as illegitimate)?',
    'Reveal-preference test: if legal constraint were removed (treaty renegotiation) without abandoning integration, which member states would immediately legislate labor-market or welfare protections, and how strict? If suppression is structural, many would legislate protections; if internalized, few would, showing the framing is efficacious.',
    'If suppression is structural, the constraint is sustained by enforcement and would weaken upon constraint removal; if internalized, the constraint has successfully re-shaped preferences and would persist even without formal enforcement. This affects whether computed extraction is reversible or locked in by preference change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of national labor-market restrictions is sustained by Court enforcement or by member-state preference alignment with integration doctrine.').

omega_variable(
    reading_coexistence_or_foreclosure,
    'Do the integration-primary and sovereignty-primary readings coexist as genuinely live positions held by different actors, or does the integration-primary reading logically foreclose sovereignty-primary within the federation framework?',
    'Institutional precedent: has the Court ever held that member-state labor-market restrictions are compatible with federalism, or does the doctrine treat all non-derogation restrictions as anti-federal? Empirical: do member states continue to assert sovereignty-primary claims as live positions, or have they abandoned them as incoherent with federation?',
    'If integration-primary forecloses sovereignty-primary, the kernel contest is decided within the framework and the constraint is doctrine-driven mandatrophy. If they coexist, the kernel remains contested and the constraint is negotiated institutional balance. This affects whether the constraint is immutable or renegotiable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_or_foreclosure, conceptual, 'Whether the integration-primary reading logically excludes or permits coexistence with the sovereignty-primary reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 1957, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1957, federation_membership_treaty__integration_primary, theater_ratio, 1957, 0.08).
narrative_ontology:measurement_basis(fede_tr_t1957, observed).
narrative_ontology:measurement(fede_tr_t1986, federation_membership_treaty__integration_primary, theater_ratio, 1986, 0.12).
narrative_ontology:measurement_basis(fede_tr_t1986, observed).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_treaty__integration_primary, theater_ratio, 2004, 0.18).
narrative_ontology:measurement_basis(fede_tr_t2004, observed).
narrative_ontology:measurement(fede_tr_t2015, federation_membership_treaty__integration_primary, theater_ratio, 2015, 0.24).
narrative_ontology:measurement_basis(fede_tr_t2015, observed).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_treaty__integration_primary, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(fede_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t1957, federation_membership_treaty__integration_primary, base_extractiveness, 1957, 0.15).
narrative_ontology:measurement_basis(fede_be_t1957, observed).
narrative_ontology:measurement(fede_be_t1986, federation_membership_treaty__integration_primary, base_extractiveness, 1986, 0.38).
narrative_ontology:measurement_basis(fede_be_t1986, observed).
narrative_ontology:measurement(fede_be_t2004, federation_membership_treaty__integration_primary, base_extractiveness, 2004, 0.52).
narrative_ontology:measurement_basis(fede_be_t2004, observed).
narrative_ontology:measurement(fede_be_t2015, federation_membership_treaty__integration_primary, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement_basis(fede_be_t2015, observed).
narrative_ontology:measurement(fede_be_t2024, federation_membership_treaty__integration_primary, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(fede_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1957, federation_membership_treaty__integration_primary, suppression_requirement, 1957, 0.22).
narrative_ontology:measurement_basis(fede_su_t1957, observed).
narrative_ontology:measurement(fede_su_t1986, federation_membership_treaty__integration_primary, suppression_requirement, 1986, 0.38).
narrative_ontology:measurement_basis(fede_su_t1986, observed).
narrative_ontology:measurement(fede_su_t2004, federation_membership_treaty__integration_primary, suppression_requirement, 2004, 0.51).
narrative_ontology:measurement_basis(fede_su_t2004, observed).
narrative_ontology:measurement(fede_su_t2015, federation_membership_treaty__integration_primary, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement_basis(fede_su_t2015, observed).
narrative_ontology:measurement(fede_su_t2024, federation_membership_treaty__integration_primary, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(fede_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1957, tn=2024
narrative_ontology:measurement(fede_grid_01, federation_membership_treaty__integration_primary, accessibility_collapse(class), 1957, 0.22).
narrative_ontology:measurement(fede_grid_02, federation_membership_treaty__integration_primary, accessibility_collapse(class), 2024, 0.76).
narrative_ontology:measurement(fede_grid_03, federation_membership_treaty__integration_primary, accessibility_collapse(individual), 1957, 0.35).
narrative_ontology:measurement(fede_grid_04, federation_membership_treaty__integration_primary, accessibility_collapse(individual), 2024, 0.79).
narrative_ontology:measurement(fede_grid_05, federation_membership_treaty__integration_primary, accessibility_collapse(organizational), 1957, 0.28).
narrative_ontology:measurement(fede_grid_06, federation_membership_treaty__integration_primary, accessibility_collapse(organizational), 2024, 0.84).
narrative_ontology:measurement(fede_grid_07, federation_membership_treaty__integration_primary, accessibility_collapse(structural), 1957, 0.18).
narrative_ontology:measurement(fede_grid_08, federation_membership_treaty__integration_primary, accessibility_collapse(structural), 2024, 0.89).
narrative_ontology:measurement(fede_grid_09, federation_membership_treaty__integration_primary, resistance(class), 1957, 0.62).
narrative_ontology:measurement(fede_grid_10, federation_membership_treaty__integration_primary, resistance(class), 2024, 0.51).
narrative_ontology:measurement(fede_grid_11, federation_membership_treaty__integration_primary, resistance(individual), 1957, 0.68).
narrative_ontology:measurement(fede_grid_12, federation_membership_treaty__integration_primary, resistance(individual), 2024, 0.52).
narrative_ontology:measurement(fede_grid_13, federation_membership_treaty__integration_primary, resistance(organizational), 1957, 0.74).
narrative_ontology:measurement(fede_grid_14, federation_membership_treaty__integration_primary, resistance(organizational), 2024, 0.61).
narrative_ontology:measurement(fede_grid_15, federation_membership_treaty__integration_primary, resistance(structural), 1957, 0.55).
narrative_ontology:measurement(fede_grid_16, federation_membership_treaty__integration_primary, resistance(structural), 2024, 0.64).
narrative_ontology:measurement(fede_grid_17, federation_membership_treaty__integration_primary, stakes_inflation(class), 1957, 0.15).
narrative_ontology:measurement(fede_grid_18, federation_membership_treaty__integration_primary, stakes_inflation(class), 2024, 0.72).
narrative_ontology:measurement(fede_grid_19, federation_membership_treaty__integration_primary, stakes_inflation(individual), 1957, 0.12).
narrative_ontology:measurement(fede_grid_20, federation_membership_treaty__integration_primary, stakes_inflation(individual), 2024, 0.68).
narrative_ontology:measurement(fede_grid_21, federation_membership_treaty__integration_primary, stakes_inflation(organizational), 1957, 0.08).
narrative_ontology:measurement(fede_grid_22, federation_membership_treaty__integration_primary, stakes_inflation(organizational), 2024, 0.51).
narrative_ontology:measurement(fede_grid_23, federation_membership_treaty__integration_primary, stakes_inflation(structural), 1957, 0.09).
narrative_ontology:measurement(fede_grid_24, federation_membership_treaty__integration_primary, stakes_inflation(structural), 2024, 0.44).
narrative_ontology:measurement(fede_grid_25, federation_membership_treaty__integration_primary, suppression(class), 1957, 0.11).
narrative_ontology:measurement(fede_grid_26, federation_membership_treaty__integration_primary, suppression(class), 2024, 0.69).
narrative_ontology:measurement(fede_grid_27, federation_membership_treaty__integration_primary, suppression(individual), 1957, 0.14).
narrative_ontology:measurement(fede_grid_28, federation_membership_treaty__integration_primary, suppression(individual), 2024, 0.58).
narrative_ontology:measurement(fede_grid_29, federation_membership_treaty__integration_primary, suppression(organizational), 1957, 0.18).
narrative_ontology:measurement(fede_grid_30, federation_membership_treaty__integration_primary, suppression(organizational), 2024, 0.74).
narrative_ontology:measurement(fede_grid_31, federation_membership_treaty__integration_primary, suppression(structural), 1957, 0.22).
narrative_ontology:measurement(fede_grid_32, federation_membership_treaty__integration_primary, suppression(structural), 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__integration_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, regional_welfare_system_divergence).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, credential_recognition_and_licensing).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, labor_union_bargaining_power_eu).

% DUAL FORMULATION NOTE:
% The federation_membership_treaty kernel constrains three constraint stories corresponding to three contested readings: integration_primary (this story, free movement as constitutive), sovereignty_primary (free movement as conditional on member-state consent), and subsidiarity_balance (free movement bounded by proportionality). Each reading produces different beneficiary/victim structures, different extraction mechanics, and different type classifications—all three are derived from the same underlying treaty text and institutional practice, but the readings diverge on what principles authorize the treaty and whether the implementation is inevitable or negotiable. Stories must be kept distinct: each instantiates one reading's constraints as ε-invariant, without hedging across readings. The network links establish family relationship; consumers can examine how constraint structure shifts as the kernel is read differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__integration_primary, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
