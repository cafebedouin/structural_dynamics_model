% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration with Free Movement Rights
 *   domain: political_economy/federalism
 *
 * SUMMARY:
 *   The integration reading frames federation membership as irreversible
 *   incorporation into a supranational order where free movement is a
 *   constitutional right and national border enforcement is illegitimate.
 *   This reading instantiates a constraint that benefits mobile professional
 *   citizens and multinational employers by guaranteeing labor-market access
 *   across borders while extracting rents from local labor markets, displaced
 *   workers, and national welfare systems. The reading's core authority claim
 *   is that membership in the federation constitutionally entails surrendered
 *   border sovereignty—a claim contested by the sovereignty reading, which
 *   frames membership as a conditional treaty preserving national authority
 *   over borders. This story generates the integration reading only, as a
 *   single ε-invariant constraint; the sovereignty reading is a separate
 *   constraint story (federation_membership__sovereignty_reading) with its
 *   own ε and stakeholder structure.
 *
 * KEY AGENTS:
 *   - mobile_professional_citizens: benefit from unrestricted labor-market access across federation; high exit options (arbitrage)
 *   - multinational_employers: benefit from unified labor pools and reduced visa overhead; institutional power, mobile exit
 *   - supranational_authority: sets and enforces free-movement policy; claims irreversible membership and delegitimizes border control
 *   - local_labor_markets: suffer wage suppression and job displacement; powerless, trapped exit
 *   - displaced_workers: face wage competition without reciprocal mobility advantage; powerless, constrained exit
 *   - national_welfare_systems: bear expanded eligibility costs; constrained exit (membership cost)
 *   - national_governments: lose border-control authority; identity-locked exit (membership as irreversible)
 *   - sovereignty_reading_advocates: excluded from supranational adjudication by the integration reading's authority frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership__integration_reading, 0.52).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration with Free Movement Rights").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, '2354ecac-2866-4345-9879-7eeffc549a37').
narrative_ontology:cs_kernel_codification('2354ecac-2866-4345-9879-7eeffc549a37', formalized).
narrative_ontology:cs_authority_grounding('2354ecac-2866-4345-9879-7eeffc549a37', extraction).
narrative_ontology:cs_interpretation_layer_present('2354ecac-2866-4345-9879-7eeffc549a37').
narrative_ontology:cs_reading_relation('2354ecac-2866-4345-9879-7eeffc549a37', federation_membership__sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('2354ecac-2866-4345-9879-7eeffc549a37', foundational, membership_constitutionally_irreversible).
narrative_ontology:cs_axiom_status(membership_constitutionally_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('2354ecac-2866-4345-9879-7eeffc549a37', membership_constitutionally_irreversible, conventional).
narrative_ontology:cs_axiom('2354ecac-2866-4345-9879-7eeffc549a37', foundational, border_enforcement_illegitimate_constraint).
narrative_ontology:cs_axiom_status(border_enforcement_illegitimate_constraint, holdable).
narrative_ontology:cs_axiom_grounding('2354ecac-2866-4345-9879-7eeffc549a37', border_enforcement_illegitimate_constraint, deontological).
narrative_ontology:cs_axiom('2354ecac-2866-4345-9879-7eeffc549a37', secondary, free_movement_constitutional_right).
narrative_ontology:cs_axiom_status(free_movement_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('2354ecac-2866-4345-9879-7eeffc549a37', free_movement_constitutional_right, deontological).
narrative_ontology:cs_reference_frame('2354ecac-2866-4345-9879-7eeffc549a37', irreversible_supranational_integration).
narrative_ontology:cs_drift_state('2354ecac-2866-4345-9879-7eeffc549a37', contemporary_sovereignty_resurgence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2354ecac-2866-4345-9879-7eeffc549a37', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_professional_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, multinational_employers).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, supranational_authority).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, displaced_workers).
narrative_ontology:constraint_victim(federation_membership__integration_reading, national_welfare_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership__integration_reading, national_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain unrestricted labor-market access across all federation member states; can migrate to pursue employment, education, and career advancement without visa requirement or skill verification. Benefit from supranational legal protections treating their movement as a constitutional right. Face no border restriction beyond background checks identical to nationals.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_professional_citizens, beneficiary,
    moderate, biographical, arbitrage, global).

% Access a unified labor pool across jurisdictions without visa sponsorship cost or regulatory fragmentation. Can relocate workers between offices and subsidiaries across borders without immigration processing. Benefit from wage competition and specialized-skill arbitrage across regional markets.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, multinational_employers, beneficiary,
    institutional, generational, mobile, global).

% Enforces federation membership as irreversible integration: sets free-movement policy, adjudicates border disputes, overrides national labor-market restrictions, and de-legitimizes national border enforcement as violations of supranational constitutional right. Derives institutional authority from the claim that federation membership implies surrendered sovereign border control.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_authority, agenda_setter,
    institutional, generational, analytical, global).

% Experience wage suppression and job displacement from unrestricted labor in-migration. Cannot restrict labor inflow or adjust local wages upward without violating supranational free-movement law. Lack collective exit: the constraint operates on them through supranational enforcement, not through negotiation.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    powerless, biographical, trapped, regional).

% Face job competition from federation-wide labor supply without reciprocal mobility advantage (many lack skill or capital to migrate successfully). Lose rents from locational scarcity of labor. Have no voice in the free-movement rule's design or enforcement, though the rule directly determines their employment prospects.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, displaced_workers, payer,
    powerless, biographical, constrained, regional).

% Must extend social insurance and safety-net benefits to mobile federation citizens as a cost of membership, while losing fiscal capacity to invest in local labor-market training and place-based development. Bear the cost of migrant healthcare, education, and unemployment without offsetting revenue from the beneficiaries of labor arbitrage.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_welfare_systems, payer,
    organized, generational, constrained, national).

% Lost the legitimate power to enforce border control or labor-market regulation in the domain of intra-federation movement. Membership in the federation is presented as irreversible and incompatible with border sovereignty, trapping them in the constraint. Could theoretically exit the federation but that exit carries existential institutional, diplomatic, and economic costs.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_governments, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, national_governments, excluded).

% Argue federation membership is a conditional treaty relationship preserving national border legitimacy and labor-market governance. Are structurally excluded from supranational adjudication because the integration reading frames their position as illegitimate border protectionism. Cannot participate as co-authors of migration policy; their exclusion is maintained by the interpretive authority that this reading establishes.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, sovereignty_reading_advocates, excluded,
    institutional, generational, constrained, global).

% Examines the structural relationship between the claim that federation membership is irreversible integration and the distribution of gains and losses across labor markets, workers, and welfare systems. Records the tension between the beneficiary framing (constitutional right to move) and the victim framing (enforced wage competition without consent).
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__integration_reading, supranational_authority).
narrative_ontology:fixing_cost_class(federation_membership__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified labor-market access eliminates visa gatekeeping and reduces transaction costs for multinational employment. Allows efficient labor allocation by skill and comparative advantage across jurisdictions without state-by-state negotiation. Creates uniform citizenship rights and reciprocal legal protections for mobile citizens.
% TRANSFER_FUNCTION: Moves labor-market rents and locational scarcity premiums from local workers and regional labor markets to mobile professional citizens and multinational employers. Transfers fiscal burden of expanded welfare eligibility to national governments while concentrating employment gains to those with mobility capital (skills, capital, networks).
% ABSENT_VOICES: Displaced local workers, non-mobile citizens in high-immigration regions, and sovereignty-reading advocates (who would frame membership as conditional and border legitimacy as retained) are excluded from the supranational mechanism that enforces free movement. National labor unions and regional governments that might negotiate differentiated movement rules are structurally barred from co-authoring the constraint.
% DISAPPEARANCE_RATIONALE: If the integration reading's enforcement vanished overnight, national governments would immediately reimpose border controls and labor-market restrictions; wage pressure would ease in local labor markets; welfare systems would tighten eligibility to nationals; multinational employers would face visa costs and regional labor fragmentation. The supranational labor-market unification would collapse and be replaced by segmented national and bilateral labor regulation.
% FOUNDING_PROBLEM: Fragmented post-war European labor markets created inefficiency and reduced mobility for skilled workers after economic integration created cross-border firms and supply chains. Visa gatekeeping by national governments prevented efficient labor allocation and locked workers into lower-wage jurisdictions despite economic opportunities elsewhere.
% FOUNDING_PROBLEM_CORROBORATION: Supranational authorities and mobile-worker advocates attest the founding problem remains live: labor mobility is still constrained by language and credential barriers even within the federation. Displaced-worker advocates and national governments attest the founding problem is substantially solved by reduced-cost goods trade and that continued free movement now operates as rent extraction rather than efficiency gain; academic labor-economics studies outside the supranational authority show wage suppression in local markets correlates with intra-federation migration flows.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) measures the constraint's operation within this reading's own lights: it rents labor-market scarcity from local workers and regions, enforced through supranational override of national labor regulation. Suppressiveness (0.52) is moderate because the extraction is defended through legal authority (the constitution of federation membership), not purely coercive barriers—national governments retain nominal authority but find it overridden. Theater ratio (0.28, rising to plateaued plateau by t=25) reflects that the supranational apparatus increasingly performs legitimacy maintenance ('defending mobile citizens' rights', 'protecting constitutional principles') rather than functional coordination—the coordination benefit (reduced visa cost) is real and front-loaded; the theatrical activity (adjudicating border-enforcement disputes, overriding national labor policy) grows as the constraint matures. Accessibility collapse (0.71) is high: once the integration reading is accepted as constitutional, alternatives (negotiated labor agreements, bilateral visa regimes) become structurally illegitimate; the reading forecloses them as violations of supranational law. Resistance (0.74) is substantial and rising: displaced workers, national governments, and sovereignty-reading advocates actively contest the constraint's legitimacy, even as the supranational machinery enforces it.
 *
 * PERSPECTIVAL GAP:
 *   From the supranational-authority and mobile-citizen seats, the reading is internally coherent: membership entails free movement, borders are illegitimate constraints, the coordination is real. From the displaced-worker seat, the reading is extractive cover: the coordination benefit accrues to those who can move; the extraction is wage suppression that benefits distant employers while harming local workers who cannot escape. From the national-government seat, the reading contains a foreclosure claim (membership = irreversibility = no border authority) that feels coercive because membership itself was presented as a voluntary commitment with different implications at the time of accession.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile citizens and multinational employers are near the beneficiary end of directionality (d ~ 0.2–0.3): they gain labor-market access and arbitrage without bearing enforcement cost. Supranational authority sits at d ~ 0.1 (full beneficiary of the coordination function it manages). Local labor markets and displaced workers are at d ~ 0.9 (full targets: extraction flows from them, exit is suppressed). National welfare systems are at d ~ 0.75 (substantial targets: bear fiscal costs, constrained exit). National governments are at d ~ 0.85 (targets whose nominal authority is overridden). The sovereignty-reading advocates are effectively excluded (not directly extracted from, but their voice is suppressed by the supranational authority claiming legitimacy).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids simple mandatrophy because the founding problem (visa gatekeeping reducing labor allocation efficiency) remains contested: supranational authorities and mobile-worker advocates point to residual barriers (credential recognition, language, practice-dependent professional licensing); displaced-worker and national-government advocates point to empirical labor-market suppression in host regions, suggesting the founding efficiency problem is solved and the constraint now operates as rent extraction. The classification resolves as Tangled Rope because (1) genuine coordination function exists (reduced transaction costs for intra-federation employment); (2) asymmetric extraction exists (rents flow from local labor to mobile workers and multinational employers); (3) active enforcement exists (supranational adjudication overriding national labor regulation). If the founding problem is dead (visa barriers solved by credential harmonization and EU recognition treaties), the classification would shift toward Snare under prolonged measuring (Piton if the supranational apparatus became mostly theatrical). The present measurement captures the constraint in a state where the coordination and extraction are still entangled and both function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Has the founding problem—visa gatekeeping reducing efficient labor allocation—actually been solved by credential harmonization and reciprocal recognition treaties, making the free-movement rule now mostly extractive?',
    'Comparative labor-market analysis of visa-time costs, credential-recognition barriers, and wage premiums in federated vs. bilateral-treaty regions. Controlled comparison of labor-allocation efficiency with vs. without free movement, holding economic integration constant.',
    'If solved, the constraint reclassifies from Tangled Rope (coordination + extraction) toward Snare (extraction cover) or Piton (coordination atrophied, enforcement now theatrical). If unsolved, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether founding coordination problem persists or has been superseded.').

omega_variable(
    membership_irreversibility_claim,
    'Is federation membership genuinely irreversible, or do national governments retain meaningful exit options (withdrawal, renegotiation) even if costly?',
    'Historical analysis of federation-exit precedent and cost. Institutional analysis of whether membership terms are amendable by member consensus or supranational unilateral decree. Test whether national governments, if sufficiently threatened, would withdraw despite costs.',
    'If genuinely irreversible, national governments face identity-locked exit and bear high effective extraction (d → 0.9). If revocable at member discretion (even if costly), exit is constrained but not identity-locked; effective extraction lowers (d → 0.7–0.75). The classification holds either way, but directionality and theater dynamics shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(membership_irreversibility_claim, empirical, 'Whether membership is structurally irreversible or costly-but-revocable.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the integration reading logically foreclose the sovereignty reading within any single institutional framework, or do they represent genuinely coexistent positions?',
    'Formal analysis of the core premises: if integration = irreversible membership with surrendered borders, can the same institutional framework simultaneously hold sovereignty = conditional membership retaining borders? Or are these logically incompatible axioms?',
    'If foreclosed: the integration reading wins the kernel contest; the sovereignty reading becomes illegitimate by definition within this framework. The victorious reading''s authority becomes harder to contest. If coexistent: both readings remain live; the constraint''s legitimacy is contestable; supranational authority''s claim to override national border enforcement is disputed. This maps to the CS axiom relationships in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether integration and sovereignty readings are logically incompatible or both structurally holdable.').

omega_variable(
    supranational_authority_legitimacy,
    'What grounds the supranational authority''s legitimacy claim—democratic representation, constitutional delegation, institutional tradition, or extraction benefit?',
    'Genealogy of the supranational authority''s founding mandate and amendment power. Survey of member-state consent to its border-authority override. Institutional-history analysis of whether authority expanded beyond originally delegated scope.',
    'If grounded in democratic representation or constitutional delegation with member consent: the authority''s enforcement is legitimate (low theater ratio, suppression is justified). If grounded in institutional tradition or extraction benefit: legitimacy is contested; theater rises as the authority performs legitimacy maintenance rather than functional governance; resistance should increase. Feeds the theater_ratio interpretation and the reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_authority_legitimacy, conceptual, 'What grounds the supranational authority''s claim to override national labor regulation.').

omega_variable(
    alternative_reading_authority,
    'Is the sovereignty reading structurally excluded from supranational adjudication, or merely losing inter-reading competition?',
    'Institutional analysis: can a national government present a sovereignty-reading argument in supranational courts and have it heard on its merits? Or does the integration reading''s authority frame make the sovereignty claim automatically illegitimate (border-protectionist, illegitimate constraint)? Historical record of sovereignty-reading advocates'' access to supranational mechanisms.',
    'If excluded: the integration reading''s authority enforces its own legitimacy by delegitimizing competing interpretations. This is self-reinforcing and highly extractive (suppression and theater both rise). If merely losing: the sovereignty reading remains a live competitor; supranational authority must justify its choice; extraction is more contested and less stable. Affects both suppression and directionality (whether national governments are actively suppressed or simply outvoted).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_authority, empirical, 'Whether sovereignty reading is structurally excluded from adjudication or competitively marginalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(fede_tr_t5, federation_membership__integration_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(fede_tr_t10, federation_membership__integration_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(fede_tr_t15, federation_membership__integration_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(fede_tr_t20, federation_membership__integration_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(fede_tr_t25, federation_membership__integration_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(fede_tr_t30, federation_membership__integration_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(fede_tr_t35, federation_membership__integration_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(fede_tr_t40, federation_membership__integration_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t5, federation_membership__integration_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fede_be_t10, federation_membership__integration_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(fede_be_t15, federation_membership__integration_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(fede_be_t20, federation_membership__integration_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(fede_be_t25, federation_membership__integration_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(fede_be_t30, federation_membership__integration_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(fede_be_t35, federation_membership__integration_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(fede_be_t40, federation_membership__integration_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__integration_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fede_su_t5, federation_membership__integration_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(fede_su_t10, federation_membership__integration_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(fede_su_t15, federation_membership__integration_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(fede_su_t20, federation_membership__integration_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(fede_su_t25, federation_membership__integration_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(fede_su_t30, federation_membership__integration_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(fede_su_t35, federation_membership__integration_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement(fede_su_t40, federation_membership__integration_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership__integration_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership__integration_reading, national_labor_market_regulation).
narrative_ontology:affects_constraint(federation_membership__integration_reading, welfare_system_fiscal_burden).

% DUAL FORMULATION NOTE:
% The federation_membership kernel decomposes into two structurally distinct constraints: this integration_reading (supranational authority legitimate, borders illegitimate, membership irreversible) and the sovereignty_reading (membership is conditional treaty, national authority retained, borders negotiable). The ε values differ sharply: integration_reading measures high extraction from local labor markets (0.68) because it imposes free movement as constitutional right without consent. The sovereignty_reading would measure extraction closer to coordination (ε ~ 0.35) because border restrictions are treated as legitimate negotiation. The two readings have incompatible core premises about membership irreversibility and border legitimacy, yet coexist as live positions held by different institutional actors. They are linked by network.affects_constraints because the legitimacy and enforcement of one directly shapes the resource availability and institutional standing of the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership__integration_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
