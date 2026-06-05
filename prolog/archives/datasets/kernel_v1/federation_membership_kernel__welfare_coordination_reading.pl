% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Welfare Coordination and Posted Worker Extraction
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The European Union's approach to free movement embodies a specific
 *   institutional reading of how federal labor mobility should coordinate
 *   with national welfare states. This constraint instantiates the
 *   welfare-coordination reading of a contested kernel:
 *   federation_membership_kernel, which also hosts two sibling readings
 *   (integration_reading and member_sovereignty_reading). The
 *   welfare-coordination reading posits that EU free movement operates
 *   through coordination of national welfare systems rather than
 *   supranational harmonization. Member states retain design autonomy over
 *   their welfare institutions while the EU enforces anti-social-dumping
 *   rules to prevent competitive degradation. Posted workers — temporarily
 *   deployed from lower-wage member states to higher-wage ones under a 2-year
 *   exemption from receiving-state social contributions — become a vector for
 *   cost-competition extraction: sending-state employers benefit from the
 *   wage differential; posting companies capture arbitrage rent;
 *   receiving-state labor markets face dual pressure (posted-worker
 *   undercutting of low-skill wages, plus displacement of permanent migrants
 *   from welfare-dependent sectors); posted workers themselves are trapped in
 *   a low-exit-cost temporary-visa status. The constraint exhibits tangled
 *   rope structure: genuine coordination function (labor-market flexibility,
 *   skill-gap filling) coexists with asymmetric extraction (posted-worker
 *   undercutting, sending-state public-service degradation). The
 *   welfare-coordination reading distinguishes itself from the
 *   integration_reading (which would harmonize welfare floors
 *   supranationally) and the member_sovereignty_reading (which would restrict
 *   free movement to protect national welfare and labor institutions). This
 *   story generates one constraint following the welfare-coordination reading
 *   alone.
 *
 * KEY AGENTS:
 *   - Posted Workers: Primary victims (powerless/trapped) — visa-dependent, contract-bound, welfare-access excluded for 2 years; experience maximum extraction through wage undercutting and remittance pressure
 *   - Sending-State Employers: Primary beneficiaries (institutional/arbitrage) — capture wage-cost differential and labor-deployment flexibility; operate within legal framework that preserves sending-state welfare autonomy
 *   - Posting Companies: Secondary beneficiaries (institutional/arbitrage) — coordinate cross-border deployment; benefit from anti-social-dumping predictability and welfare-coordination stability
 *   - Receiving-State Labor Markets: Mixed victims/beneficiaries (moderate/constrained) — benefit from labor-market flexibility and skill-gap filling; harmed by posted-worker undercutting in low-skill sectors
 *   - Receiving-State Governments: Powerful mixed actors (powerful/mobile) — benefit from labor flexibility and welfare-autonomy preservation; face fiscal pressure from permanent migration and labor-market segmentation
 *   - Sending-State Governments: Institutional beneficiary (institutional/arbitrage) — benefit from emigration as pressure-release valve and welfare-cost reduction; maintain welfare-system design autonomy
 *   - Receiving-State Labor Unions: Organized victims (organized/constrained) — harmed by posted-worker wage undercutting; constrained exit options (cannot unilaterally close borders)
 *   - Analytical Observer: Meta-perspective risk (analytical/analytical) — risks naturalizing the welfare-coordination reading as an immutable federal logic (false summit) rather than one contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.52).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.58).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Welfare Coordination and Posted Worker Extraction").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, '6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9').
narrative_ontology:cs_kernel_codification('6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9', formalized).
narrative_ontology:cs_authority_grounding('6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9', extraction).
narrative_ontology:cs_interpretation_layer_present('6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9').
narrative_ontology:cs_reading_relation('6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9', foundational, welfare_autonomy_preservation_necessary).
narrative_ontology:cs_axiom_status(welfare_autonomy_preservation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9', welfare_autonomy_preservation_necessary, deontological).
narrative_ontology:cs_axiom('6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9', foundational, labor_mobility_coordination_through_cost_differential).
narrative_ontology:cs_axiom_status(labor_mobility_coordination_through_cost_differential, holdable).
narrative_ontology:cs_axiom_grounding('6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9', labor_mobility_coordination_through_cost_differential, empirically_contingent).
narrative_ontology:cs_reference_frame('6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9', member_state_welfare_autonomy_with_labor_mobility).
narrative_ontology:cs_drift_state('6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9', contemporary_2024_posting_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6e0a72c9-c57a-4d3c-803a-db8b6b68c9b9', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, sending_state_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, posting_companies).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_institutional_framework).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_state_labor_markets).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_public_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POSTED WORKER (SNARE) — Trapped by visa dependency, contract-to-employer linkage, and inability to access receiving-state welfare during posting period. Suppression is near-total: no exit without losing legal status; no arbitrage available. Experiences maximum extraction through wage undercutting (posting wage floor ≈80% of receiving-state floor for 2 years), housing cost burden, and remittance pressure. Beneficiary framework (sending-state employers) organizes institutional mechanisms to prevent exit.
constraint_indexing:constraint_classification(federation_membership_kernel__welfare_coordination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: RECEIVING-STATE LABOR MARKET (TANGLED ROPE) — Genuine coordination function: free movement enables employer access to skilled workers, reduces job vacancy rates in sectors with structural shortages (healthcare, construction, hospitality). Simultaneous extraction: posted workers undercut wage floors; permanent migrants face displacement in low-skill sectors. Suppression exists (immigration restrictions, intra-EU mobility caps) but is not total — receiving states can regulate through posted worker directives and welfare-access rules. Mixed benefits and costs produce tangled rope rather than snare.
constraint_indexing:constraint_classification(federation_membership_kernel__welfare_coordination_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: SENDING-STATE GOVERNMENT (ROPE) — Experiences the constraint as pure coordination: free movement allows workers to exit unemployment, generating tax revenues from remittances, reducing public-service burden, and enabling social stability through labor export. The welfare-coordination reading (this reading) preserves sending-state design autonomy — the sending state can structure its welfare system independently and does not face supranational harmonization pressure. This is the beneficiary perspective: the constraint enables sending states to use emigration as a pressure-release valve while maintaining welfare autonomy. No exit option exists because the sending state IS the coordinating authority for its welfare design.
constraint_indexing:constraint_classification(federation_membership_kernel__welfare_coordination_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: POSTING COMPANY (ROPE) — Coordinates labor supply across borders via the posted-worker mechanism. Experiences the constraint as efficient coordination: deploys workers to labor-shortage sectors at lower cost than hiring locally, maintains workforce flexibility, and operates within the EU's legal framework. The 2-year posting window with reduced social contributions creates genuine arbitrage opportunity — legal differential in welfare-access rules. Benefits from anti-social-dumping rules (predictability, reduced competition from informal hiring) and from welfare-coordination preservation (no harmonization pressure that would eliminate the cost differential).
constraint_indexing:constraint_classification(federation_membership_kernel__welfare_coordination_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: RECEIVING-STATE GOVERNMENT (TANGLED ROPE) — Powerful actor with mobile exit options. Genuine coordination benefit: free movement provides labor-market flexibility and enables filling structural skill gaps without requiring supranational integration of welfare systems (the welfare-coordination reading preserves receiving-state design autonomy). Simultaneous extraction pressure: posted workers compete with domestic labor in low-skill sectors; permanent migrants reduce welfare-system generosity (fiscal pressure). The receiving state can exit through labor-market closure (EU mobility restrictions, sector-specific bans) but at cost of economic inefficiency and intra-EU political conflict. The constraint's enforcement (ECJ interpretation of freedom of movement + anti-social-dumping rules) preserves both mobility and welfare autonomy, making this tangled rope rather than snare or pure rope.
constraint_indexing:constraint_classification(federation_membership_kernel__welfare_coordination_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: RECEIVING-STATE LABOR UNION (TANGLED ROPE) — Organized collective with constrained exit. Genuine coordination benefit: free movement can expand the labor supply and reduce unemployment, enabling wage gains elsewhere in the economy. Extraction pressure: posted workers undercut union wage floors in construction, transportation, and hospitality sectors; this reduces bargaining power for unionized jobs. The constraint's enforcement (anti-social-dumping rules, minimum wage application to posted workers) provides some protection but not full compensation. Exit options are constrained: unions can pressure for sector-specific restrictions but cannot unilaterally close borders. Beneficiary and victim simultaneously — mixed extraction.
constraint_indexing:constraint_classification(federation_membership_kernel__welfare_coordination_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: EU INSTITUTIONAL FRAMEWORK (SCAFFOLD) — Temporary coordination structure with explicit sunset logic. The welfare-coordination reading treats the constraint as a transitional arrangement: member states coordinate national welfare systems (not harmonized supranational systems) while the EU enforces anti-social-dumping rules to prevent competitive degradation. The sunset is built into the reading itself: as labor-cost convergence proceeds (Eastern EU countries' wages rise toward Western averages), the posted-worker differential shrinks, reducing extraction and eliminating the need for anti-social-dumping enforcement. Estimated convergence timeline: 15-25 years (ongoing since 2004 accession). The EU sees this as a bounded, phase-specific mechanism enabling integration while protecting welfare-state institutions.
constraint_indexing:constraint_classification(federation_membership_kernel__welfare_coordination_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT RISK (MOUNTAIN) — From a civilizational perspective, labor mobility and welfare-state autonomy appear as immutable natural laws: labor always seeks highest-wage equilibria, and welfare systems cannot be harmonized across cultures with different preferences for redistribution. Free movement coordinates these 'natural' forces while preserving autonomy. However, this naturalizes what is actually a contingent institutional choice: the EU CHOSE to preserve member-state welfare design autonomy rather than create supranational welfare coordination. The integration_reading chooses differently. The member_sovereignty_reading chooses differently. The mountain perspective risks false summits — it treats the welfare-coordination reading's institutional design as inevitable rather than one contingent reading among others.
constraint_indexing:constraint_classification(federation_membership_kernel__welfare_coordination_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federation_membership_kernel__welfare_coordination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federation_membership_kernel__welfare_coordination_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits genuine extraction from posted workers (wage undercutting, welfare-access denial, visa-dependency trapping) and from receiving-state labor markets (low-skill wage depression, permanent-migrant displacement). However, extractiveness is not extreme because: (1) posting flows respond to genuine labor-shortage signals in construction, healthcare, and hospitality; (2) anti-social-dumping rules enforce minimum wage application to posted workers, reducing the extraction differential; (3) receiving-state governments retain considerable discretion over welfare access and labor-market regulation. Suppression (0.58): Moderate-high. Posted workers face near-total suppression (visa-dependency, contract-to-employer linkage, welfare-access exclusion); receiving-state labor markets face moderate suppression (immigration rules, intra-EU mobility caps, labor-market closure options); receiving-state governments face milder suppression (ECJ interpretation of freedom of movement prevents unilateral closure but still allows regulation). Theater ratio (0.41): Moderate-low. The welfare-coordination mechanism is substantially functional: it does coordinate labor flows and does enforce anti-social-dumping rules. However, there is performative content in the anti-social-dumping enforcement (member states often apply rules selectively) and in the claim that welfare coordination is genuinely 'coordinating' rather than 'allowing competitive cascading' (sending states lose tax revenue; receiving states face fiscal pressure; neither is fully optimizing welfare provision). The theater ratio is rising slightly over time (0.35 → 0.41 over 10 years) as enforcement of anti-social-dumping rules becomes more ritualistic and the actual posting flows diverge from the stated coordination rationale.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. Posted workers (powerless/trapped) see pure extraction (snare): they are trapped by visa dependency and welfare-access exclusion, with no arbitrage available. Posting companies (institutional/arbitrage) see pure coordination (rope): they are solving labor-shortage problems at lower cost. Receiving-state labor markets (moderate/constrained) and governments (powerful/mobile) see tangled rope: genuine benefits from flexibility coexist with real extraction costs. The analytical observer risks a false summit (mountain): the welfare-coordination reading appears as an immutable federal principle, naturalizing what is actually a contingent institutional choice. The constraint's classification as tangled_rope from the system-level analytical perspective reflects that this is a mixed-function mechanism with measurable extraction — not a pure coordination problem and not a pure extraction trap, but a deliberate institutional design that enables labor mobility while extracting rents from the most vulnerable agents (posted workers and receiving-state low-skill labor).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the extraction flow. Posted workers (d ≈ 0.95): fully trapped victims with no exit or arbitrage; derive maximum f(d) ≈ 1.42. Sending-state employers and posting companies (d ≈ 0.10): primary beneficiaries with arbitrage options; derive near-negative f(d) ≈ -0.01. Receiving-state labor (d ≈ 0.65): mixed — benefit from labor market flexibility, harmed by undercutting; moderate f(d) ≈ 1.00. Receiving-state governments (d ≈ 0.52): roughly symmetric (benefit from flexibility, harmed by fiscal pressure); f(d) ≈ 0.65. The chi formula χ = ε × f(d) × σ(S) applies scope modifier σ(continental) = 1.1, amplifying the effective extraction slightly beyond base ε. The result is χ ≈ 0.57 at the system level (tangled_rope threshold), with dramatic variation across perspectives: posted workers experience χ ≈ 0.73 (approaching snare); posting companies experience χ ≈ 0.51 (rope-to-tangled-rope boundary); receiving-state governments experience χ ≈ 0.63 (tangled-rope center).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the welfare-coordination reading is itself a contingent institutional choice, not an inevitable response to economic constraints. The constraint exhibits tangled-rope structure (mixed coordination and extraction) because the EU deliberately chose to preserve member-state welfare autonomy while enabling labor mobility, rather than choosing the integration_reading (supranational welfare harmonization) or the member_sovereignty_reading (labor-mobility restrictions to protect welfare and labor institutions). The mandatrophy question — is this coordination or extraction? — has the answer: it is both, by design. The welfare-coordination reading maximizes member-state autonomy and labor-market flexibility at the cost of extracting rents from posted workers and low-skill labor in receiving states. The three sibling readings would classify differently: integration_reading would show higher tangled_rope or snare classification (harmonizing welfare floors would reduce extraction); member_sovereignty_reading would show rope classification (prioritizing welfare protection over mobility). This constraint demonstrates that classification depends not on objective economic reality but on which institutional reading of the federation_membership_kernel is adopted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    posted_worker_surplus_extraction,
    'Does the posted-worker differential (2-year welfare-access exemption, reduced social contributions) represent genuine coordination cost or extractive rent-seeking by sending-state employers?',
    'Comparative analysis of posting flows and wage differentials: if flows follow labor-shortage signals (healthcare, construction, hospitality — genuine skill gaps), coordination interpretation supported; if flows concentrate in lowest-wage sectors regardless of skill demand, extraction interpretation supported. Causal analysis of employer posting decisions: do firms post workers because of skill availability or wage-cost arbitrage?',
    'If coordination cost: the constraint is correctly classified as tangled rope (mixed benefits). If extractive rent: the constraint should be reclassified as snare from receiving-state labor perspective (higher χ). Affects assessment of whether anti-social-dumping rules are sufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posted_worker_surplus_extraction, empirical, 'Whether posted-worker differential is coordination cost or extraction').

omega_variable(
    welfare_coordination_feasibility,
    'Is coordination of national welfare systems sustainable as EU membership expands to lower-wage, higher-social-need economies? Or does cost-pressure eventually force supranational harmonization (integration_reading) or member-state exit (member_sovereignty_reading)?',
    'Longitudinal analysis of welfare-system divergence and convergence pressures. Fiscal analysis: do receiving states'' welfare costs rise as permanent migration accumulates? Do sending states'' welfare revenues decline as working-age population emigrates? Political-economy analysis: do integration-reading and member_sovereignty_reading pressures accumulate faster than welfare convergence?',
    'If coordination proves sustainable: welfare-coordination reading is structurally stable. If pressures accumulate: the constraint drifts toward integration_reading (supranational harmonization) or member_sovereignty_reading (closure). Affects long-term classification and sunset plausibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_coordination_feasibility, empirical, 'Whether welfare coordination is sustainable long-term').

omega_variable(
    reading_contingency,
    'Is the welfare-coordination reading the outcome of structural-economic constraints, or a contingent EU institutional choice that could have been different?',
    'Historical-institutional analysis: what were the alternative readings debated during the Maastricht Treaty, Amsterdam Treaty, and Eastern enlargement negotiations? Which constituencies advocated for welfare-coordination vs integration_reading vs member_sovereignty_reading? What was contingent in the political process that selected welfare-coordination?',
    'If contingent: the reading is one interpretation among structurally viable alternatives; all three readings (welfare-coordination, integration, member_sovereignty) are conceptually coherent readings of the federation_membership_kernel. If structurally determined: the reading emerges necessarily from the economics of labor migration and welfare-state theory. Affects whether this constraint should be classified as mountain (inevitable) or tangled_rope (contingent institutional arrangement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contingency, conceptual, 'Whether welfare-coordination reading is contingent or structurally determined').

omega_variable(
    false_summit_naturalization,
    'Does the welfare-coordination reading risk naturalizing a contingent institutional choice as an immutable law of federal organization?',
    'Comparative institutional analysis: do other federal systems (US, Canada, Switzerland, Australia) preserve member-state welfare autonomy while enabling labor mobility? Or do they adopt integration_reading approaches (centralized minimum welfare floors) or member_sovereignty_reading approaches (labor mobility restrictions)? Cross-jurisdictional patterns indicate whether welfare-coordination is inevitable or contingent.',
    'If other successful federations use integration_reading or member_sovereignty_reading: the welfare-coordination reading''s mountain classification is a false summit — it naturalizes a choice that other societies make differently. If welfare-coordination appears across multiple successful federal systems: it may reflect structural constraints on federal welfare coordination. Affects whether Perspective 8 (analytical observer) should classify as mountain or should be reclassified downward as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, empirical, 'Whether welfare-coordination reading risks false summits').

omega_variable(
    sending_state_fiscal_externality,
    'Do sending states experience net fiscal benefit or net fiscal cost from the welfare-coordination reading? Are they compensated for loss of working-age taxpayers?',
    'Fiscal accounting: compare remittance inflows to sending states vs tax revenue losses from emigration and reduced public-service demands. Include opportunity cost: what would the emigrated workers'' tax contributions have been if employed domestically? Compare to welfare-coordination reading''s assumption that emigration enables sending states to maintain welfare autonomy by reducing public-service demand.',
    'If net fiscal benefit: sending-state government perspective (Perspective 3) is correctly classified as rope (pure coordination benefit). If net fiscal cost: the perspective should be reclassified as tangled_rope or snare (extraction downward). Affects assessment of whether the constraint is symmetric (coordination) or asymmetric (extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sending_state_fiscal_externality, empirical, 'Whether sending states experience fiscal benefit or cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_welf_tr_t0, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fed_welf_tr_t5, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(fed_welf_tr_t10, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 10, 0.41).

% Extraction over time
narrative_ontology:measurement(fed_welf_be_t0, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fed_welf_be_t5, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(fed_welf_be_t10, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fed_welf_su_t0, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(fed_welf_su_t5, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(fed_welf_su_t10, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, posted_worker_wage_depression_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, receiving_state_labor_market_segmentation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel: federation_membership_kernel. The integration_reading and member_sovereignty_reading are sibling constraints with different extraction profiles and institutional structures. All three readings govern the same EU institutions (freedom of movement, welfare access, labor protections) but interpret them differently. The welfare-coordination reading models the constraint as it currently operates (anti-social-dumping rules + member-state welfare autonomy). The integration_reading models the constraint under supranational welfare harmonization (higher welfare floors across member states, lower extraction from posted workers). The member_sovereignty_reading models the constraint under labor-mobility restrictions (lower posting flows, different victim set). Network links enable comparative analysis of how classification varies across readings of the same institutional kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
