% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__subsidiarity_balance, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Federation Membership Treaty: Subsidiarity Balance Between Free Movement and National Interest
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The subsidiarity balance reading operationalizes free movement within the
 *   European Union (and analogous federal migration regimes) as a constrained
 *   right: legitimate national interests in protecting labor markets, welfare
 *   systems, and public order permit mobility restrictions that are
 *   proportionate and non-discriminatory, but do not permit blanket
 *   exclusion. This constraint instantiates one reading of the contested
 *   kernel 'federation_membership_treaty,' which encompasses three
 *   structurally distinct legal positions: integration_primary (free movement
 *   is constitutive of the single market; restrictions are presumptively
 *   illegitimate), sovereignty_primary (free movement is conditional on
 *   member state consent; states retain broad authority), and
 *   subsidiarity_balance (the reading authoring this constraint: free
 *   movement operates within proportionality bounds). The subsidiarity
 *   reading is the jurisprudentially dominant position in EU migration law
 *   (Case law: Carpenter, Metock, Lassal; CJEU doctrine on public policy
 *   exceptions and proportionality review). However, tensions persist: member
 *   states continually test the boundary through welfare eligibility rules,
 *   labor market impact tests, and residency requirements. The constraint
 *   exhibits mixed coordination and extraction characteristics depending on
 *   the observer's structural position. From the perspective of high-skill
 *   mobile workers, it is pure coordination (rope). From the perspective of
 *   low-skill native workers, it is extraction (snare). From the
 *   institutional perspective of federation authorities, it is mixed
 *   (tangled_rope): both enabling labor market integration and constraining
 *   state capacity to protect welfare systems. The theater ratio (0.62)
 *   reflects that proportionality doctrine provides rhetorical legitimacy to
 *   restrictions whose real enforcement mechanisms are administratively
 *   diffuse and empirically soft (welfare eligibility technicalities,
 *   residency period counting rules) rather than bright-line movement bans.
 *   The constraint's evolution shows increasing theater over 30 years: as
 *   intra-federation economic integration has deepened, explicit mobility
 *   restrictions have become politically infeasible, so enforcement has
 *   migrated to welfare eligibility and secondary effects, creating
 *   appearance of protection without sharp mobility bars.
 *
 * KEY AGENTS:
 *   - High-Skill Mobile Workers: Primary beneficiary (institutional/arbitrage) — capture wage arbitrage and career advancement across member state labor markets; can arbitrage to non-federation opportunities if restrictions tighten
 *   - Low-Skill Native Workers in Exposed Sectors: Primary victim (powerless/trapped) — face wage suppression and employment displacement in sectors open to intra-federation mobility; no exit mechanism (retraining costly, relocation options limited, market participation mandatory)
 *   - Integrated Service Sectors (Construction, Healthcare, Agriculture): Secondary beneficiary (institutional/arbitrage) — benefit from labor supply expansion; can source workers from other member states without wage-setting constraints
 *   - Member State Labor Market Authorities: Mixed (institutional/constrained) — genuine coordination function (labor matching) and real extraction (fiscal costs, lost regulatory autonomy); cannot unilaterally exit treaty but can calibrate enforcement
 *   - Federation Integration Authority (CJEU, Commission): Institutional actor (institutional/constrained) — both enforces constraint and benefits from it; proportionality doctrine legitimates authority's role; exit option is institutional (treaty rewrite) rather than practical
 *   - Progressive Coalition (Trade Unions, Migrant Rights NGOs): Organized (organized/constrained) — see constraint as temporary with sunset condition (harmonization will eventually eliminate proportionality bounds); currently enforcing theater to maintain coalition credibility
 *   - Sovereignty-Centric State Governments: Institutional actor (institutional/arbitrage) — maintain restriction rhetoric but enforcement is theatrical; arbitrage to alternative trade relationships if federation becomes too constraining
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.38).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.48).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.38).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Federation Membership Treaty: Subsidiarity Balance Between Free Movement and National Interest").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '6ec3fe3d-a04a-448f-a1b6-094ff412ed14').
narrative_ontology:cs_kernel_codification('6ec3fe3d-a04a-448f-a1b6-094ff412ed14', formalized).
narrative_ontology:cs_authority_grounding('6ec3fe3d-a04a-448f-a1b6-094ff412ed14', lineage).
narrative_ontology:cs_interpretation_layer_present('6ec3fe3d-a04a-448f-a1b6-094ff412ed14').
narrative_ontology:cs_reading_relation('6ec3fe3d-a04a-448f-a1b6-094ff412ed14', federation_membership_treaty__integration_primary, influences).
narrative_ontology:cs_reading_relation('6ec3fe3d-a04a-448f-a1b6-094ff412ed14', federation_membership_treaty__sovereignty_primary, influences).
narrative_ontology:cs_axiom('6ec3fe3d-a04a-448f-a1b6-094ff412ed14', foundational, proportionality_constrains_state_discretion).
narrative_ontology:cs_axiom_status(proportionality_constrains_state_discretion, holdable).
narrative_ontology:cs_axiom_grounding('6ec3fe3d-a04a-448f-a1b6-094ff412ed14', proportionality_constrains_state_discretion, deontological).
narrative_ontology:cs_axiom('6ec3fe3d-a04a-448f-a1b6-094ff412ed14', foundational, graduated_constraint_appropriate_to_domain).
narrative_ontology:cs_axiom_status(graduated_constraint_appropriate_to_domain, holdable).
narrative_ontology:cs_axiom_grounding('6ec3fe3d-a04a-448f-a1b6-094ff412ed14', graduated_constraint_appropriate_to_domain, instrumental).
narrative_ontology:cs_reference_frame('6ec3fe3d-a04a-448f-a1b6-094ff412ed14', proportionality_doctrine_as_limiting_principle).
narrative_ontology:cs_drift_state('6ec3fe3d-a04a-448f-a1b6-094ff412ed14', contemporary_post_refugee_crisis_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6ec3fe3d-a04a-448f-a1b6-094ff412ed14', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, high_skill_mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, integrated_service_sectors).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, multinational_enterprises).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, local_labor_market_protection).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, welfare_system_fiscal_integrity).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, low_skill_resident_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-SKILL NATIVE WORKERS (SNARE) — Face wage suppression and employment displacement in sectors exposed to intra-federation mobility (construction, agriculture, personal services). No exit mechanism: retraining is resource-constrained, geographic relocation is costly, and EU labor market integration is mandated. Bear extraction in wages and employment probability while beneficiaries (high-skill mobile workers) capture gains. Suppression is structural: welfare reductions, labor market regulations, and limited political voice in setting mobility terms.
constraint_indexing:constraint_classification(federation_membership_treaty__subsidiarity_balance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MEMBER STATE LABOR MARKET AUTHORITIES (TANGLED ROPE) — Genuine coordination function: free movement enables labor mobility that reduces sectoral skill shortages and enables cross-border employment matching. Simultaneous extraction: states bear costs of welfare demand from mobile workers and lose fiscal capacity for welfare provision without ability to set unilateral eligibility rules. Exit option is constrained: states cannot reimpose hard borders without treaty violation, but can apply proportionality tests and public policy exceptions (limited leverage). Suppression is moderate: formal exit pathways exist (treaty amendment, Brexit precedent) but cost is very high.
constraint_indexing:constraint_classification(federation_membership_treaty__subsidiarity_balance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-SKILL MOBILE WORKERS (ROPE) — Experience the constraint as pure coordination: free movement enables geographic arbitrage for wage optimization, career advancement across member state labor markets, and access to sectoral clusters (tech hubs, financial centers). Arbitrage exit option: can relocate to non-member or third-country opportunities if federation becomes restrictive. Net beneficiary with minimal experienced extraction — the constraint solves their coordination problem at near-zero personal cost.
constraint_indexing:constraint_classification(federation_membership_treaty__subsidiarity_balance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FEDERATION INTEGRATION AUTHORITY (TANGLED ROPE) — Genuine coordination function: adjudicates proportionality tests, prevents race-to-the-bottom in welfare eligibility, and maintains single-market architecture. Simultaneous extraction: authority's institutional power derives from the treaty itself; its legitimacy depends on the treaty remaining the binding rule. The authority both enforces the constraint and benefits from its continuation (institutional survival). Exit option constrained: cannot unilaterally rewrite the proportionality doctrine without legitimacy collapse, but can calibrate enforcement intensity. Suppression moderate: formal amendment procedures exist but require consensus.
constraint_indexing:constraint_classification(federation_membership_treaty__subsidiarity_balance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: PROGRESSIVE LABOR AND CIVIL RIGHTS COALITION (SCAFFOLD) — Organized actors (European Trade Union Confederation, migrant rights NGOs, left-wing parties) see subsidiarity balance as a temporary compromise framework with sunset condition: as intra-federation harmonization of labor standards and welfare eligibility increases, the proportionality bounds can progressively loosen without triggering welfare exploitation. The constraint has an implicit sunset: full political union with harmonized tax-welfare systems would eliminate the need for mobility restrictions. Current enforcement is theater: proportionality doctrine provides appearance of protection while allowing continued extraction through work permit restrictions and welfare eligibility gaps. Sunset mechanism: harmonization roadmap reduces the legitimacy of restrictions as the fiscal/labor exploitation pathway narrows.
constraint_indexing:constraint_classification(federation_membership_treaty__subsidiarity_balance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: SOVEREIGNTY-CENTRIC STATE GOVERNMENTS (PITON) — Maintain national mobility restrictions ostensibly grounded in 'legitimate national interests' (welfare protection, labor market stability) but enforcement has become largely theatrical: most real restrictions are absorbed through welfare eligibility technicalities, residency period requirements, and narrow labor market impact exceptions rather than explicit movement bans. The constraint persists through institutional inertia — renouncing mobility restrictions entirely is politically infeasible, but actual enforcement is delegated to administrative channels with weak teeth. Exit option: arbitrage toward non-member trade relationships (e.g., selective trade deals) if federation becomes too restrictive. Theater ratio (0.62) reflects that stated national interest protections are often broader than enforced actual restrictions.
constraint_indexing:constraint_classification(federation_membership_treaty__subsidiarity_balance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Civilizational perspective risks naturalization: proportionality doctrine appears as an inevitable law of federal governance — the tension between mobility and sovereignty is inherent to any federal structure, making proportionality bounds a natural feature rather than a contingent institutional choice. However, false summit signature detects: beneficiary and victim declarations reveal the constraint is actively constructed (integrated service sectors benefit; low-skill workers bear costs). The 'natural law' framing obscures political choices about who bears adjustment costs.
constraint_indexing:constraint_classification(federation_membership_treaty__subsidiarity_balance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federation_membership_treaty__subsidiarity_balance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federation_membership_treaty__subsidiarity_balance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, TR),
    TR >= 0.70.

:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint creates measurable asymmetric extraction — high-skill workers capture gains from unrestricted mobility while low-skill workers bear adjustment costs in saturated sectors. The extractiveness is not extreme because genuine coordination functions exist (labor matching across sectors, skill shortage resolution, multinational enterprise coordination), and the proportionality doctrine provides (albeit ambiguous) limiting principles. The value reflects the constraint's hybrid character: roughly 40% extractive asymmetry, 60% genuine coordination benefit. Over the 30-year interval, extractiveness has increased modestly (0.28 → 0.38) as the composition of mobile workers has shifted toward higher-skill, higher-wage workers whose gains are more concentrated, and as welfare state exposure has increased. Suppression (0.48): Moderate-high. Member states and low-skill workers face real barriers to exit or constraint reversal. Member states cannot unilaterally reimpose hard borders without treaty breach (high formal barrier); low-skill workers cannot exit labor market participation without welfare income loss (structural constraint). However, suppression is not total: proportionality doctrine provides formal dispute resolution mechanisms, and sovereignty-centric states can apply welfare eligibility technicalities within the rules. Over time (0.42 → 0.48), suppression has increased as administrative enforcement mechanisms have become more sophisticated and as fiscal welfare systems have become more sensitive to migration-induced demand. Theater ratio (0.62): High and rising. The constraint's enforcement is substantially performative: stated protections of 'legitimate national interests' are rhetorically central but empirically soft. Real restrictions operate through administrative channels (welfare eligibility waiting periods, residency duration counting, labor market impact assessment exceptions) rather than explicit mobility bans. The theater ratio has increased over time (0.48 → 0.71) because as EU integration has deepened, explicit mobility restrictions have become politically infeasible in member state legislatures; enforcement has therefore migrated to welfare administration where restrictions are less visible and more deniable. This migration of restriction mechanism to administrative channels explains the rising theater: proportionality doctrine provides rhetorical legitimacy while actual enforcement is delegated to bureaucratic procedures.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why single-perspective analysis fails in federal migration regimes. The beneficiary perspective (high-skill mobile workers) perceives pure coordination (rope): the constraint solves their mobility matching problem. The victim perspective (low-skill native workers) perceives extraction (snare): they bear costs without voice in setting the constraint's terms. The institutional perspectives split: federation authorities see genuine mixed function (tangled_rope), while sovereignty-centric states see the constraint as inert ritual (piton) because their enforcement capacity is limited. The organized labor perspective sees a temporary framework with sunset conditions (scaffold) — proportionality bounds become unnecessary as harmonization progresses. The civilizational-analytical perspective risks seeing proportionality as an inevitable law of federalism (mountain), naturalizing what is actually a contingent institutional choice about who bears adjustment costs. The largest perspectival gaps occur between high-skill beneficiaries (rope perception) and low-skill victims (snare perception), and between integration-oriented federation authorities (tangled_rope, seeing genuine mixed function) and sovereignty-centric states (piton, seeing theatrical enforcement). These gaps reflect real structural differences in how the constraint operates: the same rule appears as coordination to beneficiaries and as extraction to victims, as mixed function to authorities and as ritual to sovereigntists.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options per agent. High-skill mobile workers (beneficiary, arbitrage exit) derive low d → low/negative chi → experience the constraint as coordination (rope). Low-skill native workers (victim, trapped exit) derive high d → high chi → experience extraction (snare). Member state authorities (mixed: both beneficiary in coordination function and victim in fiscal extraction; constrained exit) derive mid-range d → moderate chi → experience mixed coordination-extraction (tangled_rope). The federation authority (beneficiary through institutional survival, constrained exit) derives moderate-low d → rope-to-tangled_rope range. Progressive coalition (organized power enabling constrained exit to future harmonization scenario) derives moderate d → moderate chi → tangled_rope with sunset perspective. Sovereignty-centric governments (institutional, arbitrage toward non-federation alternatives) derive low d → rope-to-piton range (the latter reflecting their low chi despite institutional power because actual enforcement is theatrical). The analytical observer (analytical context) derives canonical d ≈ 0.72 per the directionality table → moderate-high chi → sees tangled_rope structural reality beneath the mountain naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (Type II): The constraint resolves the tension between treating the boundary as coordination (rope — proportionality doctrine as legitimate limiting principle) and treating it as extraction (snare — proportionality doctrine as rhetorical cover for labor market power asymmetry) by showing that both are structurally correct from their respective positions. The mandatrophy is not 'which perspective is right?' but 'why do incompatible perspectives both report accurate structural observations?' The answer: the constraint contains genuine coordination functions (labor matching, reducing sectoral skill shortages, enabling multinational enterprises) AND genuine extraction asymmetry (high-skill workers capture gains, low-skill workers bear adjustment costs, states lose regulatory autonomy). Neither perspective is mistaken. The mandatrophy resolves through perspectival decomposition: the constraint IS rope from the beneficiary's view (genuine problem-solving), IS snare from the victim's view (real extraction without voice), IS tangled_rope from the institutional view (mixed function), IS piton from the sovereigntist view (theatrical enforcement). The theater ratio rising over time (0.48 → 0.71) indicates that the coordination function is stable while the enforcement has become increasingly performative — proportionality doctrine provides legitimacy while actual restrictions migrate to administrative channels. This shift suggests the constraint may be transitioning from genuine tangled_rope toward piton-with-facade: the coordination benefits persist, but the protection narrative has become detached from enforcement reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_indeterminacy,
    'What quantitative threshold separates ''legitimate national interest'' (justifying mobility restriction) from ''protectionist disguise'' (unjustified restriction)? Where is the line between proportionate and disproportionate constraint?',
    'Empirical analysis of proportionality doctrine application: compare approved national restrictions (welfare eligibility gaps, labor market testing) to rejected restrictions; identify decision rule from case law; test against counterfactual scenarios',
    'If threshold is vague: proportionality becomes a cover story for unstated extraction (classification shifts toward snare). If threshold is operationalized: constraint appears more as genuine rope (coordination around a clear standard). Current ambiguity creates theater ratio elevation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_indeterminacy, conceptual, 'Indeterminacy in operationalizing ''proportionality'' and ''legitimate national interest'' in proportionality balance framework').

omega_variable(
    welfare_harmonization_counterfactual,
    'If intra-federation welfare standards and labor protections were fully harmonized (tax-welfare systems aligned across member states), would the proportionality bounds be legitimate, or would they become clearly extractive?',
    'Comparative analysis: current constraint classification in low-harmonization regime vs hypothetical classification in high-harmonization regime; test whether suppression and extractiveness would change given fiscal equilibrium',
    'If legitimate in harmonized case: current constraint is extractive only because of fiscal mismatch (classification should remain tangled_rope). If still extractive: proportionality doctrine is ideological cover for labor market power asymmetry (shift toward snare). Current omega reflects whether this is a problem of institutional design (solvable by harmonization) or of structural asymmetry (unsolvable within federation logic).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_harmonization_counterfactual, conceptual, 'Whether proportionality bounds are justified by fiscal mismatch or reflect structural power asymmetry').

omega_variable(
    integration_primary_foreclosure_ambiguity,
    'Does the subsidiarity_balance reading foreclose the integration_primary reading (unrestricted free movement as constitutive right), or do they coexist as competing live positions within federation jurisprudence?',
    'Jurisprudential analysis: court decisions, constitutional doctrine evolution; determine whether integration_primary remains a live legal theory (advocated by judges, scholars) or has been formally superseded. If both readings remain live across different member states or judicial coalitions, they coexist. If one has been formally overruled, it is foreclosed.',
    'If foreclosed: subsidiarity_balance reading has replaced integration_primary as the binding framework (kernel contest is resolved). If coexist: both readings are institutionally active (kernel contest is ongoing; constraint classification from integration_primary perspective would be rope or mountain, not tangled_rope). Current omegas document the contest structure per Rule 2.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(integration_primary_foreclosure_ambiguity, empirical, 'Whether integration_primary reading has been formally foreclosed or remains a live jurisprudential position').

omega_variable(
    fiscal_externality_attribution,
    'What proportion of observed welfare demand from mobile workers represents genuine fiscal externality (cost to state welfare systems) vs. income supplement to mobile workers whose primary income comes from member state employment (i.e., normal welfare utilization, not extraction)?',
    'Fiscal accounting: disaggregate welfare costs; identify which benefits are legitimately attributed to migration externality (family allowances for non-resident dependents, healthcare costs uncompensated by tax contributions) vs. normal welfare income support. Measure contribution gaps (taxes paid vs. benefits received) by nationality and residence duration.',
    'If externality is small: proportionality bounds are not justified by fiscal protection; classification shifts toward snare (low-skill workers are extracted without legitimate protection justification). If externality is substantial: bounds are justified by real fiscal risk; classification remains tangled_rope (mixed coordination and extraction). Current measurement reflects extraction framing without accounting for externality magnitude.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_externality_attribution, empirical, 'Magnitude of fiscal externality from mobile worker welfare utilization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fedtreaty_theater_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.48).
narrative_ontology:measurement(fedtreaty_theater_t15, federation_membership_treaty__subsidiarity_balance, theater_ratio, 15, 0.62).
narrative_ontology:measurement(fedtreaty_theater_t30, federation_membership_treaty__subsidiarity_balance, theater_ratio, 30, 0.71).

% Extraction over time
narrative_ontology:measurement(fedtreaty_extract_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fedtreaty_extract_t15, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(fedtreaty_extract_t30, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(fedtreaty_suppress_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(fedtreaty_suppress_t15, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(fedtreaty_suppress_t30, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, labor_market_welfare_fiscal_externality).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% The subsidiarity_balance constraint is the jurisprudentially dominant reading of federation_membership_treaty. Two sibling constraints encode alternative readings (integration_primary and sovereignty_primary). All three share the same kernel (federation movement rights) but with structurally distinct beneficiary/victim sets and proportionality thresholds. The decomposition reflects that the kernel contest is not a matter of perspective (same observer seeing different types depending on position) but of actual structural difference in legal doctrine: each reading instantiates a different rule with different extractiveness. Subsidiarity_balance (ε=0.38) is moderate because it balances coordination and protection; integration_primary would show lower ε (no protection except emergency exceptions); sovereignty_primary would show higher ε (broader protection, lower coordination benefit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__subsidiarity_balance, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
