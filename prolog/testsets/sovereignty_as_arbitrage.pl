% ============================================================================
% CONSTRAINT STORY: sovereignty_as_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereignty_as_arbitrage, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sovereignty_as_arbitrage
 *   human_readable: The Alberta Prosperity Project (State-Building Arbitrage)
 *   domain: political/economic
 *
 * SUMMARY:
 *   The Alberta Prosperity Project exemplifies state-building arbitrage: the
 *   framing of provincial sovereignty as a mechanism for capturing resource
 *   rents currently redistributed through federal equalization transfers and
 *   monetary union membership. The constraint exhibits genuine coordination
 *   challenges (resource extraction authority is jurisdictionally ambiguous;
 *   federal framework imposes costs that could theoretically be reduced
 *   through provincial autonomy) layered beneath extractive misrepresentation
 *   (the APP frame obscures real costs of independence while naturalizing
 *   federal constraints as pure extraction rather than coordination
 *   mechanisms). The theater ratio has increased over time as the APP
 *   narrative has shifted from policy critique (equalization reform, resource
 *   control) to symbolic sovereignty demand (constitutional identity,
 *   state-building) — the performative content has grown while the specific
 *   policy content has become more abstract. The extractiveness score (0.52)
 *   reflects the hybrid nature: genuine institutional tension (federal
 *   constraints on resource policy are real) combined with misdirection about
 *   what independence would actually solve. The suppression component (0.58)
 *   is substantial because the APP frame constrains public debate — it
 *   presents a false binary (federal integration or sovereign state) that
 *   suppresses discussion of intermediate institutional reforms (asymmetric
 *   federalism, resource property rights clarification, equalization formula
 *   redesign). The constraint operates at multiple scales: for the APP
 *   organizational core, it is a beneficial coordination problem (unite
 *   provincial interests, present unified demands); for the federal fiscal
 *   system, it is an extractive mechanism (threatens to dissolve federal
 *   legitimacy); for Alberta resource exporters, it is a mixed
 *   coordination-extraction hybrid; for the Canadian monetary union, it is a
 *   structural threat with no actual exit mechanism (hence snare
 *   classification from that perspective).
 *
 * KEY AGENTS:
 *   - Alberta Prosperity Project Organizational Core: Primary beneficiary (institutional/arbitrage) — benefits from framing sovereignty as simple arbitrage; mobilizes political support; gains organizational legitimacy as voice of provincial grievance
 *   - Alberta Resource Export Sector: Secondary beneficiary (organized/constrained) — benefits from potential provincial autonomy over extraction policy and royalty structures; constrained by need for federal-level negotiation and currency union membership
 *   - Federal Fiscal Integration System: Primary victim (powerless/trapped) — equalization transfers and revenue-sharing are embedded in provincial budgets; severance triggers fiscal crisis; cannot exit or renegotiate unilaterally
 *   - Canadian Monetary Union: Primary victim (powerless/trapped) — membership is structurally enforced; exit requires sovereignty infrastructure; no partial exit options available
 *   - Federal Renewal Coalition: Institutional actor (organized/constrained) — sees sovereignty arbitrage as temporary coordination failure resolvable through constitutional reform; constrained by need to preserve federation while addressing legitimate provincial autonomy grievances
 *   - Canadian Constitutional Settlement: Institutional actor (institutional/arbitrage) — maintains formal legitimacy through procedural formalism; functionally degraded; persists through inertia and historical precedent
 *   - Analytical Observer: Neutral position (analytical/analytical) — sees both genuine coordination challenges and genuine misdirection about their solution; identifies false arbitrage framing as suppression mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereignty_as_arbitrage, 0.52).
domain_priors:suppression_score(sovereignty_as_arbitrage, 0.58).
domain_priors:theater_ratio(sovereignty_as_arbitrage, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereignty_as_arbitrage, extractiveness, 0.52).
narrative_ontology:constraint_metric(sovereignty_as_arbitrage, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sovereignty_as_arbitrage, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereignty_as_arbitrage, tangled_rope).
narrative_ontology:human_readable(sovereignty_as_arbitrage, "The Alberta Prosperity Project (State-Building Arbitrage)").
narrative_ontology:topic_domain(sovereignty_as_arbitrage, "political/economic").

domain_priors:requires_active_enforcement(sovereignty_as_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereignty_as_arbitrage, app_organizational_core).
narrative_ontology:constraint_beneficiary(sovereignty_as_arbitrage, resource_export_interests).
narrative_ontology:constraint_beneficiary(sovereignty_as_arbitrage, provincial_autonomy_advocates).
narrative_ontology:constraint_victim(sovereignty_as_arbitrage, federal_fiscal_integration).
narrative_ontology:constraint_victim(sovereignty_as_arbitrage, inter_provincial_redistribution).
narrative_ontology:constraint_victim(sovereignty_as_arbitrage, canadian_monetary_union).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL FISCAL INTEGRATION (SNARE) — The equalization transfer system and federal revenue-sharing mechanisms have no exit option once embedded in provincial budgets. Provinces become dependent on federal flows; severing creates immediate fiscal crisis. The constraint extracts legitimacy from the federal framework by proposing to dissolve it while maintaining its benefits — a contradictory exit that is not actually available.
constraint_indexing:constraint_classification(sovereignty_as_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CANADIAN MONETARY UNION (SNARE) — Currency union membership is structurally enforced: exit requires new central bank, reserves, forex infrastructure, and capital controls. Alberta cannot exit unilaterally without catastrophic transaction costs. The APP proposal naturalizes monetary independence while the structural reality is maximal entrapment — high suppression (cannot leave), high extraction (must solve currency problem but claims arbitrage solves it).
constraint_indexing:constraint_classification(sovereignty_as_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: ALBERTA RESOURCE EXPORT SECTOR (TANGLED ROPE) — Benefits from provincial autonomy over resource extraction and royalty structures (coordination benefit: unified extraction policy, simplified investment frameworks). Bears costs of federal environmental standards, interprovincial trade constraints, and equalization payments. Mixed extraction and coordination — not trapped, but constrained by national frameworks. Moderately powerful but cannot dissolve federal jurisdiction unilaterally.
constraint_indexing:constraint_classification(sovereignty_as_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: APP ORGANIZATIONAL CORE (ROPE) — Benefits from framing sovereignty as simple arbitrage (dissolve federal constraints, capture resource rents, exit transfer obligations). Experiences sovereignty negotiation as pure coordination problem: align provincial interests, present unified demands to federal negotiators. Beneficiary position with exit options (can dissolve, restructure, pivot to electoral politics). Arbitrage access — can shop demands between federal and provincial contexts.
constraint_indexing:constraint_classification(sovereignty_as_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: FEDERAL RENEWAL COALITION (SCAFFOLD) — Democratic federalism advocates see the sovereignty-arbitrage frame as a temporary coordination failure solvable by institutional reform: clearer resource property rights, reformed equalization formulas, enhanced provincial autonomy within federation. This perspective treats the constraint as a temporary scaffolding problem with a sunset clause (institutional redesign, constitutional renewal) rather than a structural dissolution problem. High organization, constrained exit (must reform federation), belief in alternative pathway.
constraint_indexing:constraint_classification(sovereignty_as_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CANADIAN CONSTITUTIONAL SETTLEMENT (PITON) — The 1982 constitution and federalism architecture were designed to balance sovereignty, resource distribution, and monetary union. The framework persists through institutional inertia and historical legitimacy, but its functional coherence has degraded: equalization formulas reflect 1970s-80s economic structures; resource extraction has shifted; interprovincial dynamics have changed. The constitution is maintained performatively (constitutional conferences, formal amendment processes) but the real coordination is negotiated outside it. Theater ratio high — the formalism persists despite functional obsolescence.
constraint_indexing:constraint_classification(sovereignty_as_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (STATE CAPACITY) (TANGLED ROPE) — From a global, civilizational view, the APP claim exhibits both genuine coordination challenge (provincial resources are jurisdictionally ambiguous; federal authority over natural resources is contingent and contested) and genuine extraction (the arbitrage framing obscures costs: new independent state must build sovereignty infrastructure, faces increased capital costs, loses scale economies of federation, inherits currency risk). The constraint is real but mislabeled — it's not a simple arbitrage opportunity; it's a hybrid coordination problem with irreducible trade-offs.
constraint_indexing:constraint_classification(sovereignty_as_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereignty_as_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereignty_as_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereignty_as_arbitrage, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereignty_as_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sovereignty_as_arbitrage, TR),
    TR >= 0.70.

:- end_tests(sovereignty_as_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The constraint reflects real federal constraints on provincial resource policy coupled with genuine claims about resource rents captured through federal mechanisms (equalization, corporate tax coordination). However, the arbitrage frame obscures major offsetting costs: establishing independent sovereignty requires building central banking, currency, reserve infrastructure (~CAD 50-100B in transition costs estimated in comparable cases); capital markets would impose risk premiums on independent Alberta securities; commodity price volatility becomes unshared (no federal fiscal smoothing); scale economies in public services are lost. The arbitrage claim that independence would be economically net-positive is not obviously true — the extraction claimed (federal extraction) may be offset by hidden extraction in sovereignty infrastructure costs. Moderate-high extractiveness reflects this mismatch. Suppression (0.58): The APP frame imposes significant constraint on policy debate by presenting a binary choice (federal integration or sovereignty) that suppresses discussion of intermediate institutional solutions (asymmetric federalism, resource property rights, equalization reform without dissolution). The framing also suppresses analysis of actual costs by presenting them as solvable through arbitrage rather than as genuine trade-offs. This binary framing is a suppression mechanism — it constrains the set of negotiable outcomes. Theater ratio (0.68): Increasing over time. The APP has shifted from specific policy critique (equalization formulas should be reformed, provincial resource control should be clarified) toward symbolic sovereignty demand (provincial identity, constitutional independence, state-building narrative). The symbolic content is performative — it energizes political identity without committing to specific policy positions or cost acceptance. The constitutional settlement itself is increasingly performative (formal amendment procedures rarely used; real negotiation happens outside them), and the APP frame capitalizes on this gap between formal constitutional authority and functional reality.
 *
 * PERSPECTIVAL GAP:
 *   The federal fiscal integration system and Canadian monetary union see the constraint as pure extraction (Snare) — they experience the APP proposal as an existential threat with no exit mechanism and no alternative response available. The APP organizational core sees coordination (Rope) — they frame the problem as uniting provincial interests and presenting demands to federal negotiators; they have exit options (dissolve the organization, pivot to electoral politics) and operate from an advantaged position (control of provincial resources, constitutional standing). Alberta resource exporters see a mixed picture (Tangled Rope) — they benefit from provincial autonomy but are constrained by federal frameworks and monetary union requirements; they experience extraction but also coordination benefits. The Federal Renewal Coalition sees a temporary problem (Scaffold) — they believe institutional reform can address the underlying tensions without dissolution; they have a sunset clause (constitutional modernization is ongoing). The Canadian Constitutional Settlement sees its own degraded ritual (Piton) — the formalism persists but the function has atrophied; the APP frame exploits this gap by suggesting that formal constitutionalism is theater masking real power asymmetries. The analytical observer sees the full hybrid structure (Tangled Rope): genuine institutional tension layered beneath misdirection about what independence would solve. The perspectival gap reveals that the APP proposal is not a simple arbitrage opportunity but a renegotiation of federal power dynamics disguised as economic optimization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from each agent's structural position: whether they benefit or bear costs, and whether they have exit options. The APP organizational core has low d (high beneficiary status, arbitrage exit options) — they derive negative experienced extractiveness from the constraint (they benefit from presenting it as a coordination problem). The federal fiscal system has high d (victim status, trapped exit options) — it experiences maximum extraction. Alberta resource exporters have moderate d (mixed beneficiary-victim status, constrained exit options) — they experience moderate extraction masked by coordination benefits. The resource sector benefits from potential autonomy but cannot exit federal frameworks unilaterally. The Federal Renewal Coalition has moderate d (organized status, constrained exit options but with reform pathway) — they experience the constraint as solvable, thus moderate extraction. The analytical observer has moderate-high d (analytical status, global scope, civilization timescale) — they see both the genuine coordination problem and the misdirection, thus moderate-high extraction (the constraint obscures rather than clarifies). The derived d values produce perspectival gaps: low d for APP (Rope classification) versus high d for federal system (Snare classification) — the same structural phenomenon produces opposite classifications depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY TENSION UNRESOLVED: The constraint sits at the boundary between coordination problem (Rope) and extraction mechanism (Snare) without being fully either. The APP narrative naturalizes the coordination problem (federal constraints on resources are presented as pure extraction) while minimizing the extraction in independence itself (the machinery of sovereignty is presented as technically unproblematic, costs absorbed into gains). The mandatrophy is resolved by recognizing that the constraint is genuinely hybrid (Tangled Rope): there are real coordination gains available from provincial autonomy, and real extraction losses embedded in federal mechanisms, but also real costs (monetary infrastructure, capital risk premiums, scale economies) embedded in sovereignty. The false arbitrage framing is a suppression mechanism: it presents the problem as solvable through simple institutional dissolution when the actual solution requires renegotiation of federal power dynamics while accepting real trade-offs. The mandatrophy_resolved flag is set to false because the system has not yet committed to accepting both the benefits AND the costs — the current constraint still relies on presenting arbitrage as asymmetrically favorable, which is the mark of mislabeled extraction. Full resolution would require explicit acknowledgment that provincial autonomy has real value (coordination benefit) but also real costs (sovereignty infrastructure, risk premium, scale loss) — that this is a genuine institutional redesign problem, not a simple wealth transfer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_rents_stability,
    'Can provincial resource extraction rents remain stable and substantial under independence, or does sovereignty trigger capital flight, stranded assets, and commodity price volatility?',
    'Comparative analysis of resource-exporting breakaway regions (Norway/UK North Sea, Singapore/Malaysia, South Sudan/Sudan); modeling of post-independence fiscal trajectories; investor confidence surveys conditional on sovereignty scenarios',
    'If stable: arbitrage framing is valid (independence captures rents without major loss). If volatile: arbitrage dissolves (sovereignty introduces currency and political risk premiums that exceed equalization savings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_rents_stability, empirical, 'Whether resource rents remain stable post-independence').

omega_variable(
    federal_redistribution_necessity,
    'Is interprovincial redistribution (equalization) a necessary feature of fiscal federation or a contingent policy choice? Could Alberta achieve equivalent services AND lower taxes without federal transfers through more efficient extraction?',
    'Comparative provincial public finance analysis; cost modeling for healthcare, education, infrastructure under independent Alberta scenarios; regional economic growth simulations with and without federal co-investment',
    'If necessary: equalization is coordination mechanism (Rope-type constraint on redistribution); APP frames coordination failure as extraction. If contingent: equalization is rent-capture mechanism (Snare-type for net contributors); APP correctly identifies extraction, but independence doesn''t solve the underlying problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_redistribution_necessity, conceptual, 'Whether federal redistribution is necessary or contingent').

omega_variable(
    monetary_union_exit_costs,
    'What are the actual infrastructure and transaction costs of establishing independent monetary sovereignty for a post-independence Alberta? Can these be absorbed by resource rents or do they exceed the claimed arbitrage gains?',
    'Central bank establishment modeling; reserve requirements analysis; transaction cost studies of currency conversion and forex infrastructure; comparative cases (Scotland, Catalonia exit simulations); capital flow projections',
    'If costs < rents: arbitrage frame is viable (independence gains exceed sovereignty infrastructure costs). If costs > rents: arbitrage dissolves (hidden extraction embedded in monetary union is revealed; independence trade-offs are not favorable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monetary_union_exit_costs, empirical, 'Transaction costs of establishing independent monetary sovereignty').

omega_variable(
    political_will_threshold,
    'What level of support would constitute genuine popular mandate for sovereignty as arbitrage versus public expression of dissatisfaction with federal distribution mechanisms?',
    'Longitudinal polling data; referendum intent studies; multinomial analysis of support drivers (resource policy vs redistribution grievance vs identity politics); comparison with opinion on specific alternative arrangements (asymmetric federalism, resource control devolution)',
    'If >65% support independence per se: APP organizational frame is capturing real popular will (Rope-type coordination problem). If support collapses to <40% when asked about specific trade-offs: APP is theater (Piton-type) — performance of grievance without structural commitment to costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_will_threshold, empirical, 'Threshold level of political support for genuine sovereignty mandate').

omega_variable(
    federal_response_trajectory,
    'Will federal government treat sovereignty arbitrage as negotiable (institutional reform) or non-negotiable (constitutional integrity)? Does the federal response trajectory reshape the constraint from Tangled Rope to Snare?',
    'Federal government public statements, parliamentary debates, and negotiating positions; modeling of constitutional amendment likelihood; analysis of federal willingness to offer enhanced provincial autonomy as an alternative to dissolution',
    'If federal permits renegotiation: Scaffold or Tangled Rope classification holds (pathway exists for resolution). If federal refuses negotiation: constraint hardens into Snare for both federal system AND for APP supporters (exit becomes impossible via this pathway; must choose between dissolution or integration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_response_trajectory, preference, 'Federal government''s response trajectory and willingness to negotiate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereignty_as_arbitrage, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_arb_tr_t0, sovereignty_as_arbitrage, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sov_arb_tr_t3, sovereignty_as_arbitrage, theater_ratio, 3, 0.55).
narrative_ontology:measurement(sov_arb_tr_t6, sovereignty_as_arbitrage, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(sov_arb_be_t0, sovereignty_as_arbitrage, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sov_arb_be_t3, sovereignty_as_arbitrage, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(sov_arb_be_t6, sovereignty_as_arbitrage, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereignty_as_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(sovereignty_as_arbitrage, canadian_equalization_regime).
narrative_ontology:affects_constraint(sovereignty_as_arbitrage, federal_resource_extraction_authority).
narrative_ontology:affects_constraint(sovereignty_as_arbitrage, monetary_union_exit_costs).

% DUAL FORMULATION NOTE:
% The Alberta Prosperity Project constraint decomposes into three structurally distinct constraints: (1) the equalization transfer system (federal redistribution mechanism, potentially Rope or Snare depending on perspective); (2) federal resource extraction authority (jurisdictional ambiguity, Tangled Rope); (3) monetary union membership (exit costs, Mountain or Snare depending on whether costs are technical inevitability or contingent institutional choice). The APP reframes all three as 'simple arbitrage resolvable through sovereignty,' but each has distinct structural properties and epsilon values. The sovereignty_as_arbitrage constraint is downstream of all three — it claims they can be solved together through institutional dissolution, but they are distinct problems with separate solutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereignty_as_arbitrage, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
