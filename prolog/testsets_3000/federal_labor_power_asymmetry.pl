% ============================================================================
% CONSTRAINT STORY: federal_labor_power_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_labor_power_asymmetry, []).

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
 *   constraint_id: federal_labor_power_asymmetry
 *   human_readable: Federal Labor Power Asymmetry
 *   domain: labor_law/institutional_political_economy
 *
 * SUMMARY:
 *   The federal labor power asymmetry represents a structural constraint
 *   built into U.S. labor law and institutional practice. The constraint
 *   emerges from a cascade of federal policies: the Taft-Hartley Act (1947)
 *   prohibiting secondary boycotts and closed-shop agreements, right-to-work
 *   statutes (24 states) eliminating mandatory union membership in unionized
 *   workplaces, at-will employment doctrine, and NLRA restrictions on
 *   bargaining scope. These policies create a legal and economic environment
 *   where individual workers face suppression of collective action capacity,
 *   while employers retain full capacity to coordinate labor sourcing and
 *   resist unionization. The constraint exhibits all six classification types
 *   depending on perspective, making it a diagnostic case for how
 *   institutional power asymmetries are naturalised versus recognized as
 *   contingent. From a worker perspective, it is a pure extraction mechanism
 *   (Snare). From an organized union perspective, it is a mixed
 *   coordination-extraction hybrid (Tangled Rope) where genuine collective
 *   action benefits exist alongside extraction. From an employer perspective,
 *   it is coordination (Rope) enabling predictable labor cost management.
 *   From a reform perspective, it is a temporary institutional failure with
 *   sunset logic (Scaffold). From the regulatory apparatus perspective, it is
 *   degraded ritual with atrophied function (Piton). From an economic
 *   naturalization perspective, it is immutable law (false Mountain). The
 *   extractiveness value (0.58) reflects that the asymmetry persists through
 *   both structural suppression (legal prohibition of secondary action,
 *   at-will employment) and institutional maintenance (NLRB enforcement
 *   delays, employer litigation strategies). The theater ratio (0.48)
 *   reflects that regulatory compliance (NLRB certification elections, unfair
 *   labor practice investigations) maintains appearance of protection while
 *   substantive power asymmetry persists.
 *
 * KEY AGENTS:
 *   - Individual Workers: Primary victims (powerless/trapped) — face legal and economic barriers to collective action; at-will employment enables retaliation
 *   - Union Collectives: Secondary victims (organized/constrained) — can organize in pockets but face systemic suppression through Taft-Hartley, right-to-work laws, and certification barriers
 *   - Employers: Primary beneficiaries (institutional/arbitrage) — benefit from restriction on secondary action, ability to source workers individually, geographic flexibility, and legal delays in organizing drives
 *   - Multinational Capital: Secondary beneficiary (powerful/mobile) — coordinates labor arbitrage globally within federal framework that prevents domestic collective response
 *   - Progressive Reform Coalition: Institutional actor (moderate/constrained) — sees constraint as reversible through PRO Act and sectoral bargaining proposals; perceives sunset path
 *   - NLRB and Labor Department: Institutional regulators (institutional/arbitrage) — maintain compliance ritual while substantive asymmetry persists; arbiter of extractive capture itself
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent federal policy choices as immutable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_labor_power_asymmetry, 0.58).
domain_priors:suppression_score(federal_labor_power_asymmetry, 0.65).
domain_priors:theater_ratio(federal_labor_power_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_labor_power_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(federal_labor_power_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(federal_labor_power_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_labor_power_asymmetry, tangled_rope).
narrative_ontology:human_readable(federal_labor_power_asymmetry, "Federal Labor Power Asymmetry").
narrative_ontology:topic_domain(federal_labor_power_asymmetry, "labor_law/institutional_political_economy").

domain_priors:requires_active_enforcement(federal_labor_power_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_labor_power_asymmetry, employers_organized_capital).
narrative_ontology:constraint_victim(federal_labor_power_asymmetry, workers_collective_action).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL WORKER (SNARE) — Faces collective action prohibition (Taft-Hartley restrictions, right-to-work laws, at-will employment) with no viable exit. Cannot strike without losing job security, cannot bargain collectively without employer retaliation, cannot organize without legal and economic penalties. Maximum suppression: legal framework explicitly criminalizes collective action alternatives.
constraint_indexing:constraint_classification(federal_labor_power_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNION COLLECTIVE (TANGLED ROPE) — Partially organized but faces severe constraints. Genuine coordination function: collective bargaining solves wage coordination and benefit pooling problems. Simultaneous extraction: federal labor framework restricts bargaining scope, Taft-Hartley prohibits secondary boycotts, right-to-work laws hollow out union financing. Union has some agency and exit paths (strategic sectors, public employee organizing) but operates within a regulatory cage designed to limit their power.
constraint_indexing:constraint_classification(federal_labor_power_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYER COALITION (ROPE) — Benefits from the federal labor framework which restricts union organizing, enables individual worker replacements, and prevents secondary action. Experiences the constraint as coordination: predictable labor costs, ability to source workers from non-union regions, capacity to shift production geographically. Net beneficiary with exit optionality — can relocate operations, outsource, or shift production to union-resistant jurisdictions.
constraint_indexing:constraint_classification(federal_labor_power_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MULTINATIONAL CAPITAL (TANGLED ROPE) — Coordinates global labor arbitrage within federal framework. Genuine coordination: capital mobility, supply chain optimization. Asymmetric extraction: U.S. workers face wage competition from low-labor-cost jurisdictions; federal labor framework enables this arbitrage by restricting domestic collective action response. Powerful actor with high mobility — can exit to jurisdictions with even weaker labor protections.
constraint_indexing:constraint_classification(federal_labor_power_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PROGRESSIVE LABOR REFORM COALITION (SCAFFOLD) — Sees the federal labor asymmetry as a temporary institutional failure with reversibility. Proposed remedies (PRO Act, sectoral bargaining, card-check organizing) represent sunset logic: if these reforms pass, the asymmetry collapses. Low effective extraction from this perspective because the coalition perceives agency and an exit path through legislative change. Theater is moderate — the reform movement exists but operates against institutional inertia.
constraint_indexing:constraint_classification(federal_labor_power_asymmetry, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY COMPLIANCE APPARATUS (PITON) — NLRB and Department of Labor enforce a framework whose primary function has atrophied. Labor law nominally protects collective action but in practice enables employer defense through legal delay, unit fragmentation, right-to-work exemptions, and supervisor reclassification. The regulatory apparatus performs enforcement ritual (certification elections, unfair labor practice investigations) but the actual power asymmetry persists despite compliance. Theater ratio high — the apparatus maintains legitimacy through procedural performance while the substantive balance of power remains captured.
constraint_indexing:constraint_classification(federal_labor_power_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ECONOMIC NECESSITY VIEW (MOUNTAIN) — From a civilizational universal perspective, labor power asymmetry is inherent to market economies: capital mobility always exceeds labor mobility, employer coordination capacity always exceeds worker coordination capacity absent institutional intervention. This perspective sees the federal constraints as natural consequences of economic structure. However, the structural data reveals this as false naturalization: the specific federal legal framework (Taft-Hartley, right-to-work statutes, at-will employment) is contingent institutional choice, not economic law.
constraint_indexing:constraint_classification(federal_labor_power_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_labor_power_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_labor_power_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_labor_power_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federal_labor_power_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federal_labor_power_asymmetry, TR),
    TR >= 0.70.

:- end_tests(federal_labor_power_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The federal legal framework systematically extracts from workers through suppression of collective action. Measurement trajectory shows accumulation: 1947 (Taft-Hartley) established secondary boycott prohibition; 1960s-1980s right-to-work expansion reduced union revenue and membership; 1980s-present hostile NLRB appointment cycles and misclassification (gig work, supervisors) have further increased asymmetry. The value reflects that extraction is sustained and growing, not declining. Suppression (0.65): High. Multi-layered suppression mechanisms: legal (Taft-Hartley secondary action prohibition, at-will employment, duty to bargain narrowness), economic (employer ability to relocate, individual worker replaceability, wage competition from non-union regions), and institutional (NLRB process delays averaging 3+ years for unfair labor practice cases, certification election delays). Workers face alternatives removal and legal penalties for secondary action. Organized unions face structural suppression through right-to-work hollow-out of union finances. Theater ratio (0.48): Moderate. NLRB maintains legitimacy through procedural performance (certification elections, ULP investigations) but the substantive enforcement mechanism has been captured through appointment of union-hostile general counsels and judges. The regulatory apparatus performs the appearance of protection without delivering substantive rebalance. The theater is not as high as degraded institutions (which show >0.70) because some genuine bargaining still occurs, particularly in public sector and certain private industries. The trend shows theater rising as compliance becomes more performative relative to substantive outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence. The individual worker sees a pure extraction mechanism with no escape (Snare) — collective action is legally forbidden, individual action is impossible due to information asymmetry and free-riding, exit is only through job loss. The organized union sees a mixed bag (Tangled Rope) — genuine coordination benefits (pooled wage bargaining, benefit security, workplace safety) exist alongside extraction (restriction on secondary action prevents multi-sector leverage, right-to-work empties union coffers). The employer sees efficient coordination (Rope) — the federal framework enables them to coordinate labor markets while preventing worker coordination. The reform coalition sees a temporary failure (Scaffold) — legislation (PRO Act) would restore the sunset by enabling card-check organizing and secondary action, creating an exit path. The regulatory apparatus sees degraded function (Piton) — NLRB certification and ULP investigation theater persists despite the constraint's substantive capture. The economic naturalization sees immutable asymmetry (false Mountain) — but the comparative institutional evidence (Germany, Scandinavia, Canada) reveals this as contingent federal choice, not economic law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the extraction flow. Individual workers (powerless/trapped) experience maximum d (~0.95) because they bear full costs of suppression and have no exit. Organized unions (organized/constrained) experience moderate-high d (~0.65) because they retain some exit paths (organizing in strong sectors, political mobilization) and some coordination benefits, but face systematic suppression. Employers (institutional/arbitrage) experience very low d (~0.10) because they are net beneficiaries and retain full geographic and sourcing mobility. Multinational capital (powerful/mobile) experiences low d (~0.15) because they benefit from arbitrage and have unrestricted exit to lower-labor-cost jurisdictions. Reform coalition (moderate/constrained) experiences moderate d (~0.55) because they perceive agency and an exit path through institutional change. Regulatory apparatus (institutional/arbitrage) experiences low d (~0.12) as an apparatus maintaining beneficiary-aligned interests. The analytical observer (analytical/analytical) experiences moderate-high d (~0.70) because cross-position analysis reveals the extent of naturalisation in the economic-necessity frame.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the tension between coordination and extraction is real and multi-perspectival. The individual worker's Snare classification is not 'wrong' — it correctly reflects their structural experience. The employer's Rope classification is not 'dishonest' — it correctly reflects their coordination benefits. The gap between them IS the constraint. The mandatrophy is resolved by recognizing that federal labor law performs an institutional choice to distribute that gap: to the extent that workers cannot collectively bargain, they have zero exit and experience pure extraction. To the extent that employers can coordinate labor sourcing while workers cannot, the employers experience coordination benefits. The mandatrophy is not 'is it Rope or Snare?' but 'which perspective are you measuring from and why?' The Tangled Rope classification is the analytical equilibrium: genuine coordination functions (collective bargaining does solve wage coordination problems) coexist with asymmetric extraction (suppression of secondary action prevents workers from leveraging cross-industry solidarity). The Scaffold perspective reveals that this is not inevitable: sectoral bargaining and card-check reforms would rebalance the extraction while preserving coordination. The false Mountain perspective reveals the danger of naturalizing institutional choices as economic law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_regime_counterfactual,
    'How much of the measured labor power asymmetry is due to federal legal structure versus irreducible economic factors (capital mobility, employer coordination advantages)?',
    'Comparative institutional analysis: U.S. labor outcomes vs. economies with sectoral bargaining (Germany, Scandinavia), card-check organizing (Canada), or stronger secondary action rights (UK pre-Thatcher). If legal changes produce convergence, asymmetry is contingent institutional choice.',
    'If legal factors dominate (>60%): the constraint is a Tangled Rope with high mandatrophy potential — institutional reform can rebalance without eliminating coordination. If economic factors dominate (>60%): the mountain perspective gains credibility, and reform faces structural limits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_regime_counterfactual, empirical, 'Legal vs. economic causation in labor power asymmetry').

omega_variable(
    right_to_work_extraction_mechanism,
    'Do right-to-work laws reduce union effectiveness by free-riding (rational actor model) or by strategic employer defunding (extractive capture)?',
    'Longitudinal study of union power and density changes post-right-to-work adoption; mechanism identification through membership surveys and financial data; analysis of employer rhetoric and anti-union spending patterns.',
    'If rational free-riding: suppression is moderate (workers rationally choose individual gains). If employer strategic targeting: suppression is high (deliberately engineered to prevent collective action). Changes classification weight toward Snare vs. Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_to_work_extraction_mechanism, empirical, 'Right-to-work mechanism: free-riding vs. strategic extraction').

omega_variable(
    taft_hartley_secondary_action_necessity,
    'Are secondary boycott restrictions a legitimate response to coercive abuse or a fundamental constraint on worker power that prevents coordination across industries?',
    'Historical case analysis of pre-Taft-Hartley secondary action campaigns; assessment of genuine coercion vs. effective leverage; comparison to other legal systems allowing secondary action with different abuse rates.',
    'If abuse was genuine and secondary bans reduce coercion: restriction is a reasonable coordination gate (Rope framing). If bans are pretextual and prevent legitimate multi-sector organizing: bans are extractive capture (Snare framing). Shifts perspectival gap width.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taft_hartley_secondary_action_necessity, conceptual, 'Whether secondary boycott restrictions prevent coercion or prevent legitimate worker coordination').

omega_variable(
    organized_labor_coalition_trajectory,
    'Is union decline due to structural economic obsolescence or due to deliberate political extraction (Taft-Hartley + right-to-work + hostile NLRB appointments)?',
    'Synthetic control analysis comparing U.S. union decline to peer economies; disaggregation by sector (public vs. private, unionized vs. non-unionized); timeline correlation with specific legislative and enforcement changes.',
    'If structural obsolescence: union decline is inevitable (mountain perspective). If political extraction: decline is reversible through institutional reform (Scaffold perspective). Determines whether labor power asymmetry is temporary or permanent feature of U.S. system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organized_labor_coalition_trajectory, empirical, 'Union decline causation: economic vs. political').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_labor_power_asymmetry, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fedlab_tr_t0, federal_labor_power_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fedlab_tr_t25, federal_labor_power_asymmetry, theater_ratio, 25, 0.42).
narrative_ontology:measurement(fedlab_tr_t50, federal_labor_power_asymmetry, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(fedlab_be_t0, federal_labor_power_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fedlab_be_t25, federal_labor_power_asymmetry, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(fedlab_be_t50, federal_labor_power_asymmetry, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_labor_power_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(federal_labor_power_asymmetry, labor_market_segmentation).
narrative_ontology:affects_constraint(federal_labor_power_asymmetry, gig_economy_misclassification).
narrative_ontology:affects_constraint(federal_labor_power_asymmetry, union_corruption_vulnerability).

% DUAL FORMULATION NOTE:
% Federal labor power asymmetry is upstream to multiple labor market constraints. Labor market segmentation (dual labor market with union/non-union tiers) is a direct consequence of the asymmetry. Gig economy misclassification (independent contractor vs. employee) operates within the suppression framework created by federal labor law. Union corruption vulnerability emerges from organizational weakness induced by the asymmetry. The network links describe causal dependency: the federal constraint creates conditions for these downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federal_labor_power_asymmetry, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
