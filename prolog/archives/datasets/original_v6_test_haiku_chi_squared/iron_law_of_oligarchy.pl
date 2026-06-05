% ============================================================================
% CONSTRAINT STORY: iron_law_of_oligarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iron_law_of_oligarchy, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: iron_law_of_oligarchy
 *   human_readable: The Iron Law of Oligarchy
 *   domain: political/social
 *
 * SUMMARY:
 *   Robert Michels' iron law of oligarchy proposes that all complex
 *   organizations, regardless of their initial democratic commitments,
 *   inevitably develop oligarchic structures. This constraint reveals the
 *   tension between the coordinating function of hierarchies and their
 *   extractive properties. Organizations begin with genuine democratic
 *   intentions (time 0: low extractiveness 0.20, low theater 0.30) and
 *   democratic legitimacy claims. But as they scale and face coordination
 *   challenges, leadership consolidates authority. Authority asymmetries
 *   create information gaps. Information gaps enable rent-seeking. Democratic
 *   forms persist as theater (time 10: theater 0.68) while real power
 *   concentrates (extractiveness 0.52). The constraint is neither pure
 *   coordination (that would be Rope) nor pure extraction (that would be
 *   Snare) — it is a hybrid where oligarchic structure solves genuine
 *   coordination problems while simultaneously enabling leadership to extract
 *   rents beyond what coordination necessitates. The mandatrophy question is
 *   acute: does the iron law describe structural inevitability (mountain),
 *   contingent institutional failure (tangled rope), or the predictable
 *   outcome of specific design choices that can be redesigned (scaffold)?
 *
 * KEY AGENTS:
 *   - Rank-and-File Members: Primary victims (powerless/trapped) — have no realistic exit from organizations that monopolize participation in their domain; bear full cost of oligarchic extraction without decision power
 *   - Organizational Leadership: Primary beneficiaries (institutional/arbitrage) — capture authority, status, and often material rewards; experience the constraint as enabling necessary coordination
 *   - Intermediate Cadre: Mixed actor (moderate/constrained) — receive some leadership benefits (career advancement, agency at scale) while bearing enforcement burden; constrained by need to implement leadership directives
 *   - Democratic Institutional Memory: Victim of erosion (organized/constrained) — organization maintains democratic theater but actual power has migrated; the democratic function itself is suppressed while its forms persist
 *   - Democratic Reform Movement: Organized alternative builder (organized/mobile) — sees oligarchy as a temporary design problem with solutions; investing in participatory governance, digital transparency, and rotating leadership
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks mistaking contingent institutional outcomes for immutable laws of organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iron_law_of_oligarchy, 0.52).
domain_priors:suppression_score(iron_law_of_oligarchy, 0.65).
domain_priors:theater_ratio(iron_law_of_oligarchy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iron_law_of_oligarchy, extractiveness, 0.52).
narrative_ontology:constraint_metric(iron_law_of_oligarchy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(iron_law_of_oligarchy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iron_law_of_oligarchy, tangled_rope).
narrative_ontology:human_readable(iron_law_of_oligarchy, "The Iron Law of Oligarchy").
narrative_ontology:topic_domain(iron_law_of_oligarchy, "political/social").

domain_priors:requires_active_enforcement(iron_law_of_oligarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iron_law_of_oligarchy, organizational_leadership).
narrative_ontology:constraint_beneficiary(iron_law_of_oligarchy, bureaucratic_apparatus).
narrative_ontology:constraint_victim(iron_law_of_oligarchy, rank_and_file_members).
narrative_ontology:constraint_victim(iron_law_of_oligarchy, organizational_democratic_function).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RANK-AND-FILE MEMBER (SNARE) — Individual members have no realistic exit from organizations that monopolize participation in their domain. Union members cannot opt out of union governance; party members cannot form alternative parties without losing all organizational leverage. Trapped without alternatives. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZATIONAL LEADERSHIP (ROPE) — Experiences the constraint as enabling coordination: hierarchical structures solve collective action problems, enable resource concentration, and allow decisive action that flat democracies cannot achieve. Leadership benefits from coordination function without perceiving extraction. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERMEDIATE CADRE (TANGLED ROPE) — Mid-level organizers see both coordination benefits (access to resources, career advancement, ability to effect change at scale) and extraction (pressure to enforce leadership discipline, constrained autonomy, subordination to bureaucratic rules they didn't author). d≈0.58, f(d)≈0.73, σ=1.1 → χ≈0.42.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: DEMOCRATIC IDEAL / INSTITUTIONAL MEMORY (PITON) — The constraint operates by maintaining theatrical adherence to democratic forms (votes, elections, committees) while actual power concentrates in leadership. Theater_ratio=0.68 reflects that democratic processes remain visible and performative, but decision-making authority has migrated to oligarchic structures. The organization maintains democratic theater because members expect it and because the pretense confers legitimacy. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.35.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DEMOCRATIC REFORM MOVEMENT (SCAFFOLD) — Organized reformers (participatory democracy advocates, liquid democracy proponents, horizontal organization practitioners) see oligarchy as a temporary institutional problem with a sunset: digital communication, transparent governance tools, and rotating leadership protocols can rebuild democratic control. These movements temporarily accept some hierarchical structure as coordination cost while building alternatives. d≈0.35, f(d)≈0.32, σ=1.1 → χ≈0.18.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational perspective, oligarchy appears as an immutable structural property: as organizations scale, coordination costs force delegation; delegated authority creates information asymmetries; information asymmetries create power differentials; power differentials select for oligarchic structures. The constraint appears inevitable — not a contingent institutional choice but a law of organizational physics. However, the structural data (ε=0.52, suppression=0.65, theater=0.68) contradicts pure natural law. This perspective risks false summitry: naturalizing what is actually a contingent outcome of specific institutional designs.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iron_law_of_oligarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iron_law_of_oligarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iron_law_of_oligarchy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iron_law_of_oligarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iron_law_of_oligarchy, TR),
    TR >= 0.70.

:- end_tests(iron_law_of_oligarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Oligarchic leadership extracts status, authority, and often material compensation beyond what coordination necessity requires. However, the extraction is not maximal because leadership does solve genuine coordination problems — removing leadership entirely would reduce organizational capacity. The measurement trajectory (0.20→0.38→0.52) shows progressive extraction accumulation as the organization ages and power consolidates. Suppression (0.65): Significant. Members face barriers to exiting (organization monopolizes their domain), challenging leadership (information asymmetries favor leadership), or building alternatives (collective action problems, resource concentration). But suppression is not total — some members do exit, some organizations have experienced member revolts, and alternative governance models exist. Theater ratio (0.68): High and increasing. Democratic procedures (voting, elections, committee deliberations) persist and are performed, but decision-making authority has migrated to oligarchic structures. The theater has increased over time (0.30→0.52→0.68) as organizations sophisticate the performance of democracy while centralizing actual power. Claimed type (tangled_rope): The constraint combines genuine coordination benefits (hierarchies solve agency problems in large groups, enable resource concentration, allow decisive action) with asymmetric extraction (leadership captures disproportionate benefits, uses information asymmetries to sustain power). Both beneficiaries (leadership) and victims (members) are required by the schema; both exist. Enforcement is active: leadership continuously works to sustain oligarchic structures through selective information, co-optation of potential challengers, and control of organizational resources.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is profound. The leadership sees a coordination mechanism (Rope) — they are solving real problems that flat democracies cannot handle. The rank-and-file members see extraction with no escape (Snare) — they bear costs without decision power. The intermediate cadre experience both benefits and constraints (Tangled Rope) — they are partially co-opted by access to leadership but remain subordinate. The democratic ideal has degraded into theater (Piton) — democratic forms persist through institutional inertia while their function erodes. The reform movement sees a temporary problem with solutions (Scaffold) — participatory governance and digital transparency can rebuild democratic control. The natural law observer risks seeing inevitability (Mountain) — all organizations above a certain scale are doomed to oligarchy — but the structural data reveals this as a false summit: the extractiveness (0.52) and theater (0.68) are contingent on specific institutional designs, not intrinsic to organization itself. Different organization designs (cooperatives, federated networks, rotating leadership) produce different outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Organizational Leadership: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Rank-and-File Members: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Members have no realistic exit options because the organization often monopolizes their domain (union, political party, professional association). Intermediate Cadre: Victim + constrained (with some beneficiary properties) → d≈0.58, f(d)≈0.73. Moderate extraction. They are partially co-opted but remain subordinate. Democratic Ideal: Victim + constrained → d≈0.68, f(d)≈1.08. The democratic function is suppressed while its forms persist. Democratic Reform Movement: Organized + mobile → d≈0.35, f(d)≈0.32. Low effective extraction because this group has agency and sees exit paths (building alternative governance structures). Analytical Observer: analytical → d≈0.72, f(d)≈1.15. The mountain classification comes from naturalizing contingent outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves as follows: Michels' iron law is often interpreted as a natural law — oligarchy is inevitable, an iron law as inescapable as gravity. This interpretation produces the mountain perspective. However, the structural data contradicts pure natural law. The constraint has substantial theater (0.68), indicating that oligarchy must perform democratic legitimacy — this performative requirement itself suggests the constraint is contingent, not natural. The extractiveness is moderate (0.52), not maximal — if oligarchy were purely structural inevitability (like a mountain), extraction would be lower and suppression would be complete. Instead, the high theater and moderate extraction indicate a hybrid tangled_rope structure: oligarchy persists because it solves real coordination problems (beneficiaries: organizational leadership) while simultaneously enabling rent-seeking (victims: rank-and-file members, democratic function). The empirical resolution: organizations with different governance designs (cooperatives, federated networks, rotating leadership, participatory budgeting) show that oligarchy is not inevitable at the same scale. These exist at scale (thousands of members) without reproducing Michels' pattern. Therefore, the iron law describes a common outcome of specific institutional designs (centralized authority, appointment of successors, information concentration), not a structural inevitability. The constraint is better understood as tangled_rope — real coordination benefits coupled with extractive rent-seeking — rather than mountain. The natural law observer is committing false summitry by naturalizing a contingent institutional failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scale_threshold_for_oligarchy,
    'At what organization size (or complexity metric) does oligarchy become structurally inevitable vs. contingent on design choices?',
    'Empirical comparison of organizations at different scales with different governance structures; identification of organizations that remain relatively democratic at scale (cooperative federations, open-source projects, networked movements) vs. those that oligarchize rapidly',
    'If threshold exists and is universal: mountain classification strengthened. If scale is not determinative and governance design matters more: constraint is tangled_rope or snare, not mountain. If design completely prevents oligarchy: constraint is rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_threshold_for_oligarchy, empirical, 'Organization size threshold for inevitable oligarchization').

omega_variable(
    information_technology_escape,
    'Do digital communication, radical transparency, and algorithmic decision-making create genuine alternatives to hierarchical authority structures, or do they reproduce oligarchy in new forms?',
    'Longitudinal analysis of platform governance (Reddit, Wikipedia, GitHub) and digital-native organizations (Pirate Parties, Liquid Democracy experiments, DAO governance); tracking whether decentralization technologies actually reduce power concentration or merely obscure it',
    'If technology enables escape: scaffold perspective is real, and sunset is achievable within generational timescale. If technology reproduces oligarchy: constraint transitions from tangled_rope to piton (theater shifts online but extraction persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_technology_escape, empirical, 'Whether digital governance tools enable escape from oligarchy').

omega_variable(
    extraction_vs_inevitable_coordination_cost,
    'Does oligarchy extract surplus value (enrichment of leadership beyond coordination necessity) or is it the minimal cost of coordination in large organizations?',
    'Comparison of leadership compensation and privilege levels across organizations at same scale with different governance structures; analysis of whether oligarchic privilege reflects actual coordination difficulty or rent-seeking layered onto coordination',
    'If oligarchic privilege is coordination cost: constraint is rope (low extraction, high coordination value). If privilege exceeds functional necessity: constraint is snare (extraction dominates). This determines whether Michels described a law or documented a contingent institutional failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_inevitable_coordination_cost, empirical, 'Whether oligarchy is necessary coordination cost or extractive rent').

omega_variable(
    member_countermobilization_capacity,
    'Under what conditions can rank-and-file members organize sufficient countervailing power to constrain oligarchic extraction without destroying the organization?',
    'Historical analysis of successful member revolts and failed attempts; identification of institutional designs (transparent budgets, rotated leadership, revocable mandates) that enable effective member constraint',
    'If member countermobilization is rare/difficult: snare classification confirmed. If common and effective: constraint is tangled_rope with strong victim agency. This determines whether exit options should be ''trapped'' or ''constrained'' for rank-and-file agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_countermobilization_capacity, empirical, 'Capacity for rank-and-file members to organize countervailing power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iron_law_of_oligarchy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iron_law_tr_t0, iron_law_of_oligarchy, theater_ratio, 0, 0.3).
narrative_ontology:measurement(iron_law_tr_t5, iron_law_of_oligarchy, theater_ratio, 5, 0.52).
narrative_ontology:measurement(iron_law_tr_t10, iron_law_of_oligarchy, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(iron_law_be_t0, iron_law_of_oligarchy, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(iron_law_be_t5, iron_law_of_oligarchy, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(iron_law_be_t10, iron_law_of_oligarchy, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iron_law_of_oligarchy, enforcement_mechanism).
narrative_ontology:affects_constraint(iron_law_of_oligarchy, democratic_deficit_in_organizations).
narrative_ontology:affects_constraint(iron_law_of_oligarchy, collective_action_dilemma_in_large_groups).

% DUAL FORMULATION NOTE:
% The iron law of oligarchy is downstream of two distinct structural constraints: (1) the collective action dilemma in large groups (people have difficulty coordinating without centralized authority), and (2) the democratic deficit problem (once authority is centralized, it is difficult to constrain without destroying coordination). This story focuses on how the oligarchic outcome emerges from the tension between these two upstream constraints. The downstream constraint 'democratic_deficit_in_organizations' models the erosion of democratic function once oligarchy is established.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
