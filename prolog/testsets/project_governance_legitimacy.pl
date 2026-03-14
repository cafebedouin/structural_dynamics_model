% ============================================================================
% CONSTRAINT STORY: project_governance_legitimacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_project_governance_legitimacy, []).

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
 *   constraint_id: project_governance_legitimacy
 *   human_readable: Project Governance Legitimacy Constraint
 *   domain: organizational_governance/institutional_authority
 *
 * SUMMARY:
 *   Project governance legitimacy constraints arise when the authority
 *   structure required to coordinate complex work diverges from stakeholder
 *   agency and decision participation. The constraint exhibits classic
 *   extraction mechanics: leadership consolidates authority to improve
 *   execution speed and coherence, benefiting from efficient coordination
 *   while bearing no cost; contributors sacrifice voice and agency, paying
 *   suppression costs (limited input, constrained exit) while bearing no
 *   benefit. The constraint is not that governance is hard — legitimacy
 *   crises emerge specifically when the gap between claimed stakeholder
 *   participation and actual decision authority becomes institutionalized.
 *   This story models governance legitimacy as a tangled rope: genuine
 *   coordination (shared resource governance, aligned incentives around
 *   project success) coexists with asymmetric extraction (decisions imposed
 *   despite stakeholder objections, authority maintained through suppression
 *   of alternatives). The theater ratio (0.68) reflects that formal
 *   governance procedures (voting, consultation, oversight meetings) are
 *   increasingly performative — the real decisions concentrate in leadership,
 *   while the procedures provide legitimacy appearance. As organizations
 *   scale, governance legitimacy constraints predictably degrade: theater
 *   increases (more extensive consultation rituals with less real voice),
 *   suppression persists (contributors cannot exit without career cost), and
 *   extractiveness accumulates (authority concentration accelerates).
 *
 * KEY AGENTS:
 *   - Project Leadership: Primary beneficiary (institutional/arbitrage) — consolidates authority, improves execution speed, captures efficiency gains and decision discretion
 *   - Project Contributors: Primary victim (powerless/trapped) — embedded through career dependence and sunk knowledge investment; forced to accept decisions with no meaningful voice
 *   - Organized Stakeholders: Secondary actors (moderate/constrained) — maintain partial voice through coalitions but constrained by switching costs and relationship investments; both coordinate and suffer extraction
 *   - Governance Reform Coalition: Organized agents (organized/constrained) — transparency initiatives, stakeholder councils, participatory budgeting building alternative pathways with sunset logic
 *   - Formal Authority Structure: Institutional persistence (institutional/arbitrage) — written governance charter, voting procedures, oversight mechanisms maintained through inertia despite low functional legitimacy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing governance centralization as inherent to organizational scale rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(project_governance_legitimacy, 0.52).
domain_priors:suppression_score(project_governance_legitimacy, 0.58).
domain_priors:theater_ratio(project_governance_legitimacy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(project_governance_legitimacy, extractiveness, 0.52).
narrative_ontology:constraint_metric(project_governance_legitimacy, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(project_governance_legitimacy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(project_governance_legitimacy, tangled_rope).
narrative_ontology:human_readable(project_governance_legitimacy, "Project Governance Legitimacy Constraint").
narrative_ontology:topic_domain(project_governance_legitimacy, "organizational_governance/institutional_authority").

domain_priors:requires_active_enforcement(project_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(project_governance_legitimacy, project_leadership).
narrative_ontology:constraint_beneficiary(project_governance_legitimacy, central_authority).
narrative_ontology:constraint_victim(project_governance_legitimacy, project_contributors).
narrative_ontology:constraint_victim(project_governance_legitimacy, stakeholder_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMBEDDED CONTRIBUTOR (SNARE) — Contributors lack meaningful exit options. Trapped by career dependence on project credentials, sunk investment in domain knowledge, and organizational lock-in. Must accept governance decisions with no voice. Minimum agency; maximum extraction.
constraint_indexing:constraint_classification(project_governance_legitimacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ORGANIZED STAKEHOLDER (TANGLED ROPE) — Stakeholders are constrained by switching costs and relationship investments, but maintain partial voice through coalitions. Some coordination benefit (shared resource governance) exists alongside asymmetric extraction (decisions imposed despite objections). Moderate agency with meaningful constraints.
constraint_indexing:constraint_classification(project_governance_legitimacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROJECT LEADERSHIP (ROPE) — Leadership experiences governance legitimacy as coordination mechanism. Can arbitrage across competing stakeholder demands. Benefits from authority consolidation while delivering functional coordination. Experiences constraint as enabling rather than extractive.
constraint_indexing:constraint_classification(project_governance_legitimacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GOVERNANCE REFORM COALITION (SCAFFOLD) — Organized reform actors (stakeholder councils, transparency initiatives, participatory budgeting pilots) see the legitimacy bottleneck as a temporary coordination failure with sunset logic. Alternative governance pathways (distributed decision-making, stakeholder councils) are building exits from centralized authority. Extraction persists only while old governance model blocks alternatives.
constraint_indexing:constraint_classification(project_governance_legitimacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL AUTHORITY STRUCTURE (PITON) — The written governance charter, voting procedures, and oversight mechanisms persist through institutional inertia despite low functional legitimacy. The authority structure sees itself as degraded — maintained because transition costs are high, not because it commands genuine consent. Theater ratio dominates real coordination function.
constraint_indexing:constraint_classification(project_governance_legitimacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some governance legitimacy gap is inherent to collective coordination: complex decisions always require some authority concentration, and the gap between stakeholder preference and leadership decision is a structural feature of scaled organizations. This perspective risks naturalizing what is actually a contingent institutional choice. False summit risk: observing that centralized governance is more efficient than pure consensus does not make legitimacy constraints immutable laws of organizational physics.
constraint_indexing:constraint_classification(project_governance_legitimacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(project_governance_legitimacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(project_governance_legitimacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(project_governance_legitimacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(project_governance_legitimacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(project_governance_legitimacy, TR),
    TR >= 0.70.

:- end_tests(project_governance_legitimacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Leadership captures significant benefits from authority consolidation (execution speed, decision coherence, resource allocation control) while contributors absorb costs. However, this is not maximal extraction because genuine coordination function exists — shared commitment to project success aligns many leadership-contributor interests. The extracted surplus reflects authority asymmetry, not total antagonism. Suppression (0.58): Moderate-high. Contributors face material barriers to exit (career dependence on credentials, sunk knowledge investment) and constrained voice options (formal channels are performative, informal channels are suppressed). But suppression is not total — some contributors have mobility, some organizations do respond to coalition pressure. Theater ratio (0.68): High and rising. Governance procedures (voting, consultation, oversight meetings) have become increasingly performative relative to actual decision mechanisms. As complexity rises and authority must concentrate, the theater increases to maintain legitimacy appearance while real decisions concentrate in leadership. The trajectory in measurements shows this degradation over time.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across power and exit axes. Leadership sees coordination (Rope) — they are solving the legitimate problem of coordinating complex work at scale. Contributors see extraction (Snare) — their voice is irrelevant to outcomes. Organized stakeholders see mixed function and extraction (Tangled Rope) — they maintain partial voice but decisions override objections. The reform coalition sees a temporary problem with alternatives (Scaffold) — participatory governance pathways are building exits. The formal authority structure sees itself as degraded (Piton) — procedures persist through inertia despite low legitimacy. The civilizational observer risks seeing immutable natural law (Mountain) — complexity requires authority concentration — but the structural data reveals this as false summit: the authority concentration is contingent on organizational choice, not inherent to coordination problems.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership derives low directionality (d ≈ 0.15) from beneficiary status with arbitrage options — they can exit the governance role while retaining institutional position. Contributors derive high directionality (d ≈ 0.90) from victim status with trapped exit — they cannot leave without career damage. Stakeholders derive moderate directionality (d ≈ 0.55) from mixed victim status with constrained exit — they can escalate pressure but cannot withdraw easily. The sigmoid f(d) transforms these values to effective extraction experienced by each group. Leadership experiences χ as enabling coordination; contributors experience χ as maximum extraction; stakeholders experience χ as moderate extraction with some agency.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandate mandatrophy when the governance authority claims two incompatible mandates: (1) to coordinate complex project work efficiently, and (2) to preserve stakeholder voice and participatory legitimacy. As projects scale, the efficiency mandate systematically overrides the legitimacy mandate — not because both are false, but because the authority structure chooses efficiency and rationalizes it through necessity framing ('at this scale, we must concentrate authority'). The false natural law (mountain perspective) naturalizes this choice as inherent to coordination. The resolution is to recognize that the two mandates are genuinely in tension and to measure the tradeoff explicitly: How much stakeholder voice is sacrificed for how much efficiency gain? If governance reforms (participatory budgeting, stakeholder councils, transparent decision criteria) can deliver substantial voice without catastrophic efficiency loss, then the extraction is contingent, not necessary. The tangled rope classification resolves the mandatrophy by naming both the coordination function and the asymmetric extraction as real structural features.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_definition_ambiguity,
    'Is legitimacy defined by formal procedural compliance, stakeholder consent, outcomes quality, or perceived fairness?',
    'Comparative legitimacy assessment: measure stakeholder perception against formal authority claims and actual decision outcomes; identify which dimension drives exit behavior',
    'If outcome-based: constraint softens with demonstrated success (governance becomes legitimate through results). If consent-based: constraint tightens under any authority concentration (extractiveness increases). If procedural: legitimacy persists even with low stakeholder agency (enables piton classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_definition_ambiguity, conceptual, 'Definition of governance legitimacy across dimensions').

omega_variable(
    alternative_governance_viability,
    'Can distributed stakeholder governance actually coordinate complex projects, or does it fragment decision-making and reduce execution capacity?',
    'Longitudinal case studies of organizations implementing participatory governance; measurement of decision speed, cost, and outcome quality under distributed vs centralized authority',
    'If viable: scaffold perspective confirmed and extractiveness of current model is revealed (legitimacy constraint is contingent). If not viable: centralization becomes structurally necessary, moving constraint toward mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_governance_viability, empirical, 'Whether distributed governance can functionally coordinate complex projects').

omega_variable(
    legitimacy_substitution_mechanisms,
    'Can theater (performance of legitimacy) or efficiency gains substitute for genuine stakeholder voice in maintaining contributor commitment?',
    'Survey data on contributor retention and satisfaction; correlation between formal legitimacy performance and actual exit rates; threshold analysis of when theater decouples from real commitment',
    'If substitution works: suppression can remain high while extractiveness is experienced as lower (theater maintains apparent legitimacy). If substitution fails: suppression must decrease or constraint collapses (contributors exit despite theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_substitution_mechanisms, empirical, 'Whether legitimacy theater can substitute for genuine stakeholder voice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(project_governance_legitimacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proj_gov_tr_t0, project_governance_legitimacy, theater_ratio, 0, 0.52).
narrative_ontology:measurement(proj_gov_tr_t3, project_governance_legitimacy, theater_ratio, 3, 0.62).
narrative_ontology:measurement(proj_gov_tr_t6, project_governance_legitimacy, theater_ratio, 6, 0.68).
narrative_ontology:measurement(proj_gov_tr_t9, project_governance_legitimacy, theater_ratio, 9, 0.71).

% Extraction over time
narrative_ontology:measurement(proj_gov_be_t0, project_governance_legitimacy, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(proj_gov_be_t3, project_governance_legitimacy, base_extractiveness, 3, 0.46).
narrative_ontology:measurement(proj_gov_be_t6, project_governance_legitimacy, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(proj_gov_be_t9, project_governance_legitimacy, base_extractiveness, 9, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(project_governance_legitimacy, enforcement_mechanism).
narrative_ontology:affects_constraint(project_governance_legitimacy, organizational_trust_degradation).
narrative_ontology:affects_constraint(project_governance_legitimacy, contributor_exit_acceleration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(project_governance_legitimacy, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
