% ============================================================================
% CONSTRAINT STORY: qualified_immunity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity, []).

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
 *   constraint_id: qualified_immunity
 *   human_readable: Qualified Immunity Doctrine
 *   domain: political/legal
 *
 * SUMMARY:
 *   Qualified immunity is a judicial doctrine that shields police officers
 *   and other government officials from civil liability unless they violated
 *   a 'clearly established' constitutional right. Established in Harlow v.
 *   Fitzgerald (1982) and refined through subsequent SCOTUS decisions, the
 *   doctrine has evolved from a coordination mechanism (protecting officials
 *   from frivolous suits) into an extraction and suppression mechanism that
 *   systematically bars remedies for constitutional injuries. The constraint
 *   exhibits the full range of DR classifications: from the injured
 *   civilian's perspective, it is a snare with no exit; from the police
 *   department's perspective, it is a beneficial coordination mechanism
 *   (rope); from the civil rights advocate's perspective, it is a snare with
 *   suppressed legislative alternatives; from the reform coalition's
 *   perspective, it is a temporary institutional arrangement with a policy
 *   sunset (scaffold); from the common law tradition's perspective, it is a
 *   degraded ritual maintained by institutional inertia (piton); and from an
 *   analytical civilizational perspective that naturalizes it as inherent to
 *   state function, it is a false mountain. The constraint's theater_ratio
 *   has increased from 0.40 to 0.65 over the interval, indicating that the
 *   performative justification (careful judicial review, clearly established
 *   law standard) has grown while the actual functional constraint on police
 *   conduct has weakened. Extractiveness has risen from 0.35 to 0.68 as the
 *   doctrine's application has become increasingly permissive and as the gap
 *   between stated doctrine (careful review) and actual outcomes
 *   (near-blanket immunity) has widened.
 *
 * KEY AGENTS:
 *   - Injured Civilians: Primary victims (powerless/trapped) — suffer constitutional injuries with no civil remedy pathway; cannot exit jurisdiction or regime
 *   - Law Enforcement Officers: Primary beneficiaries (institutional/arbitrage) — protected from civil liability; can restructure operations within doctrine's edges; benefit substantially from immunity umbrella
 *   - Police Departments: Secondary beneficiaries (institutional/arbitrage) — reduce litigation costs; face reduced accountability pressure; benefit from collective immunity protection
 *   - Civil Rights Advocates: Secondary victims (moderate/constrained) — must litigate at SCOTUS level for any doctrine change; legislative override faces union resistance; exit constrained by institutional barriers
 *   - Supreme Court: Powerful institutional actor (powerful/mobile) — maintains doctrine through precedent; benefits from reduced docket burden and simplified gatekeeping rule; can shift doctrine through new precedent
 *   - Reform Coalition: Organized actors (organized/constrained) — civil rights organizations, affected communities, legislative advocates; see reform as feasible through electoral change and statutory override; constrained by current legal structure but mobilizing for policy change
 *   - Individual Officers: Moderate agents (moderate/constrained) — benefit from liability protection but experience reputational harm and ethical constraint when doctrine shields misconduct; trapped in employment structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity, 0.68).
domain_priors:suppression_score(qualified_immunity, 0.75).
domain_priors:theater_ratio(qualified_immunity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity, extractiveness, 0.68).
narrative_ontology:constraint_metric(qualified_immunity, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(qualified_immunity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity, snare).
narrative_ontology:human_readable(qualified_immunity, "Qualified Immunity Doctrine").
narrative_ontology:topic_domain(qualified_immunity, "political/legal").

domain_priors:requires_active_enforcement(qualified_immunity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity, municipal_governments).
narrative_ontology:constraint_victim(qualified_immunity, civilians_with_constitutional_injuries).
narrative_ontology:constraint_victim(qualified_immunity, victims_of_excessive_force).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INJURED CIVILIAN (SNARE) — Victim with no meaningful exit from the constraint. Police misconduct causes injury; qualified immunity bars civil remedy; suppression of alternatives through doctrine prevents legislative override. Trapped by jurisdiction, unable to escape prior harm. Maximum extraction from this agent's position.
constraint_indexing:constraint_classification(qualified_immunity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POLICE DEPARTMENT (ROPE) — Institutional beneficiary with arbitrage options. Qualified immunity reduces liability costs; creates coordination benefit (officers act with confidence without constant litigation threat). Benefits substantially exceed costs. Can often restructure operations to navigate doctrine's edges.
constraint_indexing:constraint_classification(qualified_immunity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: INDIVIDUAL OFFICER (TANGLED ROPE) — Faces genuine coordination problem (needs legal protection from frivolous suits) but also faces extraction via career risk, disciplinary exposure, and reputational harm when doctrine shields truly wrongful conduct. Exit constrained by employment; experiences both protection and restraint on ethical action.
constraint_indexing:constraint_classification(qualified_immunity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS ADVOCATE (SNARE) — Constrained by institutional barriers. Cannot exit the jurisdiction; legislative override faces police union resistance and judicial entrenchment. Must continuously litigate at SCOTUS level. Extraction mechanism: endless litigation cost with minimal doctrine change, suppression of legislative alternatives.
constraint_indexing:constraint_classification(qualified_immunity, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPREME COURT (TANGLED ROPE) — Doctrine provides coordination function (clear rule for judicial gatekeeping, reduces caseload at district level). Also enables extraction: doctrine insulates police conduct from review, suppresses alternative checks (legislative oversight, administrative remedies). Court benefits from simplified doctrine and reduced docket burden but is increasingly criticized for enabling misconduct.
constraint_indexing:constraint_classification(qualified_immunity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM COALITION (SCAFFOLD) — Organized actors (civil rights groups, affected communities) perceive qualified immunity as a temporary institutional arrangement with a policy sunset. Reform movements frame abolition as inevitable (George Floyd Act, state-level alternatives). Low effective extraction because organized agents see exit path and coalition strength. Theater_ratio lower from this view: doctrine appears performatively justified but substantively weakening.
constraint_indexing:constraint_classification(qualified_immunity, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: COMMON LAW TRADITION (PITON) — Qualified immunity is justified by reference to historical common law principles and judicial efficiency. However, the doctrine's actual function (absolute immunity in most cases) has degraded far from its stated purpose. Theater ratio high: academic coverage and case-law citations maintain appearance of meaningful review while actual review rarely succeeds. Doctrine persists through institutional inertia, not functional necessity.
constraint_indexing:constraint_classification(qualified_immunity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some immunity for government actors is inherent to functioning state apparatus: if every official act could trigger individual liability, governance would collapse. This perspective naturalizes qualified immunity as an immutable structural feature. However, the structural data contradicts the mountain classification: qualified immunity is a doctrine (contingent, embedded in precedent) not a law of nature. The engine's false summit detector identifies this as naturalization of institutional arrangements.
constraint_indexing:constraint_classification(qualified_immunity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qualified_immunity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qualified_immunity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(qualified_immunity, TR),
    TR >= 0.70.

:- end_tests(qualified_immunity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The doctrine's initial function (protecting officials from frivolous suits) has eroded as the 'clearly established law' standard has become increasingly permissive. Courts grant qualified immunity in the vast majority of cases by finding no clearly established law at the level of specificity demanded by recent precedent. The extraction derives from: (1) systematic denial of civil remedy to injured parties, (2) suppression of alternative accountability mechanisms (administrative, criminal, legislative), and (3) concentration of decision-making power in police departments with minimal external constraint. The rise from 0.35 to 0.68 reflects the doctrine's increasing permissiveness over 40 years. Suppression (0.75): Very high. Qualified immunity suppresses alternatives through: (1) doctrine itself (civil liability barred), (2) judicial gatekeeping (summary judgment before trial), (3) institutional inertia (SCOTUS reluctance to overturn precedent), (4) political economy (police unions block legislative reform), and (5) naturalization (doctrine framed as inherent to state function rather than contingent doctrinal choice). Theater ratio (0.65): Moderate-high. The doctrine's stated justification (careful judicial review, clearly established law standard) maintains appearance of meaningful constraint while actual review rarely succeeds. Academic literature and judicial opinions cite the test's protections; actual outcomes show near-blanket immunity. Theater ratio has risen as the gap between stated and actual function has widened.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is fundamental and irreconcilable from within the doctrine itself. The beneficiary (police department) experiences rope or near-zero extraction; the victim (injured civilian) experiences snare (maximum extraction); the advocate (civil rights organization) experiences snare with suppressed alternatives; the organized reform coalition experiences scaffold (temporary, solvable). These classifications derive from different structural positions, not from different measurements of the same phenomenon. They cannot be averaged or reconciled — they are simultaneous truths from different vantage points. The gap reveals that qualified immunity functions fundamentally differently for different agents: it is a coordination mechanism for police, a snare for victims, and a degraded ritual for the legal tradition. The presheaf over the observation site (the set of all perspectives) is the complete description; no single perspective captures the full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position: beneficiary status, victim status, and exit options. Police departments (beneficiaries with arbitrage options) experience low d values (high beneficiary discount), producing negative or near-zero χ from their perspective — they see the constraint as beneficial coordination. Injured civilians (victims with trapped exit) experience high d values, producing maximum χ — they experience pure extraction. Civil rights advocates (moderate power, constrained exit) experience moderate-high d, experiencing significant extraction. The reform coalition (organized power, constrained exit) experiences lower d than powerless victims because organizational capacity increases perceived exit options — they see a solvable problem with a policy sunset. The piton classification derives from theater_ratio (0.65 ≥ 0.70 threshold approached), indicating that the constraint's performative justification increasingly exceeds its actual function. The mountain classification at the analytical level is marked as a false summit: the derivation chain produces mountain because extractiveness is scored near 0.68 (high) and suppression is scored near 0.75 (very high), but the analytical observer's perspective (universal scope, civilizational time horizon) risks naturalizing a contingent doctrine as an immutable law. The engine's false summit detector should flag this as naturalization of institutional arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION OF MANDATROPHY (extraction > 0.70): Qualified immunity resolves the mandatrophy by demonstrating that a high-extraction constraint can function legitimately as a hybrid mechanism when viewed from the beneficiary's perspective (coordination function) while functioning as pure extraction from the victim's perspective. The constraint is not 'really' snare or 'really' rope — it is simultaneously both, depending on structural position. The mandatrophy is resolved not by recalibrating metrics but by accepting perspectival divergence as structural fact. From the police department's perspective, the doctrine solves a genuine coordination problem (officers need protection from frivolous suits). From the civilian's perspective, it is pure extraction with suppressed alternatives. Both are true. The doctrine's increasing extractiveness (rising from 0.35 to 0.68) reflects that the benefits to the beneficiary have solidified while the costs to victims have accumulated. The doctrine remains stable because the beneficiary (law enforcement) has greater institutional power than the victims (dispersed, unorganized injured parties) and because the doctrine is entrenched in SCOTUS precedent. The scaffold perspective (reform coalition) offers a path to resolution: abolish or substantially reform the doctrine through legislative action and/or new SCOTUS precedent. The current status is unresolved mandatrophy with high institutional stability — the constraint serves beneficiaries well enough that change requires external (political, electoral) intervention, not internal (legal, doctrinal) evolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clearly_established_law_threshold,
    'What standard of ''clearly established law'' actually provides meaningful constraint on police discretion vs. serves as doctrinal cover for qualified immunity grants?',
    'Quantitative analysis of qualified immunity success rates pre/post Saucier and Pearson shifts; comparison of ''clearly established law'' standard stringency across circuits; longitudinal study of cases where doctrine would have blocked cases before precedent shifted',
    'If threshold is meaningfully constraining: doctrine may approach a true coordination function (moderate extraction). If threshold is permissive: doctrine functions as de facto immunity (high extraction, snare classification from victim perspective holds).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clearly_established_law_threshold, empirical, 'Whether clearly-established-law standard constrains immunity or enables blanket protection').

omega_variable(
    frivolous_suit_counterfactual,
    'In a regime without qualified immunity, what percentage of individual officer liability suits would be truly frivolous vs. legitimate misconduct claims currently barred by doctrine?',
    'Comparative study with states/countries that use different immunity standards (comparative law); simulation models of litigation behavior absent qualified immunity; historical analysis of pre-Harlow (1982) litigation patterns',
    'If frivolous suits >> 10%: coordination function is substantial, doctrine may justify moderate suppression. If frivolous suits << 5%: doctrine extracts primarily by blocking legitimate claims, not by filtering noise. Doctrine then appears as pure extraction mechanism (snare) with false coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frivolous_suit_counterfactual, empirical, 'Proportion of frivolous vs. legitimate suits that qualified immunity blocks').

omega_variable(
    reform_pathway_structural_feasibility,
    'Is abolishing or substantially reforming qualified immunity structurally feasible within the current constitutional and political architecture, or does the doctrine serve irreducible governance functions that would require systemic alternatives?',
    'Analysis of state-level immunity alternatives and their outcomes; examination of alternative liability regimes (European models, administrative law frameworks); assessment of whether structural changes could preserve legitimate officer protections while enabling meaningful remedy',
    'If feasible: scaffold perspective is structural (reform timeline ~10-20 years, sunset clause real). If infeasible: doctrine may be mountain-adjacent (deep structural constraint requiring fundamental governance redesign). If partially feasible: tangled_rope classification confirmed (mixed function and extraction, requiring hybrid reform).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_pathway_structural_feasibility, conceptual, 'Whether qualified immunity abolition is structurally feasible or requires systemic alternatives').

omega_variable(
    democratic_legitimacy_deficit,
    'To what extent does qualified immunity doctrine insulate police conduct from democratic oversight and accountability mechanisms, vs. legitimate separation of powers between courts and legislatures?',
    'Comparative analysis of legislative immunity reform attempts and judicial blocking; survey of public opinion on police accountability vs. qualified immunity doctrine awareness; analysis of whether doctrine serves primarily to block statutory reform or to manage frivolous litigation',
    'If doctrine primarily blocks democratic reform: extraction mechanism and suppression gate are clear (snare classification). If doctrine reflects legitimate judicial prudence: tangled_rope or rope classification may be more accurate. If deficit is near-total: mandatrophy shift toward pure snare possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_deficit, preference, 'Whether qualified immunity insulates police from democratic oversight').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qi_tr_t0, qualified_immunity, theater_ratio, 0, 0.4).
narrative_ontology:measurement(qi_tr_t20, qualified_immunity, theater_ratio, 20, 0.52).
narrative_ontology:measurement(qi_tr_t40, qualified_immunity, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(qi_be_t0, qualified_immunity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qi_be_t20, qualified_immunity, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(qi_be_t40, qualified_immunity, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity, police_accountability_vacuum).
narrative_ontology:affects_constraint(qualified_immunity, civil_rights_litigation_barrier).
narrative_ontology:affects_constraint(qualified_immunity, municipal_government_liability_reduction).

% DUAL FORMULATION NOTE:
% Qualified immunity itself is a single constraint, but its effects propagate through multiple downstream institutional mechanisms: (1) police_accountability_vacuum — the absence of meaningful internal or external accountability, (2) civil_rights_litigation_barrier — the structural elimination of the Section 1983 remedy pathway, (3) municipal_government_liability_reduction — the downstream benefit to municipal governments that face reduced vicarious liability. These are distinct constraints with their own extractiveness values but are structurally dependent on qualified immunity's continued force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity, institutional, 0.15).
constraint_indexing:directionality_override(qualified_immunity, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
