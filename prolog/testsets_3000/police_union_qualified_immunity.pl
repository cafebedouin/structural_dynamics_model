% ============================================================================
% CONSTRAINT STORY: police_union_qualified_immunity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_police_union_qualified_immunity, []).

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
 *   constraint_id: police_union_qualified_immunity
 *   human_readable: Police Union Qualified Immunity
 *   domain: criminal_justice/employment_law
 *
 * SUMMARY:
 *   Police union qualified immunity represents a structural extraction
 *   mechanism embedded in American criminal justice that protects officers
 *   and police departments from civil liability for constitutional
 *   violations. Qualified immunity originated in 1982 (Harlow v. Fitzgerald)
 *   as a doctrinal compromise intended to balance individual officer
 *   accountability with operational flexibility — a genuine coordination
 *   problem. Over 40+ years, the doctrine has evolved into near-absolute
 *   immunity in practice while maintaining a nominal 'clearly established
 *   law' standard that is rarely met. The constraint exhibits multiple
 *   classification types depending on structural position: pure extraction
 *   from the perspective of injured civilians and marginalized communities
 *   (Snare), coordination mechanism from the perspective of police
 *   departments and unions (Rope), mixed coordination-extraction from
 *   individual officer perspective (Tangled Rope), institutional theater from
 *   the perspective of the legal doctrine itself (Piton), and contingent
 *   rather than inevitable from the analytical/comparative perspective. The
 *   extractiveness trajectory shows accumulation over the interval: from 0.45
 *   (2010s, post-Ferguson activism creating pressure for reform) to 0.68
 *   (2020s, reform momentum stalled in legislatures, Supreme Court reaffirmed
 *   immunity doctrine). Theater ratio has increased from 0.42 to 0.58 as the
 *   doctrine's nominal 'clearly established' standard becomes increasingly
 *   decoupled from functional accountability.
 *
 * KEY AGENTS:
 *   - Injured Civilians: Primary victims (powerless/trapped) — no civil suit remedy; bear full cost of misconduct with no recourse
 *   - Marginalized Communities: Primary victims (powerless/identity_locked) — disproportionate exposure to police misconduct; identity-fused with state subordination through historical racialization of policing
 *   - Police Departments: Primary beneficiaries (institutional/arbitrage) — benefit from reduced liability exposure, simplified risk allocation, enables aggressive enforcement without fear of civil judgments
 *   - Police Unions: Primary beneficiaries (powerful/arbitrage) — use qualified immunity as negotiating tool, member retention mechanism, and shield against external accountability pressure
 *   - Individual Officers: Mixed (moderate/constrained) — benefit from personal immunity but trapped in departments with high misconduct cultures; cannot exit without career damage
 *   - Civil Rights Advocacy Coalition: Organized agents (organized/constrained) — legally and politically mobilized to challenge immunity; constrained by litigation costs and political barriers
 *   - Legal Doctrine Itself: Institutional (institutional/arbitrage) — persists through inertia; serves original beneficiaries despite doctrinal degradation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — reveals qualified immunity as contingent U.S. choice, not law of nature; most liberal democracies enable direct accountability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(police_union_qualified_immunity, 0.68).
domain_priors:suppression_score(police_union_qualified_immunity, 0.72).
domain_priors:theater_ratio(police_union_qualified_immunity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(police_union_qualified_immunity, extractiveness, 0.68).
narrative_ontology:constraint_metric(police_union_qualified_immunity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(police_union_qualified_immunity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(police_union_qualified_immunity, snare).
narrative_ontology:human_readable(police_union_qualified_immunity, "Police Union Qualified Immunity").
narrative_ontology:topic_domain(police_union_qualified_immunity, "criminal_justice/employment_law").

domain_priors:requires_active_enforcement(police_union_qualified_immunity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(police_union_qualified_immunity, police_departments).
narrative_ontology:constraint_beneficiary(police_union_qualified_immunity, individual_officers).
narrative_ontology:constraint_victim(police_union_qualified_immunity, civilians_subjected_to_misconduct).
narrative_ontology:constraint_victim(police_union_qualified_immunity, civil_rights_claimants).
narrative_ontology:constraint_victim(police_union_qualified_immunity, constitutional_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INJURED CIVILIAN (SNARE) — Victim of police misconduct with no meaningful recourse. Qualified immunity eliminates civil suit remedy; criminal prosecution requires separate political will; internal discipline is opaque and toothless. Trapped with no exit — cannot sue, cannot appeal, cannot compel accountability. Maximum experienced extraction.
constraint_indexing:constraint_classification(police_union_qualified_immunity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITY (SNARE / IDENTITY_LOCKED) — Communities subject to disproportionate police contact and misconduct experience qualified immunity as a structural extraction mechanism embedded in their relationship with law enforcement. Identity-locked through racialized historical experience of policing — exit from subordination would require not just legal reform but reconstituting the community's relationship with state authority itself. Suppression operates through normalized state violence and epistemic closure about policing necessity.
constraint_indexing:constraint_classification(police_union_qualified_immunity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: POLICE DEPARTMENT ADMINISTRATION (ROPE) — Benefits from qualified immunity as a coordination mechanism: enables risk-taking in enforcement without liability exposure, reduces pension and insurance burdens, and simplifies officer hiring/retention. Experiences constraint as coordination — the rules enable collective action (aggressive policing) that would be individually risky if officers faced civil suit. Net beneficiary with arbitrage options (can lobby, negotiate union contracts, allocate risk internally).
constraint_indexing:constraint_classification(police_union_qualified_immunity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICE UNION (ROPE) — Extracts value by using qualified immunity as a negotiating tool and retention mechanism. Union operates as coordinator: ensures member protection, standardizes defense costs, and maintains solidarity against external accountability pressure. Experiences constraint as enabling collective bargaining power. Powerful actor with multiple arbitrage options (contract renegotiation, legal advocacy, political mobilization).
constraint_indexing:constraint_classification(police_union_qualified_immunity, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INDIVIDUAL OFFICER (TANGLED_ROPE) — Mixed experience: qualified immunity provides genuine coordination function (protects officers from frivolous suits, reduces risk of personal ruin) AND creates asymmetric extraction (enables misconduct without consequence, creates collective reputation damage, traps officers in departments with high-misconduct cultures). Constrained — cannot exit without career damage; cannot remain without complicity. Moderate power — collective bargaining gives some leverage but individual officer is subordinate to union and department.
constraint_indexing:constraint_classification(police_union_qualified_immunity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CIVIL RIGHTS ADVOCACY COALITION (TANGLED_ROPE) — Organized agents (legal nonprofits, civil rights groups, victim organizations) see qualified immunity as both a coordination problem (the constraint exists to enable law enforcement) and an extraction mechanism (it eliminates civilian accountability). Coalition is constrained by limited litigation resources and political pressure to defend 'officer safety' but has some agency through legislative advocacy and precedent litigation. Extractiveness is real but not maximal because coalition can partially circumvent (state law remedies, selective publicity, building political coalitions).
constraint_indexing:constraint_classification(police_union_qualified_immunity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: QUALIFIED IMMUNITY DOCTRINE (PITON) — The doctrine itself persists through institutional inertia and legal theater. Supreme Court originated qualified immunity as a doctrinal compromise (Harlow v. Fitzgerald 1982) designed to balance officer accountability with operational flexibility. Over 40+ years, the doctrine has become degraded: it functions as near-absolute immunity rather than the 'sliding scale' it nominally provides. Theater ratio high because courts invoke 'clearly established law' standard that is rarely met, creating performative due process. The doctrine persists not because it serves its original function well but because institutional actors benefit from it and alternatives are not yet institutionalized.
constraint_indexing:constraint_classification(police_union_qualified_immunity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / COMPARATIVE VIEW (SNARE) — From a civilizational/comparative perspective, qualified immunity is not a natural law of policing but a specific U.S. doctrinal choice. Most liberal democracies (UK, Canada, Australia, EU states) enable civil suits against police and maintain functional policing through direct accountability rather than immunity. This perspective reveals qualified immunity as structurally contingent — not inevitable, not required by the nature of law enforcement, but a particular extraction mechanism institutionalized in American law. The global view shows the snare classification is generalizable, not naturalized.
constraint_indexing:constraint_classification(police_union_qualified_immunity, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(police_union_qualified_immunity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(police_union_qualified_immunity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(police_union_qualified_immunity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(police_union_qualified_immunity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(police_union_qualified_immunity, TR),
    TR >= 0.70.

:- end_tests(police_union_qualified_immunity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint systematically eliminates civil suit remedy for constitutional violations, creating near-absolute immunity in practice through the 'clearly established law' requirement that is rarely satisfied. Extractiveness increased from 0.45 to 0.68 over the interval as Supreme Court decisions made qualified immunity even more difficult to overcome and legislative reform efforts stalled. The increase reflects accumulation of favorable precedent for immunity rather than change in underlying mechanism — the doctrine has become more entrenched. Suppression (0.72): High. Multiple barriers prevent accountability: (1) civil suits blocked by qualified immunity, (2) criminal prosecution requires separate political will and high evidentiary burden, (3) internal discipline is opaque and rarely results in meaningful consequences, (4) union contracts often require strong due process protections that delay/obstruct discipline, (5) qualified immunity is framed as necessary for 'officer safety' creating public narrative suppression. Theater ratio (0.58): Moderate-high. The 'clearly established law' standard creates performative due process — courts invoke the standard as though it applies meaningful constraint while de facto granting immunity in most cases. The interval shows theater increasing from 0.42 to 0.58 as the gap between nominal doctrine (balancing framework) and actual practice (near-absolute immunity) widened. Theater is lower than doctrinal theater in some medical licensing contexts because the extraction is straightforward — courts literally say 'immunity granted' rather than performing complex ritualistic evaluation.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals extraction mechanisms that would be invisible from a single perspective. The police union perspective (Rope) sees pure coordination — qualified immunity enables risk-taking. The injured civilian perspective (Snare) sees pure extraction — no accountability. Neither is wrong; both are right from their position. The gap is the structure. An observer seeing only the police perspective would conclude qualified immunity is necessary coordination. An observer seeing only the victim perspective would conclude it is pure oppression. The analytical perspective reveals both are true: coordination function is real (departments do need to manage risk), but the coordination benefit accrues entirely to police while the extraction cost accrues entirely to civilians. The constraint is coordination WITH ASYMMETRIC EXTRACTION — exactly the definition of Tangled Rope — except that from the victim perspective, the extraction is so complete and suppression so high that it appears as Snare. The perspectival gap is not ambiguity; it is a diagnostic signature of the extraction mechanism's success.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries/victims flow: (1) Police departments benefit from reduced liability exposure and simplified risk allocation — they are net beneficiaries. (2) Police unions benefit from using qualified immunity as negotiating tool and member retention mechanism — they are net beneficiaries. (3) Individual officers receive mixed benefits (personal immunity) and costs (complicity, reputation damage, institutional trap) — they are partly beneficiary, partly victim. (4) Injured civilians are pure victims — they bear full cost of misconduct with no recourse. (5) Marginalized communities are pure victims with the additional layer that the extraction is normalized through historical racialization of policing. (6) Civil rights advocates are victims of the extraction (their clients are harmed) and also targets of the constraint (their legal strategy is blocked). The directionality derivation follows: beneficiaries with arbitrage options (departments, unions) get low d; trapped victims get high d; mixed actors get moderate d. The chi formula χ = ε × f(d) × σ(S) applies scope modifier σ(S=national) = 1.0, so f(d) directly scales ε. For powerless/trapped agents, f(d) ≈ 1.42, pushing effective extraction toward maximum despite the 'moderate' base extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE OR TANGLED ROPE? The constraint classifies as Snare (pure extraction) from the victim perspectives because: (1) base extractiveness ≥ 0.46 (ε = 0.68), (2) suppression ≥ 0.60 (s = 0.72), (3) effective extraction χ ≥ 0.66. For powerless/trapped victims, f(d) ≈ 1.42, so χ = 0.68 × 1.42 × 1.0 ≈ 0.96 — extreme extraction. However, the constraint ALSO appears as Rope (pure coordination) from police perspective because the beneficiary experiences negative effective extraction (constraint subsidizes them). The mandatrophy is resolved by recognizing that qualified immunity IS coordination (it solves the real problem of managing police liability risk) but with completely asymmetric extraction (all coordination benefit goes to police, all extraction cost goes to civilians). This makes it Tangled Rope from the system perspective — it has both genuine coordination function AND asymmetric extraction. From specific agent perspectives, it appears as pure Snare (victims) or pure Rope (beneficiaries) because the asymmetry is so extreme. The engine's job is to detect that BOTH dynamics are true — there is real coordination happening, and real extraction happening, and they are fused in the same institutional mechanism. The mandatrophy is the false choice between 'this is just coordination' (beneficiary framing) and 'this is just extraction' (victim framing). The truth is the fusion: qualified immunity coordinates police liability risk management while extracting accountability rights from civilians.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clearly_established_law_threshold,
    'What standard should apply to ''clearly established law'' for qualified immunity denial? Current threshold requires prior case law establishing *identical* conduct as unconstitutional — is this threshold set to protect officers or to enable immunity?',
    'Comparative analysis: Supreme Court qualified immunity denials vs. denials under state law tests; analysis of how often ''clearly established'' requirement actually bars recovery vs. serves as absolute immunity barrier',
    'If threshold is set too high (current doctrine): qualified immunity is de facto absolute immunity (Snare). If threshold is set appropriately: qualified immunity becomes functional balancing of interests (Tangled Rope or Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clearly_established_law_threshold, conceptual, 'Whether ''clearly established law'' standard functions as designed or provides de facto absolute immunity').

omega_variable(
    union_collective_action_necessity,
    'Does qualified immunity serve a genuine collective action coordination function for police departments, or would functional departments operate similarly under direct individual liability?',
    'Empirical comparison: police performance metrics, misconduct rates, recruitment/retention, and operational metrics in jurisdictions with (a) qualified immunity, (b) state law immunity that is narrower, (c) no immunity but high indemnification. Compare to international police forces without qualified immunity.',
    'If coordination function is genuine: qualified immunity is Rope or Tangled Rope (enables necessary collective action with some extraction overhead). If function is not genuine: qualified immunity is pure extraction (Snare), and coordination rationale is cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(union_collective_action_necessity, empirical, 'Whether qualified immunity serves genuine coordination function or is pure extraction').

omega_variable(
    marginalized_community_exit_modulation,
    'For the identity_locked perspective applied to marginalized communities: is the binding mechanism cognitive capture (the community has internalized the inevitability of police sovereignty) or structural (the community genuinely has no alternative institutions or has tried and failed to create them)?',
    'Historical analysis: documented movements for alternative public safety, community policing models, civilian oversight adoption; comparison with communities that have successfully reconstituted police accountability relationships',
    'If primarily cognitive: identity_lock is accurate — the constraint is changeable if the identity frame breaks. If primarily structural: identity_locked is misclassified as trapped (high-cost barriers) or constrained (surmountable barriers). Classification of the marginalized community perspective determines whether the constraint appears as culturally embedded (identity_locked rope) or as externally imposed (trapped snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_community_exit_modulation, conceptual, 'Whether identity-lock for marginalized communities is cognitive capture or structural barrier').

omega_variable(
    individual_officer_complicity_threshold,
    'At what point does an officer''s participation in a misconduct-heavy department constitute complicity that makes the tangled_rope classification inaccurate, and the officer becomes a willing beneficiary (rope) rather than a constrained participant?',
    'Behavioral analysis: officer participation in complaints against colleagues, whistleblowing rates, voluntary exit rates, demographic patterns in who stays vs. leaves high-misconduct departments',
    'If complicity threshold is high: most officers are genuinely constrained (Tangled Rope). If threshold is low: many officers have chosen the extraction (Rope), and institutional response should target retention incentives differently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individual_officer_complicity_threshold, preference, 'Threshold for individual officer complicity in misconduct environment').

omega_variable(
    civil_rights_coalition_bargaining_power,
    'Has the civil rights coalition''s bargaining power increased or decreased over the 2010-2025 period? If increasing, does this modulate the constraint from Snare toward Tangled Rope, or from Tangled Rope toward Rope?',
    'Longitudinal analysis: successful litigation outcomes, legislative wins (qualified immunity reform bills), media/political salience of police accountability, voter support for reform measures across time',
    'If coalition power is increasing: the constraint is transitioning from Snare toward Tangled Rope, and the interval measurements should show theater_ratio declining and base_extractiveness declining as institutional response increases. If power is stalled: constraint remains Snare with high extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civil_rights_coalition_bargaining_power, empirical, 'Whether civil rights coalition is gaining or losing bargaining power over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(police_union_qualified_immunity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(puqi_tr_t0, police_union_qualified_immunity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(puqi_tr_t7, police_union_qualified_immunity, theater_ratio, 7, 0.5).
narrative_ontology:measurement(puqi_tr_t15, police_union_qualified_immunity, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(puqi_be_t0, police_union_qualified_immunity, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(puqi_be_t7, police_union_qualified_immunity, base_extractiveness, 7, 0.58).
narrative_ontology:measurement(puqi_be_t15, police_union_qualified_immunity, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(police_union_qualified_immunity, enforcement_mechanism).
narrative_ontology:affects_constraint(police_union_qualified_immunity, civil_rights_litigation_access).
narrative_ontology:affects_constraint(police_union_qualified_immunity, police_accountability_culture).
narrative_ontology:affects_constraint(police_union_qualified_immunity, state_tort_immunity_doctrine).

% DUAL FORMULATION NOTE:
% Qualified immunity interacts with several related constraints: (1) civil rights litigation access (Section 1983 lawsuits blocked by qualified immunity threshold), (2) police accountability culture (immunity reduces internal incentives for discipline), (3) state tort immunity doctrine (parallel state law doctrines provide alternative paths but are less developed). Each upstream/downstream relationship reflects how qualified immunity serves as a chokepoint preventing other accountability mechanisms from functioning. The network decomposition separates the doctrinal mechanism (qualified immunity itself) from its institutional effects (culture, litigation access, alternative remedies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(police_union_qualified_immunity, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
