% ============================================================================
% CONSTRAINT STORY: qualified_immunity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   human_readable: Qualified Immunity Doctrine in U.S. Law Enforcement
 *   domain: political/legal/police_accountability
 *
 * SUMMARY:
 *   Qualified immunity is a judge-created doctrine that shields law
 *   enforcement officers from civil liability for constitutional rights
 *   violations unless the plaintiff can prove the officer violated a 'clearly
 *   established' right. Originating in Harlow v. Fitzgerald (1982) as a
 *   coordination mechanism to protect officers from paralyzing litigation,
 *   the doctrine has evolved into a systematic barrier to accountability.
 *   Citizens harmed by police misconduct face near-total suppression of
 *   remedy: even when rights are violated, qualified immunity bars recovery
 *   unless identical circumstances were litigated and decided before the
 *   violation occurred. This creates an irreducible Catch-22: rights cannot
 *   be 'clearly established' without prior litigation, but qualified immunity
 *   prevents that litigation from proceeding. The constraint exhibits
 *   features of a pure extraction mechanism (Snare) from the perspective of
 *   injured citizens and civil rights advocates, while law enforcement
 *   experiences it as necessary coordination overhead (Rope). The doctrine's
 *   theater ratio has increased substantially since 1982 as courts have
 *   invoked increasingly formalist 'clearly established law' reasoning to
 *   dismiss cases at summary judgment, transforming qualified immunity from a
 *   balanced burden-shift into a near-absolute bar. The extractiveness
 *   trajectory shows gradual expansion from 1982 (when the doctrine was
 *   narrower) through 2024 (when Supreme Court jurisprudence has
 *   systematically broadened immunity). Multiple states have begun abolishing
 *   or modifying qualified immunity, creating a potential sunset dynamic.
 *
 * KEY AGENTS:
 *   - Injured Citizens: Primary victim (powerless/trapped) — cannot exit; bear full cost of constitutional violations without remedy
 *   - Civil Rights Advocates: Secondary victim (moderate/constrained) — trapped in unwinnable litigation; extraction of advocacy resources
 *   - Law Enforcement Officers and Agencies: Primary beneficiary (institutional/arbitrage) — protected from personal and departmental liability; experience doctrine as necessary coordination for decisive action
 *   - Municipal Governments and Taxpayers: Mixed actor (organized/constrained) — benefit from officer immunity transfer but bear liability costs via municipal insurance
 *   - Federal Judiciary: Institutional maintainer (institutional/arbitrage) — sustains the doctrine through summary judgment rulings and formalist interpretation; maintains through institutional inertia rather than active coordination function
 *   - State Legislatures and Reform Coalition: Organized agents (organized/mobile) — see qualified immunity as replaceable; building alternative accountability frameworks with sunset trajectory
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the doctrine as immutable when it is a contingent U.S. institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity, 0.68).
domain_priors:suppression_score(qualified_immunity, 0.78).
domain_priors:theater_ratio(qualified_immunity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity, extractiveness, 0.68).
narrative_ontology:constraint_metric(qualified_immunity, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(qualified_immunity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity, snare).
narrative_ontology:human_readable(qualified_immunity, "Qualified Immunity Doctrine in U.S. Law Enforcement").
narrative_ontology:topic_domain(qualified_immunity, "political/legal/police_accountability").

domain_priors:requires_active_enforcement(qualified_immunity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity, municipal_governments).
narrative_ontology:constraint_victim(qualified_immunity, citizens_harmed_by_police).
narrative_ontology:constraint_victim(qualified_immunity, constitutional_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INJURED PLAINTIFF (SNARE) — Trapped without exit. Even when constitutional rights are violated, qualified immunity bars recovery unless the right was 'clearly established' at the time of violation. d≈0.98, f(d)≈1.42, σ=1.0 → χ≈0.96. Maximum extraction: no remedy, no recourse, no alternative venue.
constraint_indexing:constraint_classification(qualified_immunity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS ADVOCATES (SNARE) — Constrained by the doctrine's Catch-22: rights are only clearly established if already litigated, but litigation requires surviving summary judgment, which qualified immunity prevents. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.78. Systematic extraction of advocacy resources into unwinnable litigation.
constraint_indexing:constraint_classification(qualified_immunity, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LAW ENFORCEMENT AGENCIES (ROPE) — Experience qualified immunity as coordination. The doctrine enables officers to act decisively in ambiguous situations without fear of personal liability. This is genuinely functional for coordination: it solves the collective action problem of officers freezing when faced with complex situations. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net beneficiary. The extraction is real but law enforcement frames it as necessary coordination overhead.
constraint_indexing:constraint_classification(qualified_immunity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MUNICIPAL GOVERNMENTS (TANGLED ROPE) — Municipalities benefit from qualified immunity (reduced officer liability transfers to government insurance). But they also bear the cost: litigation expenses, settlements, tax burden. Exit is constrained by federal statute. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.44. Mixed: receive immunity benefit while bearing liability costs. Active enforcement required to maintain the doctrine's statutory frame.
constraint_indexing:constraint_classification(qualified_immunity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — State legislatures and federal reformers see qualified immunity as a temporary institutional arrangement that can be replaced. Several states have abolished or modified the doctrine (Colorado, Connecticut). Federal proposals exist. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.31. Low effective extraction because these agents have political mobility and see a sunset path.
constraint_indexing:constraint_classification(qualified_immunity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL JUDICIARY (PITON) — The judiciary maintains qualified immunity through summary judgment rulings despite knowing the doctrine prevents constitutional accountability. This is performative institutional maintenance: courts invoke formalist reasoning ('clearly established law') to avoid confronting the systemic harm. theater_ratio=0.65 (high). The doctrine persists through judicial inertia, not because it achieves its stated coordination purpose of protecting officers. d≈0.10, f(d)≈-0.02, σ=1.0 → χ≈-0.01. Negligible net extraction from judiciary's perspective; the institution maintains the constraint through institutional habit.
constraint_indexing:constraint_classification(qualified_immunity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT) — From a civilizational perspective, one might argue that qualified immunity is necessary to prevent paralyzing litigation against officers, making it an immutable feature of any large-scale law enforcement system. However, the structural data (ε=0.68, suppression=0.78, theater=0.65) contradicts this. Multiple democracies (UK, Canada, Australia) operate effective police systems without qualified immunity, proving the 'necessity' framing is incorrect. This perspective represents a false summit: the constraint is not a natural law but a contingent U.S. institutional choice.
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
 *   Extractiveness (0.68): High. The doctrine systematically bars recovery for constitutional violations. The 'clearly established law' standard functions as a near-absolute bar because new fact patterns are constantly encountered, and qualified immunity shields officers precisely when rights are most uncertain. The extractiveness has increased from 0.35 (1982) to 0.68 (2024) as Supreme Court jurisprudence (Kisela v. Hughes, Bahrampour v. Lampert) has broadened immunity by requiring plaintiffs to identify prior cases with nearly identical facts. Suppression (0.78): Very high. Injured citizens face near-total suppression of remedy. Courts dismiss qualified immunity cases at summary judgment (before trial) in the vast majority of cases. Publication bias ensures that immunity is rarely tested — cases don't reach verdict. Career risk exists for plaintiffs: pursuing civil rights litigation is resource-intensive and often yields nothing. Alternative exit routes (state tort law, criminal prosecution) are constrained by state-level variation and prosecutorial reluctance. Theater ratio (0.65): Moderate-high. Courts invoke formalist language ('clearly established law', 'objective reasonableness') to justify immunity rulings, but the underlying logic is performative. The phrase 'clearly established law' sounds like a neutral standard but functions as a moving target that systematically favors immunity. The theatrical content has increased as courts have developed increasingly sophisticated formalist reasoning to protect officers.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence across the observation site. Injured citizens see pure extraction (Snare) with no escape route and no remedy. Law enforcement sees coordination (Rope) — the doctrine solves the real problem of officers needing to act decisively without fear of personal bankruptcy. Civil rights advocates see a structural trap (Snare) with a perverse incentive structure. Municipalities see mixed extraction-coordination (Tangled Rope) — they benefit from immunity but bear the cost via taxpayer liability. The judiciary sees an institutional norm (Piton) — qualified immunity is maintained through formalist reasoning and inertia, not because it achieves its stated coordination function. State reformers see a temporary institution (Scaffold) — alternative accountability frameworks are emerging. The analytical observer risks seeing the doctrine as a natural law immutable to large-scale law enforcement (Mountain), but this is a false summit: other democracies operate effective police systems without qualified immunity. The perspectival gap reveals the fundamental asymmetry: the constraint benefits institutional actors (law enforcement, municipalities) while extracting from vulnerable individuals (citizens harmed by police, civil rights advocates with no standing).
 *
 * DIRECTIONALITY LOGIC:
 *   Injured citizens: Victim + trapped → d≈0.98, f(d)≈1.42. Maximum extraction. No alternative remedy available; constitutional rights are suspended by the doctrine. Civil rights advocates: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction. Can organize through advocacy but face structural barriers (summary judgment, Catch-22 circularity). Law enforcement: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Can exit the constraint by exercising authority decisively; the doctrine removes threat of personal liability. Municipalities: Both + constrained → d≈0.50, f(d)≈0.65. Mixed. Benefit from officer immunity but bear taxpayer liability; exit is constrained by federal statute and state policy variation. Federal judiciary: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.02. Institutional custodian, not net victim or beneficiary. Maintains the doctrine through institutional habit rather than active extraction. State reformers: Organized + mobile → d≈0.40, f(d)≈0.40. Low effective extraction; have political capacity and see exit path (legislative modification/abolition). The directionality pattern confirms the snare classification: high extraction from trapped victims, benefits for institutional beneficiaries, constrained exit for secondary actors, and negligible extraction for the judicial custodian (which maintains the constraint through institutional inertia rather than active benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint's extractiveness (0.68) exceeds the snare floor (0.66), and mandatrophy must be resolved. The resolution shows that qualified immunity is definitionally extractive, not coordinative. The original Harlow (1982) justification—that officers need protection from paralyzing litigation to act decisively—was genuinely coordinative (Rope-like). But the doctrine has systematically expanded such that it now functions as near-total immunity: citizens cannot recover even for clear constitutional violations if the 'right' is not clearly established. The expansion from ε=0.35 (1982) to ε=0.68 (2024) reflects rent-seeking layering: the coordination benefit has atrophied while the extraction mechanism has strengthened. The theater ratio (0.65) shows that courts now maintain the doctrine through formalist reasoning ('clearly established law') rather than functional coordination. This is the classic Piton trajectory: a Rope (or weakly coordinative constraint) has degraded into extraction sustained by institutional theater. The mandatrophy is resolved by recognizing that qualified immunity is extractive, not coordinative. Law enforcement's perspective (Rope) reflects their genuine benefit-cost ratio, but the macro-level constraint is Snare: it systematically bars remedy for citizens while protecting officers. The doctrine should be classified as Snare at the analytical level (the Catch-22 circularity and empirical evidence that alternatives exist make this clear), while acknowledging the law enforcement perspective sees it as coordination. The Piton perspective (judicial maintenance through theater) is the most architecturally significant: the constraint is sustained not by genuine coordination function but by courts' institutional habit of invoking formalist reasoning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clearly_established_law_circularity,
    'Can the ''clearly established law'' standard ever be satisfied, or does it create an irreducible Catch-22 where rights cannot be clearly established without prior litigation that qualified immunity prevents?',
    'Historical analysis of rights successfully certified as ''clearly established''; rates of summary judgment dismissal vs. verdict emergence; comparison of ''clearly established'' determination across Circuit courts',
    'If circularity is irreducible: qualified immunity functions as a complete bar to recovery (Snare confirmed). If circularity can be broken: qualified immunity functions as a burden-shifting mechanism (weaker extraction, Tangled Rope more prominent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clearly_established_law_circularity, conceptual, 'Whether clearly established law standard creates irreducible circularity').

omega_variable(
    officer_decisionmaking_inhibition,
    'Does removal of qualified immunity actually inhibit officer decisionmaking in complex situations, or do officers in non-qualified-immunity jurisdictions (and countries) act equally decisively?',
    'Comparative empirical study: officer response times and decisiveness in qualified-immunity vs. non-qualified-immunity states; use-of-force rates; civilian complaint patterns; officer morale surveys',
    'If inhibition is real: qualified immunity''s coordination function is genuine (Rope perspective validated). If inhibition is minimal: the coordination benefit is theater (Snare or Piton classification more accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(officer_decisionmaking_inhibition, empirical, 'Whether qualified immunity removal actually inhibits officer decisionmaking').

omega_variable(
    constitutional_remedy_alternative_sufficiency,
    'Can alternative accountability mechanisms (state tort law, statutory damages, administrative discipline, federal habeas corpus) adequately substitute for qualified immunity removal?',
    'Comparative liability analysis: damages awards under state tort law in non-qualified-immunity states; settlement patterns; administrative discipline rates; effectiveness of criminal prosecutions for civil rights violations',
    'If alternatives are sufficient: qualified immunity is redundant extraction (pure Snare). If alternatives are insufficient: removal creates genuine coordination problems (qualified immunity has Rope components).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_remedy_alternative_sufficiency, empirical, 'Whether alternative accountability mechanisms can substitute for qualified immunity removal').

omega_variable(
    municipal_insurance_moral_hazard,
    'Does qualified immunity, by transferring liability to municipalities/taxpayers, create moral hazard that reduces departmental accountability and training investment?',
    'Analysis of departmental discipline rates, training quality, and risk management practices in high-qualified-immunity vs. lower-immunity jurisdictions; correlation between municipal liability exposure and officer discipline/retraining',
    'If moral hazard is significant: qualified immunity extracts from the taxpayer collective while reducing systemic accountability (Snare with multi-victim structure). If minimal: municipal insurance functions as efficient risk distribution (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(municipal_insurance_moral_hazard, empirical, 'Whether qualified immunity creates moral hazard in municipal accountability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qimm_theater_1982, qualified_immunity, theater_ratio, 1982, 0.4).
narrative_ontology:measurement(qimm_theater_2000, qualified_immunity, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(qimm_theater_2024, qualified_immunity, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(qimm_extractiveness_1982, qualified_immunity, base_extractiveness, 1982, 0.35).
narrative_ontology:measurement(qimm_extractiveness_2000, qualified_immunity, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(qimm_extractiveness_2024, qualified_immunity, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity, police_qualified_immunity_interaction).
narrative_ontology:affects_constraint(qualified_immunity, municipal_liability_framework).
narrative_ontology:affects_constraint(qualified_immunity, civil_rights_remedy_exhaustion).

% DUAL FORMULATION NOTE:
% Qualified immunity decomposes into three structurally distinct claims: (1) Officer personal liability shield (ε≈0.35, coordination function genuine but limited), (2) Clearly established law standard (ε≈0.70, Catch-22 circularity makes remedy impossible), and (3) Judicial maintenance through formalist reasoning (ε≈0.65, theater-based piton). This constraint story focuses on the integrated doctrine (ε=0.68) but should be linked to downstream constraints that address specific jurisdictional impacts and reform scenarios.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
