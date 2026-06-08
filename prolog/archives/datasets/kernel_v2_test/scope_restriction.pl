% ============================================================================
% CONSTRAINT STORY: scope_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scope_restriction, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: scope_restriction
 *   human_readable: Scope Restriction of Dirty Hands to Political Domain
 *   domain: political_philosophy/normative_ethics/applied_ethics
 *
 * SUMMARY:
 *   The scope restriction of dirty hands to the political domain is a
 *   boundary-drawing practice in normative ethics. Walzer's 1973 essay
 *   'Political Action: The Problem of Dirty Hands' acknowledges that private
 *   individuals face structurally similar dilemmas but restricts analytical
 *   focus to political actors. This restriction has become conventional in
 *   political philosophy: dirty hands is treated as a problem of political
 *   ethics, not general moral theory. The constraint coordinates analysis by
 *   creating a bounded domain but potentially extracts from non-political
 *   contexts by denying them the same analytical framework. The restriction's
 *   status is ambiguous: is it a substantive claim about moral reality
 *   (political dirty hands is distinctive) or a methodological choice
 *   (political cases are analytically tractable)? Walzer's text supports the
 *   weaker reading, but the literature has often treated it as the stronger
 *   claim. The theater_ratio (0.42) reflects moderate performative content:
 *   much discussion of 'political distinctiveness' rehearses the boundary
 *   without justifying it. The extractiveness (0.28) is low-moderate: the
 *   restriction does coordinate analysis, but it also suppresses recognition
 *   of structurally identical dilemmas in medicine, business, family life,
 *   and personal relationships.
 *
 * KEY AGENTS:
 *   - Political Theorists: Primary beneficiaries (institutional/mobile) — the restriction creates a specialized subdomain with its own literature, conferences, and career paths
 *   - Political Actors: Primary beneficiaries (institutional/arbitrage) — the restriction provides legitimating vocabulary for role-differentiated morality in public office
 *   - Political Philosophy Subdiscipline: Institutional beneficiary (institutional/mobile) — the restriction justifies a distinct research program
 *   - Applied Ethicists: Mixed position (moderate/constrained) — benefit from the analytical framework but constrained by its arbitrary boundary
 *   - Private Moral Agents: Victims (powerless/trapped) — denied access to the legitimating framework for their own tragic choices
 *   - Analytical Observer: Sees coordination function (analytical/analytical) — the restriction serves tractability, not suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scope_restriction, 0.28).
domain_priors:suppression_score(scope_restriction, 0.35).
domain_priors:theater_ratio(scope_restriction, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scope_restriction, extractiveness, 0.28).
narrative_ontology:constraint_metric(scope_restriction, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(scope_restriction, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scope_restriction, rope).
narrative_ontology:human_readable(scope_restriction, "Scope Restriction of Dirty Hands to Political Domain").
narrative_ontology:topic_domain(scope_restriction, "political_philosophy/normative_ethics/applied_ethics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scope_restriction, political_theorists).
narrative_ontology:constraint_beneficiary(scope_restriction, political_actors).
narrative_ontology:constraint_beneficiary(scope_restriction, political_philosophy_subdiscipline).
narrative_ontology:constraint_vindicates(scope_restriction, political_domain_distinctiveness).
narrative_ontology:constraint_vindicates(scope_restriction, public_private_moral_asymmetry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLITICAL THEORIST (ROPE) — The scope restriction solves a genuine coordination problem: it creates a bounded domain for analyzing role-specific moral constraints without collapsing into general moral philosophy. Enables specialized analysis of political responsibility, institutional constraints, and public accountability. Low extraction — the restriction serves analytical tractability.
constraint_indexing:constraint_classification(scope_restriction, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: POLITICAL ACTOR (ROPE) — The restriction provides legitimating vocabulary for role-differentiated morality in public office. Coordinates expectations about when ordinary moral constraints can be overridden for public goods. Net beneficiary — the scope restriction creates conceptual space for justifying actions that would be impermissible in private life.
constraint_indexing:constraint_classification(scope_restriction, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: APPLIED ETHICIST (TANGLED ROPE) — The restriction both enables and constrains. It coordinates analysis of political cases but suppresses recognition of structurally identical dilemmas in medicine, business, family life, and personal relationships. The arbitrary boundary extracts from non-political domains by denying them the same analytical framework. Mixed coordination and extraction.
constraint_indexing:constraint_classification(scope_restriction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIVATE MORAL AGENT (SNARE) — The restriction denies that private-sphere actors face structurally identical tragic choices. A parent choosing between honesty and protecting a child, a doctor choosing between patient autonomy and preventing harm, a friend choosing between loyalty and truth — all face dirty hands dilemmas but are excluded from the legitimating framework. The scope restriction extracts by withholding moral recognition.
constraint_indexing:constraint_classification(scope_restriction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — The restriction is a coordination mechanism for managing analytical complexity. Walzer's acknowledgment that dirty hands applies to private life but restriction of analysis to politics reflects a pragmatic boundary: political cases have public visibility, institutional structure, and role-differentiation that make them analytically tractable. The restriction is not a claim about moral reality but a methodological choice. Low extraction — the boundary serves tractability, not suppression.
constraint_indexing:constraint_classification(scope_restriction, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scope_restriction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scope_restriction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scope_restriction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(scope_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The scope restriction does solve a genuine coordination problem — it creates a bounded domain for analyzing role-specific moral constraints without collapsing into general moral philosophy. Political cases have public visibility, institutional structure, and role-differentiation that make them analytically tractable. However, the restriction also suppresses recognition of structurally identical dilemmas in non-political domains. A parent choosing between honesty and protecting a child, a doctor choosing between patient autonomy and preventing harm, a friend choosing between loyalty and truth — all face dirty hands dilemmas but are excluded from the analytical framework. The extraction is real but not severe: the framework is not withheld by active enforcement, and applied ethicists can (and do) extend dirty hands analysis to non-political domains. The boundary is porous. Suppression (0.35): Low-moderate. The restriction is maintained by convention and citation patterns, not by active gatekeeping. No enforcement mechanism prevents extending dirty hands to private life — Walzer himself acknowledges the extension. The suppression is soft: it operates through framing and focus rather than prohibition. Theater ratio (0.42): Moderate. Much discussion of political distinctiveness rehearses the boundary without justifying it. The restriction is often treated as obvious or self-evident, but the justification gap (omega 1) remains open. The performative content has increased over time as the restriction has become conventional — early discussions engaged the boundary question; later work assumes it.
 *
 * PERSPECTIVAL GAP:
 *   The political theorist and political actor see pure coordination (Rope) — the restriction creates a bounded analytical domain and legitimating vocabulary. The applied ethicist sees mixed coordination and extraction (Tangled Rope) — the framework is valuable but the boundary is arbitrary and suppresses non-political applications. The private moral agent sees extraction (Snare) — the restriction denies moral recognition to their tragic choices. The analytical observer sees coordination (Rope) — the boundary is pragmatic, not principled, but it serves analytical tractability. The gap reveals that the restriction's function depends on structural position: beneficiaries experience coordination; those excluded from the framework experience extraction. The key question (omega 1) is whether the restriction can be justified on principled grounds or whether it is an arbitrary boundary that serves the interests of political philosophy as a subdiscipline.
 *
 * DIRECTIONALITY LOGIC:
 *   Political theorists and political actors are primary beneficiaries. The restriction creates a specialized analytical domain (political ethics) with its own literature, conferences, and career paths. Political actors benefit from legitimating vocabulary that distinguishes their role-specific obligations from ordinary morality. Both groups have mobile or arbitrage exit options — they can work in general moral philosophy or other domains if the restriction becomes untenable. Applied ethicists occupy a mixed position: they benefit from the analytical framework (dirty hands provides a powerful lens for tragic choice) but are constrained by its arbitrary boundary (why should political cases get special treatment?). Their exit options are constrained — they work within the restriction but push against it. Private moral agents are victims: they face structurally identical dilemmas but are denied the legitimating framework. A parent's choice to lie to protect a child is not recognized as a dirty hands case, even though it has the same structure as a politician's choice to deceive for public safety. Private agents have no exit — they cannot opt out of the moral dilemmas they face. The analytical observer sees coordination: the restriction serves tractability by focusing on cases with public visibility and institutional structure. The boundary is methodological, not metaphysical.
 *
 * MANDATROPHY ANALYSIS:
 *   The scope restriction resolves a potential mandatrophy between coordination and extraction by showing that both are present and perspectival. From the political theorist's position, the restriction is legitimate coordination — it creates analytical tractability. From the private moral agent's position, the restriction is extraction — it denies moral recognition. The mandatrophy is not 'which is it?' but 'for whom?' The restriction coordinates analysis for those within the political domain while extracting from those outside it. The analytical observer's task is to recognize both functions and assess whether the coordination benefits justify the extraction costs. The omega variables identify the unresolved questions: Is the restriction principled or arbitrary? Does Walzer claim substantive distinctiveness or methodological focus? Does universal application preserve or dilute analytical precision? Does institutional role rather than political domain mark the relevant boundary? These questions determine whether the restriction is justified coordination or unjustified extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    justification_gap,
    'Can the scope restriction be justified on principled grounds, or is it an arbitrary analytical boundary?',
    'Examination of whether political dirty hands has structural features (publicity, institutional role, collective agency) that genuinely distinguish it from private-sphere cases, or whether the restriction is merely conventional.',
    'If principled: the restriction is legitimate coordination (Rope from all perspectives). If arbitrary: the restriction is extraction from non-political domains (Tangled Rope or Snare from applied ethics and private agent perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justification_gap, conceptual, 'Whether political domain has principled distinctiveness or restriction is arbitrary').

omega_variable(
    walzer_inconsistency,
    'Does Walzer''s acknowledgment of private-life applicability undermine his restriction to political analysis?',
    'Close reading of Walzer''s text: does he claim dirty hands is unique to politics (strong restriction) or merely that political cases are his analytical focus (weak restriction)? If weak, the ''restriction'' is methodological, not substantive.',
    'If strong restriction: Walzer''s position is internally inconsistent. If weak restriction: the ''constraint'' is a misreading — Walzer is making a scope choice, not a scope claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(walzer_inconsistency, conceptual, 'Whether Walzer''s restriction is substantive claim or methodological choice').

omega_variable(
    domain_proliferation,
    'If dirty hands is recognized as universal, does the concept lose analytical precision through domain proliferation?',
    'Empirical analysis of whether extending dirty hands to medicine, business, family life, etc. produces genuine insights or dilutes the concept into ''any hard moral choice.'' Test: do domain-specific applications identify new structural features or merely relabel familiar dilemmas?',
    'If proliferation preserves precision: the scope restriction is unjustified extraction. If proliferation dilutes: the restriction serves coordination by maintaining analytical focus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_proliferation, empirical, 'Whether universal application preserves or dilutes analytical precision').

omega_variable(
    institutional_role_threshold,
    'Is there a threshold of institutional role-differentiation below which dirty hands analysis loses traction?',
    'Comparative analysis: do dirty hands dilemmas in highly institutionalized private roles (corporate executive, hospital administrator, university president) have more in common with political cases than with informal private roles (parent, friend, neighbor)? If yes, the relevant boundary is institutional vs. informal, not political vs. private.',
    'If institutional role is the relevant variable: the political/private boundary is a proxy for institutional/informal, and the scope restriction is mislabeled. If political domain is genuinely distinctive: the restriction is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_role_threshold, conceptual, 'Whether institutional role rather than political domain is the relevant boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scope_restriction, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scope_rest_tr_t0, scope_restriction, theater_ratio, 0, 0.3).
narrative_ontology:measurement(scope_rest_tr_t15, scope_restriction, theater_ratio, 15, 0.35).
narrative_ontology:measurement(scope_rest_tr_t30, scope_restriction, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(scope_rest_be_t0, scope_restriction, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(scope_rest_be_t15, scope_restriction, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(scope_rest_be_t30, scope_restriction, base_extractiveness, 30, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scope_restriction, information_standard).

% DUAL FORMULATION NOTE:
% The scope restriction is downstream of political_exceptionalism (the broader claim that political morality is distinctive). If political_exceptionalism is unjustified, the scope restriction inherits that defect. The two constraints are linked but distinct: political_exceptionalism is a substantive moral claim; scope_restriction is an analytical boundary practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
