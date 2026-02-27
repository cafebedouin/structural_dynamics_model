% ============================================================================
% CONSTRAINT STORY: israel_norwegian_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_norwegian_law, []).

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
 *   constraint_id: israel_norwegian_law
 *   human_readable: The Norwegian Law (Amendment to Article 42c) — Israeli Legislative Exit
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The Norwegian Law (Amendment to Article 42c of the Israeli Basic Law)
 *   permits ministers to resign from the Knesset and be replaced
 *   automatically by the next member of their party's list. The constraint
 *   simultaneously solves a coordination problem (how to handle ministerial
 *   vacancies in a coalition without government collapse) and enables
 *   extraction (executive ministers can reduce parliamentary accountability
 *   while maintaining executive power). The mechanism exhibits a perspectival
 *   split: coalition leadership and executive ministries experience it as
 *   pure coordination or beneficial hybrid; backbench legislators and small
 *   coalition partners experience it as extraction. The constraint's theater
 *   ratio (0.65) reflects that the mechanism is invoked rhetorically as a
 *   necessary constitutional feature while operating administratively as a
 *   routinized succession process. Over the observation interval (30 years),
 *   both theater and extractiveness have increased as the mechanism has been
 *   used more frequently and as parliamentary oversight norms have
 *   intensified, making the constraint appear increasingly performative
 *   rather than functionally necessary.
 *
 * KEY AGENTS:
 *   - Executive Ministers: Primary beneficiaries (institutional/arbitrage) — can resign from Knesset to focus on executive duties without triggering coalition renegotiation; reduce parliamentary constraints
 *   - Coalition Leadership: Coordinating beneficiaries (institutional/arbitrage) — benefit from automatic succession preventing ministerial vacancies and government instability
 *   - Backbench Legislators: Primary victims (powerless/trapped) — advancement to ministerial position is automatic when they advance on party list; lack autonomy in accepting or refusing promotion
 *   - Small Coalition Partner Parties: Secondary victims (moderate/constrained) — lose control over ministerial slots when resignations trigger automatic succession; constrained by coalition arithmetic
 *   - Parliamentary Oversight Institutions: Structural victims (institutional/constrained) — reduced capacity to hold ministers accountable for parliamentary votes/positions they no longer hold
 *   - Reform-Minded Opposition: Organized pressure (organized/constrained) — advocate for sunset clauses or repeal; have some structural power but are limited by current coalition control
 *   - Analytical Observer: Constitutional comparatist — can assess the law from a civilizational/global perspective on parliamentary democracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_norwegian_law, 0.38).
domain_priors:suppression_score(israel_norwegian_law, 0.48).
domain_priors:theater_ratio(israel_norwegian_law, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_norwegian_law, extractiveness, 0.38).
narrative_ontology:constraint_metric(israel_norwegian_law, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(israel_norwegian_law, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_norwegian_law, tangled_rope).
narrative_ontology:human_readable(israel_norwegian_law, "The Norwegian Law (Amendment to Article 42c) — Israeli Legislative Exit").
narrative_ontology:topic_domain(israel_norwegian_law, "political/constitutional").

domain_priors:requires_active_enforcement(israel_norwegian_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_norwegian_law, executive_ministers).
narrative_ontology:constraint_beneficiary(israel_norwegian_law, coalition_stability_mechanism).
narrative_ontology:constraint_victim(israel_norwegian_law, parliamentary_accountability).
narrative_ontology:constraint_victim(israel_norwegian_law, backbench_legislators).
narrative_ontology:constraint_victim(israel_norwegian_law, electoral_representation_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BACKBENCH LEGISLATOR (SNARE) — Trapped in a system where ministerial advancement depends on legislative obedience; lacks mechanisms to challenge or exit the constraint. A legislator on a coalition party list has no choice: if a minister ahead leaves the Knesset, the next person on the list is automatically promoted, regardless of readiness or preference. The constraint extractiveness is fully experienced — parliamentary autonomy is sacrificed for coalition stability that benefits others.
constraint_indexing:constraint_classification(israel_norwegian_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL COALITION PARTNER PARTY (SNARE) — Constrained by coalition arithmetic. A small party benefits from the mechanism when it provides ministerial slots, but is extracted from when those slots are vacated and 'replaced' by the next list member without negotiation. The party cannot renegotiate or veto — it is subject to the constraint even as it nominally agrees to coalition terms. Moderate power but no real exit: leaving the coalition triggers government collapse.
constraint_indexing:constraint_classification(israel_norwegian_law, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE MINISTRY (TANGLED ROPE) — Benefits from the coordination function: ministers can resign to focus on executive duties without triggering government collapse or forcing coalition renegotiation. The mechanism also enables extraction — the ministry can bypass legislature constraints and maintain executive stability. The ministry has arbitrage: it can use the mechanism when convenient, or negotiate alternatives if needed. Mixed coordination (solves coalition instability) and extraction (enables executive dominance).
constraint_indexing:constraint_classification(israel_norwegian_law, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COALITION LEADERSHIP (ROPE) — Coalition leaders benefit from the mechanism as a pure coordination device: automatic succession prevents empty ministerial portfolios and stabilizes the governing coalition without forcing renegotiation with each resignation. The mechanism is largely functional for this agent — it solves a genuine collective action problem (how to handle ministerial vacancies without crisis). Extraction is minimal; the constraint is experienced as legitimate coordination.
constraint_indexing:constraint_classification(israel_norwegian_law, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM-MINDED OPPOSITION COALITION (SCAFFOLD) — Organized groups calling for sunset provisions or repeal see the Norwegian Law as a temporary arrangement under pressure. Proposals for enhanced parliamentary oversight, direct appointment (rather than automatic succession), or sunset clauses after one generation represent a structural sunset: the mechanism is increasingly challenged and may be revised or replaced by more democratic alternatives. The opposition has some power but is constrained by current coalition math — they frame the law as a transitional compromise.
constraint_indexing:constraint_classification(israel_norwegian_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPARATIVE CONSTITUTIONAL VIEW (PITON) — From a comparative democracy perspective, the Norwegian Law represents an increasingly performative artifact: other democracies have eliminated similar legislative-executive coupling without government collapse (e.g., parliamentary ministers in many systems maintain dual roles or resign cleanly without automatic succession rules). The mechanism persists through institutional inertia — Israeli coalition politics has normalized it, but its functional value declines as oversight demands and transparency norms increase. Theater ratio is high because the law is often invoked rhetorically to justify executive decisions while the actual succession process is administratively routine.
constraint_indexing:constraint_classification(israel_norwegian_law, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_norwegian_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_norwegian_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_norwegian_law, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(israel_norwegian_law, TR),
    TR >= 0.70.

:- end_tests(israel_norwegian_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint provides genuine coordination benefit (prevents coalition collapse from ministerial vacancies) but also enables executive extraction (ministers bypass parliamentary accountability). The extractiveness is not as high as a pure snare (0.55+) because the mechanism does solve a real problem — but it's higher than pure coordination (ε ≤ 0.35) because it systematically privileges executive over legislative interests. The value reflects the hybrid nature. Suppression (0.48): Moderate. Backbench legislators cannot refuse automatic advancement; small coalition partners cannot renegotiate ministerial allocations; parliamentary oversight institutions have reduced visibility into ministerial decision-making. But suppression is not total (0.60+) because the constraint operates transparently within a formal legal framework, and alternative political arrangements are theoretically available (repeal, constitutional amendment, coalition renegotiation). Theater ratio (0.65): High and increasing. The mechanism is invoked as a necessary constitutional feature, but its actual functionality is debatable — other democracies maintain stable governments without it. The performative component (rhetorical justification) has increased as the law has been used more frequently and as its necessity has been more openly questioned. The law persists partly through institutional inertia and partly because it genuinely serves the coalition equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between coalition leaders/executive ministers (who see rope or tangled rope with low extraction) and backbench legislators/small partners (who see snare with high extraction). This gap arises from power asymmetry: beneficiaries have institutional power and arbitrage options, while victims are trapped or constrained. The secondary gap is between the executive's experience of necessary coordination and the analytical observer's assessment of unnecessary performance — the reform opposition occupies this middle ground, seeing the mechanism as a temporary scaffold that could be replaced by better alternatives. The theater ratio reveals the gap: the law is invoked as necessary (beneficial coordination narrative) while operating as optional routine (performative succession process).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary d-values are low because executive ministers and coalition leaders have high exit options (arbitrage) — they could theoretically dissolve the coalition, renegotiate, or operate under alternative rules. Their power is institutional, so they can shape the rules themselves. Victim d-values are high because backbench legislators are trapped (automatic advancement with no choice) and small coalition partners are constrained (cannot renegotiate). The analytical observer at civilizational scope derives a piton classification because the comparative evidence (other democracies without this law) suggests the constraint persists through theater rather than necessity. The coalition leadership's institutional/arbitrage perspective produces rope because they genuinely experience the law as solving their coordination problem (ministerial vacancies) without significant constraint on their agency.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE GATE RESOLUTION: The constraint successfully gates as tangled rope because it has (1) genuine beneficiary groups (executive ministers, coalition leadership) who experience coordination benefit, (2) genuine victim groups (backbench legislators, small coalition partners, parliamentary oversight institutions) who experience extraction, and (3) active enforcement (the law requires ministerial resignations to trigger succession; the mechanism is maintained through constitutional amendment and coalition agreements). The mandatrophy is resolved by showing that the mechanism solves a real problem (coalition instability from ministerial vacancies) while creating an extraction advantage for executives. The perspectival gap (rope vs snare vs scaffold) reflects different structural positions, not a confusion about the constraint's type. The piton perspective reveals the law as increasingly performative as comparative evidence (other stable democracies without it) mounts, but the constraint remains tangled rope at the primary classification level because the coordination function is real even as its necessity is questioned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coalition_stability_necessity,
    'Is the Norwegian Law actually necessary for coalition stability, or do other parliamentary democracies maintain stable governments without automatic ministerial succession rules?',
    'Comparative analysis of parliamentary systems (UK, Germany, Australia) with dual legislative-executive roles; correlation of ministerial vacancy timelines with government collapse rates; coalition durability data from systems with and without automatic succession',
    'If unnecessary: the law is revealed as extraction disguised as coordination, and extractiveness increases to snare range (ε ≥ 0.55). If necessary: coordination benefit is confirmed, and the law remains tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_stability_necessity, empirical, 'Whether the Norwegian Law is structurally necessary for coalition stability').

omega_variable(
    automatic_succession_representation_impact,
    'Does automatic succession (replacement by next list member) meaningfully undermine electoral representation and party list accountability, or is it a standard feature of party list systems?',
    'Empirical study of party list systems across democracies; measurement of voter expectations vs actual outcomes when list members advance; analysis of backbench legislator political efficacy before/after next-list-member advancement; cross-national norms for ministerial advancement in proportional representation systems',
    'If significant harm to representation: victims'' classification strengthens (higher d values); suppression score increases. If standard feature: the law is less extractive, and supplants pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automatic_succession_representation_impact, empirical, 'Impact of automatic succession on electoral representation and party list integrity').

omega_variable(
    executive_accountability_mechanism,
    'Do ministerial resignations under the Norwegian Law actually reduce executive accountability, or are resigned ministers still accountable through other mechanisms (party discipline, future electoral consequences, personal reputation)?',
    'Case study analysis of ministerial resignations and subsequent accountability: party punishment, electoral consequences, career impacts; comparison of accountability mechanisms for resigned ministers vs those who remain in parliament; longitudinal tracking of public perception of executive constraints',
    'If accountability is effectively maintained: extraction is lower, suppression is lower, the law is closer to rope. If accountability is reduced: extraction increases, suppression increases, the law moves toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_accountability_mechanism, empirical, 'Whether ministerial resignations reduce executive accountability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_norwegian_law, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(norw_tr_t0, israel_norwegian_law, theater_ratio, 0, 0.45).
narrative_ontology:measurement(norw_tr_t15, israel_norwegian_law, theater_ratio, 15, 0.6).
narrative_ontology:measurement(norw_tr_t30, israel_norwegian_law, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(norw_be_t0, israel_norwegian_law, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(norw_be_t15, israel_norwegian_law, base_extractiveness, 15, 0.33).
narrative_ontology:measurement(norw_be_t30, israel_norwegian_law, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_norwegian_law, enforcement_mechanism).
narrative_ontology:affects_constraint(israel_norwegian_law, coalition_government_stability).
narrative_ontology:affects_constraint(israel_norwegian_law, legislative_oversight_capacity).
narrative_ontology:affects_constraint(israel_norwegian_law, ministerial_accountability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israel_norwegian_law, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
