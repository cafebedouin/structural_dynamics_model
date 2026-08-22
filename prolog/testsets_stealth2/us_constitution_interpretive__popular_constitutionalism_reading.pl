% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism Reading of Constitutional Interpretive Authority
 *   domain: legal/political
 *
 * SUMMARY:
 *   In the American constitutional order, the question of who gives the
 *   Constitution its binding meaning is itself contested. This story
 *   instantiates the popular-constitutionalism answer: constitutional meaning
 *   is shaped by popular political movements and democratic contestation,
 *   with courts interpreting provisionally rather than finally. The
 *   arrangement solves a real legitimacy problem — a text claiming authority
 *   from 'We the People' cannot rest its binding force on nine unelected
 *   lawyers alone — and it has a real operating record: abolition, suffrage,
 *   the New Deal settlement, and the civil-rights transformation were all won
 *   in movement politics before or alongside courtroom victory. The same
 *   machinery carries asymmetric costs: minorities whose shelter depended on
 *   insulated judicial enforcement are exposed precisely at majoritarian
 *   surges, institutions needing stable rules absorb perpetual reopening, and
 *   the bench's finality claim — the specific object of contest — is steadily
 *   displaced. The claim/metric split is deliberate: the type is claimed from
 *   structure (genuine coordination plus asymmetric burden plus enforcement
 *   dependence), while the metrics describe observed operation, including a
 *   rising share of ritualized participation. KEY AGENTS (by structural
 *   relationship): - popular_movements: Primary beneficiary
 *   (organized/constrained) — abolitionist, suffrage, labor, and civil-rights
 *   traditions that claim and win constitutional meaning through mobilization
 *   - legislative_majorities: Beneficiary and co-administrator
 *   (institutional/arbitrage) — assert constitutional readings through
 *   statute and venue-shop between branches - anti_elitist_claimants:
 *   Beneficiary (moderate/constrained) — citizens claiming constitutional
 *   standing against credentialed interpreters - state_governments:
 *   Beneficiary and co-administrator (institutional/constrained) —
 *   departmentalist assertion and state-level experimentation -
 *   counter_majoritarian_dependent_minorities: Primary bearer of costs
 *   (powerless/trapped) — shelter formerly provided by insulated courts now
 *   exposed to majoritarian definition - constitutional_settlement_seekers:
 *   Cost-bearing seat (powerful/constrained) — institutions repricing plans
 *   around perpetually reopened questions - judicial_finality_advocates:
 *   Cost-bearing seat (institutional/identity_locked) — legal elites whose
 *   professional identity is fused with the finality ideal being displaced -
 *   federal_judiciary: Dual-positioned administrator and cost-bearer
 *   (institutional/identity_locked) — retains routine interpretation while
 *   its supremacy claim is the contested object - noncitizen_residents:
 *   Excluded voice (powerless/constrained) — governed by popularly-made
 *   meaning without franchise - constitutional_theorists: Analytical observer
 *   (analytical/analytical) — maps the authority structure from outside the
 *   contest
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.49).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.43).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.49).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.43).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, '2f5a2b78-3130-4774-81e7-1656602734c5').
narrative_ontology:cs_kernel_codification('2f5a2b78-3130-4774-81e7-1656602734c5', fixed_text).
narrative_ontology:cs_authority_grounding('2f5a2b78-3130-4774-81e7-1656602734c5', practice).
narrative_ontology:cs_reading_relation('2f5a2b78-3130-4774-81e7-1656602734c5', us_constitution_interpretive__originalist_reading, influences).
narrative_ontology:cs_reading_relation('2f5a2b78-3130-4774-81e7-1656602734c5', us_constitution_interpretive__living_constitution_reading, influences).
narrative_ontology:cs_axiom('2f5a2b78-3130-4774-81e7-1656602734c5', foundational, popular_sovereignty_constitutes_interpretive_authority).
narrative_ontology:cs_axiom_status(popular_sovereignty_constitutes_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('2f5a2b78-3130-4774-81e7-1656602734c5', popular_sovereignty_constitutes_interpretive_authority, deontological).
narrative_ontology:cs_axiom('2f5a2b78-3130-4774-81e7-1656602734c5', foundational, judicial_supremacy_normatively_unwarranted).
narrative_ontology:cs_axiom_status(judicial_supremacy_normatively_unwarranted, holdable).
narrative_ontology:cs_axiom_grounding('2f5a2b78-3130-4774-81e7-1656602734c5', judicial_supremacy_normatively_unwarranted, instrumental).
narrative_ontology:cs_reference_frame('2f5a2b78-3130-4774-81e7-1656602734c5', popular_sovereign_continuing_authorship).
narrative_ontology:cs_drift_state('2f5a2b78-3130-4774-81e7-1656602734c5', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f5a2b78-3130-4774-81e7-1656602734c5', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, state_governments).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_dependent_minorities).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_settlement_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, federal_judiciary).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, departmentalist_review_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize sustained campaigns to redefine constitutional provisions — abolition, suffrage, labor, civil rights — through protest, electoral pressure, and doctrinal argument outside the courtroom. Wins arrive as amended text, reversed doctrine, and newly settled understandings; losses arrive as years of mobilization spent against entrenched readings. Leaving the field means abandoning the constitutional change sought, so participation continues across generations.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, beneficiary,
    organized, generational, constrained, national).

% Pass statutes premised on their own constitutional readings, respond to adverse rulings with redrawn laws, jurisdictional proposals, or amendment campaigns, and shift between branches depending on which venue currently agrees with them. When courts align, they defer; when courts oppose, they escalate. Their constitutional latitude expands and contracts with each election.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, agenda_setter).

% Citizens and citizen groups who assert constitutional claims against credentialed interpreters — gun owners citing the Second Amendment, parents invoking educational rights, activists claiming the Ninth Amendment — insisting the text belongs to them rather than to the bar. They gain standing to argue constitutional meaning directly; they lose when their claims require sustained organization they cannot muster.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Assert independent constitutional positions through legislation, interstate compacts, and official refusal to follow federal doctrinal settlements — sanctuary policies, interposition resolutions, state constitutional floors above federal ceilings. They gain interpretive standing and policy room; they risk preemption suits and funding conditions when their readings collide with federal ones.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, state_governments, agenda_setter).

% Discrete groups — racial, religious, sexual, linguistic — whose historical protection came from insulated courts willing to stand against majorities. Each transfer of interpretive authority toward electoral numbers exposes them to redefinition at the moments they are most outnumbered; they cannot leave the jurisdiction and cannot opt out of majority-authored meaning.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_dependent_minorities, payer,
    powerless, generational, trapped, national).

% Regulated firms, lenders, employers, and institutions that plan around stable constitutional rules — what speech regulation is permissible, what conditions attach to spending, what due process requires. Every reopened question reprices compliance across their planning horizons. Relocation cannot escape federal constitutional meaning, so they absorb the volatility or lobby for restoration of settled doctrine.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_settlement_seekers, payer,
    powerful, immediate, constrained, national).

% Judges, prominent lawyers, and legal academics committed to the ideal that constitutional questions have legally determinate answers enforceable as final law. Their professional standing rests on that ideal; each successful assertion of popular or departmental authority diminishes the office they identify with. Abandoning the ideal would mean abandoning their professional identity, so they fight rearguard actions through bar institutions, scholarship, and opinions.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, identity_locked, national).

% Decides thousands of constitutional questions yearly and issues the operative word in concrete cases, yet under this arrangement its rulings carry no claim to finality — each major decision invites movement response, legislative override, and appointment warfare aimed at its composition. It retains the daily work of interpretation while losing the last-word prerogative it long asserted.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, federal_judiciary, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, federal_judiciary, agenda_setter).

% Live, work, and raise families under constitutional meanings forged by an electorate they cannot join. Deportation standards, detention limits, and equal-protection boundaries are set by contests in which they have no vote; their recourse is limited to litigation before the very institution whose finality this arrangement denies.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, noncitizen_residents, excluded,
    powerless, biographical, constrained, national).

% Scholars of law and political science who map how constitutional authority is actually allocated — documenting court-curbing episodes, movement campaigns, and departmentalist assertions — without holding a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production of constitutional meaning across a polycentric field: popular movements, electoral majorities, coordinate branches, and states all assert constitutional positions, with courts interpreting provisionally rather than finally. Solves the legitimacy problem of judicial review — how a document claiming authority from 'We the People' can be given binding meaning by an unelected tribunal — by distributing interpretive authority so that settlements carry democratic pedigree and errors can be corrected through political mobilization rather than awaiting judicial self-reversal.
% TRANSFER_FUNCTION: Moves interpretive authority — and the power to settle contested moral and political questions that rides on it — away from the federal judiciary toward movements, majorities, and coordinate branches. Simultaneously moves the costs of unsettled meaning onto those who depend on stable rules (regulated institutions, contracting parties) and onto minorities whose protection depended on insulated judicial enforcement.
% ABSENT_VOICES: Noncitizen residents live under popularly contested constitutional meaning but hold no franchise, so the mechanism that legitimates settlements for others excludes them entirely. Future generations inherit settlements struck by transient majorities without consent. And minorities during majoritarian surges are formally present but numerically overridden — the reading's own legitimating mechanism, counting heads, is what silences them at the moments they most need shelter.
% DISAPPEARANCE_RATIONALE: If popular interpretive authority vanished overnight and judicial supremacy became total, movements would redirect energy from constitutional politics into litigation and appointment campaigns, Congress would draft around anticipated judicial vetoes instead of asserting its own readings, state departmentalist traditions would wither, and constitutional change would run almost entirely through nine lives and retirement timing — the amendment rate, already near zero, would fall further as Article V politics lost its popular motor.
% FOUNDING_PROBLEM: How to reconcile judicial review with popular sovereignty: the Constitution claims authority from 'We the People,' yet judicial review empowers an unelected bench to invalidate acts of the elected branches. The Federalist-Anti-Federalist dispute over who guards the guardians never resolved; the assertion of judicial supremacy in Cooper v. Aaron and the recurring court-curbing movements against it are successive rounds of the same fight.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: judicial-finality advocates themselves — the paying seat — attest the problem is live, since their entire position exists to answer it (Cooper v. Aaron's defensive unanimity; the recurring congressional court-curbing bills documented by political scientists; the confirmation-war escalation both parties treat as existential). Mainstream constitutional-history scholarship outside the movement tradition, and the Anti-Federalist lineage itself, corroborate that the tension predates and survives every proposed settlement.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.49, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.49: the arrangement delivers genuine democratic pedigree and error correction, but identifiable seats bear real burdens even assessed by the reading's own lights — minority shelter thins at surge moments, settlement costs land on planning-dependent institutions, and the bench's authority claim is the displaced object. Suppression (0.43) is a raw structural property, unscaled by power or scope: it records the coercive and institutional work needed to hold plural interpretive authority against the gravitational pull of judicial supremacy — confirmation warfare, recurring court-curbing bills, departmentalist assertion — while leaving rival theories (originalism, living constitutionalism) freely available; suppression here targets finality, not competing ideas. Theater (0.40) is rising: hearings, symbolic resolutions, and plebiscitary gestures increasingly substitute for actual popular determination of meaning, matching the scholarly atrophy thesis. Accessibility collapse is low (0.30) because alternatives persist both institutionally (courts keep deciding) and intellectually (sibling readings stay live). Resistance (0.60) is high: the legal establishment, finality advocates, and stability-dependent actors actively contest the arrangement. The temporal series share one eight-point grid (t=0 corresponds to 1954, t=70 to 2024, decade steps); the suppression series is authored because enforcement capacity is the traced dynamic — each generation must re-win the authority question at higher cost. Receipt: the arrangement's gains rotate with electoral cycles and movement lifecycles — majorities capture latitude only while they last, movements demobilize after wins — so no named seat durably captures, an affirmative checked claim rather than a default. Fixing is prohibitive in either direction: restoring judicial finality requires suppressing rooted practices of movement and departmental assertion, and completing the displacement of courts requires amendment-level change; no single actor can alter the arrangement without systemic upheaval.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same structure with opposite experiences. From popular_movements, each transfer of authority toward contestation is standing gained — the arrangement is emancipation. From counter_majoritarian_dependent_minorities, the same transfers are shelter withdrawn — the arrangement is exposure, felt most sharply when their numbers are weakest. From federal_judiciary, it is institutional dispossession with retained routine work: still deciding everything, final about nothing. From constitutional_settlement_seekers, it is ambient volatility repricing every plan. The engine computes these divergent per-seat classifications from the power, exit, and role data; the divergence is the finding, not an inconsistency to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (popular_movements, legislative_majorities, anti_elitist_claimants, state_governments) derive low directionality — the arrangement subsidizes their constitutional agency. Victim declarations (counter_majoritarian_dependent_minorities, judicial_finality_advocates, constitutional_settlement_seekers) derive high directionality; trapped exit for minorities places them nearest the full-target end, and constrained exit keeps settlement seekers' d high despite their power. One override is authored: the federal judiciary would derive near the beneficiary end from its agenda-setting administration of daily interpretation, but the constraint's specific object is the bench's finality claim — the judiciary is the principal targeted seat with residual administrative benefit — so d is overridden to 0.70. Coalition potential cuts both ways: civil-rights organizations convert individually powerless minorities into organized participants (the mechanism working for them when aligned), which is why the minority seat is authored powerless rather than organized — its organizational capacity is contingent and issue-specific.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling judicial review with popular sovereignty — is live, so no mandatrophy is declared and none should be inferred from the rising theater series. The classification discipline matters in both directions here: a naive democracy-affirming reading would score this as pure coordination and miss the minority-extraction half; a knee-jerk mob-rule reading would score it as pure extraction and miss the legitimacy and error-correction function that even opponents rely on when they litigate. The tangled-rope structure forces both halves into one account. The genuine decay risk is tracked, not declared: if theater continues climbing while participatory depth falls, the arrangement drifts toward ritualized performance over an inertial core — a trajectory the theater series is designed to catch early.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the us_constitution_interpretive kernel; what structural differences would instantiating the originalist or living-constitution sibling produce?',
    'Generate the sibling stories and diff their beneficiary/victim sets, epsilon values, and computed types against this story; the delta isolates what turns on authority-location alone.',
    'Under the originalist sibling, victims shift toward living-document advocates and popular reinterpretation movements; under the living-constitution sibling, victims shift toward original-meaning adherents. This reading''s victim set (minorities, settlement seekers, finality advocates) is specific to locating authority in popular contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story is one of three readings of the constitutional-authority kernel.').

omega_variable(
    authority_location_disagreement,
    'Where exactly do the three readings disagree — on the fixity of constitutional meaning, on the location of interpretive authority, or on both?',
    'Analytic decomposition: test whether each sibling pair conflicts on meaning-fixity, authority-location, or both axes; the cross-cutting structure (an originalist popular constitutionalist is a coherent position) locates the live disagreement primarily on authority-location.',
    'If the disagreement is purely about authority location, foreclosure between siblings is structurally impossible and all inter-reading relations are influence or coexistence; if fixity is implicated, stronger relations become available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_location_disagreement, conceptual, 'Locating the axis on which the kernel''s readings actually diverge.').

omega_variable(
    counter_majoritarian_shelter_tradeoff,
    'Does popular contestation expose discrete minorities to more constitutional harm than judicial supremacy does — or less, given the judiciary''s own record (Dred Scott, the Lochner era, Shelby County)?',
    'Comparative outcome analysis of minority-protecting constitutional change achieved through movements and electoral politics versus through litigation, controlling for era and issue area.',
    'If movements protect minorities better on balance, the victim set shrinks and effective extraction falls toward rope territory; if worse, extraction rises and the snare-flavored reading of the arrangement strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_shelter_tradeoff, empirical, 'Whether the reading''s central tradeoff nets out for or against the minorities who pay for it.').

omega_variable(
    popular_engagement_atrophy,
    'Is the rising theater_ratio evidence that the reading''s mechanism has decayed into ritual atop a de facto judicial-supremacy regime, or do episodic surges (civil rights, Dobbs-era state contestation, movement constitutionalism) show the mechanism alive?',
    'Track participatory-depth indicators (amendment activity, movement-driven doctrinal reversals, departmentalist assertions) against ceremonial indicators (confirmation spectacle, symbolic resolutions) across the interval.',
    'If atrophy is real and continuing, the arrangement drifts toward ritualized performance over an inertial core; if surges are structurally recurring, the mechanism is live and the theatrical reading overstates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_engagement_atrophy, empirical, 'Whether popular interpretive participation is functioning or decaying into ceremony.').

omega_variable(
    settlement_cost_valence,
    'Is perpetually unsettled constitutional meaning a cost imposed on settlement seekers, or a feature — continuous error correction — whose costs are simply the price of self-government?',
    'Not resolvable by data alone: turns on the weight given to democratic legitimacy versus rule-of-law stability — a values question the corpus should carry as an open preference axis.',
    'If contestation is a feature, the settlement-seeker seat drops out of the victim set and effective extraction falls; if a cost, the seat stays and extraction holds near its authored level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_cost_valence, preference, 'Whether constitutional instability counts as extraction or as the operating cost of democratic authorship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(us_c_tr_t50, observed).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement_basis(us_c_tr_t60, observed).
narrative_ontology:measurement(us_c_tr_t70, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 70, 0.4).
narrative_ontology:measurement_basis(us_c_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t50, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 50, 0.46).
narrative_ontology:measurement_basis(us_c_be_t50, observed).
narrative_ontology:measurement(us_c_be_t60, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 60, 0.47).
narrative_ontology:measurement_basis(us_c_be_t60, observed).
narrative_ontology:measurement(us_c_be_t70, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 70, 0.49).
narrative_ontology:measurement_basis(us_c_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t10, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 40, 0.37).
narrative_ontology:measurement_basis(us_c_su_t40, observed).
narrative_ontology:measurement(us_c_su_t50, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 50, 0.39).
narrative_ontology:measurement_basis(us_c_su_t50, observed).
narrative_ontology:measurement(us_c_su_t60, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 60, 0.41).
narrative_ontology:measurement_basis(us_c_su_t60, observed).
narrative_ontology:measurement(us_c_su_t70, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 70, 0.43).
narrative_ontology:measurement_basis(us_c_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how the Constitution is interpreted' decomposes into three structurally distinct constraints sharing one kernel (us_constitution_interpretive). This story instantiates the popular-constitutionalism reading; the originalist and living-constitution siblings are separate files with their own epsilon values, beneficiary/victim sets, and computed types. The readings cross-cut rather than stack: authority-location (this reading's axis) is orthogonal to meaning-fixity (the originalist axis) and to adaptation-mechanism (the living axis), so no sibling is foreclosed; each sibling's success nonetheless changes the others' operating environment, hence the influence edges declared from this reading to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__popular_constitutionalism_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
