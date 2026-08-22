% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Territorial Border Exclusion Regime (Freedom-of-Movement Reading)
 *   domain: political philosophy/international law/migration
 *
 * SUMMARY:
 *   This story instantiates the freedom-primary reading of the
 *   border_normative_status kernel: the claim that freedom of movement is a
 *   fundamental human right which territorial exclusion impermissibly
 *   restricts, such that any exclusion requires an extraordinary
 *   justification the excluding state must affirmatively meet. Under this
 *   reading, the border is not read as an exercise of legitimate collective
 *   self-determination (the sovereignty_primary reading) nor as an authority
 *   that merely needs proportional constraint (the qualified_sovereignty
 *   reading), but as a standing rights violation whose coordination story
 *   (protecting domestic labor markets and welfare systems) is analyzed here
 *   as insufficient cover for its exclusionary function. Excluded migrants,
 *   stranded transit populations, and asylum seekers facing deterrence
 *   architecture form the core victim set; displaced domestic workers enter
 *   the victim set on this reading specifically because the border is read as
 *   failing even its own protective function, instead entrenching employer
 *   monopsony power over a captive migrant labor pool. This is one reading
 *   among three siblings sharing the same kernel; the sovereignty_primary and
 *   qualified_sovereignty readings are separate constraints with their own
 *   epsilon and stakeholder structures, not alternative measurements of this
 *   one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.78).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.85).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Territorial Border Exclusion Regime (Freedom-of-Movement Reading)").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political philosophy/international law/migration").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, 'e7038c59-0ea6-489f-b831-129927767e3b').
narrative_ontology:cs_kernel_codification('e7038c59-0ea6-489f-b831-129927767e3b', distributed).
narrative_ontology:cs_authority_grounding('e7038c59-0ea6-489f-b831-129927767e3b', distributed).
narrative_ontology:cs_reading_relation('e7038c59-0ea6-489f-b831-129927767e3b', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('e7038c59-0ea6-489f-b831-129927767e3b', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('e7038c59-0ea6-489f-b831-129927767e3b', foundational, freedom_of_movement_is_a_fundamental_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_a_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('e7038c59-0ea6-489f-b831-129927767e3b', freedom_of_movement_is_a_fundamental_right, deontological).
narrative_ontology:cs_axiom('e7038c59-0ea6-489f-b831-129927767e3b', foundational, exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('e7038c59-0ea6-489f-b831-129927767e3b', exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_axiom('e7038c59-0ea6-489f-b831-129927767e3b', secondary, bounded_membership_has_no_independent_normative_weight).
narrative_ontology:cs_axiom_status(bounded_membership_has_no_independent_normative_weight, holdable).
narrative_ontology:cs_axiom_grounding('e7038c59-0ea6-489f-b831-129927767e3b', bounded_membership_has_no_independent_normative_weight, deontological).
narrative_ontology:cs_reference_frame('e7038c59-0ea6-489f-b831-129927767e3b', westphalian_bounded_membership_default).
narrative_ontology:cs_drift_state('e7038c59-0ea6-489f-b831-129927767e3b', post_cold_war_human_rights_expansion, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e7038c59-0ea6-489f-b831-129927767e3b', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, receiving_state_incumbent_citizens).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, receiving_state_low_wage_domestic_employers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_would_be_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, stranded_transit_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, asylum_seekers_facing_deterrence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Denied entry, visa, or legal passage to territories where they could work, reunite with family, or escape danger, on the basis of nationality alone. From this reading's premise, the border enacts exclusion from opportunity and safety that would require an extraordinary justification the state does not meet; the migrant bears the full cost with no recourse against the classification itself.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_would_be_migrants, payer,
    powerless, biographical, trapped, global).

% Held at borders, in transit camps, or in third countries by interdiction and pushback policies while attempting onward movement. Physically immobilized by enforcement infrastructure (walls, patrols, detention) that this reading treats as the visible machinery of a rights violation rather than a legitimate sovereign prerogative.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, stranded_transit_migrants, payer,
    powerless, immediate, trapped, regional).

% Workers inside the excluding state whose labor market position is asserted (by the sovereignty-primary reading) to be protected by the border, but who, on the freedom-primary account, are displaced from a fairer global labor market they would benefit from joining were restrictions lifted, and whose domestic wages are in fact suppressed by monopsony power the border helps entrench rather than genuinely defended by it. They enter this reading's victim set precisely because the restriction is read as failing to serve even its stated domestic-protection function.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers, payer,
    powerless, generational, constrained, national).

% Persons with plausible protection claims subjected to deterrence architecture (offshore processing, safe-third-country returns, prolonged detention) designed to discourage arrival. Under this reading, deterrence of arrival is itself a rights violation regardless of eventual legal outcome, since the harm of exclusion attaches to the border act, not merely to wrongful refusal.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, asylum_seekers_facing_deterrence, payer,
    powerless, immediate, trapped, global).

% Hold exclusive access to territory, labor markets, welfare systems, and political voice inside the excluding state by virtue of citizenship, an allocation this reading treats as an unearned status good sustained by the border's exclusionary force rather than a legitimate collective entitlement.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, receiving_state_incumbent_citizens, beneficiary,
    organized, biographical, mobile, national).

% Contractors, agencies, and bureaucracies (border patrol forces, detention operators, surveillance technology vendors) whose budgets, headcount, and institutional mandate expand with the scale of enforcement. They administer the exclusion machinery and have direct material interest in its continuation and intensification.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_enforcement_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Benefit from a curated, controlled flow of precarious migrant labor whose legal vulnerability (partial exclusion, temporary status, deportability) suppresses wages and bargaining power more effectively than either full exclusion or full freedom of movement would. They lobby to shape enforcement intensity to their advantage, making them partial co-authors of the regime alongside the state.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, receiving_state_low_wage_domestic_employers, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, receiving_state_low_wage_domestic_employers, agenda_setter).

% Set immigration law, control admission criteria, and direct enforcement agencies. Justify exclusion through sovereignty and self-determination claims that this reading holds do not meet the extraordinary-justification threshold required to override a fundamental right; the state's discretion is the object under contest, not a background given.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluding_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% UN treaty bodies, regional human rights courts, and NGOs document border deaths, pushback violations, and detention conditions, and adjudicate or advocate against enforcement practices using human-rights frameworks that increasingly cite freedom of movement as a candidate fundamental right, though it is not yet codified as one at the international-to-domestic border level.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_rights_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The border regime, on its own terms, coordinates access to a bounded set of jobs, public services, and political membership among an existing population; this reading grants that a genuine allocation problem exists but denies that territorial exclusion of non-members is a justified solution to it.
% TRANSFER_FUNCTION: The arrangement moves life opportunity, safety, and income from excluded non-citizens to incumbent citizens and to the firms and agencies that administer or profit from the enforcement apparatus, while additionally suppressing domestic low-wage workers' bargaining position by keeping a captive, deportable migrant labor pool available to employers.
% ABSENT_VOICES: Excluded migrants themselves are structurally absent from the domestic political process that sets admission law in the states that exclude them; they have no vote, no standing, and often no legal representation at the point of exclusion. Displaced domestic workers are present in the political process but their interests are typically framed by others (either as protected insiders or ignored) rather than voiced as co-victims of a labor-suppressing border.
% DISAPPEARANCE_RATIONALE: If exclusionary border enforcement vanished overnight, global labor and settlement patterns would reorganize substantially: large-scale relocation toward higher-opportunity regions, convergence pressure on wages, collapse of the deterrence and detention industry, and a fundamental renegotiation of citizenship as the primary allocator of life chances. This reading holds that rearrangement would be a correction of an unjustified restriction, not a loss of legitimate coordination.
% FOUNDING_PROBLEM: Modern territorial border control was built to consolidate Westphalian sovereign authority, manage post-imperial and post-war population movements, and allow states to control membership, taxation, and labor markets within a bounded jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: Excluding states and their electorates attest the founding problem (need for controlled membership and labor markets) remains live and legitimate. Independent migration economists, some UN human rights rapporteurs, and cross-border labor-mobility researchers outside the excluding states' own governments attest that the scale of current restriction serves incumbent rent-protection and enforcement-industry growth far more than it serves any live coordination problem — supporting this reading's contention that the current mandate has outrun a justifiable founding purpose.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) because, by this reading's own lights, the border regime transfers substantial life-opportunity and safety from a large excluded population to a smaller incumbent and enforcement-industry beneficiary set, with the gap widening over the measured interval as enforcement infrastructure (walls, biometric screening, offshore detention, safe-third-country agreements) has scaled globally since 1990. Suppression is authored even higher (0.85) because the mechanism depends on active, hardening coercive infrastructure — physical barriers, interdiction at sea, detention, and deterrence policy — not on migrant preference; this is a raw structural property, unscaled by power or scope per the framework's rule. Theater ratio is moderate (0.4): a meaningful share of enforcement activity (humanitarian screening claims, asylum processing theater) is genuine legal process, but a rising share, on this reading, is performative deterrence signaling to domestic electorates rather than functional border management.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (excluding state governments) and the concentrated beneficiary seats, the arrangement can appear as legitimate self-determination requiring no extraordinary justification — that is precisely the premise this reading rejects. The engine computes each seat's type from the structural data; the divergence between how excluding_state_governments would narrate the arrangement and how excluded_would_be_migrants experience it is the object this reading exists to make visible, not a discrepancy to be resolved by adjusting the metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded migrants, stranded transit populations, and asylum seekers sit at the full-target end: trapped exit options, powerless structural position, and the constraint's entire operative function directed at them. Displaced domestic workers are also coded as targets under this reading, despite their citizenship status, because their exit options remain constrained by employer power that the border helps sustain, and the reading denies that the border's stated protective function actually redounds to their benefit. Receiving-state incumbent citizens, the enforcement industry, and low-wage employers sit toward the beneficiary end, with arbitrage-grade exit options (citizens can relocate freely; industry and employers can shift jurisdictions or lobbying strategies) that this reading treats as evidence the arrangement serves concentrated interests rather than a genuinely shared coordination problem.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem interview marks the founding problem's status as contested rather than dead: incumbent populations and their governments still treat controlled membership as a live coordination need, while independent researchers external to the beneficiary set increasingly attest that the scale and form of current restriction serves rent-protection and enforcement-industry growth rather than a proportionate response to any live problem. This reading does not claim the founding problem never existed; it claims the current arrangement has drifted from proportionate coordination toward an extraction-dominant equilibrium the extraordinary-justification standard would not license.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraordinary_justification_threshold,
    'What would count as an extraordinary justification sufficient to permit exclusion under this reading, and does any actual state border policy meet it?',
    'Comparative analysis of border policies against a hypothesized extraordinary-justification standard (e.g., genuine, narrowly tailored public-health or security emergencies of demonstrated severity), tested against historical cases where states claimed such justifications.',
    'If a workable standard exists and some real policies meet it, the reading''s victim set would need to be qualified rather than treated as uniformly wronged; if no real-world policy meets any workable standard, the reading''s blanket treatment of exclusion as rights violation is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraordinary_justification_threshold, conceptual, 'Whether any operationalizable extraordinary-justification standard is ever actually met by existing border regimes.').

omega_variable(
    displaced_domestic_worker_causal_claim,
    'Does border enforcement actually suppress domestic low-wage workers'' bargaining power (via captive migrant labor), or does it protect their wages as the sovereignty-primary and qualified-sovereignty readings would claim?',
    'Labor economics literature comparing wage and bargaining outcomes across variably-enforced sectors and jurisdictions (e.g., agricultural guest-worker programs vs. fully open sectors vs. fully closed sectors), controlling for other labor market factors.',
    'If enforcement genuinely protects domestic wages, displaced_domestic_workers should not appear in this reading''s victim set and the reading''s core empirical claim weakens substantially. If enforcement suppresses wages via a captive labor pool, the inclusion is empirically vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_domestic_worker_causal_claim, empirical, 'Whether border enforcement suppresses or protects domestic low-wage labor markets.').

omega_variable(
    framing_under_determination_rights_vs_membership,
    'Is the appropriate unit of analysis the individual migrant''s rights claim against a specific state, or the abstract institution of bounded political membership that freedom-of-movement claims implicitly challenge?',
    'Compare classification outcomes under (a) an individual-rights framing (each exclusion act evaluated against the excluded person''s claim) versus (b) an institutional framing (the entire system of bounded citizenship evaluated as a global distributive scheme).',
    'The individual-rights framing (adopted here) yields a tangled/snare-leaning classification concentrated on enforcement acts; an institutional framing might instead classify the entire citizenship-and-border system as a single global tangled rope with different beneficiary/victim boundaries. This story adopts the individual-rights framing because it aligns with the freedom_primary reading''s own premise that the right in question is held by persons, not by states or bounded schemes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination_rights_vs_membership, conceptual, 'Alternative framing (individual rights claim vs. institutional distributive scheme) would shift classification boundaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__freedom_primary, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(bord_tr_t1996, border_normative_status__freedom_primary, theater_ratio, 1996, 0.24).
narrative_ontology:measurement(bord_tr_t2002, border_normative_status__freedom_primary, theater_ratio, 2002, 0.28).
narrative_ontology:measurement(bord_tr_t2008, border_normative_status__freedom_primary, theater_ratio, 2008, 0.32).
narrative_ontology:measurement(bord_tr_t2014, border_normative_status__freedom_primary, theater_ratio, 2014, 0.36).
narrative_ontology:measurement(bord_tr_t2018, border_normative_status__freedom_primary, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__freedom_primary, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(bord_be_t1990, border_normative_status__freedom_primary, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(bord_be_t1996, border_normative_status__freedom_primary, base_extractiveness, 1996, 0.6).
narrative_ontology:measurement(bord_be_t2002, border_normative_status__freedom_primary, base_extractiveness, 2002, 0.65).
narrative_ontology:measurement(bord_be_t2008, border_normative_status__freedom_primary, base_extractiveness, 2008, 0.7).
narrative_ontology:measurement(bord_be_t2014, border_normative_status__freedom_primary, base_extractiveness, 2014, 0.74).
narrative_ontology:measurement(bord_be_t2018, border_normative_status__freedom_primary, base_extractiveness, 2018, 0.77).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__freedom_primary, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1990, border_normative_status__freedom_primary, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(bord_su_t1996, border_normative_status__freedom_primary, suppression_requirement, 1996, 0.65).
narrative_ontology:measurement(bord_su_t2002, border_normative_status__freedom_primary, suppression_requirement, 2002, 0.7).
narrative_ontology:measurement(bord_su_t2008, border_normative_status__freedom_primary, suppression_requirement, 2008, 0.75).
narrative_ontology:measurement(bord_su_t2014, border_normative_status__freedom_primary, suppression_requirement, 2014, 0.8).
narrative_ontology:measurement(bord_su_t2018, border_normative_status__freedom_primary, suppression_requirement, 2018, 0.83).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__freedom_primary, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the border_normative_status kernel. sovereignty_primary treats territorial exclusion as a legitimate exercise of collective self-determination (low or contested extraction, victims largely absent or reframed as non-members with no standing claim). qualified_sovereignty treats border authority as real but bounded by proportionality and human-rights review (moderate extraction, a narrower victim set limited to disproportionate enforcement). This freedom_primary reading treats exclusion itself as presumptively a rights violation (high extraction, the broadest victim set including displaced domestic workers). All three share the same underlying kernel — the normative status of the border — but instantiate structurally distinct constraints with different epsilon values, beneficiary/victim sets, and claimed types, per the epsilon-invariance principle. They are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
