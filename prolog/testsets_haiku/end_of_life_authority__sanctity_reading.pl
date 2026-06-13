% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Sanctity-of-Life Reading: Intrinsic Value Prohibition on End-of-Life Autonomy
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The sanctity-of-life reading of end-of-life authority grounds its
 *   categorical prohibition on intentional life-ending in the claim that
 *   human life possesses intrinsic value that overrides individual
 *   preference, autonomy, or suffering. Under this reading, physician role is
 *   defined as healer and life-preserver; actively ending life violates both
 *   physician duty and respect for life itself. The constraint is justified
 *   as protection for vulnerable persons (elderly, disabled, economically
 *   disadvantaged) from coercive pressure to see death as preferable. Yet
 *   structural analysis reveals: (1) the constraint denies autonomy to
 *   competent, suffering persons regardless of their explicit preference; (2)
 *   the 'protection' framing becomes a suppression mechanism for disabled and
 *   economically marginal persons who internalize the message that their
 *   end-of-life requests are per se evidence of depression, not rational
 *   choice; (3) institutional medical authority benefits from the
 *   constraint's clarity and authority, while terminally ill and disabled
 *   persons bear its costs. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as coordination (protection against coercion) while
 *   the authored metrics describe substantially extractive, actively enforced
 *   operation with rising theater ratio (enforcement increasingly oriented
 *   toward excluding autonomy advocates rather than preventing coercive
 *   pressure).
 *
 * KEY AGENTS:
 *   - institutional_medical_authority: Sets and enforces the prohibition, controls physician role definition, administers ethics committee gatekeeping.
 *   - terminally_ill_competent_adults: Deny autonomy requests; trapped in suffering frame regardless of preference.
 *   - economically_disadvantaged_elderly: Vulnerable to indirect pressure; suppression internalized via protective framing.
 *   - disabled_persons_vulnerable_to_pressure: Suppression structurally embedded: medical system reads disability-driven requests as irrational depression, denies them, person internalizes learned helplessness.
 *   - disability_advocacy_coalitions_protective_reading: Advocate for categorical prohibition as only framework protecting against coercive pressure on disabled persons; benefit from constraint alignment.
 *   - palliative_care_physicians: Benefit from clarity; bear moral injury from inability to honor requests or adequately relieve refractory suffering.
 *   - autonomy_reading_advocates: Excluded and delegitimized; voice present but marginalized.
 *   - bioethics_analytical_observer: Track real-world outcomes, suffering patterns, physician moral distress, pressure experiences across readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.68).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.72).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity-of-Life Reading: Intrinsic Value Prohibition on End-of-Life Autonomy").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '470855f4-e603-45f3-b4e5-2e01b74a5a9a').
narrative_ontology:cs_kernel_codification('470855f4-e603-45f3-b4e5-2e01b74a5a9a', fixed_text).
narrative_ontology:cs_authority_grounding('470855f4-e603-45f3-b4e5-2e01b74a5a9a', extraction).
narrative_ontology:cs_interpretation_layer_present('470855f4-e603-45f3-b4e5-2e01b74a5a9a').
narrative_ontology:cs_reading_relation('470855f4-e603-45f3-b4e5-2e01b74a5a9a', end_of_life_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('470855f4-e603-45f3-b4e5-2e01b74a5a9a', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('470855f4-e603-45f3-b4e5-2e01b74a5a9a', foundational, human_life_intrinsic_value_categorical).
narrative_ontology:cs_axiom_status(human_life_intrinsic_value_categorical, holdable).
narrative_ontology:cs_axiom_grounding('470855f4-e603-45f3-b4e5-2e01b74a5a9a', human_life_intrinsic_value_categorical, deontological).
narrative_ontology:cs_axiom('470855f4-e603-45f3-b4e5-2e01b74a5a9a', foundational, physician_role_life_preservation_not_lethal_agent).
narrative_ontology:cs_axiom_status(physician_role_life_preservation_not_lethal_agent, holdable).
narrative_ontology:cs_axiom_grounding('470855f4-e603-45f3-b4e5-2e01b74a5a9a', physician_role_life_preservation_not_lethal_agent, conventional).
narrative_ontology:cs_axiom('470855f4-e603-45f3-b4e5-2e01b74a5a9a', secondary, vulnerability_requires_protection_via_denial).
narrative_ontology:cs_axiom_status(vulnerability_requires_protection_via_denial, holdable).
narrative_ontology:cs_axiom_grounding('470855f4-e603-45f3-b4e5-2e01b74a5a9a', vulnerability_requires_protection_via_denial, empirically_contingent).
narrative_ontology:cs_reference_frame('470855f4-e603-45f3-b4e5-2e01b74a5a9a', sacred_life_doctrine_physician_healer).
narrative_ontology:cs_drift_state('470855f4-e603-45f3-b4e5-2e01b74a5a9a', contemporary_autonomy_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('470855f4-e603-45f3-b4e5-2e01b74a5a9a', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, institutional_medical_authority).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, disability_advocacy_coalitions_protective_reading).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminally_ill_competent_adults).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, economically_disadvantaged_elderly).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, disabled_persons_vulnerable_to_pressure).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint denies autonomy to competent persons regardless of preference and frames that denial as 'respect for life' and 'protection.' The extraction is the transfer of authority from individual choice to institutional gatekeeping. Suppression is higher (0.72) because enforcement requires active suppression of autonomy-reading voices (delegitimization, exclusion from policy frames), active denial of end-of-life requests regardless of competence, and internalized suppression in vulnerable persons (identity-locked via the protective framing). Theater ratio is moderate (0.41 at interval end) and rising: initial enforcement centered on genuine palliative care and protection; over 50-year interval, enforcement increasingly orients toward excluding autonomy advocates and defending the prohibition's legitimacy against growing real-world evidence that autonomy frameworks with safeguards can coexist with protection for vulnerable persons. The measurement grid captures: (1) individual-level suppression highest (0.77 at t=50) because the terminally ill and disabled persons feel the constraint directly; (2) organizational-level suppression lower (0.51 at t=50) because medical organizations have greater negotiating power and can often find informal workarounds (aggressive palliative sedation, withdrawal of support); (3) class-level resistance rising (0.66 at t=50) from social movements asserting both autonomy and protection; (4) structural-level suppression moderating slightly (0.63 at t=50) as legal jurisdictions bifurcate and the unified institutional frame weakens.
 *
 * PERSPECTIVAL GAP:
 *   Institutional medical authority perceives the constraint as genuine coordination: it protects vulnerable persons, provides clear guidance, enables trust in the physician-patient relationship, and honors life's value. From their seat, enforcement is protective gatekeeping. Terminally ill competent adults perceive the constraint as enforced extraction: their autonomy is overridden regardless of preference, their suffering is prolonged in the name of 'protection' they did not request, and the institutional frame denies their agency. From their seat, enforcement is coercion. Disabled and economically disadvantaged persons occupy a third seat: they experience both the protective framing (which acknowledges their vulnerability) and the suppressive mechanism (which denies them the very autonomy-based safeguards that might protect against coercive pressure). Identity-locked vulnerable persons internalize the message that their end-of-life requests are per se irrational, so they do not voice them, so enforcement appears successful (protection works; no one is asking for assisted dying), which validates the protective frame and closes the feedback loop. The engine computes these three divergent d values from the structural data—the authored claim does not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional medical authority benefits from the constraint's existence and enforcement: it consolidates their role as life-preserver, grants authority over end-of-life decisions, and provides professional legitimacy. Their directionality is low (d toward beneficiary end, ~0.15), derived from: (1) beneficiary status (authority, prestige, policy clarity), (2) powerful power atom (institutional), (3) arbitrage exit options (can reframe, migrate, shift frameworks). Terminally ill competent adults are the primary target (d toward 1.0, ~0.95): (1) victim status (denied autonomy, prolonged suffering), (2) powerless atom (no institutional leverage), (3) trapped exit (cannot exit the constraint except by death; criminalization suppresses covert alternatives). Economically disadvantaged elderly are secondary target (d ~0.85): (1) victim status + suppression internalization (identity-locked), (2) powerless atom, (3) identity-locked exit (internalized belief that their autonomy is not trustworthy persists after constraint denial). Disabled persons vulnerable to pressure: target seat (d ~0.85), same mechanism as elderly. Disability advocates (protective reading): slight beneficiary (d ~0.35) because their advocacy shapes policy, but not trapped; mobile exit options (can change reading, can organize, can litigate). Palliative care physicians: dual-positioned (slight target, d ~0.55): beneficiary from role clarity, payer from moral injury and inability to honor requests or adequately relieve suffering. No directionality override needed; derivation chain captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits a foundational mandatrophy tension: the founding problem (protecting vulnerable persons from coercive pressure toward death) has been substantially addressed by autonomy-based frameworks with safeguards (competence assessment, waiting periods, physician counseling, structural protections against pressure), yet the sanctity reading persists by claiming the founding problem is still live and only the categorical prohibition prevents coercive pressure. Real-world evidence from autonomy-based jurisdictions (Netherlands, Belgium, Canada, Oregon) shows: (1) pressure-driven assisted dying is rare (<5% of cases cite 'burden on family' as primary reason); (2) socioeconomic disparities exist but are less severe than feared; (3) safeguards effectively filter out cases of coercive pressure or depression-driven requests. The sanctity reading responds by reframing the founding problem: it is not 'coercive pressure from society' but 'social pressure from a culture that devalues disabled life' and 'internalized pressure from discrimination.' The autonomy frameworks cannot solve internalized discrimination—only social change can. The sanctity reading thus shifts the justification from 'the categorical prohibition protects against coercive pressure' to 'the categorical prohibition protects by refusing to legitimize end-of-life decisions in a society that devalues disabled life.' This is a genuine reading of the founding problem, not a cover story—but it moves the solution from the constraint itself (the prohibition) to a social change the constraint alone cannot achieve. The mandatrophy resolution is asymmetric: the constraint persists not because it solves the founding problem, but because (1) institutional medical authority benefits from the clarity it provides, (2) disability advocates hold it as the only framework they trust, and (3) dismantling it would require social change (disability inclusion, economic security for elderly) that exceeds the constraint's domain. The theater ratio rising over the interval (0.28 to 0.41) reflects this: enforcement increasingly orients toward defending the reading's legitimacy against external challenge rather than operationalizing the founding problem's solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (external barriers: legal prohibition, medical refusal, institutional gatekeeping) or internalized (disabled and economically disadvantaged persons internalize the message that their autonomy is not trustworthy), or both?',
    'Post-exit suppression trajectory: if terminally ill and disabled persons deny their end-of-life requests, then move to autonomy-based jurisdictions or enter spaces where autonomy-based options are normalized, do they experience residual suppression (persistent belief their autonomy is not trustworthy)? If yes, the suppression is partially internalized. Longitudinal follow-up with persons denied end-of-life requests in sanctity-based jurisdictions vs. autonomy-based jurisdictions would show the ratio of structural to internalized suppression.',
    'If suppression is primarily structural, exiting the jurisdiction eliminates it. If significantly internalized, the person carries the suppression post-exit, extending the constraint''s effective reach. If highly internalized, the constraint''s real mechanism is not the legal prohibition but the internalized identity-lock, which means the classification might shift from tangled_rope (coordination + enforced extraction) toward snare (pure extraction riding cover story of protection) because the extraction persists through internalized suppression even after legal/structural mechanisms are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism—determines whether exiting the jurisdiction fully removes suppression or carries it forward via identity-lock.').

omega_variable(
    protection_vs_coercion_asymmetry,
    'Does the sanctity-reading constraint actually protect vulnerable persons from coercive pressure, or does it function as coercive pressure (denial of autonomy) in the name of protection, with the net effect depending on the baseline pressure environment?',
    'Comparative analysis: in jurisdictions with autonomy-based frameworks + robust social safety nets (economic security, disability inclusion, healthcare access), do disabled and elderly persons experience more or less pressure toward assisted dying than in sanctity-based jurisdictions with weak safety nets? If autonomy-based jurisdictions with good safety nets show LOWER pressure than sanctity-based jurisdictions with poor safety nets, the constraint''s protective function is secondary to social conditions. If sanctity-based jurisdictions show lower pressure regardless of safety nets, the constraint itself provides protection. The resolution requires matching pairs of jurisdictions (same economic/social conditions, different end-of-life law) or large-scale longitudinal data on pressure experiences.',
    'If protection depends more on social conditions than on the legal prohibition, the constraint is justified by proxy: it appears protective but is actually benefiting from societies that happen to have other protective structures. The extraction (denial of autonomy) persists regardless. If the constraint provides protection independently, it remains justified under the protective reading. If autonomy frameworks with good safety nets are MORE protective, the reading is falsified and the constraint becomes pure extraction masked by protection framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protection_vs_coercion_asymmetry, empirical, 'Whether the sanctity constraint itself protects, or whether protection depends on background social conditions—determines if the constraint is justified or if the justification is borrowed from external structures.').

omega_variable(
    foundational_axiom_empirical_status,
    'Does human life possess intrinsic value independent of any human valuing, or does the claim of intrinsic value depend on a metaphysical or theological commitment that is not empirically falsifiable?',
    'Philosophical and theological analysis: trace the grounding_type of the sanctity axiom. If empirically_contingent (grounded in observable facts about human consciousness, relational capacity, unique properties of human life), then evidence showing disabled persons have full consciousness and relational capacity but choose end-of-life would challenge the axiom. If deontological (grounded in rights or duties that are not empirically falsifiable), no amount of suffering or preference would falsify it. If theological (grounded in divine authority), empirical evidence is not the appropriate falsifier. The reading''s axioms are specified in cs_structure.axioms; this omega documents the ambiguity in the grounding.',
    'If the axiom is empirically_contingent but unrefuted by real-world evidence (autonomy-based jurisdictions show suffering can be respected through autonomy frameworks without destroying human dignity), the axiom is holdable but increasingly challenged. If deontological, the axiom is holdable regardless of empirical outcomes because it rests on normative commitments immune to falsification. The grounding_type determines whether the reading can be foreclosed by evidence or only by theological/philosophical debate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_axiom_empirical_status, conceptual, 'Whether the intrinsic-value axiom is empirically contingent, deontological, or theological—determines what kinds of evidence could challenge or support the reading.').

omega_variable(
    coexistence_feasibility_between_readings,
    'Can the sanctity and autonomy readings genuinely coexist in a single legal/institutional framework, or does the sanctity reading''s categorical prohibition necessarily foreclose the autonomy reading?',
    'Examine frameworks that attempt both: (1) Oregon''s Death with Dignity law + disability protections; (2) nested jurisdictional options (countries with autonomy-based frameworks accepting persons from sanctity-based countries); (3) federalism models allowing state-level divergence; (4) private/public medical splits where public medicine maintains sanctity prohibition and private medicine respects autonomy. If these models function without internal contradiction and without the sanctity framework being undermined, coexistence is feasible. If the sanctity framework requires total institutional integration (all physicians bound by the prohibition, all law unified) to function, then coexistence is structurally impossible.',
    'If coexistence is feasible, the reading_relations to autonomy_reading should be ''coexists_with'' (different institutions/jurisdictions hold both). If coexistence requires subordination of autonomy reading, the relation might shift toward ''influences'' (this reading creates structural pressure that constrains the autonomy reading''s operation). If the readings are logically incompatible, the relation might shift toward ''forecloses'' (rare; the reading''s core premise rules out the sibling''s core premise in any single framework).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coexistence_feasibility_between_readings, conceptual, 'Whether sanctity and autonomy readings can coexist in practice or whether the readings foreclose each other—determines the reading_relations classification and the structural feasibility of pluralism in end-of-life law.').

omega_variable(
    disability_representation_in_policy,
    'Are disabled persons whose lived experience would inform end-of-life policy substantively represented in policy-setting bodies and institutional structures, or is the ''protection'' of disabled persons decided by non-disabled persons, medical professionals, and disability advocates who may not carry the disability experience themselves?',
    'Audit end-of-life policy committees, hospital ethics boards, and legislative working groups: (1) percentage of voting/participating members with lived disability experience; (2) mechanisms by which disabled persons'' own end-of-life preferences can shape policy (not just disability advocates'' protective reading); (3) power distribution (can disabled persons with preferences for autonomy-based options shape policy, or is their voice absorbed into the category of ''vulnerable persons needing protection''?). If disabled persons are substantially under-represented or excluded from policy-setting, the constraint''s claim to protect disabled persons is made without their own voice shaping the protection.',
    'If disabled persons are substantially absent from policy-setting, the constraint functions as extraction (institutional authority and disability advocates make decisions about disabled persons'' autonomy without their substantive input). If disabled persons are present and their diversity of preferences is incorporated, the constraint''s beneficiary status is more robust. This determines whether the constraint is ''protection framed as benign'' or ''extraction framed as protection.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_representation_in_policy, empirical, 'Representation of disabled persons in end-of-life policy-setting—determines whether the constraint''s protection is decided with or without disabled persons'' own input.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(end__tr_t7, end_of_life_authority__sanctity_reading, theater_ratio, 7, 0.31).
narrative_ontology:measurement(end__tr_t14, end_of_life_authority__sanctity_reading, theater_ratio, 14, 0.34).
narrative_ontology:measurement(end__tr_t21, end_of_life_authority__sanctity_reading, theater_ratio, 21, 0.37).
narrative_ontology:measurement(end__tr_t35, end_of_life_authority__sanctity_reading, theater_ratio, 35, 0.39).
narrative_ontology:measurement(end__tr_t50, end_of_life_authority__sanctity_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(end__be_t7, end_of_life_authority__sanctity_reading, base_extractiveness, 7, 0.56).
narrative_ontology:measurement(end__be_t14, end_of_life_authority__sanctity_reading, base_extractiveness, 14, 0.61).
narrative_ontology:measurement(end__be_t21, end_of_life_authority__sanctity_reading, base_extractiveness, 21, 0.64).
narrative_ontology:measurement(end__be_t35, end_of_life_authority__sanctity_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement(end__be_t50, end_of_life_authority__sanctity_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(end__su_t7, end_of_life_authority__sanctity_reading, suppression_requirement, 7, 0.62).
narrative_ontology:measurement(end__su_t14, end_of_life_authority__sanctity_reading, suppression_requirement, 14, 0.66).
narrative_ontology:measurement(end__su_t21, end_of_life_authority__sanctity_reading, suppression_requirement, 21, 0.69).
narrative_ontology:measurement(end__su_t35, end_of_life_authority__sanctity_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(end__su_t50, end_of_life_authority__sanctity_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__sanctity_reading, 0.1).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The end-of-life authority kernel contest consists of three structurally distinct constraints: (1) sanctity_reading (this story), which grounds prohibition in intrinsic value and institutes protection through categorical denial of autonomy; (2) autonomy_reading, which grounds decisions in individual preference and institutes protection through competence safeguards and robust social support; (3) slippery_slope_mechanism, which empirically tracks how autonomy-based frameworks expand beyond their stated bounds. Each has distinct ε, beneficiary/victim structures, and enforcement mechanisms. The sanctity reading claims protection but operates as suppression + extraction from competent persons. The autonomy reading claims harm minimization and observes empirical safeguarding. The slippery-slope mechanism claims inevitable expansion and observes real-world institutional drift. All three are live readings held by different institutional actors; none forecloses the others in practice, though each forecloses the other's core premise theoretically (sanctity: 'autonomy must be denied'; autonomy: 'autonomy must be honored'). The ε values differ substantially because the readings instantiate different constraint mechanisms: sanctity extracts via denial (0.68), autonomy via safeguarding (lower extraction), slippery-slope via empirical monitoring of institutional drift (observational rather than enforcement-based). These are linked as a family because each reading's operation depends on understanding what problem it solves and how the sibling readings would change the solution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
