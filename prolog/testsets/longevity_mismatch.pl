% ============================================================================
% CONSTRAINT STORY: longevity_mismatch
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_longevity_mismatch, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: longevity_mismatch
 *   human_readable: Evolutionary Lifespan-Partnership Duration Mismatch
 *   domain: biological/social/demographic
 *
 * SUMMARY:
 *   Human pair-bonding evolved in environments where modal lifespan was 30-45
 *   years and reproductive partnerships lasted 15-25 years (long enough to
 *   raise offspring to independence). Modern lifespans of 75-85 years create
 *   partnerships of 50-70 years — durations for which the neurochemical and
 *   psychological mechanisms of pair-bonding were not selected. The
 *   constraint manifests as endemic relationship strain in the third and
 *   fourth decades of partnership, predictable divorce rate spikes at the
 *   evolved pair-bond horizon, and the collision between cultural
 *   expectations of lifelong commitment and biological systems calibrated for
 *   shorter durations. This is a claimed mountain with declared
 *   beneficiaries, triggering FSM evaluation: the constraint is presented as
 *   natural law, but identifiable professional sectors benefit from its
 *   operation and have incentives to frame the mismatch as inevitable rather
 *   than as a target for cultural or technological intervention.
 *
 * KEY AGENTS:
 *   - divorce_industry_professionals: organized/mobile — benefit economically from dissolution rates driven by the mismatch
 *   - relationship_therapy_sector: organized/mobile — benefit from endemic strain in lifespan-extended partnerships
 *   - serial_monogamy_advocates: moderate/mobile — benefit reputationally from the constraint's visibility
 *   - long_term_partners: powerless/constrained — bear the primary costs of navigating unprecedented partnership durations
 *   - children_of_late_divorces: powerless/trapped — bear secondary costs of dissolutions driven by the mismatch
 *   - evolutionary_psychologists: analytical — document the constraint without material benefit
 *   - demographers: analytical — measure the constraint's demographic signature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(longevity_mismatch, 0.12).
domain_priors:suppression_score(longevity_mismatch, 0.08).
domain_priors:theater_ratio(longevity_mismatch, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(longevity_mismatch, extractiveness, 0.12).
narrative_ontology:constraint_metric(longevity_mismatch, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(longevity_mismatch, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(longevity_mismatch, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(longevity_mismatch, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(longevity_mismatch, mountain).
narrative_ontology:human_readable(longevity_mismatch, "Evolutionary Lifespan-Partnership Duration Mismatch").
narrative_ontology:topic_domain(longevity_mismatch, "biological/social/demographic").

domain_priors:emerges_naturally(longevity_mismatch).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(longevity_mismatch, divorce_industry_professionals).
narrative_ontology:constraint_beneficiary(longevity_mismatch, relationship_therapy_sector).
narrative_ontology:constraint_beneficiary(longevity_mismatch, serial_monogamy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(longevity_mismatch, long_term_partners).
narrative_ontology:constraint_victim(longevity_mismatch, children_of_late_divorces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Family law attorneys, mediators, and divorce financial planners whose practice volume correlates with partnership dissolution rates. They provide genuine services to people navigating separation but also benefit economically from the structural mismatch between evolved pair-bonding duration and modern lifespans. Their professional organizations lobby against simplified dissolution procedures that would reduce billable complexity.
narrative_ontology:constraint_stakeholder(longevity_mismatch, divorce_industry_professionals, beneficiary,
    organized, biographical, mobile, national).

% Marriage counselors, couples therapists, and relationship coaches whose client base depends on the strain of maintaining multi-decade partnerships. They provide real therapeutic value but also benefit from the endemic difficulty of sustaining bonds across unprecedented time horizons. The sector has grown substantially as lifespan-extended partnerships generate predictable midlife crises and compatibility drift.
narrative_ontology:constraint_stakeholder(longevity_mismatch, relationship_therapy_sector, beneficiary,
    organized, biographical, mobile, global).

% Public intellectuals, authors, and thought leaders who argue that lifelong monogamy is obsolete and advocate for sequential committed partnerships as more realistic. Their platforms and book sales benefit from the constraint's visibility; the evolutionary mismatch provides empirical grounding for their normative claims about relationship structure.
narrative_ontology:constraint_stakeholder(longevity_mismatch, serial_monogamy_advocates, beneficiary,
    moderate, biographical, mobile, global).

% Individuals in marriages or committed partnerships navigating the tension between social expectations of lifelong commitment and the biological reality that human pair-bonding mechanisms evolved for 15-25 year reproductive partnerships. They experience the constraint as endemic relationship strain in decades 3-5 of partnership, compatibility drift as individuals change substantially over 50+ year timespans, and the collision between cultural scripts of permanence and evolved bonding duration.
narrative_ontology:constraint_stakeholder(longevity_mismatch, long_term_partners, payer,
    powerless, biographical, constrained, universal).

% Adult children whose parents divorce after 20-40 years of marriage, experiencing family structure dissolution at life stages where they assumed stability. They bear emotional costs and often logistical disruption (holiday coordination, elder care planning) from dissolutions driven by the lifespan-partnership mismatch rather than acute dysfunction.
narrative_ontology:constraint_stakeholder(longevity_mismatch, children_of_late_divorces, payer,
    powerless, biographical, trapped, universal).

% Researchers studying the evolutionary basis of human mating systems and pair-bonding duration. They document the constraint through cross-cultural data on partnership stability, comparative primatology, and life-history theory. Their work establishes the empirical foundation for the mismatch claim but does not benefit materially from its existence.
narrative_ontology:constraint_stakeholder(longevity_mismatch, evolutionary_psychologists, observer,
    analytical, generational, analytical, global).

% Population scientists tracking marriage duration, divorce rates by partnership length, and life expectancy trends. They measure the constraint's demographic signature: the characteristic spike in divorce rates at 15-25 years (the evolved pair-bond horizon) and again at 40+ years (the unprecedented extension), independent of cultural context.
narrative_ontology:constraint_stakeholder(longevity_mismatch, demographers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this is a biological constraint, not a coordination mechanism. Human pair-bonding physiology coordinates reproductive partnership over the timescale it evolved for; the mismatch arises because modern lifespans extend far beyond that window.
% TRANSFER_FUNCTION: The constraint does not transfer resources between parties. It imposes costs (relationship strain, dissolution trauma, therapeutic expense) on individuals navigating partnerships across evolutionarily unprecedented durations. Secondary beneficiaries (divorce professionals, therapy sector) collect from the constraint's operation but did not create it.
% ABSENT_VOICES: Cultures with non-monogamous or non-dyadic partnership norms are structurally excluded from the dominant discourse, which treats lifelong dyadic monogamy as the only legitimate form. Their alternative arrangements (which often accommodate the lifespan-partnership mismatch through different structural forms) are marginalized in policy and cultural representation.
% DISAPPEARANCE_RATIONALE: If the constraint vanished — if human pair-bonding mechanisms spontaneously adapted to support 50-70 year partnerships with the same neurochemical stability as 15-25 year bonds — the world would rearrange substantially: divorce rates would collapse, the therapy and legal sectors would contract, and cultural scripts around partnership would shift. But the constraint cannot disappear through human action; it is a feature of our evolved biology interacting with extended lifespans. The verdict is 'world_unchanged' because the constraint is not a human arrangement that could be removed.
% FOUNDING_PROBLEM: Not applicable — this is not a constructed arrangement with a founding problem. The constraint is the collision between an evolved biological feature (pair-bonding duration calibrated to ancestral lifespans of 30-45 years) and a demographic shift (modern lifespans of 75-85 years).
% FOUNDING_PROBLEM_CORROBORATION: Evolutionary biologists, anthropologists, and demographers across institutions document the mismatch through converging lines of evidence: comparative primatology (human pair-bond neurochemistry resembles species with serial monogamy over 15-25 year windows), cross-cultural divorce rate patterns (the 15-25 year spike appears across societies with different norms), and life-history theory (ancestral mortality curves show modal lifespan ending near the pair-bond horizon). No party benefits from asserting the constraint exists; the corroboration is from disinterested empirical research.
narrative_ontology:disappearance_verdict(longevity_mismatch, world_unchanged).
narrative_ontology:founding_problem_status(longevity_mismatch, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(longevity_mismatch, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-24',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(longevity_mismatch, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(longevity_mismatch_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(longevity_mismatch, ExtMetricName, E),
    domain_priors:suppression_score(longevity_mismatch, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(longevity_mismatch),
    narrative_ontology:constraint_metric(longevity_mismatch, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(longevity_mismatch, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(longevity_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint is primarily a biological fact imposing costs, not a mechanism concentrating benefits. The modest extraction that exists flows to professional sectors (divorce industry, therapy) that have organized around the constraint's predictable effects. Suppression is very low (0.08) because the constraint does not depend on preventing alternatives — individuals are free to choose non-monogamous structures, shorter commitments, or serial partnerships. Theater ratio is near-zero (0.05) because there is minimal performative maintenance; the constraint operates through biological mechanisms, not institutional enforcement. Accessibility collapse is high (0.88) because once the evolutionary mismatch is understood, alternatives to the strain (adapting cultural expectations, choosing different partnership structures) become visible but the underlying biological calibration remains fixed. Resistance is low (0.15) because the constraint is not actively defended by any party; it simply exists as a feature of our evolutionary history colliding with demographic change. The measurement series shows slight upward drift in extractiveness and theater as professional sectors have grown around the constraint's effects, and modest increase in suppression as cultural norms have intensified expectations of lifelong monogamy despite the biological mismatch.
 *
 * PERSPECTIVAL GAP:
 *   From the analytical seats (evolutionary psychologists, demographers), this is a straightforward biological constraint — a mismatch between evolved mechanisms and modern conditions, with no normative content. From the long-term partner seats, the same structure operates as a source of endemic strain and a challenge to cultural scripts of lifelong commitment. From the beneficiary seats (divorce professionals, therapy sector), the constraint is a stable source of demand for their services. The engine should compute these seats differently: analytical seats should see mountain (low extraction, high accessibility collapse, minimal suppression); partner seats should see the constraint as more extractive because they bear its costs directly; beneficiary seats should see it as coordination (they provide real services) with modest extraction (they benefit from its persistence).
 *
 * DIRECTIONALITY LOGIC:
 *   Divorce industry professionals and the therapy sector are structural beneficiaries (d near 0.2-0.3): they collect revenue from the constraint's operation but did not create it and could not remove it. Serial monogamy advocates are weaker beneficiaries (d near 0.35): they gain platform and credibility from the constraint's visibility. Long-term partners are the primary targets (d near 0.75): they bear the direct costs of navigating partnerships across evolutionarily unprecedented durations, with constrained exit because dissolution itself is costly and culturally stigmatized. Children of late divorces are secondary targets (d near 0.8): they bear costs with even less agency. The analytical observers sit at d near 0.5: they study the constraint without being materially affected by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (if it has one) would be 'sustain lifelong dyadic partnerships as the normative form.' That mandate has not outlived its function because the constraint is not a constructed arrangement with a function — it is a biological fact. However, the CULTURAL RESPONSE to the constraint (insisting on lifelong monogamy as the only legitimate form despite the mismatch) may constitute mandatrophy: the cultural script persists even as the biological substrate makes it increasingly difficult to fulfill. The omega variables address this ambiguity: is the strain inevitable (pure mountain), or is it amplified by cultural rigidity that could be relaxed (false summit with extractive overlay)?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_vs_cultural_component,
    'How much of the measured strain in long-term partnerships is irreducible biological mismatch versus culturally imposed rigidity around partnership form?',
    'Cross-cultural comparison of partnership stability and satisfaction in societies with different norms around monogamy, divorce, and relationship structure. If societies with more flexible norms show substantially lower strain at the same partnership durations, the cultural component is significant.',
    'If the strain is primarily biological, the constraint is a pure mountain and the modest extraction to professional sectors is incidental. If the strain is substantially amplified by cultural rigidity, the constraint is a false summit: a real biological feature overlaid with extractive cultural enforcement that benefits those who profit from dissolution and therapeutic intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_vs_cultural_component, empirical, 'Proportion of partnership strain attributable to biological mismatch versus cultural rigidity.').

omega_variable(
    intervention_possibility,
    'Could biomedical or social interventions meaningfully extend the duration of stable pair-bonding, or is the 15-25 year horizon a hard constraint?',
    'Research into the neurochemical basis of pair-bonding (oxytocin, vasopressin systems) and whether pharmaceutical or behavioral interventions can sustain bonding beyond the evolved window. Also, longitudinal studies of intentional communities experimenting with alternative partnership structures.',
    'If interventions can extend stable bonding, the constraint is partly addressable and the current strain is a transitional problem (scaffold-like). If the horizon is fixed, the constraint is a permanent feature and cultural adaptation (accepting shorter partnerships, normalizing serial monogamy) is the only response.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_possibility, empirical, 'Whether the pair-bond duration limit is modifiable through intervention.').

omega_variable(
    beneficiary_amplification,
    'Do the professional sectors that benefit from the constraint (divorce industry, therapy) actively resist cultural or policy changes that would reduce partnership strain?',
    'Analysis of lobbying activity, professional organization positions on divorce law reform, and public messaging from therapy sector leaders. If these sectors oppose simplified dissolution procedures, relationship structure diversity, or other strain-reducing changes, they are extractive beneficiaries. If they support such changes, they are incidental beneficiaries of an unavoidable constraint.',
    'If beneficiaries actively resist strain reduction, the constraint has an extractive overlay and should be classified as false summit (mountain claim with tangled rope operation). If they do not resist, the constraint is a pure mountain with incidental beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_amplification, empirical, 'Whether beneficiaries actively defend the constraint''s cultural rigidity.').

omega_variable(
    alternative_structure_suppression,
    'Are alternative partnership structures (polyamory, relationship anarchy, intentional serial monogamy) genuinely available, or are they suppressed by legal and cultural mechanisms that benefit from dyadic lifelong monogamy?',
    'Legal analysis of partnership recognition, custody law, tax treatment, and inheritance rights for non-traditional structures. Cultural analysis of stigma, representation, and institutional accommodation. If alternatives face substantial legal or social barriers, suppression is higher than the base metric suggests.',
    'If alternatives are genuinely available, the low suppression score is accurate and the constraint is a mountain. If alternatives are suppressed, the constraint is a false summit: the biological mismatch is real, but cultural and legal enforcement amplifies its costs and channels people into the strain-maximizing structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_structure_suppression, empirical, 'Whether alternative partnership structures face systematic suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(longevity_mismatch, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(long_tr_t0, longevity_mismatch, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(long_tr_t0, observed).
narrative_ontology:measurement(long_tr_t30, longevity_mismatch, theater_ratio, 30, 0.025).
narrative_ontology:measurement_basis(long_tr_t30, observed).
narrative_ontology:measurement(long_tr_t60, longevity_mismatch, theater_ratio, 60, 0.03).
narrative_ontology:measurement_basis(long_tr_t60, observed).
narrative_ontology:measurement(long_tr_t90, longevity_mismatch, theater_ratio, 90, 0.04).
narrative_ontology:measurement_basis(long_tr_t90, observed).
narrative_ontology:measurement(long_tr_t120, longevity_mismatch, theater_ratio, 120, 0.045).
narrative_ontology:measurement_basis(long_tr_t120, observed).
narrative_ontology:measurement(long_tr_t150, longevity_mismatch, theater_ratio, 150, 0.05).
narrative_ontology:measurement_basis(long_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(long_be_t0, longevity_mismatch, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(long_be_t0, observed).
narrative_ontology:measurement(long_be_t30, longevity_mismatch, base_extractiveness, 30, 0.09).
narrative_ontology:measurement_basis(long_be_t30, observed).
narrative_ontology:measurement(long_be_t60, longevity_mismatch, base_extractiveness, 60, 0.1).
narrative_ontology:measurement_basis(long_be_t60, observed).
narrative_ontology:measurement(long_be_t90, longevity_mismatch, base_extractiveness, 90, 0.11).
narrative_ontology:measurement_basis(long_be_t90, observed).
narrative_ontology:measurement(long_be_t120, longevity_mismatch, base_extractiveness, 120, 0.115).
narrative_ontology:measurement_basis(long_be_t120, observed).
narrative_ontology:measurement(long_be_t150, longevity_mismatch, base_extractiveness, 150, 0.12).
narrative_ontology:measurement_basis(long_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(long_su_t0, longevity_mismatch, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(long_su_t0, observed).
narrative_ontology:measurement(long_su_t30, longevity_mismatch, suppression_requirement, 30, 0.065).
narrative_ontology:measurement_basis(long_su_t30, observed).
narrative_ontology:measurement(long_su_t60, longevity_mismatch, suppression_requirement, 60, 0.07).
narrative_ontology:measurement_basis(long_su_t60, observed).
narrative_ontology:measurement(long_su_t90, longevity_mismatch, suppression_requirement, 90, 0.075).
narrative_ontology:measurement_basis(long_su_t90, observed).
narrative_ontology:measurement(long_su_t120, longevity_mismatch, suppression_requirement, 120, 0.078).
narrative_ontology:measurement_basis(long_su_t120, observed).
narrative_ontology:measurement(long_su_t150, longevity_mismatch, suppression_requirement, 150, 0.08).
narrative_ontology:measurement_basis(long_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(longevity_mismatch, attachment_coordination).
narrative_ontology:boltzmann_floor_override(longevity_mismatch, 0.08).

% DUAL FORMULATION NOTE:
% This constraint is not part of a decomposed family. It stands alone as a biological-demographic mismatch claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
