% ============================================================================
% CONSTRAINT STORY: adverse_effect_guarantee_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adverse_effect_guarantee_kernel_flat_control, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: adverse_effect_guarantee_kernel_flat_control
 *   human_readable: INA H-2A Adverse Effect Wage Bar
 *   domain: administrative law/labor economics/immigration policy
 *
 * SUMMARY:
 *   The INA's adverse-effect bar has been continuously in force since the
 *   1986 H-2A framework: no party disputes that the statutory sentence
 *   prohibiting wage-depressing guestworker admission is still law. What has
 *   moved is DOL's Adverse Effect Wage Rate (AEWR) methodology — the
 *   regulatory instrument that translates the statutory guarantee into an
 *   actual dollar figure. Successive rulemakings have changed the wage
 *   surveys used, the geographic and occupational aggregation, and the
 *   treatment of piece-rate versus hourly work, each time producing a
 *   different number while leaving the underlying statutory bar textually
 *   untouched. This story authors the substrate as a single flat constraint:
 *   one commitment, one set of metrics, with the contestation over what
 *   'satisfies' the bar landing in perspectival divergence across stakeholder
 *   seats and in omega variables, rather than in decomposed reading-stories.
 *
 * KEY AGENTS:
 *   - department_of_labor: administers and revises the AEWR methodology that operationalizes the statutory guarantee
 *   - agricultural_employers: pay the AEWR, benefit from methodology loosening, organized lobbying presence
 *   - h2a_guestworkers: receive the AEWR wage, powerless over its construction, visa-tied to sponsoring employer
 *   - domestic_farmworkers: the statute's named intended beneficiaries, experience methodology changes as wage ceiling effects
 *   - united_farm_workers_members: excluded from rulemaking control but active in litigation and comment
 *   - federal_courts: reviews methodology challenges but cannot dictate the measuring instrument DOL must use
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adverse_effect_guarantee_kernel_flat_control, 0.58).
domain_priors:suppression_score(adverse_effect_guarantee_kernel_flat_control, 0.52).
domain_priors:theater_ratio(adverse_effect_guarantee_kernel_flat_control, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adverse_effect_guarantee_kernel_flat_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(adverse_effect_guarantee_kernel_flat_control, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(adverse_effect_guarantee_kernel_flat_control, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(adverse_effect_guarantee_kernel_flat_control, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(adverse_effect_guarantee_kernel_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adverse_effect_guarantee_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(adverse_effect_guarantee_kernel_flat_control, "INA H-2A Adverse Effect Wage Bar").
narrative_ontology:topic_domain(adverse_effect_guarantee_kernel_flat_control, "administrative law/labor economics/immigration policy").

domain_priors:requires_active_enforcement(adverse_effect_guarantee_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(adverse_effect_guarantee_kernel_flat_control, adverse_effect_guarantee_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adverse_effect_guarantee_kernel_flat_control, agricultural_employers).
narrative_ontology:constraint_beneficiary(adverse_effect_guarantee_kernel_flat_control, h2a_guestworkers).
narrative_ontology:constraint_victim(adverse_effect_guarantee_kernel_flat_control, domestic_farmworkers).
narrative_ontology:constraint_victim(adverse_effect_guarantee_kernel_flat_control, united_farm_workers_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(adverse_effect_guarantee_kernel_flat_control, agricultural_employers).
narrative_ontology:constraint_victim(adverse_effect_guarantee_kernel_flat_control, h2a_guestworkers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and periodically revises the methodology for the Adverse Effect Wage Rate (AEWR) — the instrument that operationalizes the statutory bar. Controls which wage surveys, occupational classifications, and regional aggregations count as evidence of 'adverse effect.' Can tighten or loosen the measuring instrument through rulemaking without touching the statutory text itself, which lets it satisfy or defeat the commitment administratively.
narrative_ontology:constraint_stakeholder(adverse_effect_guarantee_kernel_flat_control, department_of_labor, agenda_setter,
    institutional, generational, analytical, national).

% Petition for H-2A certification and pay the AEWR as set by DOL's current methodology. Lobby continuously for methodology changes (broader occupational categories, coarser regional aggregation) that lower the computed wage floor. Bear the wage they must pay but capture the labor supply the program exists to guarantee; a lower AEWR is a direct cost reduction for them.
narrative_ontology:constraint_stakeholder(adverse_effect_guarantee_kernel_flat_control, agricultural_employers, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(adverse_effect_guarantee_kernel_flat_control, agricultural_employers, payer).

% Receive the wage the current AEWR methodology sets and depend entirely on continued employer sponsorship for legal presence. Benefit from having any wage floor at all relative to undocumented alternatives, but cannot contest the methodology that sets it — visa status is tied to the sponsoring employer, foreclosing any exit that would let them bargain over the number.
narrative_ontology:constraint_stakeholder(adverse_effect_guarantee_kernel_flat_control, h2a_guestworkers, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(adverse_effect_guarantee_kernel_flat_control, h2a_guestworkers, payer).

% Are the statute's named intended beneficiaries — the 'similarly employed U.S. workers' whose wages the bar exists to protect from suppression. Experience the AEWR as a wage ceiling in practice: employers calibrate offers to the certified rate, and when methodology changes lower that rate, domestic workers' bargaining position erodes without any change to the statutory text they were told protects them.
narrative_ontology:constraint_stakeholder(adverse_effect_guarantee_kernel_flat_control, domestic_farmworkers, payer,
    powerless, biographical, constrained, regional).

% Litigate and comment against AEWR methodology revisions, arguing that DOL's technical changes are a laundering mechanism that defeats the statutory guarantee without repealing it. Are heard in comment periods and litigation but do not control the rulemaking pen; their objections are recorded, not binding.
narrative_ontology:constraint_stakeholder(adverse_effect_guarantee_kernel_flat_control, united_farm_workers_members, excluded,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(adverse_effect_guarantee_kernel_flat_control, united_farm_workers_members, payer).

% Review challenges to AEWR methodology under arbitrary-and-capricious and Chevron/Loper Bright-successor deference standards. Can vacate or remand a methodology but cannot compel DOL to adopt any particular measuring instrument, leaving the agency wide latitude to try again with a different formula that produces a similar result.
narrative_ontology:constraint_stakeholder(adverse_effect_guarantee_kernel_flat_control, federal_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides agricultural employers a lawful, predictable channel for temporary foreign labor while nominally guaranteeing that importing that labor will not be used to undercut the wages of domestic farmworkers doing the same work — a genuine coordination problem (seasonal labor shortage vs. wage-suppression risk) that the statute was built to solve simultaneously.
% TRANSFER_FUNCTION: Moves bargaining leverage and wage floor from domestic farmworkers to agricultural employers whenever the AEWR measuring instrument is loosened, and moves it back toward domestic farmworkers whenever the instrument is tightened; H-2A guestworkers receive whatever wage the instrument currently yields, without say in the instrument's design.
% ABSENT_VOICES: Domestic farmworkers as a class are formally protected but structurally underrepresented in the rulemaking process that actually sets the number; individual workers rarely comment on Federal Register notices, and UFW's institutional voice is present but non-binding. H-2A workers themselves are almost entirely absent from the process that sets the wage floor they will receive.
% DISAPPEARANCE_RATIONALE: Agricultural employers would say the world barely changes — H-2A flows continue on some replacement standard, since the labor need is real and would be met one way or another. UFW and domestic farmworker advocates would say the world rearranges sharply: without any wage floor requirement, AEWR-level protection collapses to whatever the market alone would clear, which they argue is materially lower given seasonal labor's monopsony structure. The two camps disagree about the counterfactual, not just the value of the current arrangement.
% FOUNDING_PROBLEM: Congress needed a way to admit temporary agricultural labor to meet seasonal shortages without that admission becoming a tool for employers to displace or underbid the wages of the domestic workforce already doing that work — the 1986 IRCA-era H-2A framework was built explicitly to prevent guestworker admission from functioning as a wage-suppression backdoor.
% FOUNDING_PROBLEM_CORROBORATION: DOL and agricultural employer associations attest the founding problem is substantially addressed by the current AEWR methodology and ongoing labor shortages justify continued reliance on it. UFW, worker advocacy groups, and several federal court opinions (including successful APA challenges to specific AEWR methodology changes) attest that the founding problem persists in substance — that methodology revisions have repeatedly been used to erode the wage floor the statute was meant to guarantee — corroboration that comes from outside the employer-DOL axis that benefits from a lower measured AEWR.
narrative_ontology:disappearance_verdict(adverse_effect_guarantee_kernel_flat_control, contested).
narrative_ontology:founding_problem_status(adverse_effect_guarantee_kernel_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(adverse_effect_guarantee_kernel_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(adverse_effect_guarantee_kernel_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(adverse_effect_guarantee_kernel_flat_control, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adverse_effect_guarantee_kernel_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(adverse_effect_guarantee_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(adverse_effect_guarantee_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high 0.58 and rising over the interval because the AEWR methodology has trended, across multiple rulemakings, toward broader occupational and regional aggregation that produces lower computed wage floors — a drift that operates entirely beneath the unrepealed statutory text. Suppression sits at 0.52: it is not zero, because domestic farmworkers face real barriers to contesting the methodology (individual comment periods, costly litigation, monopsonistic regional labor markets that limit their own exit), but it is not total, because UFW retains standing to sue and has won specific APA challenges. Theater ratio rises to 0.47 because an increasing share of DOL's methodological activity is defended in the language of technical statistical refinement while its practical effect is wage-floor erosion — the coordination function (matching seasonal labor need to supply) persists genuinely, but a growing extractive layer rides on the same administrative machinery.
 *
 * PERSPECTIVAL GAP:
 *   From DOL and agricultural employers' seats, the constraint looks like a working Rope: a real coordination problem (seasonal labor shortage) solved by a wage-floor mechanism that is periodically refined for statistical accuracy. From domestic farmworkers' and UFW's seats, the same structure computes as a Tangled Rope shading toward Snare: the coordination function is real, but the wage floor it produces has been repeatedly loosened through methodology changes that function as a laundering mechanism defeating the statutory guarantee without ever amending it. The engine's per-seat computation from differing power/exit/scope atoms is expected to reproduce this divergence structurally rather than requiring either side's framing to be adopted wholesale.
 *
 * DIRECTIONALITY LOGIC:
 *   Agricultural employers derive a low-d, beneficiary-leaning position: they pay the AEWR but capture the labor-supply guarantee and benefit directly from any methodology change that lowers it, with organized power and only constrained (not trapped) exit since they can adjust hiring mix. H-2A guestworkers also derive toward the beneficiary end structurally (they receive wages they would not otherwise access) but their trapped exit options and powerless position mean the derivation should sit closer to the middle than a pure beneficiary reading would suggest — visa-tied dependency means they cannot bargain over the instrument that sets their wage, which caps how much benefit-derived subsidy the framework should assign them. Domestic farmworkers and UFW members derive high-d, target-leaning positions: they are the statute's named beneficiaries in text but bear the actual cost when the measuring instrument moves, with powerless-to-organized power split and constrained exit (regional labor markets, few alternative employers) amplifying the effective extraction they experience relative to the raw statutory promise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — guestworker admission being used to undercut domestic wages — is genuinely contested as live or dead: DOL argues current methodology reasonably reflects labor market conditions, while UFW and independent court findings argue the methodology has been progressively weakened in ways that let the underlying evasion recur through a different door. Classifying this as tangled_rope rather than snare or rope avoids two mislabeling errors: treating the arrangement as pure benign coordination (which would erase the wage-floor drift domestic farmworkers experience) and treating it as pure extraction (which would erase the genuine seasonal-labor-matching function that both employers and guestworkers actually rely on). The tangled_rope classification requires exactly the beneficiary/victim/enforcement triple this story authors, and keeps the mandatrophy question — whether the 1986 anti-suppression mandate has outlived its operative function while the statute persists as text — open for resolution by the corroboration evidence rather than foreclosed by the classification itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodology_change_as_statutory_defeat,
    'Does a facially technical revision to the AEWR wage-survey methodology that produces a materially lower wage floor constitute ''defeating'' the statutory adverse-effect guarantee, or is it a legitimate exercise of DOL''s delegated technical discretion under Chevron-successor deference?',
    'Comparative analysis of AEWR levels under successive methodologies against independent labor-market wage data for the same occupations and regions, cross-checked against APA arbitrary-and-capricious rulings that have vacated specific methodology changes.',
    'If methodology changes systematically track employer preference rather than independent wage evidence, the constraint''s true operative function has shifted from guarantee to laundering mechanism even though the statutory text is unchanged — supporting a snare-leaning reading. If methodology changes track genuine improvements in wage-data quality, the tangled_rope''s coordination function remains dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodology_change_as_statutory_defeat, empirical, 'Whether AEWR methodology revisions are technical refinement or substantive evasion of the statutory bar.').

omega_variable(
    guestworker_dependency_direction,
    'Should H-2A guestworkers be modeled primarily as beneficiaries of the wage floor (relative to their pre-migration alternatives) or as targets of the same visa-dependency structure that suppresses domestic wages (since their inability to bargain is part of what makes the wage floor moveable)?',
    'Track whether AEWR reductions correlate with reduced guestworker bargaining outcomes independent of domestic wage effects, and whether guestworkers who exit sponsorship (where legally possible) achieve materially different wages.',
    'If guestworkers are primarily beneficiaries, the directionality derivation toward the beneficiary end is correct as authored. If their trapped exit options mean they are effectively co-targets alongside domestic farmworkers, the directionality should shift toward the target end and the overall extraction reading intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guestworker_dependency_direction, conceptual, 'Whether visa-tied guestworkers are structurally closer to beneficiaries or to co-targets of the wage-floor mechanism.').

omega_variable(
    judicial_check_sufficiency,
    'Is APA arbitrary-and-capricious review, as currently exercised by federal courts, a sufficient check on methodology drift, or does the courts'' inability to dictate a specific measuring instrument leave DOL free to iterate toward the same low-AEWR outcome through successive reformulations?',
    'Track the pattern of vacated AEWR rules over time: if DOL''s replacement methodologies converge back toward similar wage outcomes after each vacatur, judicial review is a formal check without substantive bite; if replacement methodologies genuinely diverge, the check is functioning.',
    'If judicial review is toothless in practice, the suppression metric is understated relative to the structural reality, and the classification should weight more heavily toward snare; if judicial review meaningfully constrains outcomes, the tangled_rope classification''s enforcement requirement is satisfied by a genuinely contestable process rather than a captured one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_check_sufficiency, empirical, 'Whether judicial review of AEWR methodology meaningfully constrains outcomes or is a formal check that DOL can iterate around.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adverse_effect_guarantee_kernel_flat_control, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adve_tr_t0, adverse_effect_guarantee_kernel_flat_control, theater_ratio, 0, 0.22).
narrative_ontology:measurement(adve_tr_t6, adverse_effect_guarantee_kernel_flat_control, theater_ratio, 6, 0.26).
narrative_ontology:measurement(adve_tr_t12, adverse_effect_guarantee_kernel_flat_control, theater_ratio, 12, 0.31).
narrative_ontology:measurement(adve_tr_t18, adverse_effect_guarantee_kernel_flat_control, theater_ratio, 18, 0.36).
narrative_ontology:measurement(adve_tr_t24, adverse_effect_guarantee_kernel_flat_control, theater_ratio, 24, 0.4).
narrative_ontology:measurement(adve_tr_t31, adverse_effect_guarantee_kernel_flat_control, theater_ratio, 31, 0.44).
narrative_ontology:measurement(adve_tr_t38, adverse_effect_guarantee_kernel_flat_control, theater_ratio, 38, 0.47).

% Extraction over time
narrative_ontology:measurement(adve_be_t0, adverse_effect_guarantee_kernel_flat_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(adve_be_t6, adverse_effect_guarantee_kernel_flat_control, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(adve_be_t12, adverse_effect_guarantee_kernel_flat_control, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(adve_be_t18, adverse_effect_guarantee_kernel_flat_control, base_extractiveness, 18, 0.5).
narrative_ontology:measurement(adve_be_t24, adverse_effect_guarantee_kernel_flat_control, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(adve_be_t31, adverse_effect_guarantee_kernel_flat_control, base_extractiveness, 31, 0.56).
narrative_ontology:measurement(adve_be_t38, adverse_effect_guarantee_kernel_flat_control, base_extractiveness, 38, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(adve_su_t0, adverse_effect_guarantee_kernel_flat_control, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(adve_su_t6, adverse_effect_guarantee_kernel_flat_control, suppression_requirement, 6, 0.34).
narrative_ontology:measurement(adve_su_t12, adverse_effect_guarantee_kernel_flat_control, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(adve_su_t18, adverse_effect_guarantee_kernel_flat_control, suppression_requirement, 18, 0.42).
narrative_ontology:measurement(adve_su_t24, adverse_effect_guarantee_kernel_flat_control, suppression_requirement, 24, 0.46).
narrative_ontology:measurement(adve_su_t31, adverse_effect_guarantee_kernel_flat_control, suppression_requirement, 31, 0.49).
narrative_ontology:measurement(adve_su_t38, adverse_effect_guarantee_kernel_flat_control, suppression_requirement, 38, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adverse_effect_guarantee_kernel_flat_control, resource_allocation).
narrative_ontology:affects_constraint(adverse_effect_guarantee_kernel_flat_control, h2a_labor_certification_process).
narrative_ontology:affects_constraint(adverse_effect_guarantee_kernel_flat_control, domestic_farmworker_wage_protection_standard).

% DUAL FORMULATION NOTE:
% This story treats the adverse-effect bar as a single flat constraint per the construction-perturbation control condition: it does not decompose the statutory guarantee from its AEWR-methodology instrument into separate reading-stories, even though the source material identifies exactly the kind of committer structure (stable kernel, contested measuring instrument) that would warrant decomposition under the ordinary authoring rules. The contestation is instead carried entirely by perspectival stakeholder divergence and the omega variables above. A companion decomposed treatment, if authored, would separate 'the statutory text is unrepealed' (near-mountain, uncontested) from 'the current AEWR methodology satisfies the statutory bar' (contested, tangled_rope-or-snare) as linked siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
