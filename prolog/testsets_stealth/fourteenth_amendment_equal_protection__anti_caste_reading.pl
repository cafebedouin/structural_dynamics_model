% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__anti_caste_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Equal Protection Anti-Caste Reading: Standing Hierarchy Arrangement
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the anti-caste reading of the Fourteenth
 *   Amendment's Equal Protection Clause - one reading of a contested kernel
 *   whose sibling, the formal-equality reading, is authored as a separate
 *   constraint. The standing arrangement under contest is the persisting
 *   structure of racial, gender, and status hierarchy in American life as
 *   governed through formally neutral rules; epsilon is authored for THAT
 *   arrangement, assessed by this reading's own lights, per the fixed
 *   referent rule for kernel readings. The reading holds that a clause
 *   guaranteeing the equal protection of the laws commands the state to
 *   dismantle caste, not merely abstain from creating it; the arrangement it
 *   contests transmits hierarchy through facially uniform property,
 *   schooling, credit, and enforcement systems that the neutrality posture
 *   shields from challenge. The sibling reading is not folded into this file:
 *   it authors its own epsilon over the same referent and its own victim set.
 *   Claim and metrics are independent: claimed_type records my structural
 *   judgment that the arrangement is a tangled rope - genuine coordination of
 *   a continental polity carrying asymmetric extraction held by active
 *   enforcement - while the metrics describe the arrangement as this reading
 *   sees it.
 *
 * KEY AGENTS:
 *   - scotus_equal_protection_interpreter: agenda-setter (institutional/arbitrage) - authors the doctrine that defines what corrective action remains lawful
 *   - racially_subordinated_communities: primary target of the standing arrangement (organized/trapped) - bear compounding caste costs with no exit from racialized position
 *   - women_facing_structural_exclusion: secondary target (organized/constrained) - bear gendered exclusion across occupational and care structures
 *   - dominant_group_incumbents: primary beneficiary (powerful/mobile) - collect positional subsidy without deliberate maintenance
 *   - legacy_wealth_holders: concentrated beneficiary (powerful/arbitrage) - transmit advantage through channels untouched by corrective doctrine
 *   - civil_rights_enforcement_bodies: administering machinery of residual corrective authority (institutional/constrained)
 *   - public_university_admissions_offices: compliance-bearing administrators (institutional/constrained)
 *   - anti_subordination_scholars: analytical observer - supplies the reading's doctrinal arguments
 *   - cross_class_coalition_advocates: excluded voice - contests the two-sided framing from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.83).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.78).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.83).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Equal Protection Anti-Caste Reading: Standing Hierarchy Arrangement").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, '08d6a7b8-1c07-4d52-8cd2-9ab607a2e236').
narrative_ontology:cs_kernel_codification('08d6a7b8-1c07-4d52-8cd2-9ab607a2e236', fixed_text).
narrative_ontology:cs_authority_grounding('08d6a7b8-1c07-4d52-8cd2-9ab607a2e236', lineage).
narrative_ontology:cs_interpretation_layer_present('08d6a7b8-1c07-4d52-8cd2-9ab607a2e236').
narrative_ontology:cs_reading_relation('08d6a7b8-1c07-4d52-8cd2-9ab607a2e236', fourteenth_amendment_equal_protection__formal_equality_reading, forecloses).
narrative_ontology:cs_axiom('08d6a7b8-1c07-4d52-8cd2-9ab607a2e236', foundational, equal_protection_mandates_subordination_dismantling).
narrative_ontology:cs_axiom_status(equal_protection_mandates_subordination_dismantling, holdable).
narrative_ontology:cs_axiom_grounding('08d6a7b8-1c07-4d52-8cd2-9ab607a2e236', equal_protection_mandates_subordination_dismantling, deontological).
narrative_ontology:cs_axiom('08d6a7b8-1c07-4d52-8cd2-9ab607a2e236', foundational, neutrality_that_entrenches_caste_denies_protection).
narrative_ontology:cs_axiom_status(neutrality_that_entrenches_caste_denies_protection, holdable).
narrative_ontology:cs_axiom_grounding('08d6a7b8-1c07-4d52-8cd2-9ab607a2e236', neutrality_that_entrenches_caste_denies_protection, empirically_contingent).
narrative_ontology:cs_reference_frame('08d6a7b8-1c07-4d52-8cd2-9ab607a2e236', reconstruction_anticaste_charter).
narrative_ontology:cs_drift_state('08d6a7b8-1c07-4d52-8cd2-9ab607a2e236', contemporary_post_sffa_doctrine, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('08d6a7b8-1c07-4d52-8cd2-9ab607a2e236', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_group_incumbents).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, legacy_wealth_holders).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, racially_subordinated_communities).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, women_facing_structural_exclusion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_enforcement_bodies).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, public_university_admissions_offices).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, anti_subordination_principle).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, carolene_products_footnote_four_logic).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, congressional_enforcement_power_section_five).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors the controlling interpretation of the Fourteenth Amendment's guarantee of equal protection. Across the Croson, Adarand, Parents Involved, and Students for Fair Admissions lines, the Court has held that most race-conscious state action fails strict scrutiny and that facially neutral practices require proof of discriminatory intent before they can be challenged. Each interpretive choice defines what corrective measures legislatures and agencies may lawfully adopt, and a future Court could revise the framework wholesale.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, scotus_equal_protection_interpreter, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the arrangement's compounding costs: median household wealth far below the national white median, neighborhood and school segregation that persisted after Brown, disproportionate contact with policing and sentencing, and reduced intergenerational mobility. There is no exit from racialized position; response runs through churches, advocacy organizations, and test-case litigation. Organized capacity is real - the litigation tradition that produced Brown continues - but the available remedies narrow with each doctrinal turn.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, racially_subordinated_communities, payer,
    organized, generational, trapped, national).

% Concentrated in lower-paid occupational segments, underrepresented in senior institutional roles, and carrying unpaid care work that prices at zero in national accounts. Intermediate scrutiny permits some corrective measures and blocks others; individual mobility varies widely while the structural position persists across generations.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, women_facing_structural_exclusion, payer,
    organized, generational, constrained, national).

% Occupy positions subsidized by inherited advantage: federally underwritten mid-century home equity, school quality tied to local property wealth, network-mediated hiring, and freedom from the policing and sentencing patterns applied to others. The arrangement requires no deliberate maintenance from them; formally uniform rules preserve their position as the default outcome.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_group_incumbents, beneficiary,
    powerful, generational, mobile, national).

% Transmit advantage through inheritance, endowments, and legacy-admission channels that remain untouched even as race-conscious channels are struck down. Assets and children move freely across jurisdictions; nothing about the arrangement constrains them.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, legacy_wealth_holders, beneficiary,
    powerful, generational, arbitrage, national).

% The Justice Department's civil rights division, the EEOC, HUD fair-housing offices, and the private civil rights bar administer whatever corrective authority doctrine leaves open - pattern-or-practice suits, consent decrees, fair-lending enforcement. Their capacity is consumed contesting the narrowing, and their toolkit expands or contracts with each interpretive turn.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_enforcement_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_enforcement_bodies, payer).

% Administered race-conscious admissions under the Grutter-era strictures, then dismantled those processes after Students for Fair Admissions and rebuilt them around socioeconomic and essay-based proxies. They absorb whichever compliance regime prevails and carry litigation risk in both directions.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, public_university_admissions_offices, payer,
    institutional, biographical, constrained, national).

% Develop the account of the Fourteenth Amendment as a charter against caste - Owen Fiss's groups-and-equality argument, Alan Freeman's substantive-equality critique - and supply the doctrinal arguments litigators deploy. They hold no enforcement power; their influence runs through briefs, opinions in dissent, and state-court adoption.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, anti_subordination_scholars, observer,
    analytical, generational, analytical, national).

% Argue that status hierarchy binds poor and working-class people across racial lines and that neither the colorblind posture nor race-exclusive remediation addresses their position. They sit largely outside the doctrinal conversation, which is framed as a two-sided contest between colorblind and race-conscious camps.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, cross_class_coalition_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_group_incumbents).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__anti_caste_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a continental republic through formally uniform rules: a single constitutional framework governing property, credit, school finance, labor markets, and political representation across fifty states, solving the problem of governing a vast heterogeneous polity without explicit status legislation.
% TRANSFER_FUNCTION: Moves opportunity, wealth, security, and public standing from racially subordinated communities and structurally excluded women toward dominant-group incumbents and legacy wealth holders - through school finance coupled to property wealth, inherited and network-mediated hiring, credit pricing, differential enforcement, and the progressive withdrawal of corrective authority.
% ABSENT_VOICES: Cross-class coalition advocates are outside the conversation, as are the subordinated communities themselves in the venues that matter: the equal-protection docket is built by institutional litigants, and the freedpeople whose subordination founded the provision had no voice in the 1877 abandonment of enforcement. Contemporary debate proceeds between colorblind and race-conscious camps; class-structured hierarchy goes largely unargued.
% DISAPPEARANCE_RATIONALE: If hierarchy stopped conferring advantage overnight, property values would decouple from school catchments, hiring pipelines would reprice, university admissions would recompose, and political coalitions built on positional anxiety would dissolve - trillions of dollars in positional advantage would reprice within a decade, and no institution would voluntarily hold the resulting losses.
% FOUNDING_PROBLEM: After emancipation, the former Confederate states enacted Black Codes and built a new racial caste system while denying freedpeople civil standing; the Fourteenth Amendment's framers sought to constitutionalize national citizenship, empower Congress to enforce it, and give the nation authority to dismantle caste rather than merely abstain from creating it.
% FOUNDING_PROBLEM_CORROBORATION: Attested entirely from outside the benefiting parties: Federal Reserve Survey of Consumer Finances wealth-gap series, U.S. Commission on Civil Rights reporting, peer-reviewed studies of segregation persistence and sentencing disparity, and the ongoing dockets of the NAACP Legal Defense Fund and similar litigators. No organ of the beneficiary set concedes the problem's salience; the corroboration record is wholly external, which is itself signal.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.83, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.83 because the referent arrangement's transfers are systemic, compounding, and intergenerational: an order-of-magnitude household wealth gap traceable to redlining and exclusion, school segregation that survived Brown, and enforcement disparities - the reading counts all of it as extraction sustained by the neutrality posture. Suppression is 0.78 and is authored as a raw structural property, unscaled by power or scope: holding the neutrality posture now requires active doctrinal machinery (strict scrutiny operating near per-se, the Washington v. Davis intent rule, SFFA's extension to admissions) deployed against democratically enacted corrective measures. Theater is 0.45: as material remedies were struck down, performative anti-racism - statements, trainings, symbolic appointments, corporate commitments largely retired by 2024 - substituted for redistribution. Accessibility collapse is honestly low at 0.35: the colorblind alternative remains fully accessible and currently ascendant, so this constraint closes off no alternatives. Resistance is 0.85: an organized litigation movement, state-level bans, and sustained political backlash meet the reading at every turn. The three temporal series share one grid (t=0..48, eight-unit steps, mapping 1978-2026); all points are observed. Coalition note: the target seats are not purely powerless - a century of test-case litigation capacity gives racially subordinated communities organized leverage, which is part of why resistance registers high on the arrangement itself.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute different arrangements from the same structure. From the target seats the arrangement is enforced extraction: costs arrive without consent and exit is unavailable. From the beneficiary seats it is default normality that demands nothing of them. From the agenda-setter seat it is principled neutrality - the discipline of treating likes alike. From the excluded seat it is a misdescription: a contest framed as colorblind-versus-race-conscious that never asks who pays along class lines. The engine derives these divergences from the structural data; the divergence between the agenda-setter's self-understanding and the target seats' experience is the measurement this story exists to preserve.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map directly: racially_subordinated_communities (victim, trapped) derive directionality near the full-target end; women_facing_structural_exclusion (victim, constrained) sit high-target; dominant_group_incumbents (beneficiary, mobile) sit low; legacy_wealth_holders (beneficiary, arbitrage) sit nearest the beneficiary end. National spatial scope makes discrimination verification harder, which amplifies effective extraction for targets - the engine owns that arithmetic. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already differentiate every seat, and the dual-positioned enforcement bodies carry their ambivalence through secondary_role rather than an override that would smear across all institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem is live, so the arrangement has not outlived a dead mandate - its function, on this reading, IS the problem. The tangled-rope classification guards both symmetrical errors. Reading the arrangement as pure rope (its own self-description: uniform rules, equal treatment) would mask the extraction; reading it as pure snare would erase the genuine coordination a continental polity requires and mispredict reform dynamics - the arrangement cannot simply be abolished, it must be re-coordinated, which is exactly what the corrective mandate proposes. The R5 consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: no mismatch, no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the fourteenth_amendment_equal_protection kernel - the anti-caste reading. The sibling formal_equality_reading would relocate the entire structure: subordinated groups would leave the victim set, race-conscious program participants would enter it, and epsilon would attach to the remedial apparatus rather than to the standing hierarchy.',
    'Doctrinal evolution (Court composition, future Section Five enforcement), ratification-history scholarship, and state constitutional experimentation; observe which reading''s victim set the operative doctrine tracks.',
    'If the formal-equality reading prevails permanently, this constraint''s operative scope collapses to hortatory and its measured extraction becomes historical; if the anti-caste reading prevails, the sibling''s victim set inverts and remedial programs become the contested arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the Equal Protection kernel controls, and therefore which arrangement sits in the referent.').

omega_variable(
    remedial_instrument_extraction,
    'The manifest''s expected structural delta flags high epsilon for remedial programs: do the corrective instruments this reading legitimates - race-conscious admissions, minority contracting set-asides, weighted electoral remedies - impose concentrated costs on identifiable non-beneficiaries (displaced applicants, passed-over contractors), making each instrument a separately classifiable arrangement rather than a pure remedy?',
    'Post-SFFA natural experiments comparing outcomes under race-conscious and race-neutral regimes; cost-incidence analysis of who bears each instrument''s burdens.',
    'If instrument-level costs are concentrated, the anti-caste program decomposes into a constraint family - baseline-dismantling plus per-instrument stories with their own epsilon values linked by network edges; if costs are diffuse or transitional, the program remains unified inside this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_instrument_extraction, empirical, 'Whether remedial instruments carry their own extractive structure separable from the baseline arrangement.').

omega_variable(
    neutrality_entrenchment_wager,
    'This reading''s second axiom holds that facially neutral rules which entrench caste deny equal protection; Washington v. Davis adopted the contrary wager that neutrality is presumptively innocent absent discriminatory intent. Which empirical claim about how hierarchy reproduces holds?',
    'Longitudinal wealth-transmission and segregation studies; natural experiments from jurisdictions adopting disparate-impact standards versus intent standards.',
    'Confirmation stabilizes this reading''s axioms and deepens axiom-overriding drift against the sibling; disconfirmation pushes this reading toward the formal-equality frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_entrenchment_wager, empirical, 'The empirical wager underneath the anti-caste reading''s treatment of neutral rules.').

omega_variable(
    status_hierarchy_scope,
    'Does status hierarchy in this reading extend beyond race and gender to wealth-based caste - inviting the cross-class coalition''s objection that the arrangement extracts along class lines the doctrine never reaches - or is it bounded to the Amendment''s historical targets?',
    'Interpretive scholarship and movement uptake: watch whether anti-caste arguments are extended to wealth-based exclusion in litigation and legislation.',
    'Broader scope adds beneficiary and victim sets, raises measured extraction further, and strengthens the excluded seat''s claim to be heard; narrower scope preserves the current structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_hierarchy_scope, conceptual, 'Boundary of the reading''s hierarchy concept.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(four_tr_t0, observed).
narrative_ontology:measurement(four_tr_t8, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(four_tr_t8, observed).
narrative_ontology:measurement(four_tr_t16, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement_basis(four_tr_t16, observed).
narrative_ontology:measurement(four_tr_t24, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement_basis(four_tr_t24, observed).
narrative_ontology:measurement(four_tr_t32, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement_basis(four_tr_t32, observed).
narrative_ontology:measurement(four_tr_t40, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(four_tr_t40, observed).
narrative_ontology:measurement(four_tr_t48, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 48, 0.45).
narrative_ontology:measurement_basis(four_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(four_be_t0, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement_basis(four_be_t0, observed).
narrative_ontology:measurement(four_be_t8, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 8, 0.73).
narrative_ontology:measurement_basis(four_be_t8, observed).
narrative_ontology:measurement(four_be_t16, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement_basis(four_be_t16, observed).
narrative_ontology:measurement(four_be_t24, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 24, 0.77).
narrative_ontology:measurement_basis(four_be_t24, observed).
narrative_ontology:measurement(four_be_t32, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 32, 0.79).
narrative_ontology:measurement_basis(four_be_t32, observed).
narrative_ontology:measurement(four_be_t40, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(four_be_t40, observed).
narrative_ontology:measurement(four_be_t48, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 48, 0.83).
narrative_ontology:measurement_basis(four_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t0, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(four_su_t0, observed).
narrative_ontology:measurement(four_su_t8, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(four_su_t8, observed).
narrative_ontology:measurement(four_su_t16, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement_basis(four_su_t16, observed).
narrative_ontology:measurement(four_su_t24, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(four_su_t24, observed).
narrative_ontology:measurement(four_su_t32, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement_basis(four_su_t32, observed).
narrative_ontology:measurement(four_su_t40, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(four_su_t40, observed).
narrative_ontology:measurement(four_su_t48, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 48, 0.78).
narrative_ontology:measurement_basis(four_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, resource_allocation).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Equal Protection' covers two structurally distinct claims sharing one ratified text and one referent arrangement. This file authors the anti-caste reading (epsilon 0.83 over the standing hierarchy; victims = subordinated groups). The sibling formal_equality_reading authors the prohibition reading over the same referent with a different victim set and a different epsilon. The sibling currently controls federal doctrine and structurally influences this reading's operating environment (SFFA foreclosing its central applications) without resolving the contest; each file links the other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
