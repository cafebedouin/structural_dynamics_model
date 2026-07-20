% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__market_licensing_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Statutory Exception â Market-Licensing Reading
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint is one reading of the contested fair-use kernel: the
 *   market-licensing reading holds that any use that could be licensed
 *   inherently harms the market for licensed uses, collapsing fair use to de
 *   minimis or unmonetizable corners. Codified in judicial precedent
 *   interpreting 17 U.S.C. Â§ 107, this reading functions as a statutory
 *   snare: it preserves the language of a copyright limitation while
 *   eliminating its practical operation through an expansive definition of
 *   potential market harm. The kernel itself remains contested, but this
 *   specific reading is structurally extractive.
 *
 * KEY AGENTS:
 *   - Major rights holders (beneficiary) â collect licensing rents from formerly fair uses
 *   - Rights management organizations (beneficiary) â administer expanded licensing regimes
 *   - Appellate benches applying the reading (agenda setter) â enforce the interpretation through precedent
 *   - Transformative creators, educators, documentary filmmakers, remix artists, libraries (payers/targets) â bear extraction through chilled speech and licensing costs
 *   - Fair use advocates (observer) â document and resist the doctrinal drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.92).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.85).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, snare).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Statutory Exception â Market-Licensing Reading").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, 'd0e8ceb3-375c-45a6-9b2d-276d61b4d3c6').
narrative_ontology:cs_kernel_codification('d0e8ceb3-375c-45a6-9b2d-276d61b4d3c6', fixed_text).
narrative_ontology:cs_authority_grounding('d0e8ceb3-375c-45a6-9b2d-276d61b4d3c6', lineage).
narrative_ontology:cs_interpretation_layer_present('d0e8ceb3-375c-45a6-9b2d-276d61b4d3c6').
narrative_ontology:cs_reading_relation('d0e8ceb3-375c-45a6-9b2d-276d61b4d3c6', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0e8ceb3-375c-45a6-9b2d-276d61b4d3c6', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_axiom('d0e8ceb3-375c-45a6-9b2d-276d61b4d3c6', foundational, potential_market_harm_defeats_fair_use).
narrative_ontology:cs_axiom_status(potential_market_harm_defeats_fair_use, holdable).
narrative_ontology:cs_axiom_grounding('d0e8ceb3-375c-45a6-9b2d-276d61b4d3c6', potential_market_harm_defeats_fair_use, conventional).
narrative_ontology:cs_axiom('d0e8ceb3-375c-45a6-9b2d-276d61b4d3c6', foundational, licensing_presumption_maximizes_welfare).
narrative_ontology:cs_axiom_status(licensing_presumption_maximizes_welfare, holdable).
narrative_ontology:cs_axiom_grounding('d0e8ceb3-375c-45a6-9b2d-276d61b4d3c6', licensing_presumption_maximizes_welfare, instrumental).
narrative_ontology:cs_reference_frame('d0e8ceb3-375c-45a6-9b2d-276d61b4d3c6', comprehensive_licensing_baseline).
narrative_ontology:cs_drift_state('d0e8ceb3-375c-45a6-9b2d-276d61b4d3c6', contemporary_copyright_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d0e8ceb3-375c-45a6-9b2d-276d61b4d3c6', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, major_rights_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, rights_management_organizations).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, remix_artists).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, libraries_and_archivists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert broad licensing claims over derivative, incidental, and transformative uses; collect royalties and settlement payments that would not flow under a narrower fair use doctrine, and leverage statutory damages to extract compliance without trial.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, major_rights_holders, beneficiary,
    powerful, generational, mobile, global).

% Administer collective licensing and enforcement infrastructure; benefit from expanded repertoire claims as fair use shrinks; collect administrative rents from increased transaction volume and licensing friction.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, rights_management_organizations, beneficiary,
    organized, generational, mobile, global).

% Issue precedential rulings interpreting the fourth fair-use factor to presume market harm from any conceivable licensing market; narrow the statutory exception to de minimis or unmonetizable uses through broad market-definition analysis.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, appellate_benches_applying_reading, agenda_setter,
    institutional, generational, analytical, national).

% Create remixes, mash-ups, fan works, and commentary; face statutory-damage exposure if a court finds a potential market for licensing their source material; self-censor or absorb licensing costs that erase margins.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, transformative_creators, payer,
    moderate, biographical, constrained, national).

% Rely on fair use for course reserves, e-reserves, and classroom display; forced into costly blanket licensing agreements or risk litigation as fair use defenses fail under broad market-harm presumptions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, educational_institutions, payer,
    organized, biographical, constrained, national).

% Include news footage, archival clips, and incidental music in nonfiction works; must clear licenses for every fragment or abandon projects when errors-and-omissions insurers demand zero fair-use risk.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers, payer,
    moderate, biographical, constrained, national).

% Produce user-generated transformative content on platforms; subject to DMCA takedowns and demonetization based on asserted potential licensing markets; lack resources to litigate fair use and lack institutional backing.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, remix_artists, payer,
    powerless, immediate, constrained, global).

% Digitize collections, provide access to print-disabled users, and preserve born-digital works; expansive market-harm readings threaten preservation and access missions by positing licensing markets for every archival use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, libraries_and_archivists, payer,
    organized, generational, constrained, national).

% File amicus briefs, publish empirical studies, and lobby for statutory reform; consistently lose on the fourth factor when courts adopt the market-licensing presumption; track doctrinal drift and document chilling effects.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, fair_use_advocates, observer,
    organized, generational, analytical, national).

narrative_ontology:fixing_cost_class(fair_use_statutory_exception__market_licensing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to preserve incentives for original creation by ensuring all potentially licensable uses generate revenue for rights holders through market exchange.
% TRANSFER_FUNCTION: Moves licensing revenue and settlement value from transformative users, educators, and archivists to rights holders and licensing administrators; moves expressive and creative opportunity in the reverse direction as unlicensed uses are chilled.
% ABSENT_VOICES: Individual remix artists without legal representation, small documentary producers, and foreign creators subject to US forum selection are structurally excluded; their works are suppressed pre-litigation but they rarely appear in reported decisions or legislative hearings.
% DISAPPEARANCE_RATIONALE: If this reading vanished, courts would revert to narrower market-harm analysis or transformativeness-predominant tests; documentary filmmakers, educators, and remix artists would regain fair-use breathing room; rights-holder revenue from speculative licensing markets would contract; and the creative economy would reorganize around broader unlicensed reuse.
% FOUNDING_PROBLEM: The statutory fair-use doctrine was built to prevent the copyright monopoly from stifling socially beneficial usesâcriticism, comment, news reporting, teaching, scholarship, and researchâwhile preserving sufficient incentive for original creation.
% FOUNDING_PROBLEM_CORROBORATION: Legislative history of the Copyright Act of 1976 and amicus filings by library and educational associations corroborate the original balancing purpose from outside the rights-holder beneficiary set; rights holders and copyright-bar counsel assert the problem remains live but frame it exclusively through market-preservation and incentive-maintenance narratives.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.92, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.92 because this reading nullifies fair use whenever any conceivable licensing market can be posited, converting a statutory limitation into a near-permission regime. Suppression is 0.85: the constraint persists through statutory-damage exposure, litigation cost asymmetry, and DMCA enforcement, actively suppressing unlicensed alternatives. Theater ratio is 0.45: market-harm analysis is still performed in opinions, but under this reading it becomes pro formaâany potential market defeats the defense. Accessibility collapse is 0.80 because once the reading is adopted, the fair-use alternative effectively disappears for ordinary users. Resistance is 0.55: libraries, educational associations, and fair-use advocates mount documented opposition, but lose on the fourth factor in courts adopting this reading.
 *
 * PERSPECTIVAL GAP:
 *   The rights-holder seat experiences this constraint as necessary incentive preservation; the transformative-creator and educator seats experience it as the elimination of statutory breathing room. The agenda-setter seat (courts) sees doctrinal continuity with statutory text; the payer seats see coercion through damage exposure. The engine computes this divergence from beneficiary/victim declarations and exit asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Major rights holders and rights management organizations are structural beneficiaries with mobile exitâthey can forum-shop and exploit global licensing infrastructureâyielding low directionality. Transformative creators, educators, documentary filmmakers, remix artists, and libraries are structural targets with constrained exit (litigation is prohibitively expensive and statutory damages are severe), yielding high directionality. Courts occupy an analytical seat with no extraction or payment. Fair-use advocates occupy an observer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Without mandatrophy screening, one might classify the entire fair-use kernel as a rope or tangled rope because copyright law overall has a coordination function. The market-licensing reading resolves this by isolating the specific interpretation: it is a snare because the coordination story (incentive preservation) is cover for pure extraction, the founding problem (balancing access and incentive) is contested, and the reading persists only through active enforcement while its practical effect nullifies the doctrine it purports to interpret.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_market_fictionality,
    'Are the licensing markets presumed by this reading empirically extant, or are they litigation fictions invented to defeat fair use?',
    'Economic audit of actual transaction volume in the posited markets versus litigation-generated hypothetical licensing scenarios.',
    'If the markets are largely speculative, base extractiveness increases and the coordination claim collapses entirely into extraction; if real, the reading retains a stronger coordination defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_market_fictionality, empirical, 'Whether posited licensing markets are real or fictional').

omega_variable(
    kernel_resolution_trajectory,
    'Will the fair-use kernel resolve toward this reading, toward the transformative-right reading, or remain permanently split?',
    'Longitudinal analysis of Supreme Court docket, circuit splits, and empirical influence of competing readings on lower-court outcomes.',
    'If the kernel resolves against this reading, the constraint dissolves or becomes a minority position; if permanently split, the constraint persists as an unstable extractive mechanism whose classification oscillates by forum.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_resolution_trajectory, conceptual, 'Whether the contested kernel stabilizes or remains split').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_market_tr_t0, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fair_use_market_tr_t8, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(fair_use_market_tr_t16, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(fair_use_market_tr_t24, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(fair_use_market_tr_t32, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 32, 0.45).
narrative_ontology:measurement(fair_use_market_tr_t40, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(fair_use_market_be_t0, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(fair_use_market_be_t8, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 8, 0.75).
narrative_ontology:measurement(fair_use_market_be_t16, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 16, 0.85).
narrative_ontology:measurement(fair_use_market_be_t24, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 24, 0.9).
narrative_ontology:measurement(fair_use_market_be_t32, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 32, 0.92).
narrative_ontology:measurement(fair_use_market_be_t40, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 40, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_market_su_t0, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fair_use_market_su_t8, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(fair_use_market_su_t16, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(fair_use_market_su_t24, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(fair_use_market_su_t32, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 32, 0.85).
narrative_ontology:measurement(fair_use_market_su_t40, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 40, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, narrow_defense_reading).

% DUAL FORMULATION NOTE:
% The fair-use statutory kernel decomposes into three structurally distinct readings. The market-licensing reading (this file) is the most extractive. The transformative-right reading and narrow-defense reading instantiate different constraints with different epsilon values, beneficiary structures, and failure modes. They are linked as a constraint family via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
