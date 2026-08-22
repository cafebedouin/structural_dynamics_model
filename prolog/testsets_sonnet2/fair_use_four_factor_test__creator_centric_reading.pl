% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use Four-Factor Test — Creator-Centric (Narrow Exception) Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This story instantiates the creator-centric reading of the fair use
 *   four-factor test: fair use as a narrow, judicially-construed exception to
 *   an otherwise exclusive property right, with the four statutory factors
 *   (purpose/character of use, nature of the work, amount used, market
 *   effect) weighed to protect creator incentives against erosion by
 *   unauthorized use. This is one of three sibling readings of the same
 *   kernel (fair_use_four_factor_test). The user-centric reading treats fair
 *   use as an affirmative right protecting public access; the
 *   transformative-use reading subordinates market harm to
 *   transformativeness. Each reading produces a structurally distinct
 *   constraint with its own epsilon and beneficiary/victim set; this file
 *   authors only the creator-centric reading, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - rights_holding_publishers: institutional beneficiary/agenda-setter — sets licensing terms, litigates narrowly
 *   - major_content_licensors: institutional beneficiary — collects fees expanded by narrow doctrine
 *   - transformative_use_creators: moderate-power payer — bears litigation risk and self-censorship
 *   - documentarians_and_critics: moderate-power payer — clears rights defensively
 *   - public_domain_commons: non-agent payer — cultural stock whose growth is slowed
 *   - independent_remix_artists: powerless payer — cannot bear cost of testing doctrine
 *   - federal_courts: institutional agenda-setter — applies the narrow construction
 *   - individual_authors_and_musicians: excluded — incentive rationale asserted in their name by rights holders who hold the actual copyright
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.68).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.6).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test — Creator-Centric (Narrow Exception) Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, 'b585d03c-859f-4fe5-baff-c24b6e7ed9c6').
narrative_ontology:cs_kernel_codification('b585d03c-859f-4fe5-baff-c24b6e7ed9c6', formalized).
narrative_ontology:cs_authority_grounding('b585d03c-859f-4fe5-baff-c24b6e7ed9c6', lineage).
narrative_ontology:cs_interpretation_layer_present('b585d03c-859f-4fe5-baff-c24b6e7ed9c6').
narrative_ontology:cs_reading_relation('b585d03c-859f-4fe5-baff-c24b6e7ed9c6', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('b585d03c-859f-4fe5-baff-c24b6e7ed9c6', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('b585d03c-859f-4fe5-baff-c24b6e7ed9c6', foundational, exclusive_right_is_default_fair_use_is_exception).
narrative_ontology:cs_axiom_status(exclusive_right_is_default_fair_use_is_exception, holdable).
narrative_ontology:cs_axiom_grounding('b585d03c-859f-4fe5-baff-c24b6e7ed9c6', exclusive_right_is_default_fair_use_is_exception, conventional).
narrative_ontology:cs_axiom('b585d03c-859f-4fe5-baff-c24b6e7ed9c6', foundational, market_harm_factor_dominates_balancing).
narrative_ontology:cs_axiom_status(market_harm_factor_dominates_balancing, holdable).
narrative_ontology:cs_axiom_grounding('b585d03c-859f-4fe5-baff-c24b6e7ed9c6', market_harm_factor_dominates_balancing, instrumental).
narrative_ontology:cs_reference_frame('b585d03c-859f-4fe5-baff-c24b6e7ed9c6', exclusive_right_with_narrow_statutory_exception).
narrative_ontology:cs_drift_state('b585d03c-859f-4fe5-baff-c24b6e7ed9c6', post_digital_remix_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b585d03c-859f-4fe5-baff-c24b6e7ed9c6', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, rights_holding_publishers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, major_content_licensors).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, legacy_media_conglomerates).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_use_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, documentarians_and_critics).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_commons).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, independent_remix_artists).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, property_right_primacy_doctrine).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, incentive_theory_of_creation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold copyrights over large catalogs and litigate or threaten litigation against uses that fall outside licensing arrangements. Under this reading, every unlicensed use is presumptively an infringement unless it survives a narrow, factor-weighted defense; publishers set licensing terms and lobby for narrow fair use doctrine because it maximizes the share of downstream use that must be paid for.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, rights_holding_publishers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__creator_centric_reading, rights_holding_publishers, agenda_setter).

% Operate licensing clearinghouses and collect fees for permitted reuse. A narrow fair use doctrine expands the market for licenses by shrinking the set of uses that can proceed without payment; they benefit directly from courts weighing the fourth factor (market harm) heavily against unlicensed transformative use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, major_content_licensors, beneficiary,
    institutional, generational, arbitrage, global).

% Own large back catalogs of film, music, and text. Their incentive-preservation argument treats fair use skeptically because any expansion of unlicensed use is framed as eroding the return on original investment, even where the challenged use is critical or transformative.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, legacy_media_conglomerates, beneficiary,
    institutional, generational, arbitrage, global).

% Produce parody, remix, sampling, and appropriation art that recontextualizes existing work. Under the creator-centric weighing, their use is treated as presumptively infringing rather than presumptively protected; they bear litigation risk, insurance costs, or self-censorship even when the eventual outcome might favor them, because the four-factor test is applied narrowly and unpredictably against them.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_use_creators, payer,
    moderate, biographical, constrained, national).

% Need to quote, excerpt, and juxtapose copyrighted material to critique or document it. Facing a narrow-exception framing, they clear rights defensively or cut material that a broader doctrine would protect, because the cost of losing a factor-weighing dispute (statutory damages, injunction) is asymmetric to the value of the excerpt.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, documentarians_and_critics, payer,
    moderate, biographical, constrained, national).

% The stock of culturally available material that would otherwise grow through transformative reuse and eventual copyright expiration. A narrow reading slows the practical rate at which works are quoted, reworked, and folded into new cultural production, effectively narrowing what functions as available culture even where formal public domain status is unaffected.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_commons, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(fair_use_four_factor_test__creator_centric_reading, public_domain_commons).

% Work without institutional legal support. They cannot absorb the cost of a four-factor dispute regardless of eventual merits, so a narrow-exception default forecloses uses that a court might well have protected, simply because the risk-adjusted cost of testing the doctrine exceeds what an individual artist can bear.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, independent_remix_artists, payer,
    powerless, biographical, trapped, global).

% Apply the four statutory factors case by case. Under this reading, courts treat fair use as an affirmative defense to be construed narrowly, weighting market-harm and commercial-purpose factors heavily, and requiring the defendant to justify departure from the default of exclusive control.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Actual working creators (as opposed to the corporate rights holders who often control their copyrights via contract) rarely appear as parties; the incentive-preservation rationale is asserted on their behalf by publishers and labels who hold the actual litigation rights, while the individual creators' own interest in being sampled, quoted, or built upon is not represented.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, individual_authors_and_musicians, excluded,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__creator_centric_reading, rights_holding_publishers).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__creator_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, litigable boundary around exclusive rights so that creators and investors can rely on a defined scope of control when deciding whether to invest in producing and distributing original works.
% TRANSFER_FUNCTION: Moves the burden of justifying reuse onto anyone who reuses copyrighted material without a license, and moves licensing revenue and litigation leverage toward rights holders and clearinghouses; the value of foregone transformative works and slowed cultural circulation is borne diffusely by downstream creators and audiences.
% ABSENT_VOICES: Individual working authors and musicians whose contracts assign copyright to publishers or labels are rarely parties to fair use litigation; the incentive argument is made in their name by rights holders who capture the resulting licensing revenue. Transformative users without institutional backing are also structurally absent from the appellate record that shapes the doctrine, because they settle or abandon claims before a favorable precedent can be set.
% DISAPPEARANCE_RATIONALE: If the narrow, creator-centric weighting of the four factors were replaced overnight by a strong presumption favoring transformative and non-market-substituting use, licensing revenue for reuse would fall, documentary and critical works would clear rights far less defensively, and a wave of previously-chilled remix, parody, and sampling work would proceed without pre-clearance — rights holders' negotiating leverage over downstream use would materially shrink.
% FOUNDING_PROBLEM: Copyright grants creators exclusive rights to incentivize the production of original works; fair use was built as a safety valve so that criticism, scholarship, news reporting, and other clearly beneficial uses would not be strangled by literal application of the exclusive right.
% FOUNDING_PROBLEM_CORROBORATION: Rights holders and licensing bodies attest that narrow construction is still necessary to prevent erosion of incentives to create. Independent legal scholars, library associations, and empirical studies of licensing markets (cited in amicus filings across circuits) attest that the incentive-erosion premise is not empirically supported for most transformative uses and that narrow construction functions primarily as a revenue-protection mechanism for intermediaries rather than a protection for individual creators.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68) because, under this reading, the default presumption runs against unlicensed use and the four factors are weighed to favor rights holders on market-harm grounds even where the challenged use is non-substitutive. Suppression (0.6) reflects the chilling effect: risk-averse creators and institutions (documentary filmmakers, museums, educators) self-censor rather than test the doctrine, and this suppression is a raw structural feature, not scaled by power or scope. Theater ratio is moderate-low (0.3) because the four-factor analysis performs real adjudicative work in litigated cases even as its default orientation favors incumbents administratively. Accessibility collapse (0.5) and resistance (0.55) are mid-range: alternatives to seeking a license (arguing fair use, waiting for public domain, using open-licensed substitutes) remain formally available, and there is real, organized resistance from library associations, open-culture advocates, and academic fair use clinics — this is not a mountain-grade collapse.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-holding publishers and content licensors sit near the full-beneficiary end: the constraint's narrow construction directly expands their licensing market and litigation leverage, and their exit options are effectively arbitrage (they can license, litigate, or forgo enforcement selectively). Transformative use creators, documentarians, and independent remix artists sit near the full-target end: they bear the risk and chilling cost of the narrow default, with constrained-to-trapped exit since the underlying material they need is often irreplaceable. Public domain commons is authored as a non-agent payer — it collects no rents but is nonetheless where the diffuse cost of doctrinal narrowness accumulates over time (slower cultural circulation, foreclosed derivative works).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (permit criticism, scholarship, and other clearly beneficial uses without gutting the exclusive right) is contested rather than dead: the underlying tension between incentive-preservation and downstream cultural production is real and ongoing, so this is not a pure zombie-mandate case. But the founding_problem_status = contested paired with disappearance_verdict = world_rearranges signals the mismatch the R5 interview is designed to surface: the narrow-construction default persists in significant part because it now generates concentrated licensing revenue for intermediaries who were never the intended beneficiaries of the incentive rationale (which was framed around individual creators, most of whom have since assigned their rights away by contract). Classifying this as tangled_rope rather than snare preserves the genuine coordination function (predictable boundaries around exclusive rights do encourage some investment) while still registering the asymmetric extraction running through the same structure — collapsing it to snare would erase the real coordination story judges and publishers use in good faith; classifying it as rope would erase the documented chilling effect and victim set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the four-factor test''s ''correct'' orientation genuinely narrow-construction-favoring, or is the creator-centric reading itself a contested interpretive choice that could equally be read as user-centric or transformative-dominant?',
    'Track circuit splits and Supreme Court fair use decisions (e.g., Google v. Oracle, Warhol v. Goldsmith) over time to see which reading the doctrine''s actual case law trajectory is converging toward, or whether it remains genuinely unsettled across jurisdictions.',
    'If courts are converging on the transformative-use or user-centric reading, this creator-centric constraint describes a doctrine in retreat rather than the settled state of the law, which would lower its practical epsilon over time even if this story''s authored value holds for the interval described.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the creator-centric reading is the operative doctrine or one contested reading among three live candidates.').

omega_variable(
    individual_creator_vs_corporate_beneficiary,
    'Does narrow fair use construction actually serve the incentive interests of individual creators, or does it primarily benefit corporate rights holders who have acquired those creators'' copyrights by contract?',
    'Empirical study of where fair-use-related licensing revenue and litigation settlements actually flow — to individual authors/musicians via royalty statements, or to publishers/labels/studios as institutional revenue.',
    'If revenue flows overwhelmingly to corporate intermediaries rather than individual creators, the incentive-theory justification for narrow construction is substantially undermined, strengthening the case that this reading functions as tangled_rope (real but attenuated coordination function) rather than a clean rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_creator_vs_corporate_beneficiary, empirical, 'Whether the beneficiary of narrow construction is the individual creator the doctrine claims to protect, or the corporate rights holder who has acquired the creator''s rights.').

omega_variable(
    chilling_effect_magnitude,
    'How much of the measured suppression (0.6) is genuine legal risk versus overcautious self-censorship driven by uncertainty rather than actual doctrinal hostility to transformative use?',
    'Survey documentary filmmakers, museums, and educators on actual takedown/litigation outcomes versus anticipated risk; compare to actual win rates for fair use defenses in litigated cases.',
    'If self-censorship substantially exceeds actual legal risk, the effective suppression is driven more by doctrinal unpredictability and asymmetric litigation costs than by the creator-centric reading itself being hostile on the merits — this would separate the coordination/predictability failure from the substantive-bias failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_magnitude, empirical, 'Whether measured suppression reflects the doctrine''s actual hostility to transformative use or downstream risk-aversion under uncertainty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1976, 0.2).
narrative_ontology:measurement(fair_tr_t1986, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1986, 0.22).
narrative_ontology:measurement(fair_tr_t1996, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1996, 0.25).
narrative_ontology:measurement(fair_tr_t2006, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2006, 0.27).
narrative_ontology:measurement(fair_tr_t2016, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2016, 0.29).
narrative_ontology:measurement(fair_tr_t2026, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1976, 0.5).
narrative_ontology:measurement(fair_be_t1986, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1986, 0.55).
narrative_ontology:measurement(fair_be_t1996, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1996, 0.6).
narrative_ontology:measurement(fair_be_t2006, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2006, 0.63).
narrative_ontology:measurement(fair_be_t2016, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2016, 0.66).
narrative_ontology:measurement(fair_be_t2026, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(fair_su_t1986, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1986, 0.46).
narrative_ontology:measurement(fair_su_t1996, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1996, 0.52).
narrative_ontology:measurement(fair_su_t2006, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2006, 0.56).
narrative_ontology:measurement(fair_su_t2016, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2016, 0.58).
narrative_ontology:measurement(fair_su_t2026, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2026, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__creator_centric_reading, 0.1).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, copyright_term_extension_doctrine).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposed from the natural-language label 'the fair use four-factor test.' Each sibling reading is authored as a separate constraint with its own epsilon, beneficiary/victim set, and cs_structure axioms, per the ε-invariance principle: the creator-centric reading (this file) authors ε=0.68 with rights holders as primary beneficiary; the user-centric reading is expected to author a substantially lower ε with an inverted beneficiary/victim structure; the transformative-use reading is expected to author an intermediate ε favoring transformative creators specifically. All three are linked via affects_constraints so contamination/coupling analysis can trace how doctrinal shifts in one reading's dominance affect the practical operation described by the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
