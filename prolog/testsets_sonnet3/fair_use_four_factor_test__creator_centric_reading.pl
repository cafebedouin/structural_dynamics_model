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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   four-factor test (17 U.S.C. §107): fair use as a narrow,
 *   judicially-cabined exception to an otherwise exclusive property right,
 *   where the four factors are weighed with factor four (effect on the
 *   market) treated as dominant, and 'market' construed broadly to include
 *   speculative licensing markets the rights holder has never actually
 *   entered. Under this reading the doctrine functions primarily to preserve
 *   creator incentive structures and licensing revenue, with unauthorized use
 *   presumptively disfavored absent a strong showing on the other factors.
 *   This is emphatically NOT the same constraint as the transformative-use
 *   reading (where transformativeness dominates and market harm is
 *   subordinated once new meaning is added) or the user-centric reading (fair
 *   use as an affirmative right protecting public access and downstream
 *   cultural production) — those are sibling constraints with their own
 *   epsilon, beneficiary sets, and victim sets, linked via
 *   network.affects_constraints. Conflating the three into one 'fair use'
 *   story would violate epsilon-invariance: the creator-centric reading has
 *   meaningfully higher extraction and a materially different victim set
 *   (transformative users, documentarians, the commons) than either sibling.
 *
 * KEY AGENTS:
 *   - established_rights_holders: primary beneficiary (institutional/arbitrage) — captures licensing revenue from a doctrine construed to favor market-harm findings
 *   - publishing_and_media_conglomerates: beneficiary (institutional/arbitrage) — funds precedent-setting litigation narrowing the doctrine
 *   - transformative_use_creators: primary target (moderate/constrained) — bears litigation risk and chilling effect
 *   - documentarians_and_critics: target (moderate/constrained) — self-censors sourcing to avoid market-harm exposure
 *   - independent_remix_artists: most exposed target (powerless/trapped) — cannot litigate, so takedown notices function as de facto adjudication
 *   - public_domain_commons: diffuse non-agent victim (powerless/trapped) — erodes functionally even without statutory change
 *   - federal_courts: agenda-setter (institutional/analytical) — administers and effectively sets the narrow-versus-broad tilt through case law
 *   - legislators: excluded (institutional/analytical) — wrote an open standard that courts have since directionally narrowed without further legislative input
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.68).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.61).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test — Creator-Centric (Narrow Exception) Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '9f400c71-815d-4e16-bdef-caa0c98c2652').
narrative_ontology:cs_kernel_codification('9f400c71-815d-4e16-bdef-caa0c98c2652', formalized).
narrative_ontology:cs_authority_grounding('9f400c71-815d-4e16-bdef-caa0c98c2652', lineage).
narrative_ontology:cs_interpretation_layer_present('9f400c71-815d-4e16-bdef-caa0c98c2652').
narrative_ontology:cs_reading_relation('9f400c71-815d-4e16-bdef-caa0c98c2652', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f400c71-815d-4e16-bdef-caa0c98c2652', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('9f400c71-815d-4e16-bdef-caa0c98c2652', foundational, copyright_is_primary_exclusive_entitlement).
narrative_ontology:cs_axiom_status(copyright_is_primary_exclusive_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('9f400c71-815d-4e16-bdef-caa0c98c2652', copyright_is_primary_exclusive_entitlement, conventional).
narrative_ontology:cs_axiom('9f400c71-815d-4e16-bdef-caa0c98c2652', foundational, speculative_licensing_markets_constitute_cognizable_harm).
narrative_ontology:cs_axiom_status(speculative_licensing_markets_constitute_cognizable_harm, holdable).
narrative_ontology:cs_axiom_grounding('9f400c71-815d-4e16-bdef-caa0c98c2652', speculative_licensing_markets_constitute_cognizable_harm, instrumental).
narrative_ontology:cs_reference_frame('9f400c71-815d-4e16-bdef-caa0c98c2652', market_harm_dominant_balancing).
narrative_ontology:cs_drift_state('9f400c71-815d-4e16-bdef-caa0c98c2652', post_warhol_goldsmith_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9f400c71-815d-4e16-bdef-caa0c98c2652', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, established_rights_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, publishing_and_media_conglomerates).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_use_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, documentarians_and_critics).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_commons).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, independent_remix_artists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds copyright in valuable back catalogs and current works; under this reading, fair use is construed narrowly so that any use touching the market for licensing (however speculative) weighs against the user. Lobbies for and litigates to keep the four factors tilted toward market-harm analysis, and captures licensing revenue that would otherwise be unpaid fair use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, established_rights_holders, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__creator_centric_reading, established_rights_holders, agenda_setter).

% Aggregates rights across large catalogs and funds litigation establishing precedent that narrows fair use in practice. Benefits directly from a doctrine that treats any unauthorized derivative market as cognizable harm under factor four, regardless of whether the original creator would have entered that market.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, publishing_and_media_conglomerates, beneficiary,
    institutional, generational, arbitrage, global).

% Collecting societies and licensing agencies whose business model depends on uses being classified as infringing-unless-licensed. A narrow fair use doctrine expands the set of uses that must clear their licensing desks, generating fee revenue.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, licensing_intermediaries, beneficiary,
    organized, biographical, mobile, national).

% Artists, remixers, and secondary creators who build new work from existing copyrighted material. Under the narrow reading, they bear the legal risk and cost of proving their use survives a market-harm-weighted four-factor test, and many self-censor or pay for licenses they might not legally need under a more permissive reading. Litigation is expensive; settling or not creating is often cheaper than winning on the merits.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_use_creators, payer,
    moderate, biographical, constrained, national).

% Rely on quoting, excerpting, and juxtaposing copyrighted footage or text to make critical or historical arguments. The narrow reading's weight on potential market substitution chills use of contemporary or commercially active material, pushing documentarians toward older, cleared, or blander sourcing than the argument requires.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, documentarians_and_critics, payer,
    moderate, biographical, constrained, national).

% The stock of culturally available material and the practices of free cultural borrowing. A narrowly construed exception shrinks the effective commons even where formal copyright terms are unchanged, because works are treated as functionally locked absent a license — the commons erodes without any change to the statute.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_commons, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(fair_use_four_factor_test__creator_centric_reading, public_domain_commons).

% Individual creators without institutional legal support who sample, remix, or repost. Cannot afford to litigate a fair use defense to judgment, so takedown notices and cease-and-desist letters function as de facto adjudication regardless of the doctrine's formal content — the narrow reading raises the credible threat behind every notice.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, independent_remix_artists, payer,
    powerless, biographical, trapped, global).

% Apply and articulate the four-factor test case by case, weighing purpose, nature, amount, and market effect. Under this reading, courts treat factor four (market harm) as the most important and construe potential licensing markets broadly, effectively administering the doctrine as a narrow exception rather than a robust affirmative right.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Wrote Section 107 as an open, non-exhaustive four-factor standard without directional instruction on which factor should dominate. Largely absent from the doctrine's actual operation since courts, not Congress, have determined the narrow-versus-broad tilt through case law; legislative history is contested and rarely revisited.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, legislators, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__creator_centric_reading, established_rights_holders).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__creator_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared adjudicative framework so courts, rights holders, and users can predict which unauthorized uses of copyrighted material will and will not be excused, avoiding case-by-case chaos and preserving incentives for creators to invest in producing original work.
% TRANSFER_FUNCTION: Under this reading, the doctrine moves the burden and cost of legal uncertainty from rights holders onto downstream users: licensing revenue and market control flow to rights holders and their intermediaries, while transformative users bear litigation risk, self-censorship costs, and licensing fees for uses that a more permissive reading would excuse outright.
% ABSENT_VOICES: Legislators who wrote an open, non-hierarchical four-factor standard are effectively absent from how it is actually applied; independent remix artists and members of the public who rely on cultural borrowing have no seat in infringement litigation they cannot afford to bring or defend, and their preferences are represented, if at all, only by amici.
% DISAPPEARANCE_RATIONALE: If the narrow, market-harm-dominant construction of fair use disappeared and were replaced by a more permissive default, licensing intermediaries would lose substantial revenue, rights holders would need to affirmatively prove concrete market harm rather than relying on speculative licensing markets, and a large volume of currently-chilled documentary, critical, and remix work would proceed without clearance — the cultural production landscape and licensing economy would visibly shift.
% FOUNDING_PROBLEM: Copyright grants exclusive rights to encourage creation, but rigid exclusivity with no exceptions would prevent criticism, scholarship, news reporting, and transformative culture-building that themselves depend on engaging with existing works — the four-factor test was built to let courts distinguish legitimate free uses from piracy without legislating an exhaustive list.
% FOUNDING_PROBLEM_CORROBORATION: Rights holders and licensing intermediaries attest the founding problem remains live: unauthorized commercial exploitation of protected works still threatens creator incentives and must be checked. Independent legal scholars, documentary filmmakers' guilds, and library associations — outside the beneficiary set — attest that courts' market-harm-dominant application has drifted from the statute's open text toward a de facto presumption against unauthorized use, turning an exception meant to protect discourse into a permission gate that primarily protects rights holders' licensing markets.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is authored at 0.68 by interval end because, under this reading, the doctrine's operation systematically transfers value from unauthorized users to rights holders by treating speculative licensing markets as cognizable harm — a construction that expands the effective scope of the property right well beyond what a plain reading of an 'exception' would suggest. Suppression (0.61) reflects that persistence depends on active enforcement: litigation threats, takedown regimes, and the in terrorem effect of statutory damages, not on voluntary participant preference. Theater ratio is moderate-low (0.30): the four-factor analysis is a real adjudicative exercise, not empty ritual, but a growing share of its function has shifted from genuinely balancing competing interests toward reliably validating rights-holder market claims. Accessibility collapse (0.58) and resistance (0.55) are mid-range because formal doctrinal alternatives (the other two readings) remain live and contested in courts and legislatures — this is not a settled natural fact but an actively defended construction.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights holders, media conglomerates, and licensing intermediaries sit near the beneficiary end: they collect licensing revenue and litigation leverage that a narrower construction of the market-harm factor generates, and their institutional power plus arbitrage-grade exit (they can choose which cases to bring, which markets to license) place them structurally at low d. Transformative creators, documentarians, and independent remix artists sit near the target end: they bear the cost of legal uncertainty, cannot easily exit the doctrine's reach (their work requires engaging existing culture), and independent artists in particular are trapped by the asymmetric cost of litigation versus compliance. The public domain commons is authored as a non-agent (agent: false) payer — it collects no rents and cannot act, but its erosion is a real structural cost the doctrine imposes, tracked for narrative completeness without feeding directionality as if it were a collecting party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing legitimate transformative and critical engagement from piracy — remains genuinely live; this is not a pure zombie mandate. But the corroboration record shows a status drift: the doctrine's application has moved from balancing free expression interests against creator incentives toward a presumption that treats any use touching a plausible licensing market as harmful, which serves rights holders' revenue interests more than the founding purpose of preserving room for criticism and transformative culture. Classifying this as tangled_rope (not snare) preserves the genuine coordination function — courts do need SOME predictable framework — while registering that the framework, as applied under this reading, has been captured to extract beyond what the coordination problem requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indeterminacy_of_market_harm_factor,
    'Is the dominant weight given to speculative licensing-market harm (factor four) required by the statutory text and precedent, or is it a judicially constructed emphasis that could as easily have settled on transformativeness or public-access grounds?',
    'Comparative doctrinal history across circuits and over time: track whether factor-four dominance was present from early post-1976 Act case law or emerged later as a contingent judicial choice, and whether circuits diverge in weighting.',
    'If factor-four dominance is a contingent judicial construction rather than a textual requirement, the creator-centric reading is more clearly one policy choice among several defensible readings rather than the doctrine''s natural or required form — strengthening the case that this reading''s high extraction is a constructed rather than inevitable feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_indeterminacy_of_market_harm_factor, conceptual, 'Whether market-harm dominance is doctrinally required or a contingent judicial emphasis.').

omega_variable(
    commons_erosion_measurement,
    'How much does the functional public domain commons actually shrink under narrow-reading enforcement, independent of the formal copyright term?',
    'Empirical study of takedown notice volume, settlement rates, and self-reported self-censorship among documentarians and remix artists, compared across jurisdictions or time periods with differing doctrinal emphasis.',
    'A large measured chilling effect would support treating public_domain_commons erosion as a substantial, quantifiable victim cost rather than a diffuse rhetorical claim; a small effect would suggest the narrow reading''s practical extraction is lower than the doctrinal framing implies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_erosion_measurement, empirical, 'Magnitude of actual chilling effect on cultural commons from narrow-reading enforcement.').

omega_variable(
    kernel_reading_dominance_over_time,
    'Is the creator-centric reading currently the operative default in courts, or has the transformative_use_reading (post-Campbell v. Acuff-Rose, and especially post-Google v. Oracle and the Warhol v. Goldsmith line) substantially displaced it as the practical governing framework?',
    'Track citation patterns and outcome rates across circuit and Supreme Court fair use decisions to determine which reading''s core premise (market-harm dominance vs. transformativeness dominance) actually predicts case outcomes in the current era.',
    'If transformative_use_reading has become dominant in practice, this creator-centric story describes a reading under retreat rather than the current operative constraint — which would not change this story''s own epsilon (each reading keeps its own value) but would materially affect which reading should be treated as the primary real-world constraint in any composite analysis of ''fair use'' as a whole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dominance_over_time, empirical, 'Whether the creator-centric reading remains dominant or has been substantially displaced by the transformative-use reading in current case law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fair_tr_t8, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(fair_tr_t16, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(fair_tr_t24, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(fair_tr_t32, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(fair_tr_t40, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fair_be_t8, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(fair_be_t16, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(fair_be_t24, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(fair_be_t32, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(fair_be_t40, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fair_su_t8, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(fair_su_t16, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(fair_su_t24, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(fair_su_t32, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(fair_su_t40, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__creator_centric_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).

% DUAL FORMULATION NOTE:
% The fair_use_four_factor_test kernel decomposes into three sibling constraint stories, each instantiating a distinct structural claim about how the four factors should be weighed: this story (creator_centric_reading, high epsilon ~0.68, rights holders as primary beneficiary, tangled_rope), fair_use_four_factor_test__transformative_use_reading (transformativeness-dominant, moderate epsilon, more balanced beneficiary structure), and fair_use_four_factor_test__user_centric_reading (fair use as affirmative right, low epsilon, public/user-centered beneficiary structure, likely rope). These are not three measurements of one constraint but three structurally distinct constraints sharing a statutory kernel — courts oscillate between them, and which reading dominates in a given era or circuit is itself an empirical and doctrinal question (see the reading_indeterminacy and kernel_reading_dominance omegas).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
