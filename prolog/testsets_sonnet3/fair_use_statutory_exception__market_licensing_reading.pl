% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Market-Licensing Reading of Fair Use's Fourth Factor (Effect on the Market)
 *   domain: intellectual_property/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested fair-use kernel: the
 *   market-licensing reading of the fourth statutory factor (effect on the
 *   market). Under this reading, the mere theoretical existence of a
 *   licensing mechanism for a given use is treated as dispositive evidence
 *   that the use harms the market for licensed uses — regardless of whether
 *   any actual market transaction was foreclosed. As licensing infrastructure
 *   (collective rights organizations, microlicensing platforms, and now
 *   AI-training marketplaces) has expanded since 1976, the space in which no
 *   licensing mechanism is even theoretically available has shrunk toward
 *   zero, and with it the practical scope of fair use under this reading.
 *   This is not a story about fair use generally — the sibling readings
 *   (narrow_defense_reading, transformative_right_reading) are separate
 *   constraints with separate epsilon values, because they instantiate
 *   structurally distinct claims about what the fourth factor requires. Do
 *   not average across them.
 *
 * KEY AGENTS:
 *   - rightsholder_licensing_entities: institutional agenda-setter, litigates and administers the licensing schemes the doctrine protects
 *   - collective_rights_organizations: organized beneficiary, revenue depends on doctrinal narrowing
 *   - independent_researchers, documentary_filmmakers, commentary_and_criticism_publishers, archival_and_preservation_institutions, algorithmic_training_data_users: payers, bear the collapse of practical fair-use availability
 *   - courts_adjudicating_factor_four: institutional observer, applies the reading as controlling law
 *   - future_licensable_use_public: excluded, no standing to defend the systemic public interest in a workable doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.89).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.72).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Market-Licensing Reading of Fair Use's Fourth Factor (Effect on the Market)").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "intellectual_property/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, '67b707ae-f3cd-429a-8331-dc88c1ba76a5').
narrative_ontology:cs_kernel_codification('67b707ae-f3cd-429a-8331-dc88c1ba76a5', fixed_text).
narrative_ontology:cs_authority_grounding('67b707ae-f3cd-429a-8331-dc88c1ba76a5', lineage).
narrative_ontology:cs_interpretation_layer_present('67b707ae-f3cd-429a-8331-dc88c1ba76a5').
narrative_ontology:cs_reading_relation('67b707ae-f3cd-429a-8331-dc88c1ba76a5', fair_use_statutory_exception__narrow_defense_reading, influences).
narrative_ontology:cs_reading_relation('67b707ae-f3cd-429a-8331-dc88c1ba76a5', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_axiom('67b707ae-f3cd-429a-8331-dc88c1ba76a5', foundational, licensability_itself_constitutes_market_harm).
narrative_ontology:cs_axiom_status(licensability_itself_constitutes_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('67b707ae-f3cd-429a-8331-dc88c1ba76a5', licensability_itself_constitutes_market_harm, conventional).
narrative_ontology:cs_axiom('67b707ae-f3cd-429a-8331-dc88c1ba76a5', secondary, transformativeness_cannot_override_demonstrated_licensing_potential).
narrative_ontology:cs_axiom_status(transformativeness_cannot_override_demonstrated_licensing_potential, holdable).
narrative_ontology:cs_axiom_grounding('67b707ae-f3cd-429a-8331-dc88c1ba76a5', transformativeness_cannot_override_demonstrated_licensing_potential, instrumental).
narrative_ontology:cs_reference_frame('67b707ae-f3cd-429a-8331-dc88c1ba76a5', market_failure_justification_for_fair_use).
narrative_ontology:cs_drift_state('67b707ae-f3cd-429a-8331-dc88c1ba76a5', post_licensing_infrastructure_proliferation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('67b707ae-f3cd-429a-8331-dc88c1ba76a5', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, rightsholder_licensing_entities).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, collective_rights_organizations).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, content_aggregator_platforms).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, independent_researchers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, commentary_and_criticism_publishers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, archival_and_preservation_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, algorithmic_training_data_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, content_aggregator_platforms).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, market_harm_is_dispositive_factor_four).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, licensability_forecloses_fair_use_availability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major publishers, studios, and rights-holding corporations litigate factor-four aggressively, arguing that any use that COULD be licensed automatically damages the market for licensing that use — regardless of whether a license was ever actually sought or a market ever actually existed for that specific use. They fund the litigation that establishes this reading as controlling precedent and administer licensing schemes that expand to fill whatever space the doctrine vacates.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, rightsholder_licensing_entities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, rightsholder_licensing_entities, beneficiary).

% Collecting societies and licensing intermediaries benefit directly from a reading that treats hypothetical licensability as market harm: every use that might otherwise be free becomes a use that must clear their licensing desk. Their institutional revenue depends on the doctrine narrowing to leave the largest possible territory to licensed transactions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, collective_rights_organizations, beneficiary,
    organized, generational, arbitrage, national).

% Large platforms with existing licensing relationships benefit from a reading that disadvantages smaller unlicensed competitors, but also pay when the same logic is turned against their own training-data or aggregation practices. They can absorb licensing costs that would exclude smaller entrants entirely — their exit option is negotiated licensing, not fair use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, content_aggregator_platforms, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, content_aggregator_platforms, payer).

% Scholars quoting, analyzing, or reproducing copyrighted material for criticism or research face a doctrine that treats the mere theoretical existence of a licensing market — even one they could never afford to access — as dispositive evidence of market harm. They cannot litigate to test the boundary and cannot afford the license; their actual choice is self-censorship or infringement risk.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, independent_researchers, payer,
    powerless, biographical, trapped, national).

% Filmmakers incorporating archival footage, music, or news clips for factual or critical purposes confront clearance requirements that did not previously exist as a practical matter, because errors-and-omissions insurers and distributors now require licenses for any use a court might deem 'licensable' — collapsing the practical space for fair use to near zero regardless of the film's critical or transformative purpose.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers, payer,
    moderate, biographical, constrained, national).

% Reviewers, critics, and journalists reproducing excerpts for commentary face the same market-substitution logic applied to snippets and thumbnails, on the theory that a licensing market for excerpts could in principle be built. Their exit is heavy editorial caution or reliance on publisher indemnification, which smaller outlets lack.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, commentary_and_criticism_publishers, payer,
    moderate, biographical, constrained, national).

% Libraries and archives digitizing orphan works or out-of-print material face the presumption that a market could theoretically be reconstituted for any work, which forecloses the fair-use rationale historically used to justify preservation copying. They cannot readily license works whose rightsholders cannot be located, yet the mere theoretical licensability defeats the defense.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, archival_and_preservation_institutions, payer,
    moderate, civilizational, trapped, national).

% Firms and researchers training models on copyrighted corpora face litigation asserting that any conceivable licensing market for training data — even one that did not exist before the litigation invented it — establishes market harm under this reading, regardless of whether the individual use is transformative in function.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, algorithmic_training_data_users, payer,
    organized, biographical, constrained, global).

% Judges applying the fourth statutory factor must decide whether hypothetical licensability alone satisfies the market-harm inquiry. Under this reading, courts treat licensing feasibility as near-conclusive, effectively converting the multi-factor balancing test into a single dispositive question — did a licensing mechanism exist or could one be built.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts_adjudicating_factor_four, observer,
    institutional, generational, analytical, national).

% The diffuse public that benefits from unlicensed criticism, scholarship, and preservation has no seat in the litigation that sets this precedent; their interest in an accessible cultural and information commons is not represented by any party with standing to argue it, because the doctrine as construed treats them as beneficiaries of a market that has not yet been built rather than as parties with a present interest to defend.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, future_licensable_use_public, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__market_licensing_reading, rightsholder_licensing_entities).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__market_licensing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a functioning market for licensed reuse by giving rightsholders a predictable, litigable basis to charge for any use with commercial substitutability — reducing free-riding on works whose value depends on controlled distribution.
% TRANSFER_FUNCTION: Moves the practical availability of fair use away from researchers, critics, archivists, and toolmakers and toward rightsholders and the licensing intermediaries who administer clearance, by converting the fourth factor from an empirical inquiry into actual market harm into a near-automatic finding whenever a licensing mechanism is conceivable.
% ABSENT_VOICES: The diffuse public whose access to criticism, scholarship, and preserved culture depends on a workable fair-use doctrine has no party representing it in the litigation that sets this reading as precedent — rightsholders and defendants argue their individual stakes, but no one argues the systemic cost of doctrinal collapse.
% DISAPPEARANCE_RATIONALE: If this reading of factor four were abandoned — if courts required proof of actual, not merely hypothetical, market substitution — large swaths of currently self-censored or license-burdened uses (commentary excerpts, archival digitization, documentary clips, some training-data uses) would proceed without clearance, licensing intermediary revenue would fall, and fair use would function as a meaningful defense rather than a near-null category for any licensable work.
% FOUNDING_PROBLEM: The fourth statutory factor was built to ask whether a challenged use actually harms the rightsholder's real or reasonably foreseeable market for the work — distinguishing genuine market substitution from uses that leave the licensing market untouched.
% FOUNDING_PROBLEM_CORROBORATION: Rightsholder litigants and licensing organizations attest the reading correctly protects nascent and derivative licensing markets from erosion. Independent legal scholars, library associations, and public-interest amici outside the beneficiary set attest the reading has been extended well past its founding purpose — treating hypothetical licensability as dispositive collapses the multi-factor test into a single question and forecloses uses the statute was written to protect.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.89, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored very high (0.89) because under this reading, licensability alone — not actual demonstrated harm — satisfies the market-harm inquiry, which as licensing infrastructure has proliferated has driven the doctrine toward near-null scope for any licensable work. Suppression (0.72) reflects the active enforcement machinery: litigation strategy, insurer clearance requirements, and platform takedown practices that operationalize the reading against uses that were never actually offered a license. Theater ratio (0.42) captures that courts still recite the four-factor balancing test even as, under this reading, factor four alone controls the outcome in practice — a performative multi-factor analysis wrapped around what functions as a single dispositive test. Accessibility collapse is authored high (0.81) because once a use is understood to be theoretically licensable, the fair-use alternative is understood by counsel and insurers to be foreclosed as a practical matter, even where litigation might technically still be available. Resistance (0.67) reflects the active pushback from research, library, and digital-rights communities documenting the doctrinal drift.
 *
 * PERSPECTIVAL GAP:
 *   From the rightsholder/licensing-entity seat, this reading is coordination: it protects a functioning market from free-riding and gives predictable value to the licensing relationships the industry has built. From the payer seats — researchers, filmmakers, archivists, critics — the identical structure operates as extraction: the same fourth-factor analysis that once asked whether THIS use actually harmed a real market now treats the mere buildability of a market as sufficient, which forecloses uses the statute's coordination function was never meant to reach. The engine computes these as different seat-level classifications from the same structural data; the divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Rightsholder licensing entities and collective rights organizations sit near the full-beneficiary end: they collect licensing revenue whose scope expands directly with the doctrine's contraction, and their exit options (arbitrage — they can license through whichever mechanism yields the most revenue) reflect structural control rather than constraint. Payer groups — researchers, filmmakers, critics, archivists, training-data users — sit near the full-target end: the reading extracts practical fair-use availability from them without their consent or compensation, and their exit options range from trapped (archivists facing orphan works) to constrained (filmmakers who can negotiate clearance but at a cost that excludes marginal projects). Content aggregator platforms are dual-positioned: large enough to negotiate licenses (beneficiary via advantage over smaller competitors) but also targets when the same market-harm logic is turned against their own aggregation or training practices.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding function of factor four — distinguishing uses that actually substitute for a licensing transaction from uses that do not — remains a live problem in principle (some uses genuinely do displace real licensing revenue). But this reading has drifted the doctrine from asking an empirical question (did this use displace an actual sale) to asking a categorical one (could this use theoretically have been licensed), which is nearly always true given modern licensing infrastructure. Classifying this as tangled_rope rather than snare preserves the genuine coordination kernel (protecting real, demonstrable licensing markets is a legitimate function) while flagging the asymmetric extraction now riding on it (foreclosing fair use for uses with no actual market impact). A pure snare reading would miss that some fraction of the doctrine's operation still serves its original coordination function; a pure rope reading would miss that the reading, as construed, now forecloses the class of uses fair use was written to protect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hypothetical_vs_actual_market_harm,
    'Should factor-four market harm be assessed by whether a licensing mechanism theoretically COULD exist for a use, or whether the specific use actually displaced a demonstrable, existing (or reasonably foreseeable and already-monetized) market?',
    'Comparative doctrinal analysis across circuits and over time: track whether courts require plaintiffs to show an actual foregone licensing transaction versus accepting bare licensability as sufficient; a growing acceptance of bare licensability without evidence of actual displaced revenue would corroborate this reading''s drift toward near-total doctrinal collapse.',
    'If courts require actual demonstrated harm, this reading''s extraction ceiling falls substantially and fair use regains functional scope for many currently-foreclosed uses. If bare licensability continues to suffice, the doctrine approaches null for any work with any conceivable licensing infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypothetical_vs_actual_market_harm, empirical, 'Whether factor four requires proof of actual market substitution or accepts hypothetical licensability alone.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the market_licensing_reading the doctrinally dominant reading of factor four, or one of three genuinely coexisting readings with no single controlling interpretation across jurisdictions?',
    'Circuit-by-circuit doctrinal survey comparing outcomes under narrow_defense_reading, transformative_right_reading, and market_licensing_reading in comparable fact patterns (commentary, archival, and AI-training cases) to determine whether one reading has become effectively controlling or whether courts continue to apply inconsistent framings.',
    'If market_licensing_reading has become effectively dominant nationally, its extraction and suppression figures understate the doctrine''s actual collapse; if the readings genuinely coexist without one controlling, the practical ceiling on fair use varies by venue and this story''s ε applies only where this reading is controlling law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading is dominant, coexisting, or venue-contingent relative to its siblings.').

omega_variable(
    ai_training_data_precedent_direction,
    'Will emerging litigation over AI training data entrench the market_licensing_reading (by accepting that a training-data licensing market''s mere buildability defeats fair use) or push courts back toward requiring actual demonstrated market harm?',
    'Track outcomes and reasoning in pending and near-term AI-training copyright litigation; a wave of decisions accepting ''a training license market could be built'' as dispositive would substantially entrench this reading across a new and economically significant domain.',
    'Entrenchment via AI-training precedent would push this reading''s extraction toward its practical ceiling across a broad new category of use; rejection would create a doctrinal counter-current available to challenge the reading''s expansion into other domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_training_data_precedent_direction, empirical, 'Whether current AI-training litigation will entrench or check this reading''s expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1976, 0.15).
narrative_ontology:measurement(fair_tr_t1994, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1994, 0.2).
narrative_ontology:measurement(fair_tr_t2005, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(fair_tr_t2013, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2013, 0.34).
narrative_ontology:measurement(fair_tr_t2020, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(fair_tr_t2026, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1976, 0.35).
narrative_ontology:measurement(fair_be_t1994, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1994, 0.48).
narrative_ontology:measurement(fair_be_t2005, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement(fair_be_t2013, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2013, 0.74).
narrative_ontology:measurement(fair_be_t2020, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2020, 0.83).
narrative_ontology:measurement(fair_be_t2026, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2026, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1976, 0.3).
narrative_ontology:measurement(fair_su_t1994, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1994, 0.42).
narrative_ontology:measurement(fair_su_t2005, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2005, 0.53).
narrative_ontology:measurement(fair_su_t2013, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2013, 0.62).
narrative_ontology:measurement(fair_su_t2020, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(fair_su_t2026, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__market_licensing_reading, 0.1).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__transformative_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the fair_use_statutory_exception kernel, decomposed per the epsilon-invariance principle because the colloquial label 'fair use' conflates structurally distinct claims about what the fourth statutory factor requires. market_licensing_reading (this story) authors the highest epsilon of the three, because it treats bare licensability as dispositive of market harm, collapsing fair use to near-null scope. narrow_defense_reading authors a moderate epsilon, requiring actual demonstrated market substitution but still construing the defense narrowly against the copyright property right. transformative_right_reading authors the lowest epsilon, treating transformativeness as capable of overriding market-harm concerns and functioning closer to a genuine coordination rope enabling cultural production. All three share the same statutory text and factual predicate (a use is challenged, factor four is applied) but diverge in what factor four is held to require — they are linked via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
