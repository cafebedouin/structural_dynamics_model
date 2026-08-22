% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Fair Use Four-Factor Test (Creator-Centric Reading)
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint is ONE READING of the fair use four-factor test kernel
 *   (17 U.S.C. § 107). The creator-centric reading treats fair use as a
 *   narrow exception to copyright property right, with the four factors
 *   weighted heavily toward protecting creator incentives and market
 *   interests of copyright holders. This reading is instantiated through
 *   judicial opinions (Sony Corp. v. Universal City Studios, Harper & Row v.
 *   Nation Enterprises, Campbell v. Acuff-Rose Music), legislative testimony
 *   by author organizations, and copyright-holding-institution amicus briefs.
 *   The kernel itself is the statutory language and doctrine; the reading is
 *   the weighting and interpretive approach applied to the four factors.
 *   Under this reading, transformativeness is one factor among four, market
 *   harm is dispositive, and derivative-use licensing revenue is protected as
 *   a primary beneficiary interest.
 *
 * KEY AGENTS:
 *   - copyright_holders: institutional beneficiary; controls licensing revenue; leverages the creator-centric reading to narrow fair use and force licensing deals
 *   - original_authors: powerful agenda-setter; articulates the creator-incentive doctrine; shapes judicial appointments and legislative proposals
 *   - transformative_users: moderate-power payer; constrained exit; face licensing costs and cease-and-desist letters under the narrow exception
 *   - scholarly_community: organized payer; constrained exit; limited scope for quotation and textual analysis under market-harm weighting
 *   - courts: institutional agenda-setter; adjudicate the four-factor test; adopt the creator-centric reading in precedent
 *   - public_domain_advocates: moderate-power payer; excluded from doctrine-setting; bear opportunity cost of restricted access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.78).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.71).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, 'f892ff10-08fa-4fd4-acba-f1647529d7d1').
narrative_ontology:cs_kernel_codification('f892ff10-08fa-4fd4-acba-f1647529d7d1', fixed_text).
narrative_ontology:cs_authority_grounding('f892ff10-08fa-4fd4-acba-f1647529d7d1', lineage).
narrative_ontology:cs_interpretation_layer_present('f892ff10-08fa-4fd4-acba-f1647529d7d1').
narrative_ontology:cs_reading_relation('f892ff10-08fa-4fd4-acba-f1647529d7d1', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_reading_relation('f892ff10-08fa-4fd4-acba-f1647529d7d1', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('f892ff10-08fa-4fd4-acba-f1647529d7d1', foundational, creator_incentives_primacy).
narrative_ontology:cs_axiom_status(creator_incentives_primacy, holdable).
narrative_ontology:cs_axiom_grounding('f892ff10-08fa-4fd4-acba-f1647529d7d1', creator_incentives_primacy, instrumental).
narrative_ontology:cs_axiom('f892ff10-08fa-4fd4-acba-f1647529d7d1', foundational, property_right_default_position).
narrative_ontology:cs_axiom_status(property_right_default_position, holdable).
narrative_ontology:cs_axiom_grounding('f892ff10-08fa-4fd4-acba-f1647529d7d1', property_right_default_position, deontological).
narrative_ontology:cs_reference_frame('f892ff10-08fa-4fd4-acba-f1647529d7d1', creator_incentive_doctrine).
narrative_ontology:cs_drift_state('f892ff10-08fa-4fd4-acba-f1647529d7d1', contemporary_digital_culture, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f892ff10-08fa-4fd4-acba-f1647529d7d1', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, original_authors).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, scholarly_community).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control access to copyrighted works and collect licensing fees. The creator-centric reading of fair use protects their ability to monetize derivatives and adaptations by narrow-construing the exception. They benefit from high barriers to fair use claims because it forces downstream users into licensing negotiations.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Set the legal doctrine through judicial appointments, briefs filed in test cases, and legislative influence. The creator-centric reading frames fair use as a narrow carve-out from their property right, not as an independent user right. They articulate the founding problem (need for creator incentives) and shape how the four factors are weighted.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, original_authors, agenda_setter,
    powerful, generational, mobile, global).

% Seek to create new works that incorporate, sample, remix, or critique existing copyrighted material. Under the creator-centric reading, the four-factor test weights against them: any market potential for the derivative work counts against fair use, and transformativeness alone is insufficient. They face licensing costs or cease-and-desist letters.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_users, payer,
    moderate, biographical, constrained, global).

% Relies on fair use to quote, excerpt, and analyze copyrighted works in academic texts. The creator-centric reading narrows the scope for scholarly quotation by raising the threshold for what counts as transformative analysis and by weighting commercial potential of the scholarly work itself against fair use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, scholarly_community, payer,
    organized, biographical, constrained, global).

% Seek to expand access to cultural works and shorten copyright terms. The creator-centric reading treats fair use as a narrow exception and the copyright term as the default, which makes public-domain-expansion efforts against institutional current. They bear the cost of restricted access during the extended copyright term.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates, payer,
    moderate, generational, constrained, global).

% Adjudicate fair use claims using the four-factor test. Under the creator-centric reading, courts weight the doctrine toward protecting creator incentives, sometimes treating market harm as dispositive and transformativeness as a weak counterweight. Their rulings establish which uses survive dismissal.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Include libraries, archive institutions, and internet preservation organizations that would benefit from broader fair use to preserve and provide access to cultural material. They are structurally excluded from the doctrine-setting conversation and must operate within constraints defined by the creator-centric reading.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_benefit_intermediaries, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__creator_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances creator incentives (rewarding original authorship through property rights) with public access to knowledge (via narrow fair use carve-out). A unified copyright system with a single exception doctrine, rather than separate schemes for licensing and public use.
% TRANSFER_FUNCTION: Transfers licensing revenue from downstream users (transformative creators, scholars, archivists) to copyright holders by narrowing the exception that would otherwise permit use without permission. The four-factor test is the mechanism; its creator-centric weight determines the transfer size.
% ABSENT_VOICES: Digital artists, remix communities, and public archives are excluded from the doctrine-development process. They would argue for broad transformativeness as an exception to market-harm analysis, but do not have standing in copyright-reform litigation and are not represented in judicial appointment processes that shape the reading.
% DISAPPEARANCE_RATIONALE: If the creator-centric four-factor test disappeared (replaced by a user-centric or transformative-use-centric reading), licensing practices would shift sharply: more uses would be permitted without licensing, downstream creators would reduce licensing expenditure, and the revenue flow to copyright holders would decrease. Scholarly citation practices would expand, remix culture would face fewer injunctions, and archive institutions would operate with less legal risk.
% FOUNDING_PROBLEM: Original authors need incentives to create; copyright property rights provide those incentives by allowing authors to control and monetize use of their work. Without strong copyright, creation would be under-supplied and the public domain would grow too slowly.
% FOUNDING_PROBLEM_CORROBORATION: Original authors and major copyright-holding institutions attest the founding problem is live and urgent: without strong incentives, creative production would decline. Courts adopting the creator-centric reading reiterate this rationale in opinions. However, empirical economists and digital-culture scholars dispute both the premise (creativity responds to many incentives beyond copyright) and the remedy (strong copyright correlates with lower remix rates, not higher overall creativity). The founding problem's status is not corroborated by independent economic evidence outside the benefiting parties.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.78) because the creator-centric reading requires downstream users to pay licensing fees for uses that might otherwise qualify as fair use under a user-centric or transformativeness-dominant reading. The test is not purely exclusionary — transformative use remains a factor — but the weighting (market harm as semi-dispositive, commercial purpose as quasi-prohibition) makes licensing the default path for most derivative works. Suppression is correspondingly high (0.71) because the doctrine's persistence depends on active enforcement through cease-and-desist letters, licensing threats, and judicial interpretation that forecasts liability. Theater ratio (0.42) reflects that the doctrine serves a real coordination function (balancing incentives and access) alongside its extractive function (protecting licensing revenue); roughly 42% of adjudicatory effort defends the revenue-protection reading rather than the balancing aspiration. The measurement series shows extraction rising sharply from t=0 to t=15 (as the creator-centric reading solidifies in case law post-2000s digital-commerce expansion) and plateauing thereafter — the doctrine stabilizes at a high extractiveness level and remains there. Theater ratio rises more slowly and flattens earlier, consistent with Goodhart drift where the nominal balancing function becomes increasingly performative once the doctrine's extractive function is entrenched.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (original-author) and beneficiary (copyright-holder) seats experience this constraint as protection of a legitimate right and incentive structure; from their seat, it is near-rope (coordinating creation with access). The target seats (transformative users, scholars) experience it as enforced licensing and restricted reuse; from their seat, it is snare-like (extraction masked as balanced exception). Courts in the creator-centric reading adopt the agenda-setter framing, weighting market harm as semi-dispositive. The engine should compute per-seat type divergence: beneficiary seat → rope-like (coordination + property protection); target seat → snare-like (extraction without negotiated exit). The commentary explains the structural asymmetry that produces this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders are the structural beneficiary: they set the doctrine (through amicus briefs, legislative lobbying, and judicial appointment influence), they capture the licensing revenue from the narrowed exception, and they directly benefit from the reading's weight against market harm and transformativeness. Their directionality is near-full beneficiary (d ≈ 0.05–0.15). Original authors are agenda-setters: they articulate the founding problem and the doctrine's rationale, positioning themselves as the doctrine's authors and primary stakeholders. Their directionality is mixed: they benefit from the property-right framing but are not the direct capturer of licensing revenue (that goes to institutional copyright holders); d ≈ 0.25–0.35. Transformative users and scholars are the targets: they bear the cost of licensing requirements, restricted quotation rights, and the chilling effect on derivative creation. Exit is constrained (creating with existing material is often creatively necessary, not merely optional); d ≈ 0.80–0.90. Public-domain advocates are payers of opportunity cost: they cannot claim works they would otherwise freely use; exit is constrained by the copyright term; d ≈ 0.75–0.85. Courts occupy the agenda-setter seat: they interpret and apply the four-factor test, and their weight toward market harm and creator incentives shapes the doctrine's extractiveness. Their directionality is neutral-institutional (d ≈ 0.50), but their decisions propagate the creator-centric reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The creator-centric reading's founding problem is 'original authors need incentives to create.' The test for mandatrophy would be: has that problem become obsolete while the doctrine persists? The evidence is mixed and contested. (1) Creativity-incentive proponents argue the problem is live: copyright holders still leverage licensing to fund creation, and the creator-incentive doctrine justifies the reading. (2) Digital-culture researchers argue the problem has shifted: derivative creativity (remix, sampling, fan works) is now a major creative driver, and the doctrine's narrow-exception reading suppresses this. (3) Empirically: creative output has expanded under digital distribution even as copyright enforcement has intensified, suggesting the doctrine is not the primary incentive. This is the mandatrophy boundary — the doctrine persists because copyright-holding institutions benefit from enforcement, even though the founding problem's urgency may have shifted. The creator-centric reading's persistence depends on continuous litigation and legislative renewal, consistent with mandatrophy: the founding justification (creator incentives) is maintained theatrically, while the actual function (protecting licensing revenue) is what enforcement preserves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creator_incentive_premise,
    'Is the creator-incentive doctrine (strong copyright = more creation) empirically true, or is it a cover story for institutional rent-collection?',
    'Cross-national comparative study of copyright strength vs. creative output, controlling for technology and distribution networks. Empirical comparison of countries with different copyright terms and enforcement levels.',
    'If strong copyright does NOT correlate with higher creation, the founding problem is obsolete and the doctrine is pure extraction (snare reclassification). If correlation is weak or mediated by other factors (distribution technology, funding, community), the doctrine is over-justified (mandatrophy candidate). If strong, the creator-incentive framing stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_incentive_premise, empirical, 'Whether copyright strength causally produces higher creative output.').

omega_variable(
    transformativeness_weight_ambiguity,
    'Is the four-factor test genuinely balanced, or does the creator-centric reading systematically privilege factor 4 (market harm) over factor 2 (transformativeness)?',
    'Quantitative analysis of judicial opinions: code all four-factor analyses in reported cases and measure the weight each factor receives in the final judgment. Compare trend over time as the creator-centric reading solidifies.',
    'If factor 4 (market harm) receives disproportionate weight, the doctrine is not balanced; it is disguised as balancing while it enforces a market-protection rule. This would support reclassification from tangled_rope (coordination + extraction) toward snare (pure extraction with coordination cover).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformativeness_weight_ambiguity, empirical, 'Whether judicial weight on the four factors matches the statutory intent of balanced weighing.').

omega_variable(
    reading_contest_foreclosure,
    'Does the creator-centric reading logically foreclose the transformative-use reading, or do they coexist as live positions held by different judicial factions?',
    'Analysis of current case law and judicial appointments: if multiple circuits and justices maintain both readings, they coexist; if one reading has systematically foreclosed the other in all major decisions, foreclosure has occurred.',
    'If readings coexist, the kernel is contested and multiple constraints apply (three files, three readings, linked via network). If one forecloses the other, the foreclosing reading may eventually claim the kernel wholly, collapsing the contest. This affects how the corpus models the kernel''s future state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether the creator-centric and transformative-use readings occupy the same legal space or whether one has foreclosed the other.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of transformative use structural (legal barriers, licensing costs, institutional gatekeeping) or internalized (creators self-censor because they have internalized copyright norms)?',
    'Natural experiment: jurisdictions that broaden fair use (EU copyright reform, UK fair use expansion) and measure whether transformative creation expands post-reform, and whether the expansion persists if the rule is later narrowed. Also: qualitative interviews with transformative creators about whether they self-censor due to legal risk or normative internalization.',
    'If structural, removing the barrier (broadening fair use) would immediately expand transformative creation. If internalized, narrowing fair use would not fully suppress transformative creation because the creator has already internalized the restriction. Mixed suppression (partly structural, partly internalized) would show partial recovery post-reform. This affects the effective suppression value and shapes therapeutic approaches to mandatrophy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of transformative creation is external (legal enforcement) or internal (creator self-censorship from norm internalization).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fair_tr_t5, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(fair_tr_t10, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(fair_tr_t15, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(fair_tr_t20, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(fair_tr_t25, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(fair_tr_t30, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(fair_tr_t35, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(fair_tr_t40, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(fair_be_t5, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement(fair_be_t10, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(fair_be_t15, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(fair_be_t20, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(fair_be_t25, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(fair_be_t30, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(fair_be_t35, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement(fair_be_t40, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(fair_su_t5, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(fair_su_t10, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(fair_su_t15, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(fair_su_t20, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(fair_su_t25, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(fair_su_t30, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(fair_su_t35, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(fair_su_t40, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__creator_centric_reading, 0.14).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).

% DUAL FORMULATION NOTE:
% The fair use four-factor test is a contested kernel with three structurally distinct readings. This story (creator_centric_reading) treats the doctrine as a narrow exception designed to protect creator incentives and copyright licensing revenue. The transformative_use_reading elevates transformativeness as a quasi-independent exception and subordinates market harm. The user_centric_reading treats fair use as an affirmative public right. These are not different measurements of one constraint — they are different constraints (different epsilon values, different beneficiary/victim structures, different claimed types) unified by the same contested statutory kernel. Each reading instantiates distinct structural relationships: creator-centric (beneficiaries = copyright holders, victims = transformative users); transformative-use (beneficiaries = transformative creators, victims = exclusive copyright licensing); user-centric (beneficiaries = public access / cultural production, victims = copyright holders' licensing revenue). The network links these three stories to model the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
