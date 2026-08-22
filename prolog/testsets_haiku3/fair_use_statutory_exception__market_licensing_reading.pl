% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Collapse Under Market Licensing Doctrine
 *   domain: intellectual_property/legal_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested fair-use kernel
 *   — the market-licensing reading. Fair use is a statutory exception to
 *   copyright (17 U.S.C. § 107) that permits certain uses without permission
 *   or payment. The market-licensing reading holds that any use that COULD be
 *   licensed (that is, that rights-holders would accept payment for)
 *   automatically harms the licensing market, and therefore CANNOT qualify as
 *   fair use. This reading collapses fair use in practice to uses that
 *   generate no licensing revenue — critical scholarship on obscure works,
 *   educational excerpts from works out of print or too expensive to license,
 *   cultural remixes that would never generate licensing demand. The
 *   constraint is not copyright itself, but this specific judicial-doctrinal
 *   interpretation that reads fair use as surviving only where no licensing
 *   market exists. Under this reading, transformative reuse, parody,
 *   criticism, and educational quotation all become subject to licensing
 *   demands, and most users — lacking resources or institutional backing —
 *   accede to licensing or cease the use. The reading's core premise is that
 *   the second factor of the four-factor fair-use test (effect on the
 *   licensing market) should be read to exclude any use that could generate
 *   licensing revenue.
 *
 * KEY AGENTS:
 *   - copyright_holders_with_licensing_infrastructure: institutional beneficiaries — set licensing prices unilaterally, capture uses that doctrine would otherwise protect
 *   - licensing_intermediaries (ASCAP, BMI, PROs, rights-clearance services): organized beneficiaries — collect transaction fees and administrative rents as fair use shrinks
 *   - transformative_users (artists, scholars, remixers): moderate-power payers — identity-locked (cannot abandon reuse without abandoning their cultural practice), pay licensing fees for uses doctrine could protect
 *   - non_commercial_educators (teachers, librarians): powerless payers — trapped, cease questioned uses rather than negotiate or pay licensing fees
 *   - research_institutions: organized payers — constrained by budget and litigation risk, license or avoid rather than assert fair use
 *   - cultural_producers (musicians, filmmakers): moderate-power payers — identity-locked, pay to avoid litigation for uses that advance their art
 *   - courts_interpreting_fair_use: institutional agenda-setter — adopt the market-licensing premise, collapse fair use in any market where licensing exists
 *   - congress: observer — has not amended the Copyright Act to clarify or override the market-licensing reading despite decades of advocacy
 *   - access_advocates: excluded — would radically reshape the doctrine but are not seated in the litigation and statutory processes that author it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.91).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.78).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, snare).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Collapse Under Market Licensing Doctrine").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "intellectual_property/legal_interpretation").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, 'c5012ce0-830f-4421-bce5-6046bd5b8c8c').
narrative_ontology:cs_kernel_codification('c5012ce0-830f-4421-bce5-6046bd5b8c8c', fixed_text).
narrative_ontology:cs_authority_grounding('c5012ce0-830f-4421-bce5-6046bd5b8c8c', extraction).
narrative_ontology:cs_interpretation_layer_present('c5012ce0-830f-4421-bce5-6046bd5b8c8c').
narrative_ontology:cs_reading_relation('c5012ce0-830f-4421-bce5-6046bd5b8c8c', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('c5012ce0-830f-4421-bce5-6046bd5b8c8c', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_axiom('c5012ce0-830f-4421-bce5-6046bd5b8c8c', foundational, any_licensable_use_harms_licensing_market).
narrative_ontology:cs_axiom_status(any_licensable_use_harms_licensing_market, holdable).
narrative_ontology:cs_axiom_grounding('c5012ce0-830f-4421-bce5-6046bd5b8c8c', any_licensable_use_harms_licensing_market, empirically_contingent).
narrative_ontology:cs_axiom('c5012ce0-830f-4421-bce5-6046bd5b8c8c', foundational, fair_use_exists_only_where_no_licensing_market_exists).
narrative_ontology:cs_axiom_status(fair_use_exists_only_where_no_licensing_market_exists, holdable).
narrative_ontology:cs_axiom_grounding('c5012ce0-830f-4421-bce5-6046bd5b8c8c', fair_use_exists_only_where_no_licensing_market_exists, conventional).
narrative_ontology:cs_reference_frame('c5012ce0-830f-4421-bce5-6046bd5b8c8c', copyright_as_property_right_maximization).
narrative_ontology:cs_drift_state('c5012ce0-830f-4421-bce5-6046bd5b8c8c', contemporary_statutory_fair_use_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5012ce0-830f-4421-bce5-6046bd5b8c8c', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, copyright_holders_with_licensing_infrastructure).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, non_commercial_educators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, research_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, cultural_producers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large rights-holders (major publishers, studios, music majors) with established licensing revenue streams benefit directly: every use that might otherwise qualify for fair use is forced into the licensing market. They set prices and terms unilaterally; the doctrine's collapse in any market where they operate means they capture uses that would otherwise be free speech.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, copyright_holders_with_licensing_infrastructure, beneficiary,
    institutional, generational, arbitrage, global).

% Rights-clearance services, licensing platforms, and collective societies (ASCAP, BMI, PROs) collect transaction fees and administrative rents on every use that flows through the licensing system. The doctrine's reach-through means their transaction volume and fee base expand as fair use shrinks.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, licensing_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Artists, scholars, and cultural producers who remix, parody, critique, or sample existing work pay licensing fees for uses that doctrine-interpretation could recognize as fair use. Their professional identity depends on reuse; they cannot simply exit the conversation without abandoning their cultural practice. They negotiate against institutional rights-holders with vastly asymmetric bargaining power.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, transformative_users, payer,
    moderate, biographical, identity_locked, global).

% Teachers, librarians, and non-profit educators citing, quoting, and excerpting copyrighted material for educational purposes face licensing demands. They operate under budget constraints and institutional mandates that often prevent them from licensing every quoted passage. Many cease the questioned use rather than negotiate with rights-holders.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, non_commercial_educators, payer,
    powerless, biographical, trapped, national).

% Universities and research centers that quote, excerpt, and cite copyrighted sources in scholarship and teaching must secure licenses or cease the practice. Budget constraints and institutional risk-aversion (fear of costly litigation) push most toward licensing or avoidance, not fair-use assertion.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, research_institutions, payer,
    organized, generational, constrained, national).

% Musicians, filmmakers, writers, and visual artists who incorporate, sample, quote, or build upon existing cultural material face licensing demands for uses that transformative-right doctrine would shelter. They are trapped between artistic integrity (which demands reuse) and legal/financial exposure.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, cultural_producers, payer,
    moderate, biographical, identity_locked, global).

% Judges and judicial doctrine interpreters decide whether a use qualifies as fair. Under the market-licensing reading, courts adopt the premise that any use that could be licensed — i.e., any use that rights-holders would accept payment for — automatically harms the licensing market and therefore cannot be fair use. This collapses the fair-use defense to non-monetizable or de minimis uses only.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts_interpreting_fair_use, agenda_setter,
    institutional, generational, analytical, national).

% Open access movements, digital rights advocates, and reformers argue that fair use should protect transformative and non-commercial reuse regardless of licensing market existence. They would radically reshape the doctrine if seated; they are excluded from the narrow coalition that authors doctrine through litigation and statutory amendment.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, access_advocates_and_reformers, excluded,
    moderate, biographical, constrained, global).

% Legislative authority that could amend the Copyright Act to clarify or expand fair use, but has not done so despite decades of advocacy. Absence of statutory amendment is treated as tacit approval of judicial fair-use interpretation, which permits the market-licensing reading to harden doctrine.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, congress, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__market_licensing_reading, copyright_holders_with_licensing_infrastructure).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__market_licensing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Copyright Act aims to balance incentives for creation (exclusive rights to creators) with public access to cultural and information goods (fair use exemption). A licensing market allocates uses and collects revenue; the market-licensing reading treats licensing as the sole legitimate mechanism for deciding who can use what.
% TRANSFER_FUNCTION: Moves fees from transformative users, educators, and researchers to copyright holders and licensing intermediaries. The constraint transfers not just money but decision-making authority: rights-holders choose which uses can be licensed (and at what price), and all monetizable uses flow through the licensing system.
% ABSENT_VOICES: Access advocates, open-culture movements, digital-commons proponents, and scholars of transformative use are structurally excluded from the coalition that interprets fair use through litigation. They would argue that fair use exists precisely to protect reuses that rights-holders would monetize if given the chance, but their voice has minimal weight in judicial and statutory processes shaped by repeat-player copyright holders.
% DISAPPEARANCE_RATIONALE: If the market-licensing reading collapsed and fair use reverted to protecting transformative and non-commercial reuse regardless of licensing markets, educators would resume citing without licenses, cultural producers would remix and sample without clearing rights, and licensing intermediaries would lose substantial transaction volume. Rights-holders would lose licensing revenue for uses courts would recognize as fair. The entire allocation mechanism would shift from 'licensing market decides' to 'courts apply transformative and public-benefit tests.'
% FOUNDING_PROBLEM: Copyright law exists to incentivize creation while preserving public access. Fair use was codified in 1976 to protect uses that do not substitute for the original work or that advance public purposes (education, criticism, news reporting). Early caselaw established a four-factor test; the question is how strictly courts apply the second factor (effect on the market for the original).
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and licensing organizations attest that fair use must yield to licensing markets to protect their revenue streams. Courts adopting the market-licensing reading cite statutory language and prior precedent to support strict application of the second factor. However, scholars, access advocates, and some judges dispute this reading: they argue that fair use's entire purpose is to protect uses that don't enter the licensing market (transformative reuse, criticism, education). Legislative testimony from cultural institutions and education organizations shows consensus that fair use is being eroded in practice by this reading; no statutory amendment has occurred despite repeated calls for clarification.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.91, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is extremely high (0.91 at interval end) because the constraint transfers decision-making authority and licensing fees to institutional rights-holders for uses that statutory fair use was intended to protect. The constraint's power comes not from explicit coercion but from the asymmetry in negotiating power and legal risk: users face litigation exposure and lack institutional backing, so most accede to licensing demands rather than assert fair use. Suppression is high (0.78) because the constraint suppresses both assertion (users do not challenge licensing demands) and alternatives (licensing is the mechanism that decides what uses are permitted). Theater is moderate (0.42): the fair-use doctrine still appears to exist in copyright law and cases still cite it, but the market-licensing reading has narrowed it so drastically that it functions only at the margins. The measurement series traces 50 years: from 1976 (when § 107 was codified and fair use seemed broad) to 2026 (when the market-licensing reading has substantially narrowed it in practice). The rising trend shows extractiveness accumulating as the reading hardens through repeated judicial adoption and as licensing infrastructure matures to capture uses that fair use would otherwise protect. Suppression rises alongside: the constraint requires active enforcement (litigation risk, institutional licensing requirements, chilling effects on reuse) to maintain the market-licensing reading against alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The gap between copyright-holder seats and transformative-user seats should be stark. From the copyright-holder perspective, the market-licensing reading is sensible doctrine: it treats copyright as property, protects licensing revenue, and incentivizes creation by maximizing the copyright holder's control and pricing power. From the transformative-user perspective, the same reading is extractive: it collapses fair use and forces licensing for uses that statute was intended to protect. Courts sit between these perspectives but are structurally closer to repeat-player copyright holders (who litigate regularly) than to dispersed transformative users (who rarely litigate). The engine should compute this divergence from the directionality data: copyright-holder seats should compute closer to 'coordination' or 'rope' (they benefit without undue suppression; the mechanism aligns their interests), while transformer-user seats should compute closer to 'snare' (extraction via suppression, constrained exit, identity-lock). The claimed type (snare) reflects the reading's overall structure from the perspective of those targeted by it.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and licensing intermediaries are beneficiaries (low d, toward 0.1–0.2): they benefit from the constraint without bearing costs; the mechanism exists to extract licensing fees from users. Transformative users and educators are targets (high d, toward 0.8–0.95): they pay licensing fees for uses that fair use might otherwise protect; many are identity-locked (cannot exit without abandoning their cultural or educational practice). Courts are the agenda-setter (moderate d, toward 0.5–0.6): they interpret and enforce the reading but do not directly benefit or pay; they are the institutional actors who lock the reading into place through repeated application. The key structural asymmetry: copyright holders have concentrated incentives and organized resources to litigate and lobby for the market-licensing reading; transformative users and educators have diffuse incentives and lack the resources to mount coordinated legal challenges. Courts face repeat-player copyright holders in litigation; access advocates appear rarely. This asymmetry drives directionality: beneficiaries sit at low d (they have arbitrage-grade exit: they can shift licensing terms and rates as the reading hardens), while victims sit at high d (they are trapped or identity-locked: they cannot reuse without licensing, and licensing fees are set unilaterally).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — balancing copyright incentives with public access — is LIVE: copyright holders continue to need incentives, and the public continues to need access to cultural works. But the mandate that fair use serve as the balance mechanism is DEAD for the market-licensing reading: the reading has redefined fair use to yield in every case where licensing markets exist, making it impossible for fair use to serve its balancing function in any institutional or commercial context. The theater_ratio shows this: while fair use is still cited in cases, it increasingly functions as a theatrical acknowledgment of a doctrine that no longer operates. The reading has abandoned the founding mandate and replaced it with pure rent-maximization: 'any use that could be licensed harms licensing markets, so no use can be fair.' This is not balancing; it is the collapse of the balance mechanism itself. Mandatrophy is UNRESOLVED: the reading persists despite the failure of its founding mandate because the institutional structure that authors doctrine (courts, repeat-player copyright holders, legislative inattention) has no actor strongly incentivized to reform it. Transformative users and educators lack the resources to mount sustained legal challenges; access advocates lack litigation standing; Congress has not acted despite repeated calls for statutory clarification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    licensing_market_definition_ambiguity,
    'What counts as a ''licensing market'' for purposes of the second fair-use factor? Does it require an actual market transaction, or does potential for licensing suffice?',
    'Statutory clarification or appellate guidance defining what licensing markets fair use must yield to. Case law shows courts use ''could be licensed'' (potential) rather than ''is licensed'' (actual), which maximizes extraction.',
    'If the definition shifts to actual-licensing-only, fair use would protect uses in dormant or non-existent markets (e.g., parody of obscure scholarship, remixes of out-of-print works). If it requires only potential, fair use collapses as authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_market_definition_ambiguity, conceptual, 'The boundary between actual and potential licensing markets defines fair use''s scope.').

omega_variable(
    transformative_use_doctrine_suppression,
    'Is the suppression of transformative-use doctrine (which would protect reuse regardless of market harm) a structural feature of the market-licensing reading, or could courts recognize both the market-harm principle and the transformative-use principle?',
    'Appellate decisions that restore weight to the ''transformative use'' factor independently of market harm, or statutory amendment listing transformative reuse as explicitly protected.',
    'If transformative use becomes independently protective, fair use expands even in licensing markets. If market-licensing reading harddens further, transformative-use doctrine becomes inert (theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_doctrine_suppression, empirical, 'Whether the reading suppresses alternatives or is the inevitable interpretation of statute.').

omega_variable(
    founding_mandate_obsolescence,
    'Has the founding mandate of fair use (balancing copyright incentives with public access) become impossible to fulfill under the market-licensing reading?',
    'Comparative analysis of how other copyright jurisdictions (EU, Canada, Australia) balance incentives and access; empirical study of fair-use assertion rates and licensing-demand patterns post-market-licensing hardening.',
    'If the mandate is obsolete, mandatrophy is unresolved (the reading persists despite abandoning its justification). If the mandate survives in pockets or alternative doctrines, mandatrophy is partially resolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_mandate_obsolescence, empirical, 'Whether the market-licensing reading has rendered its founding mandate inoperable.').

omega_variable(
    reading_vs_statutory_language_fit,
    'Does the market-licensing reading faithfully interpret 17 U.S.C. § 107, or does it override statutory text with judicially invented doctrine?',
    'Originalist statutory analysis comparing § 107''s text and legislative history to the market-licensing reading; comparative jurisprudence showing how other nations interpret similar statutory fair-use provisions.',
    'If the reading exceeds statutory authorization, Congress could reverse it with clarifying amendment. If the reading is faithful to § 107, reform requires either statutory amendment changing the statute itself or appellate judicial reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_statutory_language_fit, conceptual, 'Whether the reading is faithful to statute or represents doctrinal innovation that statute does not support.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1976, 0.18).
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(fair_tr_t2000, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(fair_tr_t2010, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(fair_tr_t2018, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2018, 0.39).
narrative_ontology:measurement(fair_tr_t2026, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1976, 0.35).
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(fair_be_t2018, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2018, 0.86).
narrative_ontology:measurement(fair_be_t2026, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2026, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1976, 0.45).
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(fair_su_t2018, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2018, 0.74).
narrative_ontology:measurement(fair_su_t2026, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2026, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__market_licensing_reading, 0.25).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, copyright_licensing_market_power).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, educational_exemptions_copyright).

% DUAL FORMULATION NOTE:
% The fair_use_statutory_exception kernel is contested across three structurally distinct readings. This constraint models the market_licensing_reading (any use that could be licensed harms licensing markets; fair use collapses to non-monetizable uses). The transformative_right_reading (fair use protects transformative reuse regardless of market effect) and narrow_defense_reading (fair use is a narrow defense within property-rights framework) are sibling constraints with different ε, beneficiary/victim structures, and founding problems. All three readings describe the same statutory text (17 U.S.C. § 107) but arrive at incompatible doctrinal conclusions. They are linked via network.affects_constraints because judicial adoption of the market-licensing reading shifts the doctrinal landscape for the other readings: the market-licensing reading makes transformative-use protection structurally harder to assert, and it narrows the space where narrow-defense reading can operate. The readings compete for institutional authority within the same legal system and the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__market_licensing_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
