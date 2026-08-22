% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__disclosure_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__disclosure_consent, []).

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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Legitimacy — Disclosure-Consent Settlement
 *   domain: economic/legal/governance
 *
 * SUMMARY:
 *   A dominant-founder dual-class structure — supervoting shares retained
 *   through an IPO while public investors buy subordinate-vote Class A — is
 *   legitimated, under this reading, by informed consent: the Securities Act
 *   disclosure regime puts the governance terms in the registration
 *   statement, investors buy with that disclosure in hand, and the disparity
 *   is priced into the offer. The constraint under classification is that
 *   legitimacy settlement itself: the standing practice under which disclosed
 *   consent, rather than control parity, settles the governance question. Per
 *   the ε-invariance principle, the colloquial label 'dual-class legitimacy'
 *   decomposes into three structurally distinct constraint stories — this
 *   disclosure-consent settlement, a founder-stewardship substantive warrant,
 *   and a minority-extraction distributive entitlement — each with its own ε,
 *   beneficiaries, and type over the shared referent. This story authors ε
 *   only for this reading's assessment of that referent; the siblings are
 *   separate files linked via network.affects_constraints. The interval maps
 *   T=0 to the 2004 Google IPO (the template event for the modern settlement)
 *   and T=21 to 2025. KEY AGENTS (by structural relationship):
 *   founder_controlling_shareholders (agenda-setter and principal
 *   beneficiary, powerful / identity_locked); minority_class_a_investors
 *   (payer, powerless / mobile — exit is the consent mechanism itself);
 *   institutional_asset_managers (payer with secondary beneficiary position,
 *   organized / constrained); employee_class_a_grantees (payer, powerless /
 *   constrained, consent never negotiated); underwriting_syndicates and
 *   exchange_listing_boards (beneficiaries, institutional / arbitrage);
 *   index_providers and proxy_advisory_firms (observers);
 *   minority_protection_advocates (excluded); securities_regulator (observer,
 *   national scope).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.31).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.27).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.31).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.27).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Legitimacy — Disclosure-Consent Settlement").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "economic/legal/governance").

domain_priors:requires_active_enforcement(dual_class_legitimacy__disclosure_consent).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, 'a10b0705-1979-42eb-90d8-d5349534b3a6').
narrative_ontology:cs_kernel_codification('a10b0705-1979-42eb-90d8-d5349534b3a6', distributed).
narrative_ontology:cs_authority_grounding('a10b0705-1979-42eb-90d8-d5349534b3a6', practice).
narrative_ontology:cs_interpretation_layer_present('a10b0705-1979-42eb-90d8-d5349534b3a6').
narrative_ontology:cs_reading_relation('a10b0705-1979-42eb-90d8-d5349534b3a6', dual_class_legitimacy__founder_stewardship, influences).
narrative_ontology:cs_reading_relation('a10b0705-1979-42eb-90d8-d5349534b3a6', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_axiom('a10b0705-1979-42eb-90d8-d5349534b3a6', foundational, informed_consent_legitimates_governance_terms).
narrative_ontology:cs_axiom_status(informed_consent_legitimates_governance_terms, holdable).
narrative_ontology:cs_axiom_grounding('a10b0705-1979-42eb-90d8-d5349534b3a6', informed_consent_legitimates_governance_terms, conventional).
narrative_ontology:cs_axiom('a10b0705-1979-42eb-90d8-d5349534b3a6', foundational, governance_disparity_priced_at_ipo).
narrative_ontology:cs_axiom_status(governance_disparity_priced_at_ipo, holdable).
narrative_ontology:cs_axiom_grounding('a10b0705-1979-42eb-90d8-d5349534b3a6', governance_disparity_priced_at_ipo, empirically_contingent).
narrative_ontology:cs_reference_frame('a10b0705-1979-42eb-90d8-d5349534b3a6', disclosure_sufficiency_settlement).
narrative_ontology:cs_drift_state('a10b0705-1979-42eb-90d8-d5349534b3a6', contemporary_passive_ownership_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a10b0705-1979-42eb-90d8-d5349534b3a6', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, founder_controlling_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, underwriting_syndicates).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, exchange_listing_boards).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, institutional_asset_managers).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, minority_class_a_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, institutional_asset_managers).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, employee_class_a_grantees).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, securities_act_disclosure_philosophy).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, contractarian_theory_of_the_firm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold supervoting shares (typically 5-20 votes per share versus one for public shares) retained through the IPO. They set the governance terms at listing — vote ratio, share classes, any sunset triggers — through the registration statement they file and the board they control. Public capital funds the company while board composition and major decisions remain theirs indefinitely. Selling control would trigger conversion of their supervoting shares, so exit from the arrangement means surrendering what the arrangement preserves; mission commitments, legacy claims, and self-concept are bound to continued control.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, founder_controlling_shareholders, agenda_setter,
    powerful, generational, identity_locked, global).

% Buy Class A shares in the IPO or aftermarket after the registration statement discloses the voting structure in risk factors and capitalization tables. Per-share economic rights match the founders'; voting power does not — no accumulation of Class A shares can outvote the founder block. Their protection under this arrangement is the price they paid and the ability to sell at any time; they cannot vote out management regardless of performance.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, minority_class_a_investors, payer,
    powerless, biographical, mobile, global).

% Hold Class A through index funds that must track benchmarks containing the issuer and through active mandates. They collect management fees on the assets, but cannot divest an index constituent without leaving the index, and they must vote the shares under stewardship codes — a duty that pits client exposure against governance objections. Their proxy votes are the only channel through which their ultimate holders' preferences reach the issuer.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, institutional_asset_managers, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, institutional_asset_managers, beneficiary).

% Receive Class A shares or options settling in Class A as compensation. They never negotiate the governance terms; the equity arrives with the voting structure already fixed. Exit is bound to vesting schedules and continued employment, so they bear the arrangement's costs while their acceptance is an employment decision rather than a purchase decision.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, employee_class_a_grantees, payer,
    powerless, biographical, constrained, global).

% Structure, price, and market dual-class offerings. They draft the disclosure that operationalizes informed consent, advise on vote-ratio and sunset design, and collect underwriting fees from issuer proceeds. They can decline any mandate and compete for the largest ones.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, underwriting_syndicates, beneficiary,
    institutional, biographical, arbitrage, global).

% Set the listing standards that admit dual-class structures and compete with rival exchanges for marquee listings. Listing and related fees from large dual-class issuers are material revenue; tightening standards unilaterally would push issuers to the competing venue.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, exchange_listing_boards, beneficiary,
    institutional, generational, arbitrage, global).

% Decide which securities enter the benchmarks that passive money tracks. Their inclusion policies — some exclude or cap dual-class additions — gate the passive ownership that now dominates Class A float. They collect licensing fees on assets tracking their indices and face pressure from both issuers seeking inclusion and stewards seeking exclusion.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, index_providers, observer,
    institutional, generational, analytical, global).

% Issue vote recommendations on director elections and charter amendments and run standing campaigns against dual-class structures. Issuers and asset managers treat their policies as de facto standards; they hold no shares and collect no fees from the arrangement they assess.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, proxy_advisory_firms, observer,
    organized, biographical, analytical, global).

% Academic corporate-law scholars, stewardship-code bodies, and institutional investor coalitions who would mandate sunset clauses, vote caps, or parity floors. They publish, petition the regulator, and testify, but hold no seat in the terms-setting conversation: the registration statement is drafted by issuer and underwriters, reviewed by the regulator for accuracy rather than adequacy, and offered to investors on take-it-or-leave-it terms.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, minority_protection_advocates, excluded,
    organized, generational, analytical, global).

% Reviews registration statements for material accuracy — including the voting-structure risk factors that carry the consent function — and enforces anti-fraud liability. It does not police control parity: merit regulation was rejected at the founding and has not returned. It studies dual-class structures periodically and has requested comment on listing-standard questions without adopting mandates.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_regulator, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__disclosure_consent, founder_controlling_shareholders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__disclosure_consent, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes and mandates disclosure of governance terms at issuance: the registration statement's capitalization tables, voting-rights descriptions, and risk factors give every buyer the same material picture of what they are purchasing, solving the information problem that would otherwise make founder-control offerings unpriceable, and letting capital markets rather than a merit regulator allocate.
% TRANSFER_FUNCTION: Moves voting control from public investors to founder-held supervoting shares while moving public capital into the issuer; embeds the price of that control transfer in the offer price Class A buyers pay; and moves underwriting and listing fees from issuer proceeds to the syndicates and exchanges that operate the settlement.
% ABSENT_VOICES: Minority-protection advocates (sunset mandates, vote caps, parity floors) have no seat in the terms-setting conversation: the registration statement is drafted by issuer and underwriters, reviewed by the regulator for accuracy rather than adequacy, and offered on take-it-or-leave-it terms. Index-constrained holders' consent is mediated by asset-manager proxy votes. Employee grantees never negotiate governance terms at all.
% DISAPPEARANCE_RATIONALE: If the disclosure-consent settlement vanished overnight — if disclosed consent no longer settled legitimacy — pending and future dual-class offerings would restructure around sunsets or parity floors, some issuers would shift to private markets or foreign venues, and existing supervoting structures would face immediate challenge from proxy-advisor policies and index exclusions; the market for US-listed founder control would reorganize around whatever standard replaced consent.
% FOUNDING_PROBLEM: The 1933 Securities Act settlement was built for an information problem: investors buying securities had no reliable picture of what they were buying, and the adopted answer was mandatory disclosure plus anti-fraud liability rather than merit regulation. When dual-class structures revived at scale (the 2004 Google IPO as template), the operative question was whether that disclosure machinery could legitimate a structure whose key term is the removal of voting parity.
% FOUNDING_PROBLEM_CORROBORATION: Securities regulators and the materiality case law attest the disclosure function is live — every dual-class registration statement is reviewed under it. Institutional investors attest they rely on disclosure daily while contesting its sufficiency; that contest is the kernel itself and constitutes corroboration from outside the founder and underwriter set. Academic corporate-law scholarship documents both the settlement's continued operation and its contested boundaries. No party attests the underlying information problem is dead.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__disclosure_consent, 0.31, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__disclosure_consent_tests).
:- end_tests(dual_class_legitimacy__disclosure_consent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε's referent is the standing dual-class arrangement assessed by this reading's own lights — not the parity-respecting arrangement a sibling reading would install. From this seat the disparity is a disclosed, priced contract term: extractiveness is authored at 0.31 (residual cost concentrated where consent is mediated or absent), suppression at 0.27 (no coercion of investors; the arrangement's defensive force operates on alternative rules — keeping parity mandates off the books — and that defensive machinery has intensified), theater_ratio at 0.30 (core voting-structure disclosure is genuine; the surrounding prospectus apparatus has bloated toward ritual), accessibility_collapse at 0.30 (alternatives remain live: one-share-one-vote issuers, index exclusion policies, sunset mandates in other venues), resistance at 0.58 (proxy-advisor campaigns, index exclusions, academic and legislative pressure). Suppression is authored as a raw structural property and is not scaled by any context dimension; extractiveness is scaled by the engine from directionality and scope. The measurement series run on one shared eight-point grid (T=0,3,6,9,12,15,18,21) with every tracked metric authored at every point — no per-metric grids. Trajectories: base_extractiveness rises 0.20 to 0.34 through T=15 as indexization thins exit-as-consent and extreme structures (non-voting listings, entrenchment controversies) stress the pricing story, then eases to 0.31 as sunset adoption spreads and index exclusions bite; theater_ratio climbs steadily with disclosure bloat; suppression_requirement climbs 0.12 to 0.27 as defending the settlement against parity campaigns required growing active enforcement (lobbying, exchange-competition arguments, disclosure refinements) — an enforcement-intensification trajectory, not decay. No cyclical dynamics: the series are monotonic or single-peaked. claimed_type is authored from this reading's structural position — contractual choice with net beneficiaries and mobile exit — and is not reconciled to the metrics; per-seat divergence is the measurement the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The founder seat and the Class A payer seats compute differently by construction: from the founder position the arrangement is a disclosure settlement it authored and priced into its own illiquidity; from the payer positions it is a voting disparity accepted at purchase. Within the payer class, exit differentiates same-power seats: mobile retail holders experience the arrangement as this reading describes (exit is the protection), index-constrained institutional holders experience it without the exit the consent theory presupposes, and vesting-locked employee grantees never faced a purchase decision at all — the same nominal constraint, three different computed positions. The excluded advocates' seat computes the arrangement from outside the consent frame entirely, which is where the minority_extraction sibling reading lives. Inter-institutionally, exchanges and the regulator sit on opposite sides of an authority boundary: exchanges compete to admit what the regulator declines to police, and underwriters monetize the boundary between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder controlling shareholders are agenda-setter and principal beneficiary — d sits near the beneficiary end, and their identity_locked exit reflects professional-institutional identity fusion: mission, legacy, and self-concept are constituted through continued control, and a founder willing to convert shares would shift the seat's position and destabilize the settlement's demand side. Underwriting syndicates and exchange listing boards are beneficiaries with arbitrage-grade exit — nearest the beneficiary end. Retail Class A investors are payers with mobile exit: this reading's own theory locates their protection in exit, so the structural derivation damps their effective extraction — the derivation encodes exactly this reading's consent logic. Institutional asset managers are payers (secondary beneficiaries through fee revenue) with constrained exit — higher d than retail despite equal or greater organized power, which is the story's central same-level divergence. Employee grantees are payers with constrained exit and no negotiated consent — high d. Index providers, proxy advisors, and the regulator are observers (analytical). No directionality overrides are authored: the derivation from declared roles, power, and exit options captures the seat structure, and overriding it would import the sibling readings' conclusions into this reading's file.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — information asymmetry at issuance — remains live, so no mandatrophy is declared: the settlement has not outlived its function, and founding_problem_status 'live' combined with disappearance_verdict 'world_rearranges' raises no capture flag. The classification discipline cuts both ways. It prevents the minority_extraction sibling from being folded into this file as pure extraction: consent and pricing are real and operative for the mobile segment, and declaring victims here would be authoring the sibling's conclusion inside the wrong constraint. It equally prevents this reading's claim from being accepted at face value where consent is mediated or absent: the per-seat computation surfaces the index-constrained and vesting-locked segments, whose positions this reading must accommodate rather than deny. Watch item: theater_ratio's steady climb (0.15 to 0.30) tracks disclosure bloat; if the consent ritual continues to inflate while pricing evidence stagnates, the settlement drifts from substance toward performance of consent — the sunset-adoption and pricing-efficiency omegas are the tripwires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the dual_class_legitimacy kernel governs legitimacy — disclosure_consent (this file), founder_stewardship, or minority_extraction?',
    'Doctrinal and market evidence: whether disclosed governance disparity is actually priced at IPO, whether consent quality holds for index-constrained holders, and whether regulators adopt parity floors or mandatory sunsets.',
    'Under minority_extraction, Class A holders become declared victims, ε rises sharply, and the type moves toward tangled_rope or snare; under founder_stewardship, the coordination function deepens and measured extraction falls further. This file''s rope claim is valid only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one reading of the dual_class_legitimacy kernel; sibling readings instantiate different constraints over the same referent.').

omega_variable(
    passive_holder_consent_quality,
    'Does exit-as-consent hold for Class A shares held through index-tracking vehicles that cannot divest without leaving the index?',
    'Time series on the index-constrained share of dual-class float: if constrained vehicles come to hold the majority of Class A dollars, the consent mechanism fails for the median holder.',
    'If consent fails for passive holders, this reading''s legitimacy claim narrows to active investors and measured extraction rises for the constrained segment even under this reading''s own lights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passive_holder_consent_quality, empirical, 'Whether the consent mechanism survives the shift from mobile retail ownership to constrained index ownership.').

omega_variable(
    ipo_pricing_efficiency,
    'Do primary-market prices actually embed the governance disparity, or are underpricing and issuance hype sufficient that Class A buyers are not compensated at purchase?',
    'Event studies comparing dual-class versus single-class IPO valuation discounts and long-run returns conditional on governance structure and vote ratio.',
    'Incomplete pricing breaks the priced-consent axiom, converts the disparity into uncompensated transfer even under this reading, and pressures the empirically_contingent foundational axiom toward foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ipo_pricing_efficiency, empirical, 'Whether the market actually prices the governance disparity the disclosure regime reveals.').

omega_variable(
    sunset_adoption_trajectory,
    'Is voluntary sunset-clause adoption converging toward binding transition limits, or is adoption cosmetic and concentrated among issuers facing scrutiny?',
    'Corpus study of sunset provisions in post-2015 dual-class IPOs: prevalence, trigger design (time-based versus control-event), and observed enforcement.',
    'Binding sunsets would re-date the arrangement as transitional support rather than steady-state settlement; cosmetic adoption leaves the settlement intact with rising theater and supports the current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_adoption_trajectory, empirical, 'Whether the settlement is drifting toward a sunset-bounded transitional form or persisting as an open-ended standing arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 21).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcl_disclosure_consent_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(dcl_disclosure_consent_tr_t0, observed).
narrative_ontology:measurement(dcl_disclosure_consent_tr_t3, dual_class_legitimacy__disclosure_consent, theater_ratio, 3, 0.17).
narrative_ontology:measurement_basis(dcl_disclosure_consent_tr_t3, observed).
narrative_ontology:measurement(dcl_disclosure_consent_tr_t6, dual_class_legitimacy__disclosure_consent, theater_ratio, 6, 0.19).
narrative_ontology:measurement_basis(dcl_disclosure_consent_tr_t6, observed).
narrative_ontology:measurement(dcl_disclosure_consent_tr_t9, dual_class_legitimacy__disclosure_consent, theater_ratio, 9, 0.22).
narrative_ontology:measurement_basis(dcl_disclosure_consent_tr_t9, observed).
narrative_ontology:measurement(dcl_disclosure_consent_tr_t12, dual_class_legitimacy__disclosure_consent, theater_ratio, 12, 0.25).
narrative_ontology:measurement_basis(dcl_disclosure_consent_tr_t12, observed).
narrative_ontology:measurement(dcl_disclosure_consent_tr_t15, dual_class_legitimacy__disclosure_consent, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(dcl_disclosure_consent_tr_t15, observed).
narrative_ontology:measurement(dcl_disclosure_consent_tr_t18, dual_class_legitimacy__disclosure_consent, theater_ratio, 18, 0.29).
narrative_ontology:measurement_basis(dcl_disclosure_consent_tr_t18, observed).
narrative_ontology:measurement(dcl_disclosure_consent_tr_t21, dual_class_legitimacy__disclosure_consent, theater_ratio, 21, 0.3).
narrative_ontology:measurement_basis(dcl_disclosure_consent_tr_t21, observed).

% Extraction over time
narrative_ontology:measurement(dcl_disclosure_consent_be_t0, dual_class_legitimacy__disclosure_consent, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(dcl_disclosure_consent_be_t0, observed).
narrative_ontology:measurement(dcl_disclosure_consent_be_t3, dual_class_legitimacy__disclosure_consent, base_extractiveness, 3, 0.23).
narrative_ontology:measurement_basis(dcl_disclosure_consent_be_t3, observed).
narrative_ontology:measurement(dcl_disclosure_consent_be_t6, dual_class_legitimacy__disclosure_consent, base_extractiveness, 6, 0.26).
narrative_ontology:measurement_basis(dcl_disclosure_consent_be_t6, observed).
narrative_ontology:measurement(dcl_disclosure_consent_be_t9, dual_class_legitimacy__disclosure_consent, base_extractiveness, 9, 0.29).
narrative_ontology:measurement_basis(dcl_disclosure_consent_be_t9, observed).
narrative_ontology:measurement(dcl_disclosure_consent_be_t12, dual_class_legitimacy__disclosure_consent, base_extractiveness, 12, 0.32).
narrative_ontology:measurement_basis(dcl_disclosure_consent_be_t12, observed).
narrative_ontology:measurement(dcl_disclosure_consent_be_t15, dual_class_legitimacy__disclosure_consent, base_extractiveness, 15, 0.34).
narrative_ontology:measurement_basis(dcl_disclosure_consent_be_t15, observed).
narrative_ontology:measurement(dcl_disclosure_consent_be_t18, dual_class_legitimacy__disclosure_consent, base_extractiveness, 18, 0.33).
narrative_ontology:measurement_basis(dcl_disclosure_consent_be_t18, observed).
narrative_ontology:measurement(dcl_disclosure_consent_be_t21, dual_class_legitimacy__disclosure_consent, base_extractiveness, 21, 0.31).
narrative_ontology:measurement_basis(dcl_disclosure_consent_be_t21, observed).

% Suppression requirement over time
narrative_ontology:measurement(dcl_disclosure_consent_su_t0, dual_class_legitimacy__disclosure_consent, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(dcl_disclosure_consent_su_t0, observed).
narrative_ontology:measurement(dcl_disclosure_consent_su_t3, dual_class_legitimacy__disclosure_consent, suppression_requirement, 3, 0.14).
narrative_ontology:measurement_basis(dcl_disclosure_consent_su_t3, observed).
narrative_ontology:measurement(dcl_disclosure_consent_su_t6, dual_class_legitimacy__disclosure_consent, suppression_requirement, 6, 0.16).
narrative_ontology:measurement_basis(dcl_disclosure_consent_su_t6, observed).
narrative_ontology:measurement(dcl_disclosure_consent_su_t9, dual_class_legitimacy__disclosure_consent, suppression_requirement, 9, 0.19).
narrative_ontology:measurement_basis(dcl_disclosure_consent_su_t9, observed).
narrative_ontology:measurement(dcl_disclosure_consent_su_t12, dual_class_legitimacy__disclosure_consent, suppression_requirement, 12, 0.22).
narrative_ontology:measurement_basis(dcl_disclosure_consent_su_t12, observed).
narrative_ontology:measurement(dcl_disclosure_consent_su_t15, dual_class_legitimacy__disclosure_consent, suppression_requirement, 15, 0.25).
narrative_ontology:measurement_basis(dcl_disclosure_consent_su_t15, observed).
narrative_ontology:measurement(dcl_disclosure_consent_su_t18, dual_class_legitimacy__disclosure_consent, suppression_requirement, 18, 0.26).
narrative_ontology:measurement_basis(dcl_disclosure_consent_su_t18, observed).
narrative_ontology:measurement(dcl_disclosure_consent_su_t21, dual_class_legitimacy__disclosure_consent, suppression_requirement, 21, 0.27).
narrative_ontology:measurement_basis(dcl_disclosure_consent_su_t21, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, information_standard).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, minority_extraction).

% DUAL FORMULATION NOTE:
% The colloquial label 'dual-class legitimacy' decomposes into three structurally distinct constraint stories per the ε-invariance principle: disclosure_consent (this file), founder_stewardship, and minority_extraction. Each authors its own ε, beneficiary structure, and claimed type over the shared referent — the standing dual-class arrangement. The disclosure settlement is upstream of both siblings: it is the procedural vehicle through which stewardship structures are admitted at IPO, and it is the burden-shifting device that minority-extraction claims must overcome (they must show consent failed, not merely that disparity exists). All family members are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
