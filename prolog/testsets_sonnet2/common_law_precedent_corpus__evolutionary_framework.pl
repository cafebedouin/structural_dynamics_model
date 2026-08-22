% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Precedent as Adaptive Framework (Evolutionary Reading)
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the 'evolutionary framework' reading of the
 *   common law precedent kernel: precedent is treated as an adaptive scaffold
 *   that contemporary normative evolution licenses courts to reinterpret or
 *   overrule when social consensus, empirical understanding, or moral
 *   judgment has shifted. This is a genuinely different constraint from the
 *   strict stare decisis reading (which treats departure as requiring
 *   extraordinary justification) and the pluralist balancing reading (which
 *   varies precedent weight by domain case-by-case) — under this reading,
 *   overruling is normalized as ordinary corrective practice rather than an
 *   exceptional event, and the judiciary is structurally empowered as a
 *   normative updater. Litigants gain broader pathways to challenge settled
 *   doctrine; the judiciary's practical authority expands relative to the
 *   legislature's formal law-updating role.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: institutional agenda-setter and beneficiary of expanded interpretive authority
 *   - litigants_seeking_doctrinal_change: moderate-power beneficiaries with a new viable pathway
 *   - marginalized_groups_under_outdated_precedent: powerless beneficiaries for whom this is often the only realistic avenue of relief
 *   - settled_expectation_holders and reliance_interest_parties: payers who bear the cost of retroactive-feeling doctrinal shifts
 *   - legislatures: excluded institutional actor whose formal updating role is partially displaced
 *   - legal_scholars: analytical observers of the doctrine's legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.42).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.28).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.42).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Precedent as Adaptive Framework (Evolutionary Reading)").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal_theory/jurisprudence/constitutional_law").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, 'c8d008e3-1478-4ed1-947c-e5023f418934').
narrative_ontology:cs_kernel_codification('c8d008e3-1478-4ed1-947c-e5023f418934', distributed).
narrative_ontology:cs_authority_grounding('c8d008e3-1478-4ed1-947c-e5023f418934', practice).
narrative_ontology:cs_interpretation_layer_present('c8d008e3-1478-4ed1-947c-e5023f418934').
narrative_ontology:cs_reading_relation('c8d008e3-1478-4ed1-947c-e5023f418934', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('c8d008e3-1478-4ed1-947c-e5023f418934', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('c8d008e3-1478-4ed1-947c-e5023f418934', foundational, normative_evolution_licenses_reinterpretation).
narrative_ontology:cs_axiom_status(normative_evolution_licenses_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('c8d008e3-1478-4ed1-947c-e5023f418934', normative_evolution_licenses_reinterpretation, instrumental).
narrative_ontology:cs_axiom('c8d008e3-1478-4ed1-947c-e5023f418934', foundational, judiciary_as_legitimate_normative_updater).
narrative_ontology:cs_axiom_status(judiciary_as_legitimate_normative_updater, holdable).
narrative_ontology:cs_axiom_grounding('c8d008e3-1478-4ed1-947c-e5023f418934', judiciary_as_legitimate_normative_updater, conventional).
narrative_ontology:cs_axiom('c8d008e3-1478-4ed1-947c-e5023f418934', secondary, overruling_is_ordinary_corrective_practice).
narrative_ontology:cs_axiom_status(overruling_is_ordinary_corrective_practice, holdable).
narrative_ontology:cs_axiom_grounding('c8d008e3-1478-4ed1-947c-e5023f418934', overruling_is_ordinary_corrective_practice, instrumental).
narrative_ontology:cs_reference_frame('c8d008e3-1478-4ed1-947c-e5023f418934', precedent_as_error_correcting_instrument).
narrative_ontology:cs_drift_state('c8d008e3-1478-4ed1-947c-e5023f418934', contemporary_doctrinal_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c8d008e3-1478-4ed1-947c-e5023f418934', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, litigants_seeking_doctrinal_change).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, marginalized_groups_under_outdated_precedent).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, settled_expectation_holders).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, reliance_interest_parties).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, living_constitutionalism_doctrine).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, law_as_social_instrument_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies precedent as a rebuttable framework rather than a fixed rule, treating shifts in social consensus, empirical understanding, or moral judgment as legitimate grounds to revisit prior holdings. Gains interpretive latitude and normative authority it did not hold under a strict stare decisis reading; decides when a precedent has become obsolete.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, beneficiary).

% Bring cases explicitly asking courts to overturn or narrow prior precedent, arguing changed social conditions or evolved norms. Under this reading they gain a viable pathway to challenge settled doctrine that would be foreclosed or heavily disfavored under a strict-binding reading.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, litigants_seeking_doctrinal_change, beneficiary,
    moderate, biographical, constrained, national).

% Live under precedent originally decided without their interests represented (e.g. historically exclusionary doctrines). This reading offers the only realistic route to relief — normative reinterpretation rather than waiting for legislative correction, which they often lack the political power to obtain.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, marginalized_groups_under_outdated_precedent, beneficiary,
    powerless, generational, trapped, national).

% Structured contracts, business arrangements, family arrangements, or compliance regimes around existing precedent. When courts treat precedent as revisable under evolving norms, their settled expectations can be upended retroactively or prospectively with little advance warning; they have no exit from a legal system that has already reorganized around a different rule.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, settled_expectation_holders, payer,
    moderate, biographical, trapped, national).

% Individuals and institutions who ordered their affairs — property transfers, criminal sentencing exposure, statutory interpretation reliance — on the assumption that established precedent would hold. Bear the direct cost when a court decides the doctrinal ground has shifted beneath them, often without having litigated or consented to the change.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, reliance_interest_parties, payer,
    powerless, biographical, trapped, national).

% Constitutionally the primary body for updating law to reflect changed norms, but under this reading the judiciary performs much of that updating function directly through reinterpretation, reducing the practical pressure and political urgency for legislative action. Legislatures are rarely parties before the court and have no formal voice in whether a precedent is deemed obsolete.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legislatures, excluded,
    institutional, generational, constrained, national).

% Study the doctrine of stare decisis and its exceptions, debate whether the evolutionary reading produces principled legal development or judicial policymaking under a legitimacy veneer. Provide competing accounts consumed by courts, litigants, and legislators.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__evolutionary_framework, diffuse).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__evolutionary_framework, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows the legal system to correct doctrines that have become factually obsolete, morally indefensible, or practically unworkable without requiring a formal constitutional amendment or waiting on legislative gridlock — precedent functions as a living framework rather than a fixed inheritance.
% TRANSFER_FUNCTION: Moves normative authority from the historical decision-makers (and from parties who relied on the old rule) to the contemporary judiciary and to litigants able to frame their claims as vindicated by evolved norms; reliance and settled-expectation costs are transferred from the beneficiaries of doctrinal change onto those who had ordered their affairs under the prior rule.
% ABSENT_VOICES: Legislatures, whose institutional role this reading partially displaces, have no formal voice in individual precedent-revision decisions. Parties who relied on the old rule but are not before the court in the specific overruling case (third parties to the doctrine, not the litigation) are never heard at all.
% DISAPPEARANCE_RATIONALE: If the evolutionary framework disappeared and pure strict stare decisis governed instead, courts would lose the doctrinal tools (changed-circumstances tests, reliance-interest balancing against social-change arguments) they currently use to overturn precedent; doctrinal correction would shift almost entirely to legislatures and constitutional amendment, slowing normative updates by decades in areas legislatures are reluctant to touch.
% FOUNDING_PROBLEM: Common law systems needed a mechanism to correct precedents that later generations recognized as wrong, obsolete, or unjust, without abandoning the stability that precedent-following otherwise provides — the founding problem was reconciling continuity with moral and factual error-correction.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal historians and constitutional scholars outside the judiciary corroborate that erroneous or unjust precedents (segregation-era doctrine, criminalization of same-sex relationships, outdated evidentiary science) have required correction mechanisms; dissenting judges and legislatures who prefer a strict-stare-decisis or pluralist-balancing approach do not dispute that the underlying error-correction problem exists, only that this reading is the right way to solve it.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).
:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) and rising modestly: as courts increasingly treat precedent as revisable, reliance-interest parties bear increasing exposure to retroactive doctrinal shifts they did not litigate or consent to, but the extraction is bounded because courts still require doctrinal argument, not mere preference. Suppression is comparatively low (0.28) because the reading does not suppress the alternative readings — strict stare decisis and pluralist balancing remain live judicial options in the same system, and dissenting judges routinely invoke them. Theater ratio is low-to-moderate (0.22) reflecting that most invocations of 'evolving norms' correspond to genuine doctrinal argument rather than pure rhetorical cover, though this creeps upward as the practice becomes more routine and less exceptional over time.
 *
 * PERSPECTIVAL GAP:
 *   From the appellate judiciary's seat, the practice is genuine adaptive coordination — correcting law that has become unjust or unworkable. From the settled_expectation_holders and reliance_interest_parties seats, the same practice reads as retroactive rule-change imposed without their participation. The engine should compute these as structurally different experiences of the same constraint rather than reconciling them to a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The appellate judiciary is the primary structural beneficiary: this reading expands its interpretive discretion and normative authority relative to a strict-binding regime, even though judges do not personally collect rents — the institutional seat gains power. Litigants seeking doctrinal change and marginalized groups under outdated precedent are beneficiaries because the reading creates a viable pathway for relief that would otherwise be foreclosed. Settled expectation holders and reliance interest parties are targets: they bear the cost of doctrinal instability without having agreed to it, and their exit options are trapped because withdrawing from the legal system's jurisdiction is not a real option. Legislatures are structurally sidelined rather than directly harmed — their exclusion is a displacement of institutional role rather than a direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling stability with moral/factual error correction — remains live (segregation-era precedent, outdated forensic science, evolving understandings of equal protection all required correction mechanisms), so this reading is not a pure mandatrophy case: the mandate has not obviously outlived its function. But the reading's growing use as ordinary corrective practice (rather than reserved for extraordinary cases) risks drift toward using 'evolving norms' as a general-purpose override for any precedent a given court finds inconvenient, which the rising extractiveness trend is intended to flag for downstream analysis rather than resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolutionary_reading_vs_sibling_readings_boundary,
    'Is the evolutionary framework a distinct jurisprudential commitment from strict stare decisis and pluralist balancing, or is it simply what strict stare decisis looks like after enough ''extraordinary justification'' exceptions have accumulated over time?',
    'Comparative doctrinal analysis of how courts articulate the standard for overruling precedent across jurisdictions and eras — does the evolutionary reading require a qualitatively different showing (a survey of contemporary social consensus) versus strict stare decisis''s showing (demonstrated unworkability or factual predicate failure)? Divergence in the articulated test would confirm structural distinctness.',
    'If the readings converge in practice despite different rhetoric, the three-way kernel decomposition overstates structural distinctness and the constraint family should be re-examined; if they remain distinct in application, the decomposition is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolutionary_reading_vs_sibling_readings_boundary, conceptual, 'Whether the evolutionary reading is genuinely structurally distinct from its siblings or a rhetorical variant.').

omega_variable(
    judicial_updater_legitimacy,
    'Is judicial empowerment as normative updater a legitimate exercise of adjudicative authority, or does it constitute judicial policymaking that should properly belong to legislatures under separation-of-powers principles?',
    'Track whether legislatures, when given the opportunity, ratify or reject judicially-initiated doctrinal changes; sustained legislative acquiescence over multiple sessions would suggest de facto legitimation, while active legislative override attempts would suggest illegitimate encroachment.',
    'If legislatures consistently acquiesce, the reading''s expansion of judicial authority is effectively validated by the political branches; if legislatures actively resist, the reading''s extraction from the excluded legislative seat is more severe than currently measured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_updater_legitimacy, preference, 'Whether judicial normative updating is a legitimate or illegitimate expansion of adjudicative power relative to legislative authority.').

omega_variable(
    reliance_interest_measurement,
    'How should the cost borne by settled_expectation_holders and reliance_interest_parties be measured when a precedent is overruled — is it the cost of the specific decision, or the systemic cost of reduced predictability across the whole body of law?',
    'Empirical study of transaction costs, insurance/compliance re-planning costs, and litigation volume changes in the periods immediately following high-profile precedent overrulings under this doctrinal approach versus jurisdictions with stricter stare decisis norms.',
    'If systemic predictability costs are large and diffuse, the true extraction from reliance parties is understated by the current 0.42 extractiveness score; if costs are concentrated and small, the current score may overstate them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reliance_interest_measurement, empirical, 'Whether the extractiveness measure fully captures diffuse systemic reliance costs versus only case-specific costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comm_tr_t8, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 8, 0.14).
narrative_ontology:measurement(comm_tr_t16, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 16, 0.17).
narrative_ontology:measurement(comm_tr_t24, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 24, 0.19).
narrative_ontology:measurement(comm_tr_t32, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 32, 0.21).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comm_be_t8, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(comm_be_t16, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(comm_be_t24, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(comm_be_t32, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(comm_su_t8, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 8, 0.23).
narrative_ontology:measurement(comm_su_t16, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 16, 0.24).
narrative_ontology:measurement(comm_su_t24, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 24, 0.26).
narrative_ontology:measurement(comm_su_t32, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 32, 0.27).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__evolutionary_framework, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the common_law_precedent_corpus kernel, each authored as a separate constraint per the epsilon-invariance principle: strict_stare_decisis (precedent binds as backward constraint requiring extraordinary justification to depart), pluralist_balancing (precedent weight varies by domain, balanced case-by-case), and this evolutionary_framework reading (precedent as adaptive framework, overruling normalized as corrective, judiciary empowered as normative updater). The three share the same underlying kernel — the doctrine of precedent itself — but diverge in beneficiary/victim structure, extractiveness, and suppression: this reading shows lower suppression (0.28) than strict stare decisis would (which suppresses litigant pathways for challenge more heavily) but somewhat higher extractiveness directed at reliance-interest parties than pluralist balancing would (which distributes the cost of unpredictability more evenly by domain).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
