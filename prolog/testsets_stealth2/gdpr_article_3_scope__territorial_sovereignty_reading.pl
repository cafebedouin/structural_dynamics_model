% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__territorial_sovereignty_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3 Scope — Territorial Sovereignty Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This story instantiates the territorial sovereignty reading of the GDPR
 *   Article 3 scope kernel: the claim that regulatory jurisdiction terminates
 *   at the border and that Article 3(2)'s targeting and monitoring tests —
 *   under which controllers with no establishment in the Union are fined,
 *   ordered, and restructured — exceed legitimate regulatory authority. The ε
 *   referent is the standing arrangement under contest: the extraterritorial
 *   application regime as it has operated since May 2018, assessed by this
 *   reading's own lights, hence the high extractiveness. The claim/metric
 *   pair is authored independently: the arrangement is claimed tangled_rope
 *   because a genuine protective coordination function (enforceable rights
 *   for roughly 450 million residents against real abuses) is fused with
 *   asymmetric extraction (costs and authority imposed on actors and polities
 *   outside the legislating constituency) held together by active
 *   enforcement. The expected structural delta is carried as follows: data
 *   localization appears throughout as the resistance mechanism third states
 *   deploy; jurisdictional conflict escalation drives the suppression
 *   trajectory; and the interest this reading exists to protect — non-EU
 *   state regulatory independence — is recorded among the vindicated
 *   propositions and in the axioms, not folded into the standing
 *   arrangement's beneficiary structure, because under the standing
 *   arrangement that interest is what is displaced. KEY AGENTS (by structural
 *   relationship): - eu_data_protection_authorities: agenda-setter
 *   (institutional/arbitrage) — sets scope guidelines and enforcement
 *   posture; collects authority and fine revenue - eu_resident_data_subjects:
 *   primary beneficiary (organized/constrained) — hold enforceable claims
 *   against processing anywhere; bear little direct cost - eu_digital_firms:
 *   dual-positioned beneficiary/payer (powerful/constrained) — gain a
 *   compliance-cost floor under foreign rivals while paying their own -
 *   non_eu_controllers_processors: primary target (powerful/trapped) — bear
 *   compliance and enforcement exposure; cannot abandon the EU market nor
 *   contract out of the targeting/monitoring tests - third_state_governments:
 *   target (institutional/constrained) — regulatory discretion inside their
 *   borders displaced; respond with blocking statutes, localization mandates,
 *   reciprocal statutes - third_country_data_subjects: excluded
 *   (powerless/trapped) — would claim symmetric rights; offered none -
 *   privacy_ngos_eu: beneficiary (organized/identity_locked) — litigation
 *   engine fused with the enforcement project - international_law_scholars:
 *   analytical observer — the seat from which this reading is principally
 *   articulated
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.74).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.7).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Scope — Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, '7282478d-4b59-41d1-94f5-b025b43c7f3b').
narrative_ontology:cs_kernel_codification('7282478d-4b59-41d1-94f5-b025b43c7f3b', fixed_text).
narrative_ontology:cs_authority_grounding('7282478d-4b59-41d1-94f5-b025b43c7f3b', extraction).
narrative_ontology:cs_interpretation_layer_present('7282478d-4b59-41d1-94f5-b025b43c7f3b').
narrative_ontology:cs_reading_relation('7282478d-4b59-41d1-94f5-b025b43c7f3b', gdpr_article_3_scope__effects_jurisdiction_reading, forecloses).
narrative_ontology:cs_reading_relation('7282478d-4b59-41d1-94f5-b025b43c7f3b', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('7282478d-4b59-41d1-94f5-b025b43c7f3b', foundational, regulatory_authority_terminates_at_border).
narrative_ontology:cs_axiom_status(regulatory_authority_terminates_at_border, holdable).
narrative_ontology:cs_axiom_grounding('7282478d-4b59-41d1-94f5-b025b43c7f3b', regulatory_authority_terminates_at_border, conventional).
narrative_ontology:cs_axiom('7282478d-4b59-41d1-94f5-b025b43c7f3b', foundational, law_without_representation_is_illegitimate).
narrative_ontology:cs_axiom_status(law_without_representation_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('7282478d-4b59-41d1-94f5-b025b43c7f3b', law_without_representation_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('7282478d-4b59-41d1-94f5-b025b43c7f3b', westphalian_territorial_jurisdiction).
narrative_ontology:cs_drift_state('7282478d-4b59-41d1-94f5-b025b43c7f3b', post_gdpr_extraterritorial_enforcement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7282478d-4b59-41d1-94f5-b025b43c7f3b', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_resident_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_digital_firms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_controllers_processors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, third_state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, privacy_ngos_eu).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_digital_firms).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, territorial_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, democratic_accountability_of_the_governed).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulatory_independence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National supervisory authorities and the European Data Protection Board set enforcement priorities, publish guidelines on when organizations outside the Union fall within scope, and impose administrative fines calculated as a share of worldwide turnover. Their budgets, staffing, and institutional weight grow with the caseload the broad scope generates, and they can revise guidelines or enforcement posture at will; no comparable body can revise them back.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, arbitrage, continental).

% People living in the Union hold enforceable rights — access, erasure, portability, objection — over personal data processed by organizations anywhere that offer them services or monitor their behavior. They pay little directly; complaints are free, and NGOs litigate on their behalf. Their practical recourse runs through the authorities and courts of their own member state.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_resident_data_subjects, beneficiary,
    organized, biographical, constrained, continental).

% Firms headquartered in the Union operate under the same rulebook as their foreign competitors, which floors rivals' compliance costs and, as foreign jurisdictions adopt comparable rules, exports standards those rivals must meet twice. They nonetheless carry their own substantial compliance expenditure and enforcement exposure, so they gain from the arrangement's competitive edge while paying into it.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_digital_firms, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, eu_digital_firms, payer).

% Organizations headquartered outside the Union that offer services to people in the Union or monitor their behavior must appoint representatives, maintain records, answer requests, and absorb fines computed on global turnover — regardless of physical presence in Europe. Leaving the market means abandoning hundreds of millions of customers; staying means compliance on terms they had no part in setting. Litigation and lobbying are the remaining levers.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_controllers_processors, payer,
    powerful, biographical, trapped, global).

% Governments outside the Union find organizations operating inside their borders answerable to a foreign regulator for conduct toward people abroad. Their formal options arrived after enactment: diplomatic objection, blocking statutes penalizing compliance with foreign orders, data-localization mandates, and reciprocal extraterritorial statutes of their own. None of these withdraws the obligation; each raises its cost.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, third_state_governments, payer,
    institutional, generational, constrained, national).

% Individuals living outside the Union whose data is processed by the same global services hold no comparable rights in most jurisdictions: the arrangement protects Europeans' data processed anywhere but offers them nothing for their own data processed identically. They had no representative in the legislative process and no procedural vehicle; their recourse is whatever their home state eventually negotiates.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, third_country_data_subjects, excluded,
    powerless, biographical, trapped, global).

% Non-profit litigators and advocacy organizations fund strategic complaints, intervene in landmark cases, and supply much of the enforcement pipeline the authorities' docket rides on. Their funding, reputation, and professional identities are built around the enforcement project; a material narrowing of scope would unsettle the premise of their institutional existence.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, privacy_ngos_eu, beneficiary,
    organized, generational, identity_locked, continental).

% Academic public lawyers and legal commentators map the jurisdictional argument in both directions — documenting the enforcement gap the arrangement answers and cataloguing the sovereignty objections raised against it. The territorial reading is articulated principally from this seat; the seat observes and publishes but decides nothing.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__territorial_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, enforceable rule-set for any organization anywhere that offers goods or services to people in the EU or monitors their behavior, closing the gap left when processing migrates beyond the reach of any one national regulator.
% TRANSFER_FUNCTION: Moves compliance costs, enforcement exposure, and standard-setting deference from controllers headquartered outside the EU (and from the regulatory discretion of their home states) toward EU institutions, member-state budgets (fine proceeds), and EU residents' enforceable claims.
% ABSENT_VOICES: Third-country governments had no vote in the regulation that binds organizations within their borders and enter only afterward through objection and counter-legislation; third-country data subjects receive no symmetric rights over data processed in their own jurisdictions; non-EU small exporters were represented only through trade associations that negotiated around, not against, the scope provision.
% DISAPPEARANCE_RATIONALE: Overnight removal of the extraterritorial application would return every non-EU processor to its home regime, strand EU residents' claims against foreign-processed data, dissolve the fine stream and the adequacy lever, and force privacy governance into treaty negotiation and bilateral comity — a wholesale reorganization of who governs cross-border data.
% FOUNDING_PROBLEM: After the Court of Justice invalidated the Safe Harbor arrangement in 2015, personal data flowing from Europe to United States platforms was governed by no enforceable rule; the GDPR's drafters answered by writing jurisdiction into the regulation itself — anyone targeting or monitoring people in the Union would be bound, wherever located.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: third-country regulators concede cross-border enforcement gaps (OECD cooperation frameworks presuppose them); multinational firms attest the gap while disputing the remedy; the Schrems II judgment and successor litigation document continuing transfer-law instability. No party seriously contends the pre-2018 gap was fictional; the dispute is over whether the drawn remedy exceeds legitimate authority.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.74 by this reading's lights: compliance architectures, fine exposure to 4% of global turnover, and processing bans are imposed on organizations that had no voice in enacting them, in territories whose governments formally objected, with no reciprocal exposure for EU firms facing mirror-image statutes — authority exercised without representation is the reading's definition of extraction, and the standing arrangement exhibits it at scale. Suppression (0.70) is the coercive maintenance layer: the arrangement persists not by participant preference but by fine escalation, cease-processing orders, and market-access leverage; third states cannot veto it and can only raise its cost. Theater (0.35) reflects a real enforcement core — landmark fines answered documented abuses — overlaid with a growing performative layer: penalties uncollectable against firms without Union assets, compliance-paperwork industries, and jurisdictional assertions aimed at audiences rather than outcomes. Accessibility_collapse (0.50): alternatives remain live and argued — adequacy-plus-mutual-legal-assistance models, convention-based comity, the market-access reframing — so understanding the arrangement does not collapse its alternatives. Resistance (0.72) is organized and state-backed: blocking statutes, data-localization mandates, the Schrems II line of litigation, and the diplomatic record of third-country objections; this story is itself an artifact of that resistance. All three tracked metrics run on one shared eight-point grid (2018–2025) so no metric row borrows another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the eu_data_protection_authorities seat the arrangement is a protection architecture it built, funds, and staffs — coordination it administers. From the third_state_governments seat the same instruments are unauthorized governance of domestic conduct: a foreign legislature writing rules for organizations operating in their territory. From the non_eu_controllers_processors seat it is coerced compliance with no exit that preserves the market; from eu_resident_data_subjects it is nearly pure protection. eu_digital_firms straddle: net winners on the leveled field, payers on their own compliance. The engine derives these directionalities from the declared beneficiary/victim structure and exit options; the divergence between the administrator's seat and the displaced-sovereignty seat is the perspectival fact this story exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the subsidized end: authorities collect authority, budgets, and fine revenue (d near 0); data subjects collect enforceable rights at negligible direct cost; EU firms collect a cost floor under foreign competitors while paying their own compliance — dual-positioned, near symmetric. Targets sit at the extracted end: non-EU controllers bear the transfer with trapped exit (the EU market cannot be abandoned, the targeting/monitoring tests cannot be contracted out of); third-state governments bear displaced regulatory discretion with no exit at all — sovereignty cannot be relocated, only defended through costly counter-measures, which is why their d sits nearest the full-target end despite institutional power. Third-country data subjects are excluded rather than coordinated: the arrangement neither protects nor charges them, and their absence is part of its structure. The interest this reading protects — non-EU state regulatory independence — is therefore a casualty of the standing arrangement and a beneficiary only of the reading's endorsed alternative; that placement is deliberate and is recorded in the vindicated propositions and axioms rather than reversed inside the beneficiary declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both failure modes. Reading the arrangement as a mountain — 'jurisdiction naturally follows effects,' 'borders are obsolete for data' — would naturalize the extraction and immunize it from the legitimacy challenge this reading mounts; emerges_naturally is false and the resistance record refutes naturality. Reading it as a snare would erase the genuine protective function that EU residents undeniably receive and that even this reading's corroborators concede answers a real gap. Tangled_rope holds both truths: coordination and extraction through one structure, actively enforced. On obsolescence: the founding problem (cross-border enforcement gap) is live, so no mandatrophy verdict is due; but the trajectory is worth watching — if comity conventions and adequacy networks mature into negotiated coordination, the unilateral assertion could sunset into a transitional arrangement, and if enforcement becomes predominantly theatrical (uncollectable fines, paperwork), inertial drift follows. The theater_ratio series is the early-warning instrument for that second path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the gdpr_article_3_scope kernel — what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'No empirical resolution — the readings are competing commitments over one fixed text (Article 3). Resolution arrives only as one reading wins institutional adoption (CJEU dicta, EDPB guideline revision, treaty codification); until then the family is linked via network.affects_constraints and each file keeps its own ε.',
    'Adopting the effects reading would relocate non-EU actors from target seats to protected seats and drive this story''s ε toward the low end; adopting the market-access reading would dissolve the jurisdictional question entirely — no assertion, hence no overreach — and recast the extraction as ordinary market conditionality. The beneficiary structure flips or evaporates accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer routing: one reading of gdpr_article_3_scope; sibling deltas and the disagreement locus (status of the targeting/monitoring tests).').

omega_variable(
    lotus_default_viability,
    'Does the territorial bound remain the operative default rule of international jurisdiction, or has state practice already superseded it with effects-based and market-access norms?',
    'Survey of ICJ/PCIJ jurisprudence, ILC work on jurisdiction and effects doctrine, and systematic state-practice collections; codification or express rejection in a multilateral instrument would settle it.',
    'If superseded, this reading''s reference frame is historically obsolete, its foreclosure edge weakens, and the standing arrangement needs no exceptional justification; if intact, the standing arrangement is a deviation from the default that carries the burden of justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lotus_default_viability, empirical, 'Whether westphalian territorial jurisdiction is still the live default of customary international law.').

omega_variable(
    reciprocity_asymmetry_test,
    'Would the Union accept mirror-image application — third states regulating EU-headquartered firms for effects on foreign residents — as readily as it asserts its own reach?',
    'Compare the Union''s positions on third-country extraterritorial statutes (United States CLOUD Act discovery demands, Chinese data-security assertions, Russian localization counter-measures): systematic opposition to mirror-image claims while maintaining its own reach evidences asymmetry.',
    'Confirmed asymmetry strengthens the extraction component of the tangled_rope computation and this reading''s core objection; demonstrated willingness to accept reciprocity would support a universal-principle reading and lower effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_asymmetry_test, empirical, 'Reciprocity test distinguishing universal principle from one-way advantage.').

omega_variable(
    localization_resistance_efficacy,
    'Does data localization actually reduce the effective reach of the extraterritorial application, or does it merely raise costs while compliance proceeds regardless?',
    'Compare enforcement outcomes, fine collection, and compliance rates in localizing versus non-localizing third states over the interval; natural experiment supplied by states that adopted localization mid-interval.',
    'If localization fails, the resistance metric overstates the effective check and the arrangement drifts toward uncontested extraction; if it works, the territorial bound retains operational force and the conflict-escalation trajectory is a bargaining dynamic rather than capitulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(localization_resistance_efficacy, empirical, 'Efficacy of the principal resistance mechanism this reading''s structural delta predicts.').

omega_variable(
    conflict_trajectory_endpoint,
    'Does jurisdictional conflict escalate toward systemic fragmentation (rival data-empires, splintered internet governance) or resolve into negotiated comity (conventions, adequacy networks, mutual recognition)?',
    'Track treaty initiatives, WTO e-commerce negotiations, bilateral adequacy decisions, and mutual legal assistance modernization through the next decade.',
    'Fragmentation vindicates this reading''s warning and hardens the territorial reference frame; negotiated comity would show the standing arrangement internalizing the bound, converting the conflict into transition — a transitional-support-shaped endpoint rather than permanent hybrid extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conflict_trajectory_endpoint, conceptual, 'Endpoint of the escalation dynamic: fragmentation versus negotiated comity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 2018, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement_basis(gdpr_tr_t2018, observed).
narrative_ontology:measurement(gdpr_tr_t2019, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement_basis(gdpr_tr_t2019, observed).
narrative_ontology:measurement(gdpr_tr_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement_basis(gdpr_tr_t2020, observed).
narrative_ontology:measurement(gdpr_tr_t2021, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2021, 0.27).
narrative_ontology:measurement_basis(gdpr_tr_t2021, observed).
narrative_ontology:measurement(gdpr_tr_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2022, 0.3).
narrative_ontology:measurement_basis(gdpr_tr_t2022, observed).
narrative_ontology:measurement(gdpr_tr_t2023, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2023, 0.32).
narrative_ontology:measurement_basis(gdpr_tr_t2023, observed).
narrative_ontology:measurement(gdpr_tr_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2024, 0.33).
narrative_ontology:measurement_basis(gdpr_tr_t2024, observed).
narrative_ontology:measurement(gdpr_tr_t2025, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2025, 0.35).
narrative_ontology:measurement_basis(gdpr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement_basis(gdpr_be_t2018, observed).
narrative_ontology:measurement(gdpr_be_t2019, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2019, 0.62).
narrative_ontology:measurement_basis(gdpr_be_t2019, observed).
narrative_ontology:measurement(gdpr_be_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(gdpr_be_t2020, observed).
narrative_ontology:measurement(gdpr_be_t2021, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2021, 0.69).
narrative_ontology:measurement_basis(gdpr_be_t2021, observed).
narrative_ontology:measurement(gdpr_be_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2022, 0.71).
narrative_ontology:measurement_basis(gdpr_be_t2022, observed).
narrative_ontology:measurement(gdpr_be_t2023, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2023, 0.72).
narrative_ontology:measurement_basis(gdpr_be_t2023, observed).
narrative_ontology:measurement(gdpr_be_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2024, 0.73).
narrative_ontology:measurement_basis(gdpr_be_t2024, observed).
narrative_ontology:measurement(gdpr_be_t2025, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement_basis(gdpr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement_basis(gdpr_su_t2018, observed).
narrative_ontology:measurement(gdpr_su_t2019, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2019, 0.48).
narrative_ontology:measurement_basis(gdpr_su_t2019, observed).
narrative_ontology:measurement(gdpr_su_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement_basis(gdpr_su_t2020, observed).
narrative_ontology:measurement(gdpr_su_t2021, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2021, 0.59).
narrative_ontology:measurement_basis(gdpr_su_t2021, observed).
narrative_ontology:measurement(gdpr_su_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2022, 0.63).
narrative_ontology:measurement_basis(gdpr_su_t2022, observed).
narrative_ontology:measurement(gdpr_su_t2023, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2023, 0.66).
narrative_ontology:measurement_basis(gdpr_su_t2023, observed).
narrative_ontology:measurement(gdpr_su_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2024, 0.68).
narrative_ontology:measurement_basis(gdpr_su_t2024, observed).
narrative_ontology:measurement(gdpr_su_t2025, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2025, 0.7).
narrative_ontology:measurement_basis(gdpr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the ε-invariance principle: the colloquial label 'GDPR's extraterritorial scope' covers three structurally distinct claims, written as three files sharing one kernel (gdpr_article_3_scope) and one ε referent (the standing extraterritorial-application arrangement) with reading-indexed ε values. This file is the territorial sovereignty reading (high ε: the arrangement is unauthorized imposition). The effects jurisdiction reading (low ε: the arrangement is legitimate protection) is the upstream sibling whose doctrinal success created the standing arrangement this reading contests; the market access reading (moderate ε: reframing rather than endorsement or condemnation) exerts reframing pressure on both. Each story carries its own beneficiaries, victims, and claimed type; no story hedges ε across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
