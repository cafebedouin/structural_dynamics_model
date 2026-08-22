% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__market_access_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: gdpr_article_3_scope__market_access_reading
 *   human_readable: GDPR Article 3 Scope — Conditional Market Access Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested GDPR Article 3(2)
 *   scope kernel: the market-access reading, under which extraterritorial
 *   application is not a jurisdictional claim over foreign soil but a
 *   condition attached to voluntary participation in the EU's consumer
 *   market. A firm outside the EU is never compelled by force of sovereign
 *   command; it is offered a choice — comply with GDPR-grade protections or
 *   forfeit access to roughly 450 million consumers. On this reading the
 *   'extraterritoriality' controversy dissolves into ordinary market-access
 *   conditionality, no different in kind from a country requiring imported
 *   goods to meet its safety standards. The beneficiary structure follows: EU
 *   data subjects get uniform protection, EU-based firms get a competitive
 *   floor that already matches their existing compliance posture, and the EU
 *   as a regulatory actor gains outsized influence over global
 *   data-governance norms via the Brussels Effect — third countries emulate
 *   GDPR because harmonizing lowers compliance friction for their own
 *   exporters, not because they are legally bound to. The payers are foreign
 *   firms and processors who must build compliance infrastructure to retain
 *   market access, and the excluded are the smallest foreign entrants for
 *   whom that fixed cost forecloses entry altogether. This is a Tangled Rope,
 *   not a Rope: it does coordinate a genuine market — a uniform baseline
 *   avoiding a race-to-the-bottom — but it does so by imposing asymmetric
 *   compliance costs on parties who have no vote in the standard, backed by
 *   real enforcement (fines, EU representative requirements, processor
 *   liability chains). It is deliberately NOT decomposed further because
 *   market-access framing is itself the single coherent claim being modeled
 *   here; the effects-jurisdiction and territorial-sovereignty readings are
 *   separate constraints with their own epsilon values, linked by
 *   network.affects_constraints, not variants folded into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.42).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.38).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3 Scope — Conditional Market Access Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, 'a65a45fc-23d2-4c19-a636-20754986ae5e').
narrative_ontology:cs_kernel_codification('a65a45fc-23d2-4c19-a636-20754986ae5e', fixed_text).
narrative_ontology:cs_authority_grounding('a65a45fc-23d2-4c19-a636-20754986ae5e', extraction).
narrative_ontology:cs_interpretation_layer_present('a65a45fc-23d2-4c19-a636-20754986ae5e').
narrative_ontology:cs_reading_relation('a65a45fc-23d2-4c19-a636-20754986ae5e', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a65a45fc-23d2-4c19-a636-20754986ae5e', gdpr_article_3_scope__territorial_sovereignty_reading, influences).
narrative_ontology:cs_axiom('a65a45fc-23d2-4c19-a636-20754986ae5e', foundational, market_participation_is_voluntary_consideration).
narrative_ontology:cs_axiom_status(market_participation_is_voluntary_consideration, holdable).
narrative_ontology:cs_axiom_grounding('a65a45fc-23d2-4c19-a636-20754986ae5e', market_participation_is_voluntary_consideration, conventional).
narrative_ontology:cs_axiom('a65a45fc-23d2-4c19-a636-20754986ae5e', secondary, conditional_access_is_not_jurisdictional_command).
narrative_ontology:cs_axiom_status(conditional_access_is_not_jurisdictional_command, holdable).
narrative_ontology:cs_axiom_grounding('a65a45fc-23d2-4c19-a636-20754986ae5e', conditional_access_is_not_jurisdictional_command, conventional).
narrative_ontology:cs_reference_frame('a65a45fc-23d2-4c19-a636-20754986ae5e', territorial_jurisdiction_baseline).
narrative_ontology:cs_drift_state('a65a45fc-23d2-4c19-a636-20754986ae5e', post_gdpr_enforcement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a65a45fc-23d2-4c19-a636-20754986ae5e', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_data_protection_authorities).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_based_compliant_firms).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_citizens_data_subjects).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, non_eu_smes_targeting_eu_market).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, third_country_data_processors).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, brussels_effect_standard_diffusion_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces Article 3(2)'s targeting/monitoring test as a condition of market access rather than a claim of sovereign jurisdiction: any firm that wants to offer goods/services to, or monitor the behavior of, people in the EU must comply with GDPR as the price of admission to that market. Frames extraterritorial reach as a standard-setting externality of market size, not an assertion of authority over foreign territory.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Already built to the GDPR standard by default since compliance is the cost of operating in their home market. Benefit competitively when foreign rivals must retrofit compliance to access the same customers, and benefit further when GDPR becomes the de facto template other jurisdictions copy, lowering the cost of expanding into markets that harmonize toward the EU standard.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_based_compliant_firms, beneficiary,
    organized, generational, mobile, continental).

% Receive a uniform floor of data protection regardless of where the processor is physically located, because any firm reaching them commercially is pulled inside the same rules. Their leverage is that market access to them is worth complying for; they do not litigate abroad, the market-access mechanism does the work.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_citizens_data_subjects, beneficiary,
    organized, biographical, constrained, continental).

% Small and mid-sized firms outside the EU that want EU customers must build GDPR-grade compliance programs (data mapping, consent flows, breach notification, EU representative appointment) even though they have no physical presence in the EU and no vote in how the rules are set. Their choice is compliance cost or forfeiting the EU market; for firms with thin margins the fixed compliance cost is proportionally much heavier than for large multinationals.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_smes_targeting_eu_market, payer,
    moderate, biographical, constrained, global).

% Process EU-resident data on behalf of controllers and are pulled into the compliance chain (processor obligations, audit rights, sub-processor liability) purely because their client's customers happen to be in the EU. They bear contractual and operational costs of a standard authored entirely by a jurisdiction where they have no representation, as a condition of remaining in the supply chain of firms that do want EU market access.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, third_country_data_processors, payer,
    moderate, biographical, constrained, global).

% Watch domestic firms voluntarily adopt GDPR-equivalent standards to retain EU market access, and increasingly legislate GDPR-like frameworks domestically (Brazil's LGPD, California's CCPA/CPRA, others) to reduce compliance friction for their own exporters and to capture some of the same protective framing for their own citizens.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_national_governments, observer,
    institutional, generational, analytical, national).

% The smallest foreign firms and startups for whom the fixed cost of GDPR-grade compliance exceeds any plausible EU revenue simply do not enter the EU market at all. They have no seat in the EU rulemaking process and are not consulted; the market-access framing treats their non-entry as a rational cost-benefit exit rather than a suppressed voice, but it is nonetheless a foreclosed option they never got to negotiate.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, small_foreign_market_entrants_priced_out, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__market_access_reading, diffuse).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes one uniform data-protection floor for anyone who wants to reach EU consumers or monitor EU residents, so that firms competing for the same customers face the same baseline rules regardless of where they are incorporated, avoiding a race to the regulatory bottom among jurisdictions competing for data-processing business.
% TRANSFER_FUNCTION: Moves compliance cost and operational burden from EU regulators (who would otherwise have to police extraterritorial data flows by other means) and EU firms (who already bear the cost) onto foreign firms and processors seeking access to EU customers, in exchange for market entry; also moves regulatory-design leverage toward the EU, whose standard becomes the template other jurisdictions adopt.
% ABSENT_VOICES: Non-EU legislatures and the smallest foreign entrants had no vote in drafting Article 3(2) yet must either comply or exit the market; their objection — that a jurisdiction is setting binding operational rules for firms with no domestic representation there — is real but is not treated as a jurisdictional claim under this reading because compliance is framed as voluntary market participation, not sovereign command.
% DISAPPEARANCE_RATIONALE: If Article 3(2)'s extraterritorial scope vanished, foreign firms serving EU customers would no longer need EU-grade compliance programs, EU firms would lose their compliance-parity advantage over foreign competitors, the Brussels Effect's global standard-diffusion mechanism would weaken substantially, and enforcement against foreign processors would collapse to whatever bilateral or WTO-style mechanisms could be improvised — a materially different global data-governance landscape.
% FOUNDING_PROBLEM: Data about EU residents was flowing to entities outside the EU that could not be reached by EU law under a purely territorial jurisdictional model, undermining the protection the GDPR was meant to guarantee and letting firms outside the EU compete unfairly against EU-based firms bound by the rules.
% FOUNDING_PROBLEM_CORROBORATION: Independent comparative-law scholarship (Bradford's Brussels Effect literature) and non-EU regulators who have since adopted GDPR-equivalent frameworks (Brazil, California, South Korea) corroborate from outside the EU's own institutions that the underlying problem — territorial jurisdiction failing to reach cross-border data processing — was real and that market-access leverage, not treaty-based jurisdiction, is what closed the gap; this corroboration comes from parties who are not EU beneficiaries and in some cases are themselves subject to the resulting compliance costs.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__market_access_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__market_access_reading_tests).
:- end_tests(gdpr_article_3_scope__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the mechanism is compliance-cost transfer rather than direct rent extraction — foreign firms pay for infrastructure they arguably should have built anyway if they process personal data responsibly, and they retain the option to exit the EU market entirely, which caps how extractive the arrangement can be under this reading. Suppression is moderate (0.38): there is no coercive enforcement reaching outside EU territory in the classical sense (no direct seizure of foreign assets absent EU presence), but the practical foreclosure of market access for non-compliant firms functions as a real constraint on their options. Theater ratio is low and rising slowly (0.12 to 0.22) reflecting that most compliance activity is functionally real (actual data protection built), with a growing but still modest share becoming compliance-theater as firms adopt checkbox GDPR programs designed to survive audits rather than protect data. Accessibility collapse (0.40) and resistance (0.35) are both moderate — alternatives to compliance exist (exit the EU market, geo-fence EU users) and are exercised by smaller firms, so collapse is partial, and resistance comes from foreign firms and their governments contesting the scope test in litigation and diplomatic channels, but it has not seriously threatened the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the EU regulator's seat, this looks like ordinary market-access conditionality — the same logic as any product-safety or environmental standard attached to importing goods, with no sovereignty overreach because no one is compelled to sell into the EU market. From a non-EU SME's seat with a thin margin, the same structure looks like an externally imposed compliance tax attached to a market they need but did not help design. The engine should register this divergence as a computed seat split, not resolve it by picking a winner — the market-access reading's claim is precisely that the conditionality framing is descriptively accurate, not that the cost to payers is zero.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data protection authorities are the agenda-setters administering the market-access condition. EU-based firms and EU data subjects are beneficiaries: firms via competitive parity and the Brussels Effect's export of their compliance advantage, subjects via protection that follows them regardless of processor location — low directionality (near-beneficiary) for both. Non-EU SMEs and third-country processors are payers: they bear the compliance cost as an entry condition to a market they don't set rules for — higher directionality (near-target), though their exit option (forfeit the EU market) is real and keeps them from the most extreme extraction end, unlike a trapped party with no exit. The smallest foreign entrants who are priced out entirely sit outside the transactional relationship altogether — they are excluded, not extracted from, which is a structurally different position the market-access reading treats as a cost-benefit exit rather than a suppression event.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — data flowing beyond the reach of EU protection because of purely territorial jurisdictional limits — remains live: cross-border data flows have only grown since 2018, and the market-access mechanism continues to close a real enforcement gap. This is not a case of an arrangement outliving its function; if anything the function (harmonizing global privacy floors via market leverage rather than direct extraterritorial command) has proven durable and self-reinforcing as more jurisdictions adopt GDPR-equivalent rules. Classifying this as Tangled Rope rather than Snare avoids mislabeling a genuine (if asymmetric) coordination achievement as pure extraction; classifying it as Tangled Rope rather than Rope avoids pretending the asymmetric compliance burden on unrepresented foreign parties is costless coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_access_vs_jurisdictional_assertion,
    'Is characterizing Article 3(2) as ''conditional market access'' rather than ''extraterritorial jurisdiction'' a substantive structural distinction, or a rhetorical relabeling of the same coercive mechanism?',
    'Examine enforcement mechanics: if the EU only ever enforces via market exclusion (fines collectable from EU-linked assets, blocking of EU market access) and never attempts to compel action from a firm with zero EU nexus, the market-access reading holds structurally. If enforcement extends to firms with no EU assets or market presence (e.g., through mutual legal assistance treaties compelling foreign courts to enforce EU fines), the reading collapses into effects-jurisdiction.',
    'If the distinction is substantive, this reading''s lower extractiveness and suppression scores are justified and the arrangement is best modeled as conditional coordination. If the distinction is rhetorical, this constraint and the effects_jurisdiction_reading describe the same underlying mechanism and should probably be understood as two framings of one higher-extraction constraint rather than genuinely different structural claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_access_vs_jurisdictional_assertion, conceptual, 'Whether market-access framing is a real structural feature or a relabeling of extraterritorial jurisdiction.').

omega_variable(
    brussels_effect_durability,
    'Does the Brussels Effect (voluntary global adoption of GDPR-equivalent standards) persist as EU market share of the global digital economy declines relative to other blocs, or is the standard-diffusion beneficiary structure contingent on current EU market weight?',
    'Track adoption rates of GDPR-equivalent frameworks in jurisdictions as EU share of global GDP/digital consumer spending shifts over the next decade; a Brussels Effect that persists despite declining EU market share would indicate genuine standard-setting authority beyond raw market leverage.',
    'If the beneficiary effect (EU regulatory influence via diffusion) is purely a function of current market size, the vindicated proposition (brussels_effect_standard_diffusion_thesis) is contingent rather than structural, and the beneficiary structure claimed here would erode as EU relative market power declines.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brussels_effect_durability, empirical, 'Whether EU standard-setting influence is durable or contingent on current market share.').

omega_variable(
    excluded_smallest_entrants_representation,
    'Should the smallest foreign entrants priced out of the EU market entirely count as suppressed parties (their exit was not meaningfully free given fixed compliance costs) or as parties for whom the cost-benefit calculation legitimately favored non-entry?',
    'Compare compliance cost as a share of expected EU revenue across firm sizes; if the ratio is systematically prohibitive only for firms below a size threshold with no correlation to actual data-protection risk posed, this suggests structural exclusion rather than rational exit.',
    'If exclusion is structural, the market-access reading''s framing of non-entry as voluntary understates real suppression for smaller economic actors, which would push resistance and accessibility_collapse metrics higher than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_smallest_entrants_representation, empirical, 'Whether smallest-entrant exclusion is genuine market choice or structural suppression via disproportionate fixed costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__market_access_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__market_access_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__market_access_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(gdpr_tr_t12, gdpr_article_3_scope__market_access_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(gdpr_tr_t16, gdpr_article_3_scope__market_access_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__market_access_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(gdpr_tr_t24, gdpr_article_3_scope__market_access_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__market_access_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__market_access_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__market_access_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(gdpr_be_t12, gdpr_article_3_scope__market_access_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(gdpr_be_t16, gdpr_article_3_scope__market_access_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__market_access_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(gdpr_be_t24, gdpr_article_3_scope__market_access_reading, base_extractiveness, 24, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gdpr_article_3_scope__market_access_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__territorial_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the gdpr_article_3_scope kernel. The effects_jurisdiction_reading treats Article 3(2) as a genuine (if novel) extension of legal jurisdiction based on targeting/monitoring effects on EU residents — higher claimed enforcement tension, jurisdiction as the operative concept. The territorial_sovereignty_reading treats any extraterritorial application as exceeding legitimate regulatory authority — highest claimed suppression/illegitimacy, sovereignty violation as the operative concept. This market_access_reading recharacterizes the same textual provision as conditional market access rather than jurisdictional assertion, which structurally lowers both enforcement tension and extraction relative to the effects-jurisdiction reading, because a firm retaining a genuine exit option (forfeit the EU market) is differently positioned than a firm subject to an unconditional legal command. All three readings share the same textual kernel (Article 3(2) GDPR) but diverge on what kind of authority claim it constitutes and who counts as extracted-from versus voluntarily participating.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
