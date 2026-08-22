% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__privacy_fundamental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__privacy_fundamental_reading, []).

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
 *   constraint_id: article17_erasure_right__privacy_fundamental_reading
 *   human_readable: Article 17 Erasure Right — Privacy-Fundamental Reading (Individual Data Sovereignty)
 *   domain: technology_governance/data_protection_law/competition_policy
 *
 * SUMMARY:
 *   This story instantiates the privacy-fundamental reading of the GDPR
 *   Article 17 erasure right: a fundamental-rights instrument granting
 *   individuals enforceable deletion claims, thereby limiting corporate data
 *   retention to arrangements that carry continuing justification. The
 *   standing arrangement under assessment is the erasure regime in operation
 *   from May 2018 (t=0) through 2026 (t=8). Assumptions: the analysis is
 *   anchored in EU/EEA jurisdiction with acknowledged extraterritorial
 *   spillover; heterogeneous controllers (search engines, social platforms,
 *   cloud providers, brokers) are aggregated into one cost-bearing seat
 *   because the obligation structure is uniform across them; and the
 *   requester-side burden is treated as low, per this reading's
 *   broad-erasure, low-friction commitment. KEY AGENTS (by structural
 *   relationship): see key_agents. The claim and the metrics are authored
 *   independently: claimed_type rope states this reading's structural
 *   assessment of the arrangement; the metric values describe its observed
 *   operation, including real costs borne by the constrained party.
 *
 * KEY AGENTS:
 *   - individual_data_subjects: Primary beneficiary (moderate/constrained) — holds the enforceable erasure claim the arrangement exists to deliver
 *   - platform_data_controllers: Primary cost-bearing constrained party (institutional/constrained) — operates the systems erasure claims land on and absorbs compliance costs and fines
 *   - supervisory_authorities: Agenda-setter (institutional/constrained) — administers complaints, issues guidelines, orders erasures, levies fines
 *   - digital_rights_advocacy_orgs: Secondary beneficiary (organized/identity_locked) — litigates test cases and monitors controller response
 *   - third_country_recipients: Excluded party (powerful/mobile) — bound by conflicting home-state regimes, not consulted in the arrangement's design
 *   - academic_privacy_law_scholars: Analytical observer (analytical/analytical) — tracks decisions and publishes performance analyses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.27).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.32).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.27).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "Article 17 Erasure Right — Privacy-Fundamental Reading (Individual Data Sovereignty)").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology_governance/data_protection_law/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, 'e6360419-c3dd-4f14-8767-86887d511be4').
narrative_ontology:cs_kernel_codification('e6360419-c3dd-4f14-8767-86887d511be4', fixed_text).
narrative_ontology:cs_authority_grounding('e6360419-c3dd-4f14-8767-86887d511be4', lineage).
narrative_ontology:cs_interpretation_layer_present('e6360419-c3dd-4f14-8767-86887d511be4').
narrative_ontology:cs_reading_relation('e6360419-c3dd-4f14-8767-86887d511be4', article17_erasure_right__competitive_moat_reading, influences).
narrative_ontology:cs_reading_relation('e6360419-c3dd-4f14-8767-86887d511be4', article17_erasure_right__censorship_mechanism_reading, influences).
narrative_ontology:cs_axiom('e6360419-c3dd-4f14-8767-86887d511be4', foundational, personal_data_remains_under_individual_normative_control).
narrative_ontology:cs_axiom_status(personal_data_remains_under_individual_normative_control, holdable).
narrative_ontology:cs_axiom_grounding('e6360419-c3dd-4f14-8767-86887d511be4', personal_data_remains_under_individual_normative_control, deontological).
narrative_ontology:cs_axiom('e6360419-c3dd-4f14-8767-86887d511be4', secondary, retention_requires_continuing_justification).
narrative_ontology:cs_axiom_status(retention_requires_continuing_justification, holdable).
narrative_ontology:cs_axiom_grounding('e6360419-c3dd-4f14-8767-86887d511be4', retention_requires_continuing_justification, deontological).
narrative_ontology:cs_reference_frame('e6360419-c3dd-4f14-8767-86887d511be4', informational_self_determination_baseline).
narrative_ontology:cs_drift_state('e6360419-c3dd-4f14-8767-86887d511be4', contemporary_post_cjeu_balancing_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e6360419-c3dd-4f14-8767-86887d511be4', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, individual_data_subjects).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, digital_rights_advocacy_orgs).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, platform_data_controllers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold personal data across search indexes, social platforms, cloud services, and data brokers. Before 2018 they could ask for deletion and be ignored; since then they can file a structured erasure request the controller must answer within one month, with escalation to the national supervisory authority and a penalty backdrop. They cannot leave the data economy, and most cannot determine which controllers hold what; the request channel is their principal lever. Requests sometimes stall on identity-verification demands or broad exemption invocations, and the burden of justifying a refusal sits formally on the controller.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, individual_data_subjects, beneficiary,
    moderate, biographical, constrained, continental).

% Operate the storage, indexing, and processing systems that erasure claims land on. Each request triggers identity verification, propagation across caches and backups, third-party notification, and audit logging. They maintain dedicated erasure pipelines and legal teams, absorb administrative fines when responses fall short, and lobby actively in legislative reviews over scope and exemptions. Exiting the EU market is not realistic at their scale, and jurisdictional restructuring offers only partial relief.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, platform_data_controllers, payer,
    institutional, generational, constrained, global).

% National data protection authorities and the European Data Protection Board receive complaints, issue guidelines, order erasures, and levy fines. Caseloads grew sharply after 2018 and budget and staffing expansion has been uneven across member states. They are bound by statutory mandates, proportionality duties, and court review; they cannot decline the docket the arrangement hands them.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, supervisory_authorities, agenda_setter,
    institutional, generational, constrained, continental).

% Litigate test cases, file complaints on behalf of individuals, and publish scorecards on controller response times and grant rates. Their casework, standing, and funding depend on the erasure claim remaining robust; several organizations were founded around precisely this mandate and have no comparable docket if the claim narrows.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, digital_rights_advocacy_orgs, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__privacy_fundamental_reading, digital_rights_advocacy_orgs, observer).

% Services and archives outside the GDPR's territorial scope receive erasure requests routed through global platforms but answer to home-state laws with no equivalent duty and sometimes conflicting preservation orders. They are rarely consulted when European rules are drafted, yet must reconcile contradictory demands, block EU-origin traffic, or absorb legal exposure.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, third_country_recipients, excluded,
    powerful, biographical, mobile, global).

% Track supervisory decisions, Court of Justice rulings, and compliance statistics; publish analyses of how the erasure regime performs against its stated aims; testify in parliamentary reviews. They hold no material stake in outcomes beyond professional standing.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, academic_privacy_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__privacy_fundamental_reading, individual_data_subjects).
narrative_ontology:fixing_cost_class(article17_erasure_right__privacy_fundamental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes a uniform deletion claim across every controller in scope, replacing millions of unenforceable bilateral requests with one procedurally defined channel: one-month response deadline, defined lawful-refusal grounds, escalation path, and penalty backdrop. It addresses a collective-action problem no individual could solve alone — no single data subject can negotiate retention terms with infrastructural platforms, but a standardized right converts each private wish into an actionable, auditable demand.
% TRANSFER_FUNCTION: Moves deletion authority from controllers to data subjects: each granted request shifts control over a data trail from platform systems to the individual. It also moves compliance expenditure out of undifferentiated retention convenience and into verification, propagation, and logging operations, and removes specified data from downstream availability in search results, broker files, and backups.
% ABSENT_VOICES: Individuals outside the GDPR's territorial scope have no enforceable equivalent and no seat in the arrangement's administration; third-country recipients bound by conflicting regimes are affected but were not consulted in its design; archival and press interests are heard only through the Article 17(3) balancing exemptions rather than as seated parties.
% DISAPPEARANCE_RATIONALE: Overnight repeal would revert controller defaults to indefinite retention, dissolve the request channel and its escalation path, strand the compliance infrastructure built since 2018, and remove the leverage behind every negotiated deletion; search-index and data-broker practices would reorganize around whatever voluntary policies remained, which pre-2018 history shows to be weak.
% FOUNDING_PROBLEM: Permanent, unconsented digital memory: once personal data entered commercial systems, individuals had no workable way to end processing after purpose expiry or consent withdrawal — pre-2018 deletion duties were vague, unenforceable, and routinely ignored.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Court of Justice rulings (Google Spain; GC and Others v CNIL) independently found the deletion gap real; supervisory-authority annual reports document sustained complaint volumes years after adoption; controller impact assessments and parliamentary impact studies acknowledge the operational reality of erasure demand. No corroborating source outside the arrangement treats the founding problem as solved.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__privacy_fundamental_reading, 0.27, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__privacy_fundamental_reading_tests).
:- end_tests(article17_erasure_right__privacy_fundamental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.27: compliance costs, deletion operations, and verification burdens are real and measurable, but from this reading's lights much of the constrained party's cost is the price of relinquishing a retention privilege that was never legitimately held, leaving a bounded residual of genuine friction (over-broad identity verification, template refusals, uneven member-state handling). Suppression is 0.32: enforcement is coercive (administrative fines up to 4% of global turnover, corrective orders) but the arrangement creates an option for its beneficiaries rather than closing alternatives — the figure reflects enforcement intensity, not exit-blocking. Theater_ratio is 0.24: substantive erasure workflows now dominate; the banner-era performative compliance of 2018–2019 receded as supervisory pressure forced operational pipelines, with residual theater in dashboard veneer and boilerplate refusals. Accessibility_collapse is 0.30: the erasure claim complements rather than displaces access, portability, and rectification channels, so alternatives persist once the constraint is understood. Resistance is 0.58: sustained litigation, legislative lobbying, jurisdictional-arbitrage attempts, and conflict with third-country legal orders (preservation and disclosure duties) meet the arrangement continuously. All three tracked metric series run on one shared time grid ({0,1,2,4,6,8}) so every metric is authored at every examined point; the suppression_requirement series is included because the story specifically traces enforcement-capacity buildup from the chaotic 2018 baseline to mature supervisory operation, not merely static suppression. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the platform_data_controllers position, the arrangement is a compulsory, finely detailed operational burden enforced by fining authorities — the payer seat will register materially higher effective extraction than the story-level scalar suggests. From the individual_data_subjects position, the same structure is a hard-won lever that finally makes deletion claims answerable. The supervisory_authorities seat experiences the arrangement as a mandate it must staff and discharge. The engine derives these per-seat classifications from the structural data; this story's rope claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   individual_data_subjects sit near the beneficiary end: the arrangement subsidizes their claim-holding, and their constrained exit (no way out of the data economy) binds them to it as protected parties rather than targets. platform_data_controllers are declared cost-bearers with constrained exit, placing them toward the target end — their scale softens but does not remove the asymmetry, since market exit from the EU is unavailable and jurisdictional restructuring yields partial relief only. supervisory_authorities derive near-symmetric directionality as administrators: they collect no rents from the arrangement's operation, gaining only mandate relevance. digital_rights_advocacy_orgs sit near the beneficiary end, with identity lock amplifying their persistence in defending the claim's breadth. third_country_recipients stand outside the benefit flow entirely — their exclusion is a boundary condition of the arrangement, not a targeted extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — permanent, unconsented digital memory — remains live: data accumulation continues to compound and the retention asymmetry the arrangement addressed persists. Accordingly, mandatrophy_resolved is not declared, and no sunset structure is authored: this reading holds the arrangement to be steady-state protection, not transitional scaffolding. The rope claim guards against mislabeling the arrangement as pure extraction despite the real, concentrated costs the payer seat bears; conversely, the temporal series guards against drift — if base_extractiveness continues climbing past the modeled range in later intervals, extraction-accumulation hypotheses fire and the coordination-versus-rent question reopens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which structural reading correctly characterizes the Article 17 arrangement — this story''s privacy-fundamental instantiation, or one of the sibling instantiations (competitive_moat_reading, censorship_mechanism_reading)?',
    'Cross-story comparison: compile all three sibling stories and compare per-seat classifications, epsilon values, and victim sets; empirical markers — compliance-cost distribution by firm size for the moat reading, requester-affiliation and takedown-pattern analysis for the censorship reading — adjudicate which structural description tracks actual operation.',
    'If the moat reading dominates, epsilon rises substantially and the payer seat''s computed type hardens toward enforced extraction; if the censorship reading dominates, the victim set changes to speech audiences and suppression reweights toward content removal. This story''s low-moderate epsilon holds only under the privacy-fundamental framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'This constraint is one reading of the article17_erasure_right kernel; sibling readings instantiate different constraints with different epsilon over the same text.').

omega_variable(
    epsilon_invariance_across_request_classes,
    'Does the broad-erasure, low-friction interpretation hold epsilon invariant across request categories (expired-purpose, consent-withdrawal, public-figure balancing), or do balancing-heavy categories carry structurally different cost incidence?',
    'Disaggregated supervisory decision statistics and controller telemetry by request category and outcome; if balancing-sensitive categories show systematically different grant rates, latency, or appeal rates, decompose into per-category sub-constraints linked through the network.',
    'Non-invariance would split this story into a clean-sovereignty core and a balancing-contested periphery with distinct epsilon values, changing the constraint-family topology and dating type transitions differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_invariance_across_request_classes, empirical, 'Whether a single epsilon covers all erasure-request classes under the broad interpretation.').

omega_variable(
    enforcement_capacity_plateau_or_ratchet,
    'Will the rising enforcement-capacity trajectory captured in the suppression_requirement series plateau at a mature-equilibrium level or continue ratcheting?',
    'Supervisory budget and staffing series, fine-frequency data, and adequacy-review cycles beyond the modeled interval.',
    'A continued ratchet pushes the payer seat''s effective extraction upward and could flip the computed payer-seat type toward tangled_rope even while the beneficiary seat holds rope; a plateau supports the current profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_plateau_or_ratchet, empirical, 'Trajectory of enforcement intensity underlying the suppression series.').

omega_variable(
    scope_attribution_global_vs_continental,
    'Is the arrangement''s operative scope continental (EU/EEA jurisdiction with incidental spillover) or genuinely global (Brussels-effect standardization that controllers engineer worldwide)?',
    'Compare controller erasure-pipeline architecture inside versus outside EU jurisdiction: if deletions propagate globally regardless of requester location, scope is functionally global; if confined to EU-targeted processing, continental.',
    'Scope feeds the engine''s effective-extraction scaling; global attribution raises scaled extraction modestly for all seats and strengthens the extraterritorial-conflict reading of the third_country_recipients seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_attribution_global_vs_continental, conceptual, 'Spatial-scope attribution affecting extraction scaling and the excluded-seat analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art17pf_tr_t0, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0, 0.34).
narrative_ontology:measurement(art17pf_tr_t1, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 1, 0.31).
narrative_ontology:measurement(art17pf_tr_t2, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2, 0.29).
narrative_ontology:measurement(art17pf_tr_t4, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(art17pf_tr_t6, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(art17pf_tr_t8, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 8, 0.24).

% Extraction over time
narrative_ontology:measurement(art17pf_be_t0, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(art17pf_be_t1, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 1, 0.21).
narrative_ontology:measurement(art17pf_be_t2, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2, 0.23).
narrative_ontology:measurement(art17pf_be_t4, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 4, 0.25).
narrative_ontology:measurement(art17pf_be_t6, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 6, 0.26).
narrative_ontology:measurement(art17pf_be_t8, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 8, 0.27).

% Suppression requirement over time
narrative_ontology:measurement(art17pf_su_t0, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(art17pf_su_t1, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 1, 0.16).
narrative_ontology:measurement(art17pf_su_t2, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2, 0.2).
narrative_ontology:measurement(art17pf_su_t4, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(art17pf_su_t6, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 6, 0.29).
narrative_ontology:measurement(art17pf_su_t8, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 8, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% GDPR Article 17 is a single statutory text supporting three structurally distinct claims, decomposed per the epsilon-invariance principle into a constraint family. This file instantiates the privacy_fundamental_reading: individual data sovereignty limiting corporate retention, with individuals as primary beneficiaries, controllers as cost-bearing constrained parties, broad erasure interpretation, and low requester-side friction; its epsilon is authored low-moderate over the standing erasure arrangement as this reading assesses it. The competitive_moat_reading (compliance-cost asymmetry as incumbent protection) and censorship_mechanism_reading (strategic erasure as speech suppression) instantiate different constraints over the same text with their own epsilon values, victim sets, and classifications. This reading is structurally upstream of both: it creates the legitimacy conditions and enforcement infrastructure whose downstream surfaces the siblings describe, which is why the reading_relations declare influences rather than mere coexistence. The confusion would live in the label 'Article 17', not in the structure; the framework models it as three linked stories, not one observable-dependent story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
