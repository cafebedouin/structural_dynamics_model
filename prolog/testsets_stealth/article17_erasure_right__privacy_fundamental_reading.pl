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
 *   constraint_id: article17_erasure_right__privacy_fundamental_reading
 *   human_readable: GDPR Article 17 Erasure Right — Privacy-Fundamental Reading
 *   domain: technology governance/data protection law
 *
 * SUMMARY:
 *   This story instantiates the privacy-fundamental reading of the Article 17
 *   erasure right. The standing arrangement under contest is the EU
 *   right-to-erasure regime: the CJEU's Google Spain delisting jurisprudence
 *   flowing into GDPR Article 17's application in May 2018 and its
 *   enforcement through the present. Assessed by this reading's own lights,
 *   the arrangement is genuine rights machinery: it gives individuals a
 *   standardized, enforceable exit from data relationships that no bilateral
 *   negotiation ever provided, and it prices that exit onto the controllers
 *   that hold the data. The epsilon authored here (0.33) is this reading's
 *   assessment of that standing arrangement — real costs borne by
 *   controllers, most of them the legitimate price of operating a fundamental
 *   right, with a moderate deadweight component in over-removal caution,
 *   vendor margins, and boilerplate friction. The claimed type (rope) and the
 *   metrics are authored independently: the engine computes per-seat
 *   classifications, and divergence between this claim and the payer seats'
 *   computed position is exactly the measurement the corpus takes. The two
 *   sibling readings — the competitive-moat reading and the
 *   censorship-mechanism reading — are separate constraint stories with their
 *   own epsilon values and beneficiary structures, linked through the network
 *   section; they are not folded into this one. KEY AGENTS (by structural
 *   relationship): - eu_data_subjects: Primary beneficiary
 *   (moderate/constrained) — hold and invoke the erasure right -
 *   platform_data_controllers: Primary cost-bearer
 *   (institutional/constrained) — fund deletion pipelines under fine exposure
 *   - small_business_controllers: Secondary cost-bearer (moderate/national) —
 *   same duties without scale economies - data_protection_authorities:
 *   Agenda-setter and institutional beneficiary — enforce the right and are
 *   resourced by enforcing it - eu_legislative_institutions: Agenda-setter —
 *   wrote the text and alone can amend it, under Charter and treaty
 *   constraints - privacy_civil_society_orgs: Beneficiary
 *   (organized/identity-locked) — litigate and extend the frame -
 *   privacy_compliance_industry: Beneficiary and receipt seat — collects the
 *   compliance spend - third_country_regulators: Beneficiary
 *   (institutional/arbitrage) — free-ride on the template -
 *   press_freedom_advocates and public_archive_interests: Excluded — bear
 *   over-delisting costs with no procedural seat - privacy_law_scholars:
 *   Analytical observer — sees text, enforcement, and sibling readings from
 *   outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.33).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.58).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.33).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "GDPR Article 17 Erasure Right — Privacy-Fundamental Reading").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology governance/data protection law").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, 'c65a8cc7-13cf-4817-bed5-a5a23dacd8ba').
narrative_ontology:cs_kernel_codification('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', fixed_text).
narrative_ontology:cs_authority_grounding('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', lineage).
narrative_ontology:cs_interpretation_layer_present('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba').
narrative_ontology:cs_reading_relation('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', foundational, data_autonomy_is_fundamental_right).
narrative_ontology:cs_axiom_status(data_autonomy_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', data_autonomy_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', foundational, retention_burden_on_controller).
narrative_ontology:cs_axiom_status(retention_burden_on_controller, holdable).
narrative_ontology:cs_axiom_grounding('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', retention_burden_on_controller, deontological).
narrative_ontology:cs_axiom('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', secondary, low_friction_erasure_constitutes_sovereignty).
narrative_ontology:cs_axiom_status(low_friction_erasure_constitutes_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', low_friction_erasure_constitutes_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', data_sovereignty_as_fundamental_right).
narrative_ontology:cs_drift_state('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', post_gdpr_application, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('c65a8cc7-13cf-4817-bed5-a5a23dacd8ba', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, privacy_civil_society_orgs).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, data_protection_authorities).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, third_country_regulators).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, privacy_compliance_industry).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, platform_data_controllers).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, small_business_controllers).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, informational_self_determination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a legally enforceable right to have personal data deleted by any service operating under EU jurisdiction, exercisable with a request the controller must answer within a month. Most never invoke it; those who do range from people escaping data-broker profiles to individuals scrubbing old posts. Leaving the platforms that hold their data is costly, so the request channel is the main lever they have over their own records.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, eu_data_subjects, beneficiary,
    moderate, biographical, constrained, continental).

% Operate the services that hold user data and must build and staff the machinery that finds, deletes, and certifies deletion across backups, logs, and downstream processors when a valid request arrives, or risk fines up to four percent of worldwide turnover. They cannot decline the duty while serving EU users, and geoblocking the EU market is commercially unthinkable for the large ones. They fund the deletion pipelines, the legal review, and the vendor contracts.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, platform_data_controllers, payer,
    institutional, generational, constrained, global).

% Face the same legal duties with a fraction of the staff: a ten-person firm handling customer data must still answer erasure requests, document deletion, and keep records, typically by buying off-the-shelf compliance tooling or outside counsel. Dropping EU customers is often the only alternative they can see, and many simply absorb the cost as overhead.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, small_business_controllers, payer,
    moderate, biographical, constrained, national).

% Receive complaints, investigate controllers, issue fines and orders, and publish the guidelines that fill in Article 17's open terms. Their budgets, staffing, and institutional weight expanded substantially with the framework they now administer, and they operate inside the legal structure they enforce — they cannot stand down without legislative change.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_protection_authorities, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__privacy_fundamental_reading, data_protection_authorities, beneficiary).

% Wrote the text in 2016 after multi-year negotiation and retain sole power to amend or repeal it, though any change must clear the Charter of Fundamental Rights, the Council of Europe's Convention 108 commitments, and the adequacy decisions that bind data flows to third countries — so unilateral rewriting is politically and legally expensive.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, eu_legislative_institutions, agenda_setter,
    institutional, generational, constrained, continental).

% Litigate test cases, file complaints, train requesters, and staff the watchdog role the framework presumes. Their funding, membership, and institutional identity are bound to the privacy-rights frame the arrangement embodies; several exist principally to enforce and extend it, and abandoning that frame would dissolve their reason to exist.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, privacy_civil_society_orgs, beneficiary,
    organized, generational, identity_locked, continental).

% Sells the deletion workflows, request-intake portals, data-mapping tools, and outside counsel that controllers buy to meet their obligations. Revenue scales with the strictness of the duty and the breadth of its interpretation; the industry lobbies for clarity and, at the margin, for breadth, and can redirect its products to other regulatory markets if demand shifts.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, privacy_compliance_industry, beneficiary,
    organized, biographical, mobile, global).

% Borrow the EU text as a drafting template — Brazil, India, South Africa, and US state laws echo its structure — gaining a tested rights framework without bearing its enforcement costs. Adoption is voluntary and reversible; the benefit is the free ride on someone else's legal engineering.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, third_country_regulators, beneficiary,
    institutional, generational, arbitrage, global).

% Handle the downstream of broad delisting: articles de-indexed, archives truncated, corrections weaponized as erasure requests. They have no seat in the requester-controller process and can intervene only after the fact through appeals and publicity; they cannot opt out of the delisting decisions others make.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, press_freedom_advocates, excluded,
    organized, biographical, trapped, continental).

% Maintain the historical record that broad erasure quietly edits. Their interest is represented only through balancing clauses the controller must self-apply at request time; no archivist is notified when a record is delisted, and the loss is discoverable only by comparing archives after the fact.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, public_archive_interests, excluded,
    moderate, civilizational, trapped, continental).

% Track the case law, measure request volumes and outcomes, and publish the analyses the authorities and courts cite. They hold no stake in the outcome and can see the whole structure — text, enforcement, and the rival interpretations — from outside.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, privacy_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__privacy_fundamental_reading, privacy_compliance_industry).
narrative_ontology:fixing_cost_class(article17_erasure_right__privacy_fundamental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the bilateral asymmetry between individuals and data controllers: no individual can negotiate deletion separately with every service holding their data, and controllers had no uniform signal for when retention duties end. Article 17 standardizes the exit — one request form, one response deadline, one enforcement backstop — and makes deletion a controller duty rather than a favor.
% TRANSFER_FUNCTION: Moves retention decisions from platform default to individual invocation: each valid request transfers deletion work, infrastructure cost, and legal exposure from the requester to the controller, and controllers pass a share of that cost to compliance vendors and counsel. Money flows from controllers (and indirectly from users via prices) into the privacy-compliance industry; control over the record flows from platforms to individuals.
% ABSENT_VOICES: Public-archive and press-freedom interests would object and have no seat: erasure requests are adjudicated bilaterally between requester and controller, with the public's interest in the record left to balancing clauses the controller self-applies at request time. No archivist or speaker is notified when content is delisted; they discover the edit by comparing archives after the fact. They are carried here as excluded stakeholders and in the strategic_erasure_abuse_rate omega rather than as classified parties.
% DISAPPEARANCE_RATIONALE: If the right vanished overnight, deletion would revert to per-company discretion and goodwill; data-broker profiles and stale records would become effectively permanent; DPA enforcement mandates and budgets would contract; the compliance industry would lose its demand floor; third-country regimes built on the template would face a legitimacy gap; and the press-freedom and archive objections would lose their current procedural handle. The arrangements of every named seat depend on the right's existence.
% FOUNDING_PROBLEM: Search engines and platforms had made past personal data permanently retrievable and non-negotiable: individuals could not remove stale, wrong, or harmful records, and controllers owed no duty to forget. The CJEU's Google Spain ruling (2014) recognized a delisting right, and GDPR Article 17 (applied 2018) codified erasure as a controller obligation.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: controller submissions to DPA proceedings concede the retention problem is real even while contesting its scope; the Council of Europe's Convention 108 (1981) predates the GDPR coalition by decades and independently grounds data-subject control as a live problem; CJEU case law treats the underlying harm as established; and consumer-union surveys document deletion demand the pre-2018 arrangement could not absorb. No corroborating source outside the beneficiary set treats the founding problem as resolved — data brokers and AI training corpora keep it open.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__privacy_fundamental_reading, 0.33, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.33: the arrangement transfers real, recurring costs to controllers — deletion pipelines across backups, logs, and downstream processors, legal review, vendor contracts — but this reading holds most of that cost to be the price of a right rather than rent; the residual is deadweight (over-removal caution, compliance-industry margin, boilerplate). Suppression 0.58: the regime is hard law with fines up to four percent of worldwide turnover and no lawful opt-out for in-scope controllers — substantial coercive machinery — but it secures a declared right rather than defending a benefit stream. Suppression is authored as the raw structural property it is; only extractiveness is scaled by directionality and scope in the engine's computation. Theater 0.24: deletion pipelines demonstrably operate at scale, but a persistent performative layer remains in dark-patterned request flows and policy boilerplate. Accessibility_collapse 0.35: informal deletion requests predate the right and still work; the right channels deletion through a formal mechanism without annihilating alternatives. Resistance 0.45: heavy pre-adoption lobbying and continuing litigation over delisting scope, decaying into normalized compliance after 2018. All three tracked series run on one shared grid (2016, 2017, 2018, 2020, 2022, 2025). The theater peak in 2018 is the compliance-theater wave at GDPR application, decaying as pipelines routinize. Suppression_requirement is authored because the interval specifically tracks an enforcement-machinery build-up (2016–2020, from adopted text to empowered DPAs and the fine era) that then stabilizes — a static enforcement picture would not capture that maturation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the platform seat the same text is a cost mandate with tail risk measured in percentage points of global turnover. From the data-subject seat it is the only working lever over records held by services they cannot affordably leave. From the DPA seat it is mandate, budget, and mission. From the compliance vendor seat it is demand. The engine derives these positions from power, exit options, and declared cost-bearing: the platform seat (institutional power, constrained exit, declared cost-bearer) should sit near the full-target end of directionality while the data-subject seat sits near the beneficiary end. A story-level rope claim coexists with that asymmetry because this reading holds the transfer to be a right's price rather than extraction — but whether the payer seats' computed classifications agree is the engine's measurement to take, not this claim's to preempt.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: eu_data_subjects (net gain, constrained exit — low d, with the right as their principal lever over their records), privacy_civil_society_orgs (identity-bound to the frame the arrangement embodies — low d), data_protection_authorities (administratively resourced by the arrangement they enforce — low d, with their agenda-setting position carried by role), third_country_regulators (arbitrage-grade free ride on the template — nearest the beneficiary pole), privacy_compliance_industry (mobile exit, direct receipt of the compliance spend — very low d). Cost-bearing declarations: platform_data_controllers (institutional power, no profitable exit from the EU market — near the full-target end) and small_business_controllers (thin margins, identical duties, no scale economies — high d despite moderate power). The victims array records who bears the arrangement's costs; this reading's verdict that those costs are legitimate lives in the moderate epsilon and in this commentary, not in denying the cost-bearing. The excluded seats (press freedom, public archives) are deliberately outside the beneficiary/victim arrays: their costs are real but procedurally unseated, which the strategic_erasure_abuse_rate omega carries rather than the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — permanent, non-negotiable retention of personal data — is live: data accumulation has only grown since 2016, and AI training corpora have reopened the retention question the right was built to answer. No mandate has outlived its function, so no mandatrophy is declared and no sunset applies. The classification work this story does is boundary-keeping: the rope claim prevents the censorship sibling's framing from reading the whole arrangement as suppression-by-request, while the declared cost-bearing structure keeps the payer seats' position visible so the rope claim cannot drift into complacency. The R5 mismatch consumer should find founding_problem_status=live paired with disappearance_verdict=world_rearranges — no zombie flag, no capture signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the privacy_fundamental_reading of the article17_erasure_right kernel; do the sibling readings (competitive_moat_reading, censorship_mechanism_reading) describe structurally real features of the same standing arrangement, and where does the disagreement bite?',
    'Cross-reading comparison of the three sibling stories'' seat classifications and epsilon values, plus enforcement-pattern data (who initiates, who pays, what is removed) showing which beneficiary structure the arrangement actually instantiates in operation.',
    'If the moat structure dominates (regressive costs, entry deterrence) or the censorship structure dominates (strategic delisting at scale), this reading''s rope claim fails at the affected seats and the family''s classification redistributes; if neither dominates, the privacy reading stands as the primary structure with the siblings as boundary phenomena.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the Article 17 kernel the standing arrangement structurally instantiates.').

omega_variable(
    effective_request_friction,
    'Is the epistemic friction of exercising the erasure right actually low, as this reading''s structure requires, or do dark patterns and procedural burden make the right nominal for most data subjects?',
    'Request abandonment rates from controller transparency reports, DPA complaint statistics, and usability audits of erasure request flows.',
    'High realized friction would raise theater_ratio, push the data-subject seat toward a nominal-benefit position, and date a drift from this reading''s reference frame earlier and deeper than the authored minor practice_drift gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_request_friction, empirical, 'Whether the reading''s low-friction premise holds in realized request experience.').

omega_variable(
    strategic_erasure_abuse_rate,
    'What share of erasure and delisting requests target lawful public-interest content — the censorship_mechanism_reading''s evidence base — rather than personal data the requester legitimately wants removed?',
    'DPA decision records, delisting appeal outcomes, and press-freedom organizations'' casework inventories, classified by target content type.',
    'A material abuse rate would add the public as a de facto cost-bearing class absent from this story''s declared structure, pushing the story-level classification toward a hybrid with asymmetric costs and validating the sibling reading inside this arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_erasure_abuse_rate, empirical, 'Rate of weaponized erasure requests against public-interest content.').

omega_variable(
    compliance_cost_incidence,
    'Do compliance costs fall regressively across controller size — the competitive_moat_reading''s evidence base — such that the arrangement shields incumbents from data-heavy entrants?',
    'Compliance-cost surveys stratified by firm size and market-entry data for data-intensive services in the EU after 2018, compared against pre-2018 baselines.',
    'Regressive incidence would mean the standing arrangement partly instantiates the moat structure inside this reading''s own referent, forcing either decomposition of the cost-bearing seat into its own story or an upward revision of effective extraction at the small-business seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_incidence, empirical, 'Regressivity of compliance costs across controller size.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 2016, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2016, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement_basis(arti_tr_t2016, observed).
narrative_ontology:measurement(arti_tr_t2017, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2017, 0.24).
narrative_ontology:measurement_basis(arti_tr_t2017, observed).
narrative_ontology:measurement(arti_tr_t2018, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement_basis(arti_tr_t2018, observed).
narrative_ontology:measurement(arti_tr_t2020, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement_basis(arti_tr_t2020, observed).
narrative_ontology:measurement(arti_tr_t2022, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2022, 0.26).
narrative_ontology:measurement_basis(arti_tr_t2022, observed).
narrative_ontology:measurement(arti_tr_t2025, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2025, 0.24).
narrative_ontology:measurement_basis(arti_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t2016, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2016, 0.24).
narrative_ontology:measurement_basis(arti_be_t2016, observed).
narrative_ontology:measurement(arti_be_t2017, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2017, 0.27).
narrative_ontology:measurement_basis(arti_be_t2017, observed).
narrative_ontology:measurement(arti_be_t2018, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2018, 0.32).
narrative_ontology:measurement_basis(arti_be_t2018, observed).
narrative_ontology:measurement(arti_be_t2020, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2020, 0.34).
narrative_ontology:measurement_basis(arti_be_t2020, observed).
narrative_ontology:measurement(arti_be_t2022, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2022, 0.35).
narrative_ontology:measurement_basis(arti_be_t2022, observed).
narrative_ontology:measurement(arti_be_t2025, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2025, 0.33).
narrative_ontology:measurement_basis(arti_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2016, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2016, 0.3).
narrative_ontology:measurement_basis(arti_su_t2016, observed).
narrative_ontology:measurement(arti_su_t2017, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2017, 0.4).
narrative_ontology:measurement_basis(arti_su_t2017, observed).
narrative_ontology:measurement(arti_su_t2018, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2018, 0.52).
narrative_ontology:measurement_basis(arti_su_t2018, observed).
narrative_ontology:measurement(arti_su_t2020, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement_basis(arti_su_t2020, observed).
narrative_ontology:measurement(arti_su_t2022, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2022, 0.58).
narrative_ontology:measurement_basis(arti_su_t2022, observed).
narrative_ontology:measurement(arti_su_t2025, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(arti_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'Article 17 right to erasure' covers three structurally distinct claims that share one kernel text but differ in epsilon, beneficiary structure, and failure modes — privacy_fundamental_reading (this story: genuine rights machinery, moderate-low epsilon, rope claim), competitive_moat_reading (incumbent protection via cost asymmetry; higher epsilon at the entry seat), and censorship_mechanism_reading (suppression instrument; victims in the public information environment). The privacy reading is doctrinally upstream: the compliance costs the moat reading cites exist because this reading's machinery was built, and the request mechanism the censorship reading weaponizes is legitimate only under this reading's frame. The edges here mark family membership and that upstream position; each sibling story links back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
