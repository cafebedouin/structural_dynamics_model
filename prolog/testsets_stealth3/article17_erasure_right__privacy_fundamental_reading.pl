% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__privacy_fundamental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Article 17 Erasure Right — Privacy Fundamental Reading
 *   domain: technological/legal
 *
 * SUMMARY:
 *   Article 17 of the GDPR grants individuals the right to obtain erasure of
 *   personal data without undue delay. This story instantiates the
 *   privacy_fundamental_reading of the article17_erasure_right kernel: the
 *   mechanism is a fundamental-rights instrument that converts individual
 *   data sovereignty from a moral claim into an enforceable legal position
 *   limiting corporate retention. The ε referent is the standing arrangement
 *   — the erasure regime as it actually operates under the GDPR, CJEU
 *   jurisprudence, and DPA enforcement — assessed by this reading's own
 *   lights: the reading holds the arrangement to be a genuine rights
 *   mechanism whose costs on controllers are duty-bearing rather than
 *   extraction. Per the ε-invariance principle, the sibling readings
 *   (competitive_moat_reading, censorship_mechanism_reading) are separate
 *   constraint stories with their own ε values, beneficiary structures, and
 *   claimed types; this file does not adjudicate between them and does not
 *   average across them. The claim/metric gap is deliberate: the reading
 *   claims rope while the authored metrics record that controllers bear real,
 *   enforced costs — the engine measures that divergence per seat; do not
 *   reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - eu_data_subjects: primary beneficiary (moderate/trapped) — hold the right; cannot exit being data subjects
 *   - large_platform_controllers: primary cost-bearer (institutional/constrained) — run deletion infrastructure, face fines, cannot exit the EU market
 *   - small_business_controllers: secondary cost-bearer (moderate/constrained) — same obligations, a fraction of the compliance capacity
 *   - supervisory_authorities: agenda setter (institutional/constrained) — issue guidelines, adjudicate complaints, levy fines
 *   - digital_rights_organizations: secondary beneficiary (organized/mobile) — litigate and advocate on the right's substrate
 *   - archival_research_community: excluded seat (moderate/constrained) — bears record narrowing without a place in request-level decisions
 *   - data_protection_scholars: analytical observer (analytical/analytical) — tracks the mechanism's operation from outside the enforcement loop
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.3).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.45).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "Article 17 Erasure Right — Privacy Fundamental Reading").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technological/legal").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, '3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940').
narrative_ontology:cs_kernel_codification('3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940', fixed_text).
narrative_ontology:cs_authority_grounding('3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940', lineage).
narrative_ontology:cs_interpretation_layer_present('3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940').
narrative_ontology:cs_reading_relation('3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940', foundational, personal_data_sovereignty_fundamental).
narrative_ontology:cs_axiom_status(personal_data_sovereignty_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940', personal_data_sovereignty_fundamental, deontological).
narrative_ontology:cs_axiom('3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940', foundational, erasure_default_controller_burden_of_justification).
narrative_ontology:cs_axiom_status(erasure_default_controller_burden_of_justification, holdable).
narrative_ontology:cs_axiom_grounding('3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940', erasure_default_controller_burden_of_justification, deontological).
narrative_ontology:cs_reference_frame('3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940', informational_self_determination_default).
narrative_ontology:cs_drift_state('3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940', contemporary_platform_data_economy, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('3c2c1ead-78c5-4ed9-bcb9-b2f7384e4940', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, digital_rights_organizations).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, large_platform_controllers).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, small_business_controllers).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, informational_self_determination_doctrine).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, data_minimization_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold personal data across search indexes, social platforms, and commercial databases they do not control. The right gives them a standardized, enforceable claim to have data erased without negotiating with each controller. Individually they hold little leverage; their position is backed collectively by supervisory authorities and rights organizations that take up complaints. Exit is not available to them — their data exists in controller systems whether or not they engage with the right.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, eu_data_subjects, beneficiary,
    moderate, biographical, trapped, continental).

% Operate search, social, and commerce infrastructure that processes erasure requests at scale. They maintain deletion pipelines, identity verification, backup purge cycles, and third-party notification chains, and face fines up to four percent of global turnover for failure. They cannot exit the EU market without forfeiting its user base, so compliance is a fixed operating cost. They litigated the right's scope — delisting geography, balancing tests — and now operate within it.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, large_platform_controllers, payer,
    institutional, generational, constrained, global).

% Bear the same erasure obligations as large platforms with a fraction of the compliance capacity: no dedicated data protection officers, manual request handling, thin margins. Compliance costs are a proportionally heavier share of revenue, and exiting the EU market is rarely viable for firms whose customers are local.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, small_business_controllers, payer,
    moderate, biographical, constrained, continental).

% National data protection authorities and the European Data Protection Board issue guidelines on erasure scope, adjudicate complaints, and levy fines; through enforcement practice they set the operative interpretation of the right. Their mandate is constituted by the regime they administer — they cannot abandon enforcement without institutional dissolution, and their discretion is bounded by the GDPR text and court review. Fine proceeds flow to member-state budgets, not to the authorities' own operations.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, supervisory_authorities, agenda_setter,
    institutional, generational, constrained, continental).

% Litigate test cases, file strategic complaints, and publish compliance scorecards. The right's existence is the substrate of their advocacy: it supplies standing, funding relevance, and a concrete instrument. They can and do shift attention across issues, so their position is chosen rather than fixed.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, digital_rights_organizations, beneficiary,
    organized, generational, mobile, continental).

% Historians, archivists, and researchers work from a record that erasure narrows. Statutory archival exceptions preserve institutional holdings, but request-level deletions — removed search results, deleted platform posts — happen bilaterally between subject and controller with no seat for those whose future work depends on the record. They discover losses when sources vanish.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, archival_research_community, excluded,
    moderate, generational, constrained, global).

% Track the right's operation empirically: request volumes, compliance rates, delisting patterns, and who invokes erasure. They assess whether the mechanism delivers the sovereignty it promises and publish outside the enforcement loop.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_protection_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__privacy_fundamental_reading, diffuse).
narrative_ontology:fixing_cost_class(article17_erasure_right__privacy_fundamental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of individual data sovereignty: no individual can bargain with every controller holding their data, and no controller internalizes the cost of retaining one person's data. The right standardizes erasure as an enforceable, uniform claim — one lever for individuals, one rule for controllers.
% TRANSFER_FUNCTION: Moves control over personal data from controllers to subjects: controllers surrender default indefinite retention and transfer engineering, legal, and process capacity into erasure handling; subjects gain an enforceable claim over whether data about them persists.
% ABSENT_VOICES: Data subjects who never learn the right exists, or who cannot navigate request and identity-verification processes — the right's protection tracks awareness. Non-EU data subjects outside the territorial scope have no seat. Archival and research communities bear a narrowing record without any seat in request-level decisions: each erasure is decided bilaterally, and no one represents the future readers of the deleted material.
% DISAPPEARANCE_RATIONALE: If the right vanished overnight, controllers would revert to indefinite-retention defaults — the pre-Google-Spain equilibrium — and individuals would lose their only standardized lever over their data's persistence. Deletion pipelines would be decommissioned, supervisory-authority erasure dockets and rights organizations' litigation dockets would empty, and the data economy would reorganize around controller discretion.
% FOUNDING_PROBLEM: Individuals could not control personal data once it entered corporate systems: search engines indexed people against their will, platforms retained posts and profiles indefinitely, and outdated, irrelevant, or excessive data persisted with no recourse — the grievance behind the Google Spain litigation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: CJEU case law (Google Spain; GC and Others) attests the persistence problem's reality; EDPB and Article 29 Working Party guidelines document continuing retention-sprawl violations; national DPA enforcement reports and the academic data-protection literature record that data collection has expanded since the right's adoption. Controllers' own compliance publications acknowledge retention sprawl even while disputing remedies. No corroborating source claims the founding problem is solved.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__privacy_fundamental_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.3: controllers transfer real engineering, legal, and process capacity into erasure handling, and the reading assesses that burden as largely proportionate to the sovereignty it protects — extraction, as distinct from duty-bearing, is the residual (over-compliance drag, manual-review defaults). Suppression 0.45: controllers are coerced — fines up to four percent of global turnover, no viable exit from the EU market — but the coercion targets a narrow conduct class (retention without justification) rather than suppressing alternatives to the arrangement. Theater 0.18: the mechanism functions — requests are processed, data deleted, fines levied; the performative share is compliance dashboards and privacy-page boilerplate, spiking around the 2018 applicability deadline. Accessibility_collapse 0.4: the no-right alternative is foreclosed within EU law, but workable alternatives persist (anonymization outside scope, consent-based retention, legitimate-interest processing, archival exceptions). Resistance 0.35: industry litigated the right's scope and lobbied its exceptions; compliance is now normalized and resistance is declining. The measurement series share one time grid (2014–2024, six points) with every tracked metric authored at every point. Suppression_requirement is tracked because the enforcement machinery genuinely matured over the interval — CJEU ruling, then GDPR codification, then the fine regime — rising to a plateau rather than ratcheting. The claimed type is authored from this reading's seat: rope. The engine, seeing beneficiaries, cost-bearers, and active enforcement, may compute tangled_rope at the story level — that divergence is the corpus datum, not an error to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the data-subject seat the arrangement is a right that works — coordination the subjects could not have built individually. From the platform seat the same structure is a fixed compliance regime with real costs and no exit — tangled-rope- or snare-flavored. From the DPA seat it is an enforcement mandate whose discretion the courts bound. The privacy reading's authoring seat is aligned with the data-subject seat; the platform seat's divergence is exactly what the per-seat computation should surface. Same-power divergence also appears: large_platform_controllers and supervisory_authorities share the institutional power atom but sit at opposite structural relations to the constraint — which is why no directionality overrides are authored: the override key is the power atom alone and would misapply across same-atom seats, so the beneficiary and cost-bearer declarations carry the differentiation instead.
 *
 * DIRECTIONALITY LOGIC:
 *   Data subjects are declared beneficiaries: the right subsidizes their sovereignty claim (d near the beneficiary end). Digital rights organizations benefit incidentally — standing and relevance — without running the mechanism. Platform and small-business controllers are declared cost-bearers in the victims array: they fund the deletion infrastructure and surrender default retention (d near the target end); the reading's assessment that this burden is legitimate duty-bearing is an evaluation of the extraction, not a denial that it falls on these seats. Supervisory authorities administer without collecting the extraction — fines flow to member-state budgets, not to the authorities' operations. The archival community bears narrowing record access as a third-party cost with no seat. No seat captures extracted value: the compliance spend is consumed inside controllers as process, and the protection diffuses across millions of subjects — hence gain_flow 'diffuse', authored after re-checking every named seat's situation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — uncontrollable persistence of personal data in corporate systems — is live and expanding; the constraint has not outlived its function, so no mandatrophy is declared. The mismatch consumer should find founding_problem_status 'live' with disappearance_verdict 'world_rearranges': no zombie flag. The classification also guards the reverse error: because the moat and censorship readings are separate constraints, this story cannot be dragged into labeling the right pure extraction by importing their victim sets — the reading's own lights assess the controller burden as duty-bearing, while the structural declarations still record who bears it so the engine can measure the divergence. Fixing or removing the arrangement requires EU-level legislative or judicial action against a salient fundamental right whose benefits are diffuse: the cost to any fixing party exceeds what it bears, which is why fixing_cost is authored 'prohibitive' even though the mechanism itself functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the privacy_fundamental_reading of kernel article17_erasure_right; what would the sibling readings (competitive_moat_reading, censorship_mechanism_reading) change structurally, and where exactly does the disagreement sit?',
    'The readings are separate constraints in separate files; resolution proceeds per reading. The disagreement is located at the primary function of the erasure mechanism: rights protection (this reading) versus incumbent cost-asymmetry (moat reading) versus suppression lever (censorship reading). Adopting a sibling reading shifts the beneficiary and cost-bearer sets: the moat reading promotes small controllers to primary cost-bearers and incumbents to beneficiaries; the censorship reading adds the public''s access to information to the cost-bearer set and strategic erasure requesters to the beneficiary set.',
    'If a sibling reading were adopted as the primary characterization, this story''s ε, beneficiary structure, and claimed type are replaced by the sibling''s — the three files must never be merged into one ε or one classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of the Article 17 kernel; sibling readings are separate constraints with their own structural data.').

omega_variable(
    legitimate_cost_vs_extraction_boundary,
    'Are platform compliance costs the legitimate price of honoring a fundamental right, or do they exceed the coordination cost of rights protection?',
    'Independent cost studies of erasure-request processing (per-request marginal cost, automation gains) set against verified rights-protection outcomes; cross-member-state comparison of supervisory-authority enforcement intensity as a natural experiment.',
    'If costs track genuine protection needs, the rope claim holds from this reading''s seat; if costs are inflated by controller design choices (identity-verification friction, manual-review defaults), the same structure computes as tangled_rope even within this reading — a measurable divergence, not one resolvable by fiat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_cost_vs_extraction_boundary, empirical, 'Whether the compliance burden is proportionate rights-protection cost or surplus extraction.').

omega_variable(
    broad_interpretation_third_party_costs,
    'Does the broad erasure interpretation stay within data sovereignty, or does it begin to trade third-party interests — the public''s access to information, the archival record — without those parties'' consent?',
    'Trajectory of CJEU balancing jurisprudence on delisting scope, and supervisory-authority decision patterns on public-figure and public-interest erasure requests.',
    'If erasure routinely overrides public-interest retention, this reading''s ε understates the arrangement''s cost to non-party interests, and the censorship reading''s cost-bearer set gains standing — pressuring this reading''s foundational axioms toward a narrower erasure default.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broad_interpretation_third_party_costs, empirical, 'Whether broad erasure externalizes costs onto third parties absent from the request-level decision.').

omega_variable(
    uptake_distribution_skew,
    'Is the right''s protection distributed evenly across data subjects, or does it concentrate among digitally literate requesters while unaware or less-resourced subjects go unprotected?',
    'Request-volume demographics against population data-subject demographics; studies of supervisory-authority outreach effectiveness.',
    'If uptake is skewed, the coordination function is partial — it protects those who invoke it — and the beneficiary declaration overstates coverage, lowering effective benefit and raising the arrangement''s residual ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uptake_distribution_skew, empirical, 'Whether erasure-right benefits concentrate among aware, resourced requesters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 2014, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2014, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement_basis(arti_tr_t2014, observed).
narrative_ontology:measurement(arti_tr_t2016, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2016, 0.12).
narrative_ontology:measurement_basis(arti_tr_t2016, observed).
narrative_ontology:measurement(arti_tr_t2018, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement_basis(arti_tr_t2018, observed).
narrative_ontology:measurement(arti_tr_t2020, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement_basis(arti_tr_t2020, observed).
narrative_ontology:measurement(arti_tr_t2022, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2022, 0.19).
narrative_ontology:measurement_basis(arti_tr_t2022, observed).
narrative_ontology:measurement(arti_tr_t2024, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2024, 0.18).
narrative_ontology:measurement_basis(arti_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t2014, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2014, 0.22).
narrative_ontology:measurement_basis(arti_be_t2014, observed).
narrative_ontology:measurement(arti_be_t2016, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2016, 0.25).
narrative_ontology:measurement_basis(arti_be_t2016, observed).
narrative_ontology:measurement(arti_be_t2018, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2018, 0.3).
narrative_ontology:measurement_basis(arti_be_t2018, observed).
narrative_ontology:measurement(arti_be_t2020, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2020, 0.31).
narrative_ontology:measurement_basis(arti_be_t2020, observed).
narrative_ontology:measurement(arti_be_t2022, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2022, 0.32).
narrative_ontology:measurement_basis(arti_be_t2022, observed).
narrative_ontology:measurement(arti_be_t2024, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2024, 0.3).
narrative_ontology:measurement_basis(arti_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2014, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2014, 0.3).
narrative_ontology:measurement_basis(arti_su_t2014, observed).
narrative_ontology:measurement(arti_su_t2016, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2016, 0.35).
narrative_ontology:measurement_basis(arti_su_t2016, observed).
narrative_ontology:measurement(arti_su_t2018, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2018, 0.45).
narrative_ontology:measurement_basis(arti_su_t2018, observed).
narrative_ontology:measurement(arti_su_t2020, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement_basis(arti_su_t2020, observed).
narrative_ontology:measurement(arti_su_t2022, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2022, 0.46).
narrative_ontology:measurement_basis(arti_su_t2022, observed).
narrative_ontology:measurement(arti_su_t2024, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2024, 0.45).
narrative_ontology:measurement_basis(arti_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'right to be forgotten' covers three structurally distinct claims about one kernel (article17_erasure_right), decomposed per the ε-invariance principle into three stories. This file (privacy_fundamental_reading) is the doctrinal upstream: the CJEU Google Spain lineage and the GDPR text ground the mechanism as a rights instrument, and the sibling readings draw their factual substrate from that structure — the moat reading reuses its compliance-cost surface, the censorship reading reuses its request-level bilateralism. Each story carries its own ε (this file: moderate, assessed as duty-bearing by the reading's lights), its own beneficiary and cost-bearer sets, and its own claimed type; the edges here record the family so purity analysis can track contamination between readings without merging their classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
