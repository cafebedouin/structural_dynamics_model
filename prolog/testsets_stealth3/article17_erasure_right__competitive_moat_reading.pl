% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__competitive_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__competitive_moat_reading, []).

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
 *   constraint_id: article17_erasure_right__competitive_moat_reading
 *   human_readable: Article 17 Right to Erasure — Competitive Moat Reading (Compliance-Cost Asymmetry)
 *   domain: technology governance / data protection / competition policy
 *
 * SUMMARY:
 *   Under the competitive-moat reading, the erasure mandate operates as a
 *   regulatory structure whose fixed compliance requirements — deletion
 *   pipelines, identity verification, audit trails, documented response
 *   deadlines — cost roughly the same to stand up for any controller,
 *   regardless of scale. For incumbents with billions of users the amortized
 *   per-user cost approaches zero; for a pre-revenue challenger it is an
 *   impassable fixed charge due before the first sale. The arrangement also
 *   delivers its nominal service: individuals do obtain deletions. That dual
 *   character is why this reading authors a genuine coordination function
 *   alongside asymmetric incidence. This file is ONE reading of the kernel
 *   'article17_erasure_right'; the privacy-fundamental and
 *   censorship-mechanism readings are separate constraints with separate
 *   epsilons and are not averaged here. KEY AGENTS (by structural
 *   relationship): - incumbent_platforms: Primary beneficiary
 *   (institutional/arbitrage) — collects moat rent from rivals' compliance
 *   burden without administering the rule - startup_challengers: Primary
 *   target (moderate/trapped) — bears regressive fixed costs with EU market
 *   exit prohibitively costly - sme_data_controllers: Secondary target
 *   (moderate/constrained) — pays a flat-size tax on continuing operations -
 *   privacy_compliance_vendor_sector: Secondary beneficiary
 *   (organized/mobile) — monetizes obligation complexity -
 *   european_legislature: Agenda-setter (institutional/constrained) — holds
 *   amendment power but faces renewal-consensus friction -
 *   data_protection_authorities: Co-agenda-setter (institutional/analytical)
 *   — administers enforcement whose volume defines their remit -
 *   digital_service_users: Dual-positioned seat (organized/constrained) —
 *   receives the deletion service, pays via thinned competition -
 *   open_source_maintainers: Excluded party (powerless/trapped) — priced out
 *   of EU-serving entirely, unrepresented in rule-making -
 *   competition_economists: Analytical observer — measures the attribution
 *   question the parties dispute
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, 0.65).
domain_priors:suppression_score(article17_erasure_right__competitive_moat_reading, 0.6).
domain_priors:theater_ratio(article17_erasure_right__competitive_moat_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__competitive_moat_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__competitive_moat_reading, "Article 17 Right to Erasure — Competitive Moat Reading (Compliance-Cost Asymmetry)").
narrative_ontology:topic_domain(article17_erasure_right__competitive_moat_reading, "technology governance / data protection / competition policy").

domain_priors:requires_active_enforcement(article17_erasure_right__competitive_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__competitive_moat_reading, 'eb3d3544-ccfe-4b61-905f-555ca29696b7').
narrative_ontology:cs_kernel_codification('eb3d3544-ccfe-4b61-905f-555ca29696b7', fixed_text).
narrative_ontology:cs_authority_grounding('eb3d3544-ccfe-4b61-905f-555ca29696b7', lineage).
narrative_ontology:cs_interpretation_layer_present('eb3d3544-ccfe-4b61-905f-555ca29696b7').
narrative_ontology:cs_reading_relation('eb3d3544-ccfe-4b61-905f-555ca29696b7', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb3d3544-ccfe-4b61-905f-555ca29696b7', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('eb3d3544-ccfe-4b61-905f-555ca29696b7', foundational, compliance_cost_asymmetry_structures_entry).
narrative_ontology:cs_axiom_status(compliance_cost_asymmetry_structures_entry, holdable).
narrative_ontology:cs_axiom_grounding('eb3d3544-ccfe-4b61-905f-555ca29696b7', compliance_cost_asymmetry_structures_entry, empirically_contingent).
narrative_ontology:cs_axiom('eb3d3544-ccfe-4b61-905f-555ca29696b7', secondary, erasure_infrastructure_requirements_are_capital_barriers).
narrative_ontology:cs_axiom_status(erasure_infrastructure_requirements_are_capital_barriers, holdable).
narrative_ontology:cs_axiom_grounding('eb3d3544-ccfe-4b61-905f-555ca29696b7', erasure_infrastructure_requirements_are_capital_barriers, empirically_contingent).
narrative_ontology:cs_reference_frame('eb3d3544-ccfe-4b61-905f-555ca29696b7', scale_neutral_compliance_baseline).
narrative_ontology:cs_drift_state('eb3d3544-ccfe-4b61-905f-555ca29696b7', post_enforcement_maturation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb3d3544-ccfe-4b61-905f-555ca29696b7', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__competitive_moat_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, incumbent_platforms).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, privacy_compliance_vendor_sector).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, startup_challengers).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, sme_data_controllers).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, digital_service_users).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, open_source_maintainers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, digital_service_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the erasure mandate through the ordinary legislative procedure and retains sole power to amend or simplify it. Faces simultaneous pressure from privacy constituencies to keep enforcement strict and from the competitiveness agenda to lighten burdens on smaller firms; amendment is possible but requires renewed trilogue consensus, so the current text persists by default.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, european_legislature, agenda_setter,
    institutional, generational, constrained, continental).

% Investigate complaint dockets, issue corrective fines, and coordinate through the European Data Protection Board. Their caseload, staffing, and budget authority grow with the volume of erasure obligations they administer; they neither buy nor sell compliance but decide what counts as adequate performance.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, data_protection_authorities, agenda_setter,
    institutional, biographical, analytical, continental).

% Operate erasure request pipelines, identity-verification stacks, and audit trails built once and amortized across billions of users, making their marginal cost per deletion trivial. Every prospective competitor must replicate that stack before serving its first user. They do not administer the rule; they collect its structural effect — a protected installed base — and can shift data processing and corporate structure across jurisdictions when convenient.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, incumbent_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Face the same fixed obligations — designated contacts, deletion workflows, response deadlines, documentation — before earning first revenue. Runway math turns each compliance hire into a postponed product milestone; abandoning the EU market forfeits the largest bloc of addressable users, so the realistic exits are absorbing the cost, delaying launch, or selling early.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, startup_challengers, payer,
    moderate, immediate, trapped, continental).

% Mid-sized merchants and publishers that purchase compliance tooling and dedicate staff to request handling. They lack the incumbent's scale to internalize the overhead yet cannot walk away from existing EU customer relationships; the obligation functions as a flat tax insensitive to their size.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, sme_data_controllers, payer,
    moderate, biographical, constrained, regional).

% Sells request-automation software, consent management, audits, and advisory retainers. Revenue scales with the complexity and stringency of the obligations rather than with deletions achieved; they benefit from the arrangement's intricacy without operating it.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, privacy_compliance_vendor_sector, beneficiary,
    organized, biographical, mobile, global).

% Receive a functioning, legally backed way to compel deletion of their personal data — a real service delivered on request. They also inhabit the market the rule reshapes: fewer new entrants mean less variety and weaker price pressure, and no individual can opt out of the regulatory environment their services sit inside.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, digital_service_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, digital_service_users, payer).

% Volunteer developers whose community tools touch personal data. The compliance overhead exceeds their total project resources, so serving EU users is effectively closed to them; they were not represented in the legislative negotiations or regulatory consultations that set the obligations they cannot meet.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, open_source_maintainers, excluded,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, open_source_maintainers, payer).

% Measure venture funding flows, entry rates, and concentration trends around the 2018 application date. Publish findings on whether observed consolidation is attributable to the erasure regime or to ambient platform economics; their work is cited by both defenders and critics but binds no one.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, competition_economists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__competitive_moat_reading, incumbent_platforms).
narrative_ontology:fixing_cost_class(article17_erasure_right__competitive_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legally enforceable procedure by which any individual can compel deletion of their personal data from every controller serving them, standardizing request channels, verification, response deadlines, and proof-of-deletion across the entire EU market.
% TRANSFER_FUNCTION: Moves compliance expenditure — legal review, engineering hours, audit fees, staffing — from data controllers to compliance vendors and advisors, with the per-revenue burden falling hardest on the smallest controllers; via foregone entry, it moves market share and pricing power from prospective challengers to incumbent platforms; it moves deletion outcomes to data subjects.
% ABSENT_VOICES: Open-source maintainers and unfunded builders priced out of serving EU users were never seated in trilogue or board consultations; venture financiers watching deal flow redirect away from EU-domiciled startups had no seat; consumers experiencing the variety loss were represented only indirectly, through advocates whose priority ranking placed data control above competitive supply.
% DISAPPEARANCE_RATIONALE: If the erasure mandate vanished overnight, deletion pipelines would unwind, the request-handling vendor sector would contract sharply, supervisory dockets would empty of erasure cases, stalled challengers would re-enter blocked EU market segments, and incumbents would lose the compliance asymmetry currently insulating their installed bases — the market for personal-data services would visibly reorganize.
% FOUNDING_PROBLEM: Search engines and online archives retained outdated, inaccurate, or irrelevant personal information indefinitely, with individuals having no procedural recourse against the display of their own history — the grievance vindicated in the 2014 ruling that preceded the codified right.
% FOUNDING_PROBLEM_CORROBORATION: Civil-society litigants and digital-rights organizations — parties outside the benefiting set — attest the original stale-archive grievance was genuine and remains partially unremedied. Competition-economic studies of post-2018 venture funding and entry rates corroborate, again from outside the beneficiary set, that the arrangement's operative effect now extends well beyond that grievance into entry deterrence. No attestation from the benefiting parties is relied upon.
narrative_ontology:disappearance_verdict(article17_erasure_right__competitive_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__competitive_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__competitive_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__competitive_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__competitive_moat_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__competitive_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article17_erasure_right__competitive_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.65 because the standing arrangement's dominant financial flow under this reading runs from the smallest controllers toward the largest incumbents' competitive position, with the vendor sector skimming a service fee along the way; the erasure service delivered to individuals is real and caps the score below snare territory. Suppression is 0.60: participation in the EU market leaves no compliant alternative to full obligation performance, and deviation is met with escalating corrective fines whose credible maximum (a percentage of global turnover) hardened over the interval. Theater ratio 0.32 reflects the documented split between ritual compliance artifacts — banner acknowledgments, boilerplate policies, staffed-but-unread registers — and deletions actually executed. Accessibility collapse is 0.48: alternatives exist (minimal-data architectures, outsourced processing, non-EU staging) but each carries its own cost floor that replicates part of the barrier, so alternatives narrow without vanishing. Resistance 0.50 records sustained industry objection at enactment, ongoing simplification campaigning, and litigation — real but never sufficient to reopen the text. The temporal series run on one shared grid (T=0,2,4,5,6,7) so every tracked metric is asserted at every examined point; the suppression_requirement series is authored deliberately because the story traces enforcement-capacity maturation — supervisory coordination consolidated, landmark fines established deterrent credibility, and the machinery hardened — not merely extraction shifting on static enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the incumbent seat the arrangement is a rounding error plus a differentiator: the stack was built once, and every rival's replication of it validates the incumbent's scale advantage — the regime looks like ordinary, even favorable, operating conditions. From the challenger seat the identical rule is an existential gate: the same fixed cost that is invisible at incumbent scale consumes the entire runway margin that decides survival. The legislature experiences it as a settled achievement defended against rollback; the excluded maintainer experiences it as a closed door they were never consulted about. The engine computes these per-seat classifications from the authored power, exit, and directional data; nothing in this story reconciles them.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent platforms sit nearest the beneficiary pole: the constraint subsidizes their position through rivals' costs while imposing negligible net burden on themselves, and their jurisdictional arbitrage keeps them mobile at the meta-level. Startup challengers sit nearest the target pole: trapped exit plus immediate horizon plus the full fixed-cost incidence drives effective burden to its ceiling — the trap matters because trapped targets register nearer full-target than mobile ones bearing identical nominal cost. SME controllers are targets at moderate intensity: continuous payment, no escape, but survivable. The vendor sector derives mild beneficiary direction — it collects fees without setting terms. Digital-service users are the deliberate dual seat: declared in both beneficiary and victim arrays to mirror their secondary_role, they sit near symmetric — the deletion service they receive is funded by the competitive thinning they endure. Open-source maintainers, declared victims via their exclusion-priced-out position and carrying payer as secondary role, sit at extreme target intensity despite paying nothing directly: the constraint costs them their entire addressable audience. Suppression is authored unscaled as a raw structural property; only extractiveness rides directionality and scope amplification.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope claim is load-bearing against two symmetrical mislabels. Reading the arrangement as pure coordination (the privacy-fundamental default) would erase the incidence asymmetry that is this reading's entire subject; reading it as pure extraction would erase the deletions demonstrably delivered to data subjects, which no seat disputes occurred. On genealogy: the founding problem — recourse against indefinite archival display — retains corroborated liveness, but the arrangement's operative center of gravity has migrated toward routine administration and market filtering, which the status-contested flag and the mismatch-checkable combination (contested status, world_rearranges verdict) expose for the zombie-screening consumer. Forward risk: if erasure tooling commoditizes fully and the moat persists anyway, the coordination shell will be running on enforcement momentum and habit — the slowly rising theater_ratio series is the tripwire for watching this reading drift toward inertial persistence, and the cost asymmetry between the fixer (legislature, facing consensus-renewal costs) and the diffuse benefit of fixing is recorded in fixing_cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of kernel article17_erasure_right (reading: competitive_moat_reading) — what structurally changes if a sibling reading is adopted instead?',
    'Cross-framing comparison of per-seat classifications across the three sibling files: adopt privacy_fundamental_reading and the beneficiary set relocates to data_subjects with epsilon collapsing toward the coordination floor; adopt censorship_mechanism_reading and the victim set relocates to speakers_and_archivists with the suppression profile elevated and the incidence asymmetry demoted to background. The disagreement is located in the beneficiary/victim structure, and no dataset inside this file resolves which location is correct — that is a commitment choice.',
    'Classification of the standing arrangement flips across the family: this file computes tangled_rope-shaped structure with incumbent-directed gains; the privacy sibling computes near-pure coordination; the censorship sibling computes suppression-dominated extraction. Cross-file comparison, not intra-file tuning, is the only resolver.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which reading of the erasure kernel this constraint is, and what siblings would change.').

omega_variable(
    cost_asymmetry_origin,
    'Is the observed compliance-cost asymmetry intrinsic to the obligation''s structure (fixed verification, documentation, and audit duties that do not shrink with firm size), or an artifact of an immature tooling market that erasure-as-a-service will eventually flatten?',
    'Longitudinal compliance-cost panel data stratified by firm size as managed-service pricing matures: if the small-to-large cost ratio persists at tooling maturity, the asymmetry is structural; if it converges toward proportionality, the moat was a transitional technology-gap effect.',
    'Structural asymmetry entrenches the tangled_rope reading and supports drift monitoring toward snare; convergence reclassifies much of the measured burden as ordinary coordination cost and pulls the reading back toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_asymmetry_origin, empirical, 'Whether the moat mechanism is durable structure or transitional tooling immaturity.').

omega_variable(
    concentration_attribution_counterfactual,
    'How much of the post-2018 concentration and entry-decline pattern is attributable to the erasure regime versus ambient platform economics — network effects, app-store gatekeeping, capital-cycle shifts — that were concentrating the market regardless?',
    'Difference-in-differences against matched sectors and jurisdictions with weaker erasure obligations, controlling for pre-trend concentration and funding cycles; natural experiments from obligation-stringency changes across adequacy regimes.',
    'High attribution loads the measured extractiveness onto this constraint and validates the moat reading''s epsilon; low attribution shrinks this constraint''s epsilon substantially, with the remainder reassigned to the ambient platform constraints that are separate stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(concentration_attribution_counterfactual, empirical, 'Counterfactual attribution of observed market consolidation to this constraint.').

omega_variable(
    incumbent_maintenance_intent,
    'Do incumbents actively maintain the barrier — lobbying for stringent implementation detail, funding compliance complexity that rivals must mirror, acquiring distressed challengers — or do they merely collect a barrier they did not curate?',
    'Transparency registries of lobbying expenditure targeted at implementing acts and guidelines, paired with acquirer-pattern analysis of compliance-distressed startups; distinguish defensive-standard participation from passive adaptation.',
    'Active curation of complexity shifts the effective agenda toward the beneficiary seat and supports reclassification pressure toward snare; passive collection leaves the tangled_rope steady state intact with the legislature holding genuine amendment agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_maintenance_intent, empirical, 'Whether beneficiaries curate the barrier or inherit it.').

omega_variable(
    privacy_autonomy_vs_competitive_welfare_weighting,
    'How should the arrangement trade individual data-control value against the competitive-supply losses this reading documents — is a thinned market a cost at all if individuals rank deletion control above variety?',
    'Not resolvable by data alone: revealed-preference studies can estimate the weights individuals actually hold, but the aggregation rule (whose ranking counts, how losses to non-users of entrant products enter the ledger) is a normative choice the parties must make.',
    'A weighting that discounts competitive supply converts much of this reading''s measured harm into acceptable coordination price and softens the classification; a weighting that prices contestability heavily hardens it. The epsilon referent stays fixed either way — only its assessment moves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privacy_autonomy_vs_competitive_welfare_weighting, preference, 'Value weighting between data-control benefits and competition losses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__competitive_moat_reading, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__competitive_moat_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(arti_tr_t2, article17_erasure_right__competitive_moat_reading, theater_ratio, 2, 0.23).
narrative_ontology:measurement(arti_tr_t4, article17_erasure_right__competitive_moat_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(arti_tr_t5, article17_erasure_right__competitive_moat_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(arti_tr_t6, article17_erasure_right__competitive_moat_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement(arti_tr_t7, article17_erasure_right__competitive_moat_reading, theater_ratio, 7, 0.32).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__competitive_moat_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(arti_be_t2, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2, 0.57).
narrative_ontology:measurement(arti_be_t4, article17_erasure_right__competitive_moat_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(arti_be_t5, article17_erasure_right__competitive_moat_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(arti_be_t6, article17_erasure_right__competitive_moat_reading, base_extractiveness, 6, 0.64).
narrative_ontology:measurement(arti_be_t7, article17_erasure_right__competitive_moat_reading, base_extractiveness, 7, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__competitive_moat_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(arti_su_t2, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2, 0.5).
narrative_ontology:measurement(arti_su_t4, article17_erasure_right__competitive_moat_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement(arti_su_t5, article17_erasure_right__competitive_moat_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(arti_su_t6, article17_erasure_right__competitive_moat_reading, suppression_requirement, 6, 0.59).
narrative_ontology:measurement(arti_su_t7, article17_erasure_right__competitive_moat_reading, suppression_requirement, 7, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__competitive_moat_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Article 17 right to erasure' covers three structurally distinct claims sharing one kernel text. This story instantiates the competitive-moat reading: beneficiaries are incumbent platforms amplified by compliance-cost asymmetry, victims are challengers and small controllers, and epsilon is authored at 0.65 for the standing erasure regime as a market-structuring device. The sibling privacy-fundamental reading authors low epsilon over the SAME standing arrangement (beneficiaries relocate to data subjects receiving sovereign control); the sibling censorship-mechanism reading authors high suppression with victims relocating to speakers and archivists. Each sibling is a separate file with its own epsilon, stakeholder surface, and classification; all three link here via network edges because the upstream privacy justification is routinely cited as cover for the downstream moat effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
