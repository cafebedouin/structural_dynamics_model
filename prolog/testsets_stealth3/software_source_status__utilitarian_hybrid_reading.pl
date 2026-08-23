% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Welfare-Contingent Licensing Governance (Utilitarian Hybrid Reading)
 *   domain: economic/political/software_engineering
 *
 * SUMMARY:
 *   This story instantiates one reading of the software-source-status kernel:
 *   the utilitarian hybrid position that neither open nor proprietary
 *   licensing is categorically required, and that the legitimate model for
 *   any given context is whichever maximizes aggregate welfare. The
 *   constraint under classification is the operative governance framework
 *   built on that position — the funder conditions, procurement rules, and
 *   firm licensing strategies that assign each software context a model by
 *   welfare assessment. Its epsilon referent is the standing mixed-ecosystem
 *   arrangement this framework governs, assessed by this reading's own
 *   lights: the reading endorses mixed ecosystems, so epsilon measures how
 *   far the ACTUAL operation of context-contingent selection departs from
 *   welfare-tracking selection — not the performance of any alternative
 *   arrangement. The claim/metrics split is deliberate: the framework is
 *   CLAIMED as rope (a genuine allocation mechanism with no categorical
 *   victim set, matching this reading's structural signature), while the
 *   authored metrics describe operation with accumulating enclosure pressure
 *   — welfare vocabulary increasingly deployed to dress competitive closure,
 *   enforcement machinery maturing around procurement and grant terms. The
 *   engine measures that divergence; the claim is not reconciled to the
 *   metrics.
 *
 * KEY AGENTS:
 *   - - research_funding_agencies + national_procurement_authorities: Agenda-setting institutional seats (institutional/mobile and institutional/constrained) — attach and enforce per-context licensing conditions on public money and public purchasing
 *   - - dual_license_vendors: Primary beneficiary with norm-shaping secondary role (powerful/arbitrage) — collects enclosure-legitimated revenue and co-authors the welfare standard
 *   - - platform_cloud_providers: Dual-positioned beneficiary/payer (powerful/mobile) — profits from open hosting, pays when openness is withdrawn
 *   - - open_source_foundations: Protected beneficiary with constrained exit (organized/generational) — stewards the commons the settlement safeguards
 *   - - volunteer_contributors_to_enclosed_projects: Contingent payer (moderate/constrained) — absorbed labor when projects close under sustainability rationales
 *   - - captive_users_of_specialized_tools: Contingent payer (moderate/trapped) — bears rising fees where closure was welfare-defended
 *   - - downstream_dependents_unconsulted: Excluded seat (powerless/trapped) — bears decisions it had no procedural voice in
 *   - - technology_policy_researchers: Analytical observer — audits stated versus revealed welfare rationales
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.38).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.26).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Welfare-Contingent Licensing Governance (Utilitarian Hybrid Reading)").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "economic/political/software_engineering").

domain_priors:requires_active_enforcement(software_source_status__utilitarian_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, 'e82d06de-f062-4652-8c7d-55b4d3596a32').
narrative_ontology:cs_kernel_codification('e82d06de-f062-4652-8c7d-55b4d3596a32', distributed).
narrative_ontology:cs_authority_grounding('e82d06de-f062-4652-8c7d-55b4d3596a32', expertise).
narrative_ontology:cs_interpretation_layer_present('e82d06de-f062-4652-8c7d-55b4d3596a32').
narrative_ontology:cs_reading_relation('e82d06de-f062-4652-8c7d-55b4d3596a32', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('e82d06de-f062-4652-8c7d-55b4d3596a32', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('e82d06de-f062-4652-8c7d-55b4d3596a32', software_source_status__property_rights_reading, influences).
narrative_ontology:cs_axiom('e82d06de-f062-4652-8c7d-55b4d3596a32', foundational, aggregate_welfare_supremacy_in_licensing).
narrative_ontology:cs_axiom_status(aggregate_welfare_supremacy_in_licensing, holdable).
narrative_ontology:cs_axiom_grounding('e82d06de-f062-4652-8c7d-55b4d3596a32', aggregate_welfare_supremacy_in_licensing, instrumental).
narrative_ontology:cs_axiom('e82d06de-f062-4652-8c7d-55b4d3596a32', foundational, no_categorical_model_mandate).
narrative_ontology:cs_axiom_status(no_categorical_model_mandate, holdable).
narrative_ontology:cs_axiom_grounding('e82d06de-f062-4652-8c7d-55b4d3596a32', no_categorical_model_mandate, empirically_contingent).
narrative_ontology:cs_reference_frame('e82d06de-f062-4652-8c7d-55b4d3596a32', welfare_maximizing_mixed_ecosystem).
narrative_ontology:cs_drift_state('e82d06de-f062-4652-8c7d-55b4d3596a32', contemporary_commercial_open_source_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e82d06de-f062-4652-8c7d-55b4d3596a32', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, dual_license_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, platform_cloud_providers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, open_source_foundations).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, enterprise_software_buyers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, public_funded_infrastructure_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, platform_cloud_providers).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, volunteer_contributors_to_enclosed_projects).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, captive_users_of_specialized_tools).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Attach open-licensing conditions to publicly funded software where their assessments indicate shared infrastructure serves the public better than restricted distribution, and relax or redirect conditions when evidence changes. Grant terms are rewritten on funding-cycle timescales, so they can move faster than procurement law.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, research_funding_agencies, agenda_setter,
    institutional, generational, mobile, continental).

% Write and enforce purchasing rules that weigh open-source requirements against vendor accountability, support contracts, and certification needs. Their rules bind thousands of agencies; revising them takes years, so they hold the framework in place more than they steer it.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, national_procurement_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Release software under open terms while selling commercial licenses and hosted tiers to customers whose compliance or integration needs exceed the open grant. Revenue arrives precisely where the closed option is justified by sustainability or scale arguments, and the firms' open-source program offices, standards participation, and policy submissions actively shape how licensing choices get evaluated.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, dual_license_vendors, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, dual_license_vendors, agenda_setter).

% Run other parties' open software as hosted services at scale, profiting from open availability without corresponding upstream contribution. When projects relicense to restrict hosted use, these providers pay real migration and rebuild costs, and they lobby hard against restrictive terms — they are simultaneously the largest beneficiaries of openness and the largest organized opponents of its withdrawal.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, platform_cloud_providers, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, platform_cloud_providers, payer).

% Hold trademarks, host infrastructure, and steward commons that the mixed-ecosystem settlement protects. They depend on corporate contributions and sponsorships that are legitimated by sustainability and public-benefit arguments, which limits how hard they can push back against the same sponsors' enclosure decisions.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_foundations, beneficiary,
    organized, generational, constrained, global).

% Choose per workload between open components and commercial products, capturing the option value the mixed ecosystem provides: commodity infrastructure at near-zero license cost, paid support where liability demands it. Switching costs exist per system but no buyer is bound to one model across their portfolio.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, enterprise_software_buyers, beneficiary,
    organized, biographical, mobile, global).

% Researchers, agencies, and civic technologists use code produced under public-funding openness conditions. Their benefit is structural — they could not individually purchase equivalent infrastructure — and their dependence means they track funding-policy changes closely but cannot shop elsewhere when a mandate lapses.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, public_funded_infrastructure_users, beneficiary,
    organized, generational, constrained, global).

% Contribute unpaid labor to projects that later convert to proprietary or source-available terms under sustainability rationales. Their merged work is absorbed into the closed product without compensation; leaving means abandoning communities, reputation, and mastery accumulated in a specific codebase, and comparable venues for the same work are scarce.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, volunteer_contributors_to_enclosed_projects, payer,
    moderate, biographical, constrained, global).

% Professions served by closed vertical tools — circuit design, clinical systems, aviation software — pay escalating license fees for products whose closed status was argued for on service, certification, and continuity grounds. Validated workflows, regulatory qualification, and training investment make switching effectively impossible even as prices rise.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, captive_users_of_specialized_tools, payer,
    moderate, biographical, trapped, global).

% The long tail of indirect dependents — applications, pipelines, teaching materials, derivative projects — that consume software through transitive dependency chains. No mechanism seats them in licensing or re-licensing decisions; they learn of changes when builds break, terms change, or downloads disappear, and they have no channel to contest the welfare reasoning that justified the change.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, downstream_dependents_unconsulted, excluded,
    powerless, civilizational, trapped, global).

% Study licensing outcomes, maintenance economics, and enclosure episodes; publish audits of whether stated welfare rationales match observed conduct. Their work feeds funder policy and procurement revision but carries no enforcement power of its own.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, technology_policy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__utilitarian_hybrid_reading, dual_license_vendors).
narrative_ontology:fixing_cost_class(software_source_status__utilitarian_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates development-and-distribution models across heterogeneous software contexts: general-purpose infrastructure is routed toward open licensing where networked maintenance and reuse dominate, while specialized, safety-regulated, or revenue-intensive tools may be routed toward proprietary licensing where sustained income funds certification and support. It settles the open-versus-closed allocation problem per domain instead of imposing a single categorical rule that would misfit half the ecosystem.
% TRANSFER_FUNCTION: Moves licensing legitimacy and, materially, surplus: public funds and volunteer labor flow toward open commons where the welfare assessment favors them; subscription and commercial-license revenue flows to vendors where closure is defended on sustainability grounds; and the recurring cost of deciding which model applies is shifted from ideology-bound standoffs to evidence-assessment processes run by funders, procurement bodies, and firm policy teams.
% ABSENT_VOICES: Downstream dependents, unconsulted user communities, and past contributors of re-licensed projects are absent from the small maintainer-vendor-funder rooms where enclosure decisions are made. They object after the fact — when builds break or terms change — and their objections arrive without a procedural seat, so the welfare calculus that justified each closure was computed over the preferences of those present.
% DISAPPEARANCE_RATIONALE: Funder grant terms, procurement rules, dual-license business plans, foundation sponsorship agreements, and enterprise sourcing strategies all presuppose per-context licensing optimization. Overnight removal forces every jurisdiction and firm back toward a categorical default — universal opening or universal propertization — breaking specialized-tool financing in the first case and fragmenting shared infrastructure in the second, followed by years of renegotiation toward some new allocation regime.
% FOUNDING_PROBLEM: The categorical doctrines had deadlocked the field: a universal openness mandate starved revenue-dependent specialized development and support, while a universal property mandate fragmented shared infrastructure and forced wasteful duplication. The hybrid framework was built to solve allocation — assigning each software context the model that maximizes total output and access rather than the model a doctrine commands.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by the academic economics-of-open-source literature, public-sector ICT and procurement reviews, and post-mortem audits of national open-source programs, all of which document that the allocation problem recurs in each new domain. Corroboration is partial in character: a large share of the loudest attestation comes from beneficiary firms' own sustainability whitepapers, so the independent evidentiary base is specifically the scholarly and audit literatures, and it is thinner than the volume of self-interested advocacy surrounding the framework.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).
:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.38 — well above a pure coordination cost but far below categorical-extraction territory — because the framework's allocation function is real while its welfare vocabulary is increasingly borrowed for enclosure justifications (open-core conversions, hosted-use restrictions, strategic de-opening). Suppression is low (0.26): the framework suppresses no alternative model — it exists to legitimize pluralism — but its enforcement arm (grant conditions, procurement compliance, trademark governance) has grown from near-nothing to a real disciplinary layer, hence the rising suppression_requirement series rather than a static scalar. Theater_ratio (0.36 and climbing) tracks open-washing: sustainability and public-benefit language attached to decisions whose revealed driver is competitive positioning. Accessibility_collapse is low (0.35) because understanding the framework collapses nothing — both models remain fully available by design. Resistance is substantial (0.55): categorical partisans on both sides reject the framework's refusal to settle, and community backlash to enclosure episodes is organized and recurring. The temporal picture is monotonic accumulation, not oscillation — no cyclical mechanism is claimed, and all three series share one six-point grid so the engine samples a consistent history. The rising base_extractiveness series is exactly the profile the extraction-accumulation abductive trigger watches; it does not reclassify, but it dates the drift toward the rope/tangled boundary. Contributor exit deserves note: reputation and community membership fuse contributors to their projects, so their constrained-exit rating carries an identity component that would raise their effective exposure if the frame broke.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute sharply different types from identical structural facts. From the agenda-setting seats the framework is evidence-based allocation they administer and can defend from their own audits; from the contingent payer seats — contributors whose work was absorbed, captive users of closed vertical tools — the same framework operates as a legitimacy machine for decisions made over their heads; from the powerful beneficiary seats it is the settlement that makes their business models licit. Same-level divergence is equally structural: the two powerful beneficiary seats differ because one (dual-license vendors) collects from closure it helps argue for while the other (cloud providers) pays when openness closes; the two moderate payer seats differ by exit — contributors retain constrained mobility across venues while captive tool users are trapped by validated workflows and regulatory qualification. The engine computes these per-seat classifications from power, exit, and declared position; nothing here adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared in base_properties drive low directionality for the organized beneficiary cluster: foundations, buyers, and public-infrastructure users derive near-subsidy positions, correctly, since the framework shields their interests at little cost to them. Two corrections are declared as overrides because the automatic derivation misprices the affected seats. First, the powerful seats (dual_license_vendors, platform_cloud_providers): the derivation reads declared beneficiaries and outputs a near-beneficiary d, but both seats co-author the welfare standard itself — their program offices and policy submissions shape which closures count as welfare-justified — so their effective position is partially extractive-adjacent; overridden to d=0.28. Second, the moderate seats (volunteer contributors, captive tool users): they appear in no beneficiary or victim declaration, so they would receive the power-atom fallback, but descriptively they are the framework's contingent payers — overridden to d=0.66. Institutional agenda-setters derive near-symmetric positions (they administer without collecting), and the powerless excluded seat derives a high target-side d reflecting its exposure to unconsulted decisions. Note on scaling: suppression enters computation as a raw structural property, unscaled by power or scope; only extractiveness is scaled, which is why the framework's growing enforcement layer registers in the suppression series without inflating per-seat extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding allocation problem is live — each new domain (hosted services, model weights, security-mandated transparency) reopens the open-versus-closed question — so no mandatrophy is declared and none should be inferred from the rising theater ratio. The classification's protective work runs the other direction: because this reading declares no categorical victim set, a lazy reading of the enclosure episodes could inflate the framework into a snare, converting contingent, seat-local costs into a fabricated victim class; conversely, the framework's genuine coordination function could be cited to wave off the accumulating capture evidence. The correct guard is the mismatch check: if the founding problem were ever attested dead while the world-rearranges verdict persisted, the capture/zombie flag should fire against the rising theater series — the framework persisting as performance after its allocation function lapsed. At the current interval endpoint the problem is live, enforcement is maturing rather than atrophying, and the honest posture is rope-with-drift, watched.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    source_status_kernel_disagreement,
    'This constraint instantiates only the utilitarian_hybrid_reading of kernel software_source_status. Where exactly is the disagreement with the sibling readings located, and what would each displacement change structurally?',
    'Track which reading successive jurisdictions, funders, and major projects converge on: adoption of the freedom-imperative reading would manufacture a categorical victim set (any proprietary operator becomes a rights violator); adoption of the property-rights reading removes welfare conditionality (closure needs no justification); adoption of the pragmatic-development reading collapses context-selection into a general open-superiority rule.',
    'Displacement by any sibling changes the beneficiary/victim structure wholesale — the hybrid''s defining feature (no categorical victims, mixed ecosystems acceptable) dissolves, and this story''s epsilon and classification cease to describe the operative constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(source_status_kernel_disagreement, conceptual, 'Committer structure: one reading of a four-reading kernel; the disagreement is located in whether licensing permissibility is categorical (freedom, property) or indexed to a decision procedure (hybrid, pragmatic-leaning hybrid).').

omega_variable(
    welfare_function_underdetermination,
    'Whose welfare does the framework aggregate, and how are individual items — developer autonomy, user access, vendor revenue, maintenance sustainability — weighted inside the calculus?',
    'Explicit specification of the social welfare function behind funder and procurement assessments; comparative audit of decisions reached under different weighting schemes.',
    'Heavy weighting of producer autonomy converges this reading toward the freedom-imperative sibling; consumer-surplus-only weighting legitimizes substantially more enclosure and raises effective epsilon; the classification swings on the aggregation choice, which the framework currently leaves implicit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_function_underdetermination, conceptual, 'The aggregate-welfare standard is underspecified; epsilon is stable only given a fixed aggregation, which no seat currently authors.').

omega_variable(
    enclosure_capture_vs_complexity,
    'Does the rising theater_ratio and base_extractiveness reflect rent-seeking capture of the welfare vocabulary, or genuine growth in the share of contexts where welfare-honest analysis supports closure?',
    'Paired audit of enclosure episodes: compare stated welfare rationales against revealed conduct (pricing, fork suppression, feature withholding), and test counterfactual openness in the affected contexts.',
    'Confirmed capture converts the drift from benign complexity into strategic enclosure and pushes affected seats'' computed types toward the tangled-rope/snare flank; confirmed honest complexity supports the rope claim at higher metric values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enclosure_capture_vs_complexity, empirical, 'Whether welfare language is being captured or the welfare-honest closure share is genuinely rising.').

omega_variable(
    enforcement_hardening_trajectory,
    'Will the maturing enforcement layer (grant conditions, procurement compliance, trademark governance) stabilize the framework as low-overhead coordination, or harden into gatekeeping that extracts compliance costs from smaller actors?',
    'Compare administrative burden and compliance failure rates across actor sizes as mandate regimes mature; track whether enforcement revisions respond to evidence or to incumbent preference.',
    'Hardening into size-differentiated gatekeeping would raise suppression for small actors specifically and push the framework across the rope/tangled boundary; stabilization preserves the rope classification with the current metric profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_hardening_trajectory, empirical, 'Direction of the enforcement-capacity trajectory the suppression_requirement series is tracing.').

omega_variable(
    welfare_standard_framing_underdetermination,
    'Is the right commitment-system framing the licensing-allocation norm administered by policy bodies, or the aggregate-welfare standard itself functioning as the kernel — an interpretive tradition that absorbs strategic closure by re-describing it as a welfare case?',
    'Test the alternative framing against the same episodes: if the standard''s interpretive layer systematically launders enclosure decisions without ever surfacing kernel revision, the standard-as-kernel framing is the better fit.',
    'Under the alternative framing, authority_grounding shifts from expertise toward extraction — the authority structure''s benefit flows from preventing the standard''s own revision — changing the commitment-system classification and the drift interpretation (practice_drift becomes acknowledged kernel management).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_standard_framing_underdetermination, conceptual, 'CS-framing underdetermination: institution-level versus standard-level kernels yield different authority and drift classifications for the same episodes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t6, software_source_status__utilitarian_hybrid_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(soft_tr_t6, observed).
narrative_ontology:measurement(soft_tr_t12, software_source_status__utilitarian_hybrid_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(soft_tr_t12, observed).
narrative_ontology:measurement(soft_tr_t18, software_source_status__utilitarian_hybrid_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement_basis(soft_tr_t18, observed).
narrative_ontology:measurement(soft_tr_t24, software_source_status__utilitarian_hybrid_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement_basis(soft_tr_t24, observed).
narrative_ontology:measurement(soft_tr_t30, software_source_status__utilitarian_hybrid_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(soft_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t6, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 6, 0.24).
narrative_ontology:measurement_basis(soft_be_t6, observed).
narrative_ontology:measurement(soft_be_t12, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement_basis(soft_be_t12, observed).
narrative_ontology:measurement(soft_be_t18, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 18, 0.31).
narrative_ontology:measurement_basis(soft_be_t18, observed).
narrative_ontology:measurement(soft_be_t24, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 24, 0.35).
narrative_ontology:measurement_basis(soft_be_t24, observed).
narrative_ontology:measurement(soft_be_t30, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(soft_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t6, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 6, 0.11).
narrative_ontology:measurement_basis(soft_su_t6, observed).
narrative_ontology:measurement(soft_su_t12, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 12, 0.14).
narrative_ontology:measurement_basis(soft_su_t12, observed).
narrative_ontology:measurement(soft_su_t18, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 18, 0.18).
narrative_ontology:measurement_basis(soft_su_t18, observed).
narrative_ontology:measurement(soft_su_t24, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 24, 0.22).
narrative_ontology:measurement_basis(soft_su_t24, observed).
narrative_ontology:measurement(soft_su_t30, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 30, 0.26).
narrative_ontology:measurement_basis(soft_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how software source should be governed' decomposes, per the epsilon-invariance principle, into four structurally distinct constraints — one per reading of the software_source_status kernel. Each reading carries its own epsilon over its own referent arrangement: the freedom reading measures the proprietary-use arrangement as rights violation; the property reading measures open mandates as expropriation; the pragmatic reading measures methodology adherence; this hybrid reading measures the mixed-ecosystem allocation arrangement by its own welfare-tracking standard. They are separate stories linked by this network edge, not one story with a measurement parameter. Upstream-downstream structure within the family: whichever reading dominates funding and procurement policy sets the resource environment for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_source_status__utilitarian_hybrid_reading, powerful, 0.28).
constraint_indexing:directionality_override(software_source_status__utilitarian_hybrid_reading, moderate, 0.66).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
