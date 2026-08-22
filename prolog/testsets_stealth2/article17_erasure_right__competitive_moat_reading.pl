% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__competitive_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: article17_erasure_right__competitive_moat_reading
 *   human_readable: Article 17 Right to Erasure — Competitive Moat Reading
 *   domain: technology governance/data protection law/competition policy
 *
 * SUMMARY:
 *   Read from the competitive-moat seat, the GDPR right to erasure is a
 *   market-structuring instrument: a uniform erasure obligation whose
 *   compliance economics are scale-asymmetric. Fulfilling Article 17
 *   correctly requires identity-verification flows, deletion cascades across
 *   microservices, backups, and downstream processors, response-clock
 *   management, and audit-proof documentation — a fixed-cost stack that
 *   amortizes to near-zero per user at incumbent volume while consuming a
 *   third or more of early engineering headcount at entrant scale. The same
 *   obligation therefore taxes challengers and subsidizes relative advantage
 *   for incumbents, who additionally convert compliance capability into a
 *   trust-assurance premium sold to enterprise customers. Enforcement
 *   machinery (fines to 4% of worldwide turnover) compels performance; a
 *   RegTech vendor ecosystem monetizes the obligation's complexity. The
 *   genuine coordination function — a standardized deletion channel
 *   individuals could never win contractually — is real and sits alongside
 *   the asymmetric incidence; hence the tangled-rope claim. This file is one
 *   reading of the article17_erasure_right kernel; the sibling readings are
 *   separate constraint files with their own epsilon values. KEY AGENTS (by
 *   structural relationship): - large_platform_incumbents: Primary
 *   beneficiary (institutional/arbitrage) — amortizes the fixed compliance
 *   stack, collects the trust premium and per-user cost advantage -
 *   eu_startup_challengers: Primary target (moderate/constrained) — bears
 *   disproportionate per-revenue compliance cost from day one -
 *   small_medium_data_controllers: Secondary target (powerless/trapped) —
 *   part-time compliance capacity, silent error accumulation, audit exposure
 *   - regtech_compliance_vendors: Secondary beneficiary (organized/mobile) —
 *   sells the compliance capability the mandate creates - data_subjects:
 *   Nominal right-holders (organized/constrained) — receive a working
 *   deletion mechanism, bear diffuse competition cost -
 *   eu_data_protection_authorities: Agenda setter (institutional/analytical)
 *   — administers guidelines, complaints, and fines -
 *   competition_authorities: Analytical observer (institutional/analytical) —
 *   investigates the market-structure effects - eu_seed_investors: Payer with
 *   arbitrage exit (powerful/arbitrage) — prices the compliance risk and
 *   allocates elsewhere
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, 0.66).
domain_priors:suppression_score(article17_erasure_right__competitive_moat_reading, 0.58).
domain_priors:theater_ratio(article17_erasure_right__competitive_moat_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__competitive_moat_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__competitive_moat_reading, "Article 17 Right to Erasure — Competitive Moat Reading").
narrative_ontology:topic_domain(article17_erasure_right__competitive_moat_reading, "technology governance/data protection law/competition policy").

domain_priors:requires_active_enforcement(article17_erasure_right__competitive_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__competitive_moat_reading, 'c25fd9d3-82ee-40e3-827a-859477b5fe49').
narrative_ontology:cs_kernel_codification('c25fd9d3-82ee-40e3-827a-859477b5fe49', fixed_text).
narrative_ontology:cs_authority_grounding('c25fd9d3-82ee-40e3-827a-859477b5fe49', lineage).
narrative_ontology:cs_interpretation_layer_present('c25fd9d3-82ee-40e3-827a-859477b5fe49').
narrative_ontology:cs_reading_relation('c25fd9d3-82ee-40e3-827a-859477b5fe49', article17_erasure_right__privacy_fundamental_reading, influences).
narrative_ontology:cs_reading_relation('c25fd9d3-82ee-40e3-827a-859477b5fe49', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('c25fd9d3-82ee-40e3-827a-859477b5fe49', foundational, compliance_cost_scales_sublinearly_with_controller_size).
narrative_ontology:cs_axiom_status(compliance_cost_scales_sublinearly_with_controller_size, holdable).
narrative_ontology:cs_axiom_grounding('c25fd9d3-82ee-40e3-827a-859477b5fe49', compliance_cost_scales_sublinearly_with_controller_size, empirically_contingent).
narrative_ontology:cs_axiom('c25fd9d3-82ee-40e3-827a-859477b5fe49', foundational, market_filtering_is_the_binding_function).
narrative_ontology:cs_axiom_status(market_filtering_is_the_binding_function, holdable).
narrative_ontology:cs_axiom_grounding('c25fd9d3-82ee-40e3-827a-859477b5fe49', market_filtering_is_the_binding_function, empirically_contingent).
narrative_ontology:cs_reference_frame('c25fd9d3-82ee-40e3-827a-859477b5fe49', scale_asymmetric_compliance_regime).
narrative_ontology:cs_drift_state('c25fd9d3-82ee-40e3-827a-859477b5fe49', contemporary_dma_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('c25fd9d3-82ee-40e3-827a-859477b5fe49', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__competitive_moat_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, large_platform_incumbents).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, regtech_compliance_vendors).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, eu_startup_challengers).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, small_medium_data_controllers).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, data_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, data_subjects).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, large_platform_incumbents).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, eu_seed_investors).
narrative_ontology:constraint_vindicates(article17_erasure_right__competitive_moat_reading, scale_advantage_under_uniform_obligations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate global consumer-data platforms with dedicated erasure-fulfillment engineering: searchable-index deletion, backup purge cycles, downstream-recipient notification, and audit trails built once and amortized across billions of records. Marginal cost per erasure request is near zero at their scale, while the same obligation requires a ten-person startup to buy or build the equivalent pipeline. They publish deletion-volume transparency reports, market compliant infrastructure to enterprise customers as a trust premium, and maintain Brussels lobbying operations that shaped the regulation's drafting and now shape its enforcement guidance. Leaving the EU market is unthinkable at their scale; shaping the rules is cheaper than leaving.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, large_platform_incumbents, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, large_platform_incumbents, payer).

% Sell erasure-workflow software, data-discovery scanning, records-of-processing tools, and compliance consulting. Every element of the erasure obligation is a line item in their demand curve; the obligation's complexity is their product roadmap. Their customer base concentrates on mid-market firms that cannot build in-house. Exit is easy — the same tooling sells into adjacent privacy regimes (CCPA, LGPD) worldwide.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, regtech_compliance_vendors, beneficiary,
    organized, biographical, mobile, global).

% Consumer-data startups in the EU face the erasure obligation at full force from day one: identity-verification flows, deletion cascades across microservices and third-party processors, thirty-day response clocks, and documentation adequate to survive a supervisory-authority audit. Founders report diverting a third or more of early engineering headcount to compliance plumbing that produces no user-visible feature. Relocating outside the EU forfeits their home market; staying means carrying a fixed cost their non-EU competitors do not.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, eu_startup_challengers, payer,
    moderate, biographical, constrained, continental).

% Regional retailers, agencies, publishers, and SaaS shops process personal data with one or two people touching compliance part-time. Erasure requests arrive through email and web forms; fulfilling them correctly across CRM, mailing lists, backups, and analytics exceeds in-house capacity, so errors accumulate silently until a complaint triggers an audit. Shutting down or going data-free ends the business; continuing means living with the exposure.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, small_medium_data_controllers, payer,
    powerless, biographical, trapped, national).

% EU residents can demand deletion of their data and increasingly do, aided by NGO template letters and one-click request forms. They receive a working deletion mechanism that no contract negotiation ever gave them. On the other side of the ledger they face a startup ecosystem thinner than comparable non-EU markets, fewer niche services, and consolidation toward platforms that can absorb the overhead — costs that arrive as missing alternatives rather than as bills.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, data_subjects, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, data_subjects, payer).

% National supervisory authorities and the EDPB issue guidelines, handle complaints, and levy administrative fines up to four percent of worldwide turnover. Their caseload skews toward large controllers because complaints concentrate there, while small-controller failures surface mostly through breaches. Published guidance acknowledges that obligations must be applied proportionately to controller size, and they depend on the regulated community for much of the technical expertise they audit.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Competition directorates and sector regulators examine whether erasure-compliance economics contribute to concentration: merger reviews weigh compliance-capability gaps between merging parties, and market studies ask whether fixed regulatory costs tilt entry decisions. They command economic staff the data-protection authorities lack, and their remedies — mandated interoperability, data-portability enforcement — would reshape the obligation's incidence without touching its text.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, competition_authorities, observer,
    institutional, generational, analytical, continental).

% Early-stage investors pricing EU consumer-data deals add a compliance-risk premium: longer diligence on data flows, reserve tranches for regulatory readiness, and a visible tilt of consumer-data seed activity toward US, UK, and Asian vehicles. Capital is the most mobile actor in the story — the same fund can back the identical founder team through a Delaware or Singapore entity and serve the EU market later, or not at all.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, eu_seed_investors, payer,
    powerful, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__competitive_moat_reading, large_platform_incumbents).
narrative_ontology:fixing_cost_class(article17_erasure_right__competitive_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes a single legally enforceable deletion mechanism across every controller–data-subject relationship in the jurisdiction, replacing case-by-case contractual negotiation that individuals could not win; publishes a common expectation for data-lifecycle hygiene (processing records, deletion cascades, processor contracts) that lets data subjects verify handling without auditing each firm themselves.
% TRANSFER_FUNCTION: Moves compliance work and capital from small and newly entering controllers into compliance infrastructure — much of it purchased from RegTech vendors — and, through the fixed-cost structure of that infrastructure, transfers addressable market share and fundraising terms from entrants to incumbents whose per-user compliance cost approaches zero.
% ABSENT_VOICES: Would-be founders who priced the compliance stack ex ante and never incorporated — the filter's casualties are invisible because they never enter the record. Also absent: small controllers without trade-association representation in Brussels consultations, and competition economists, who sat outside the data-protection rulemaking process that set the obligation's uniform design.
% DISAPPEARANCE_RATIONALE: Overnight removal would collapse the fixed compliance cost that tilts entry decisions: EU consumer-data formation would rise toward non-EU baselines, RegTech demand would fall, incumbents would lose the trust-assurance premium and the per-user cost advantage, and data subjects would lose the standardized deletion channel — reverting to jurisdiction-by-jurisdiction contractual requests that historically failed (pre-Google-Spain delisting refusals).
% FOUNDING_PROBLEM: Individuals had no workable mechanism to force deletion of persistent personal data: search engines refused delisting outright (Google Spain), data brokers retained profiles indefinitely, and no contract term or ombudsman could reach copies spread across processors and backups.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the commercial beneficiary set: CJEU case law predating codification (C-131/12, Google Spain) documents the enforcement gap the provision answers; civil-society organizations (EDRi, NOYB) and academic consumer surveys attest the underlying data-persistence problem remains live. No element of the attestation relies on platform-incumbent self-reporting.
narrative_ontology:disappearance_verdict(article17_erasure_right__competitive_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__competitive_moat_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__competitive_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__competitive_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__competitive_moat_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon 0.66 reflects this reading's assessment of the standing arrangement: the erasure obligation delivers a real service, but its incidence is scale-asymmetric, so the same rule taxes entrants and consolidates incumbents. Suppression 0.58 measures the coercive machinery compelling controller performance — administrative fines escalating through the interval (CNIL 2019, Luxembourg 2021, Meta 2023), processing bans, audit exposure — structural, unscaled by scope in the engine's arithmetic, and aimed at forcing compliance rather than closing exits. Theater 0.30: fulfillment pipelines are real and audited, but boilerplate responses, consent-banner ritual, and box-ticking records grow yearly. Accessibility_collapse 0.45: substitutes existed and persist (contractual deletion clauses, self-certification, industry ombudsmen) but were historically unenforceable against refusal, so the obligation collapsed them only partially. Resistance 0.55: sustained industry lobbying during drafting and enforcement, SME burden complaints, DMA-era renegotiation, and jurisdiction-shopping by investors. The three temporal series share one eight-point grid (2018–2025) so every metric is authored at every examined time point; suppression_requirement is tracked because enforcement-capacity change is the story's traced dynamic — the machinery visibly hardened over the interval rather than sitting static. Coalition note: SMEs are individually powerless, but their trade associations and shared-tooling cooperatives are the credible coalition path; coalition formation is the main upside risk to the payer seats' computed extraction.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the incumbent seat the arrangement reads as a costly-but-manageable obligation it excels at meeting — largest absolute budgets, free deletion delivered at scale — a rope-flavored experience. From the entrant and SME seats the same text operates as a filter met before revenue exists — snare-flavored — and for firms that never launched it is decisive yet invisible, leaving no complainant in the record. The DPA seat experiences coordination-first administration; the investor seat experiences a pricing input rather than a burden, because its exit is arbitrage. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (incumbents, RegTech vendors) derive low d; victim declarations (challengers, SMEs, and data subjects' diffuse competition cost) derive high d; exit modulates — trapped SMEs sit nearer the full-target end than constrained challengers, and analytical seats sit near zero. One explicit override: eu_seed_investors (powerful) — the derivation would read their payer role as near-full-target (~0.8), but arbitrage-grade exit (Delaware or Singapore vehicles serving the EU later, or never) places them at 0.55. Overrides for incumbents and data subjects were considered and rejected: the override mechanism keys on power_atom per story, so an institutional-atom override would also seize the DPAs and competition authorities, and an organized-atom override would seize the RegTech vendors — each collateral misstatement exceeding the correction. The incumbents' dual position rides on their secondary_role payer instead; the residual understatement of their cost-bearing is accepted rather than distorting every institutional seat with a blunt atom-keyed override.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying from the moat seat alone risks calling a snare what is structurally a tangled rope: the standardized deletion channel solves a collective-action problem individuals provably could not solve contractually (pre-2014 delisting refusals), so pure-extraction coding would erase the coordination half and mispredict behavior if the asymmetry were repaired. Conversely, a rope coding would erase the asymmetric incidence the cost data show. Mandatrophy is NOT declared: the founding problem — unreachable deletion — is live and corroborated by courts and civil society, so the arrangement has not outlived its function. The drift risk runs the other direction: theater_ratio climbing toward proxy-compliance (0.18 to 0.30) while enforcement concentrates on large controllers; if enforcement capacity decays while boilerplate grows, the structure slides toward piton, which the temporal series is positioned to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the competitive_moat_reading of the article17_erasure_right kernel; would the privacy_fundamental_reading or the censorship_mechanism_reading classify the same instrument differently, and which reading captures its operative structure?',
    'Compile the two sibling stories on a shared stakeholder surface and compare engine-computed per-seat classifications; divergence localizes the disagreement to specific structural elements (victim sets, directionality distribution, enforcement object).',
    'If the privacy_fundamental_reading dominates, the victim set becomes uniform duty-bearers (all controllers regardless of scale) and epsilon falls toward the coordination floor; if the censorship reading dominates, victims become speakers and archives and the analysis shifts to request-abuse vectors rather than compliance economics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Reading-indexed classification of the Article 17 kernel; this file authors only the competitive-moat instantiation.').

omega_variable(
    reading_disagreement_location,
    'Where exactly do the three readings disagree — on the instrument''s binding function (sovereignty limit vs. speech suppression vs. market filter), or on the incidence of its costs (which controller classes pay)?',
    'Per-seat directionality comparison across the three sibling stories: seats with convergent classifications mark shared structure; divergent seats mark the contested element.',
    'Determines whether the corpus treats the three stories as one constraint family holding three epsilon values over a shared referent, or as three constraints with different referents requiring separate integration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_disagreement_location, conceptual, 'Locates the structural element on which the sibling readings of the erasure kernel actually diverge.').

omega_variable(
    compliance_cost_scaling_empirics,
    'Does per-data-subject erasure compliance cost actually fall sublinearly with controller scale, as the moat mechanism requires?',
    'Audited compliance-expenditure disclosures scaled by records under management; supervisory-authority fee structures; RegTech procurement data segmented by firm-size band.',
    'Linear scaling collapses the asymmetry and drops this reading''s epsilon toward the privacy reading''s; strongly sublinear scaling confirms the filter and raises effective extraction on small controllers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_scaling_empirics, empirical, 'The cost-curve shape on which the entire competitive-filter mechanism rests.').

omega_variable(
    counterfactual_entry_rate,
    'Would EU consumer-data startup formation have been materially higher absent Article 17, holding other regulation constant?',
    'Difference-in-differences across jurisdictions and dates: member states with pre-existing data-protection statutes versus those without; UK post-Brexit divergence; matched non-EU markets.',
    'Isolates the moat contribution from general regulatory burden; a null result demotes the competitive-filter mechanism to a minor channel and pulls the classification toward plain rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_entry_rate, empirical, 'Counterfactual entry differential attributable specifically to the erasure obligation.').

omega_variable(
    incumbent_support_revealed_preference,
    'Did large platforms support the erasure right in anticipation of compliance-cost advantages (revealed preference for a moat), or despite expecting them?',
    'Lobbying-disclosure archives, drafting-stage position papers, and internal documents surfaced in litigation; comparison of platform positions during drafting versus enforcement-era conduct.',
    'Confirmed anticipatory support upgrades the beneficiary declaration from incidental to structural and substantiates the agenda-setting footprint attributed to incumbents; sincere support leaves the moat as an unintended consequence with weaker capture implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_support_revealed_preference, empirical, 'Whether incumbent beneficiary status was sought or merely absorbed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__competitive_moat_reading, 2018, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2018, article17_erasure_right__competitive_moat_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement_basis(arti_tr_t2018, observed).
narrative_ontology:measurement(arti_tr_t2019, article17_erasure_right__competitive_moat_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement_basis(arti_tr_t2019, observed).
narrative_ontology:measurement(arti_tr_t2020, article17_erasure_right__competitive_moat_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement_basis(arti_tr_t2020, observed).
narrative_ontology:measurement(arti_tr_t2021, article17_erasure_right__competitive_moat_reading, theater_ratio, 2021, 0.24).
narrative_ontology:measurement_basis(arti_tr_t2021, observed).
narrative_ontology:measurement(arti_tr_t2022, article17_erasure_right__competitive_moat_reading, theater_ratio, 2022, 0.26).
narrative_ontology:measurement_basis(arti_tr_t2022, observed).
narrative_ontology:measurement(arti_tr_t2023, article17_erasure_right__competitive_moat_reading, theater_ratio, 2023, 0.27).
narrative_ontology:measurement_basis(arti_tr_t2023, observed).
narrative_ontology:measurement(arti_tr_t2024, article17_erasure_right__competitive_moat_reading, theater_ratio, 2024, 0.29).
narrative_ontology:measurement_basis(arti_tr_t2024, observed).
narrative_ontology:measurement(arti_tr_t2025, article17_erasure_right__competitive_moat_reading, theater_ratio, 2025, 0.3).
narrative_ontology:measurement_basis(arti_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t2018, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2018, 0.52).
narrative_ontology:measurement_basis(arti_be_t2018, observed).
narrative_ontology:measurement(arti_be_t2019, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2019, 0.56).
narrative_ontology:measurement_basis(arti_be_t2019, observed).
narrative_ontology:measurement(arti_be_t2020, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2020, 0.59).
narrative_ontology:measurement_basis(arti_be_t2020, observed).
narrative_ontology:measurement(arti_be_t2021, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2021, 0.61).
narrative_ontology:measurement_basis(arti_be_t2021, observed).
narrative_ontology:measurement(arti_be_t2022, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2022, 0.63).
narrative_ontology:measurement_basis(arti_be_t2022, observed).
narrative_ontology:measurement(arti_be_t2023, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2023, 0.64).
narrative_ontology:measurement_basis(arti_be_t2023, observed).
narrative_ontology:measurement(arti_be_t2024, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2024, 0.65).
narrative_ontology:measurement_basis(arti_be_t2024, observed).
narrative_ontology:measurement(arti_be_t2025, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement_basis(arti_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2018, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement_basis(arti_su_t2018, observed).
narrative_ontology:measurement(arti_su_t2019, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2019, 0.48).
narrative_ontology:measurement_basis(arti_su_t2019, observed).
narrative_ontology:measurement(arti_su_t2020, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement_basis(arti_su_t2020, observed).
narrative_ontology:measurement(arti_su_t2021, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2021, 0.53).
narrative_ontology:measurement_basis(arti_su_t2021, observed).
narrative_ontology:measurement(arti_su_t2022, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2022, 0.55).
narrative_ontology:measurement_basis(arti_su_t2022, observed).
narrative_ontology:measurement(arti_su_t2023, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2023, 0.57).
narrative_ontology:measurement_basis(arti_su_t2023, observed).
narrative_ontology:measurement(arti_su_t2024, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(arti_su_t2024, observed).
narrative_ontology:measurement(arti_su_t2025, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(arti_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__competitive_moat_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% Decomposition per the epsilon-invariance principle: the colloquial label 'right to erasure' covers three structurally distinct claims with materially different epsilon, beneficiary/victim sets, and failure modes. Family: privacy_fundamental_reading (upstream, highest empirical confidence, the legitimating reading cited at enactment) influences this file (downstream critique operationalized by competition authorities), and censorship_mechanism_reading sits parallel. The upstream reading supplies the legitimacy the compliance apparatus runs on; this reading's findings feed back as pressure on the upstream reading's operating environment without foreclosing it. Each member carries its own epsilon over the shared referent; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article17_erasure_right__competitive_moat_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
