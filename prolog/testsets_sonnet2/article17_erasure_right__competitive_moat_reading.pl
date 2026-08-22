% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__competitive_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: article17_erasure_right__competitive_moat_reading
 *   human_readable: GDPR Article 17 Right to Erasure — Compliance Cost as Competitive Moat
 *   domain: technology governance / data protection law / competition policy
 *
 * SUMMARY:
 *   This story instantiates the competitive-moat reading of the Article 17
 *   (GDPR right to erasure) kernel: the same legal text that grants
 *   individuals a right to have their personal data deleted also imposes a
 *   compliance-cost structure that scales inversely with the resources of the
 *   entity subject to it. Under this reading, incumbent platforms — which
 *   already possess distributed data architectures, dedicated legal-privacy
 *   staff, and prior experience with adjacent compliance regimes — absorb the
 *   marginal cost of erasure infrastructure far more easily than early-stage
 *   startups, open-source data projects, or regional challengers. Compliance
 *   technology vendors and major consulting firms profit directly from the
 *   complexity gap. The result, under this reading, is that a right framed as
 *   protecting individuals against corporate data power simultaneously
 *   entrenches the market position of the largest corporate data holders
 *   relative to smaller competitors, because the cost of the entry ticket
 *   (erasure-capable infrastructure) is fixed rather than scaled to firm
 *   size. This is a distinct constraint from the privacy_fundamental_reading
 *   (which evaluates the same text as instantiating individual data
 *   sovereignty, with corporate data holders as the target class) and the
 *   censorship_mechanism_reading (which evaluates a distinct claim: strategic
 *   erasure requests used to suppress lawful speech or archival content). All
 *   three share the kernel text but diverge sharply in beneficiary/victim
 *   structure and in epsilon, per the epsilon-invariance principle — they are
 *   authored as three separate constraint files linked by kernel context, not
 *   as one constraint with three interpretations.
 *
 * KEY AGENTS:
 *   - dominant_platform_incumbents: primary beneficiary (institutional/arbitrage) — absorbs compliance cost as trust signal and market advantage
 *   - compliance_technology_vendors and big_four_consulting_practices: secondary beneficiaries — profit from compliance complexity directly
 *   - early_stage_data_startups, open_source_data_projects, regional_challenger_platforms: primary targets — bear disproportionate fixed compliance cost relative to resources
 *   - data_protection_authorities: agenda-setter — enforces uniformly in principle, unevenly in practice
 *   - eu_data_subjects: nominal rights-holder — benefit unevenly depending on which entity holds their data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, 0.68).
domain_priors:suppression_score(article17_erasure_right__competitive_moat_reading, 0.58).
domain_priors:theater_ratio(article17_erasure_right__competitive_moat_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__competitive_moat_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__competitive_moat_reading, "GDPR Article 17 Right to Erasure — Compliance Cost as Competitive Moat").
narrative_ontology:topic_domain(article17_erasure_right__competitive_moat_reading, "technology governance / data protection law / competition policy").

domain_priors:requires_active_enforcement(article17_erasure_right__competitive_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__competitive_moat_reading, 'a31244e0-d2dc-4832-827c-2e7c466e2096').
narrative_ontology:cs_kernel_codification('a31244e0-d2dc-4832-827c-2e7c466e2096', fixed_text).
narrative_ontology:cs_authority_grounding('a31244e0-d2dc-4832-827c-2e7c466e2096', extraction).
narrative_ontology:cs_interpretation_layer_present('a31244e0-d2dc-4832-827c-2e7c466e2096').
narrative_ontology:cs_reading_relation('a31244e0-d2dc-4832-827c-2e7c466e2096', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('a31244e0-d2dc-4832-827c-2e7c466e2096', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('a31244e0-d2dc-4832-827c-2e7c466e2096', foundational, compliance_cost_asymmetry_is_the_operative_effect).
narrative_ontology:cs_axiom_status(compliance_cost_asymmetry_is_the_operative_effect, holdable).
narrative_ontology:cs_axiom_grounding('a31244e0-d2dc-4832-827c-2e7c466e2096', compliance_cost_asymmetry_is_the_operative_effect, empirically_contingent).
narrative_ontology:cs_axiom('a31244e0-d2dc-4832-827c-2e7c466e2096', secondary, market_structure_effects_are_a_legitimate_lens_for_evaluating_data_protection_law).
narrative_ontology:cs_axiom_status(market_structure_effects_are_a_legitimate_lens_for_evaluating_data_protection_law, holdable).
narrative_ontology:cs_axiom_grounding('a31244e0-d2dc-4832-827c-2e7c466e2096', market_structure_effects_are_a_legitimate_lens_for_evaluating_data_protection_law, instrumental).
narrative_ontology:cs_reference_frame('a31244e0-d2dc-4832-827c-2e7c466e2096', gdpr_2018_baseline_compliance_regime).
narrative_ontology:cs_drift_state('a31244e0-d2dc-4832-827c-2e7c466e2096', post_2023_enforcement_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a31244e0-d2dc-4832-827c-2e7c466e2096', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__competitive_moat_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, dominant_platform_incumbents).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, compliance_technology_vendors).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, big_four_consulting_practices).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, early_stage_data_startups).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, open_source_data_projects).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, regional_challenger_platforms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, eu_data_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Already operate distributed data architectures, dedicated legal-privacy teams, and automated deletion pipelines built for other regulatory regimes (SOC2, HIPAA-adjacent work). The marginal cost of adding Article 17 erasure workflows is low relative to revenue. They publicize compliance as a trust signal, which further advantages their market position against smaller rivals who cannot make the same claim credibly.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, dominant_platform_incumbents, beneficiary,
    institutional, generational, arbitrage, global).

% Sell erasure-workflow software, data-mapping tools, and audit services directly priced to the compliance burden Article 17 creates. Their revenue scales with the complexity of the requirement, giving them an incentive to see the compliance bar stay high rather than be simplified.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, compliance_technology_vendors, beneficiary,
    organized, biographical, mobile, global).

% Run privacy-compliance advisory practices that bill hourly for helping clients build erasure pipelines, conduct data protection impact assessments, and interface with regulators. Regulatory complexity is their product; they have no incentive to advocate for simplification.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, big_four_consulting_practices, beneficiary,
    institutional, generational, arbitrage, global).

% Must build erasure capability across every system touching personal data — often before product-market fit is established — or risk enforcement action. Cannot amortize the fixed cost of building deletion-capable architecture across a large user base or revenue stream the way incumbents can. Many divert early engineering headcount to compliance instead of product, or use compliance-as-a-service vendors that consume a disproportionate share of runway.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, early_stage_data_startups, payer,
    moderate, immediate, constrained, national).

% Volunteer-maintained or thinly-funded projects that process or federate personal data (forums, decentralized social tools, research datasets) lack any dedicated legal or compliance capacity. Erasure obligations that are trivial for a well-resourced platform to formalize into a ticketing workflow are functionally unimplementable for a maintainer community, pushing many to either shut down EU-facing features or operate in quiet noncompliance and legal exposure.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, open_source_data_projects, payer,
    powerless, biographical, trapped, global).

% Compete against a dominant incumbent in a specific national or regional market. Must stand up equivalent erasure infrastructure with a fraction of the incumbent's engineering budget and legal staff, widening the gap in user trust and regulatory standing even when their underlying data practices are equally or more protective of users.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, regional_challenger_platforms, payer,
    moderate, biographical, constrained, regional).

% Draft guidance, investigate complaints, and levy fines for erasure noncompliance. Their enforcement resources concentrate disproportionately on high-visibility incumbents (better documented, higher political salience) while informal compliance expectations still apply in full to smaller entities, whether or not enforcement follows.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Hold the formal right to request deletion of their personal data. In practice, requests to well-resourced incumbents are processed through polished self-service portals, while requests to smaller or informal projects may go unanswered — the right's practical value is uneven across the market even though it is legally uniform.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, eu_data_subjects, beneficiary,
    powerless, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__competitive_moat_reading, diffuse).
narrative_ontology:fixing_cost_class(article17_erasure_right__competitive_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article 17 solves a genuine collective problem: without an enforceable erasure right, personal data persists indefinitely across systems with no mechanism for individuals to compel deletion, and no common standard for what a valid deletion request or a compliant deletion process looks like.
% TRANSFER_FUNCTION: The compliance cost of implementing erasure-capable architecture is not distributed proportionally to revenue or data volume; it moves resources (engineering time, legal spend, consulting fees) disproportionately from smaller and newer market entrants to incumbents (who amortize the cost) and to the compliance-services industry (which is paid to bridge the gap) — a competitive advantage subsidized by the regulation's fixed-cost structure.
% ABSENT_VOICES: Early-stage founders and open-source maintainers rarely appear in the regulatory consultations that shape technical implementation guidance for Article 17; the guidance is disproportionately informed by submissions from large platforms and their advisory firms, who have the resources to participate in comment periods and standard-setting bodies.
% DISAPPEARANCE_RATIONALE: If Article 17 vanished overnight, data subjects would lose a meaningful legal lever and the world would rearrange for them; but from the competitive-moat reading, the disappearance would also erase a structural advantage incumbents currently hold over challengers, so incumbents would resist removal even as they publicly frame the right as settled consumer protection. The verdict is contested because the two effects (loss of individual right vs. loss of competitive barrier) are both real and pull toward different assessments depending on which harm is weighted.
% FOUNDING_PROBLEM: Indefinite, unaccountable retention of personal data by any entity that collected it, with no individual mechanism to compel its deletion — a genuine, still-live problem the right was built to solve.
% FOUNDING_PROBLEM_CORROBORATION: Data protection authorities and academic privacy researchers (outside both the incumbent-beneficiary set and the compliance-vendor set) independently attest the underlying retention problem remains live and the right serves a real function; the competitive-moat effect documented by competition economists and startup advocacy groups is a superimposed, unintended structural consequence rather than evidence the founding problem itself is resolved or fabricated.
narrative_ontology:disappearance_verdict(article17_erasure_right__competitive_moat_reading, contested).
narrative_ontology:founding_problem_status(article17_erasure_right__competitive_moat_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__competitive_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__competitive_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__competitive_moat_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) reflects the resource transfer from smaller entrants to incumbents and the compliance-services industry via the disproportionate fixed cost of erasure infrastructure, not from any single centralized extractor but from the structural asymmetry itself. Suppression (0.58) is moderate: there is no direct coercive suppression of alternatives, but the compliance requirement functions as an effective barrier to entry that suppresses competitive entry by resource-constrained challengers. Theater ratio (0.42) is meaningfully high because a substantial share of incumbent compliance activity is now optimized for demonstrable, auditable process rather than for actually improving the erasure experience for data subjects — self-service portals and glossy transparency reports are partly a market-differentiation signal, not solely a function of the underlying right. accessibility_collapse (0.6) reflects that once compliance infrastructure becomes the market-standard cost of entry, alternative lighter-weight compliance approaches become de facto unavailable to new entrants because regulators and enforcement expectations calibrate to what incumbents have already built. resistance (0.55) is moderate: startup advocacy groups and some regulators have pushed back on disproportionate compliance burdens, but no organized challenge has altered the underlying cost structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant incumbents and compliance vendors sit near the beneficiary end of directionality because the same regulatory structure that constrains them also advantages them relative to competitors — this is the FSM-adjacent dynamic (not a mountain, since beneficiaries and coordination coexist, hence tangled_rope rather than mountain). Early-stage startups and open-source projects sit near the target end: the compliance requirement extracts disproportionate resources from them relative to their capacity to pay, with limited exit (they cannot simply opt out of EU data subjects without losing market access). Regional challengers occupy an intermediate position: constrained but not trapped, since they retain the option (at a cost) of building compliance infrastructure, just on a worse cost curve than the incumbent they compete against.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unaccountable indefinite data retention — remains genuinely live (status: live), which is what prevents this reading from being scored a pure snare: there IS a real coordination function being solved, corroborated by data protection authorities and researchers outside the beneficiary set. What keeps it in tangled_rope rather than rope is the asymmetric extraction layered on top: the same legal mechanism that solves the coordination problem for data subjects also, as an unintended but persistent side effect, entrenches incumbent market position by imposing a compliance cost structure that scales worse for smaller entities. Classifying this as tangled_rope rather than collapsing it into either 'this is just consumer protection' (rope) or 'this is just a corporate racket' (snare) is precisely the function this framework exists to perform — it holds both the genuine coordination function and the asymmetric extraction as simultaneously true facts about the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competitive_effect_intentionality,
    'Is the incumbent-advantaging compliance-cost asymmetry an intended (or at least foreseeable-and-accepted) effect of Article 17''s drafting, or a genuinely unanticipated structural side effect of a good-faith privacy mechanism?',
    'Legislative history analysis of Article 17 drafting sessions, lobbying disclosure records from the GDPR negotiation period, and comparison with alternative drafting proposals (e.g., tiered compliance thresholds by firm size) that were considered and rejected.',
    'If deliberately shaped by incumbent lobbying to raise entry barriers, this reading moves closer to snare (extraction with cover story); if genuinely unintended, it remains a cleaner tangled_rope case — real coordination function with an emergent, unaddressed extraction side effect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competitive_effect_intentionality, empirical, 'Whether the competitive-moat effect was foreseeable/intended in Article 17''s drafting or is an emergent side effect.').

omega_variable(
    compliance_cost_scaling_ambiguity,
    'Would a differently-designed erasure mechanism (e.g., a mandated shared compliance utility, tiered obligations by data volume, or a regulator-provided reference implementation) achieve the same privacy protection with a much smaller relative burden on smaller entities?',
    'Comparative regulatory design analysis: examine jurisdictions or sectors (e.g., CCPA''s different compliance thresholds) that have implemented tiered or shared-infrastructure approaches to similar rights, and measure relative compliance cost distribution across firm sizes.',
    'If a lower-asymmetry design exists and was technically and politically feasible, that strengthens the case that the current cost structure is a policy choice (not an unavoidable feature of erasure rights per se) and sharpens the tangled_rope classification toward the extraction end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_scaling_ambiguity, conceptual, 'Whether the compliance cost asymmetry is inherent to erasure rights or a specific, alterable design choice.').

omega_variable(
    cs_framing_underdetermination_kernel_vs_enforcement_layer,
    'Is the appropriate CS kernel here the Article 17 statutory text itself (fixed_text, authority via regulatory lineage), or is it more accurate to model the operative kernel as the evolving body of DPA guidance and enforcement precedent that actually determines what ''compliant erasure infrastructure'' means in practice?',
    'Track whether classification-relevant facts (compliance cost, competitive effect) are driven more by the statutory text''s fixed requirements or by the shifting, incumbent-informed technical guidance documents issued by DPAs and standards bodies over time.',
    'If the guidance layer is the true operative kernel, the interpretation_layer_present flag and authority_grounding characterization would need to shift from a relatively stable fixed_text/practice model toward a more dynamic, incumbent-influenced distributed authority — potentially deepening the extraction reading, since incumbents who participate in guidance drafting would then be co-authoring the very compliance bar that advantages them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination_kernel_vs_enforcement_layer, conceptual, 'Whether the operative kernel is the fixed statutory text or the evolving enforcement-guidance layer that actually sets the practical compliance bar.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__competitive_moat_reading, 0, 96).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__competitive_moat_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arti_tr_t16, article17_erasure_right__competitive_moat_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(arti_tr_t32, article17_erasure_right__competitive_moat_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement(arti_tr_t48, article17_erasure_right__competitive_moat_reading, theater_ratio, 48, 0.35).
narrative_ontology:measurement(arti_tr_t64, article17_erasure_right__competitive_moat_reading, theater_ratio, 64, 0.38).
narrative_ontology:measurement(arti_tr_t80, article17_erasure_right__competitive_moat_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(arti_tr_t96, article17_erasure_right__competitive_moat_reading, theater_ratio, 96, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__competitive_moat_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(arti_be_t16, article17_erasure_right__competitive_moat_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(arti_be_t32, article17_erasure_right__competitive_moat_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(arti_be_t48, article17_erasure_right__competitive_moat_reading, base_extractiveness, 48, 0.61).
narrative_ontology:measurement(arti_be_t64, article17_erasure_right__competitive_moat_reading, base_extractiveness, 64, 0.65).
narrative_ontology:measurement(arti_be_t80, article17_erasure_right__competitive_moat_reading, base_extractiveness, 80, 0.67).
narrative_ontology:measurement(arti_be_t96, article17_erasure_right__competitive_moat_reading, base_extractiveness, 96, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__competitive_moat_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(arti_su_t16, article17_erasure_right__competitive_moat_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(arti_su_t32, article17_erasure_right__competitive_moat_reading, suppression_requirement, 32, 0.48).
narrative_ontology:measurement(arti_su_t48, article17_erasure_right__competitive_moat_reading, suppression_requirement, 48, 0.52).
narrative_ontology:measurement(arti_su_t64, article17_erasure_right__competitive_moat_reading, suppression_requirement, 64, 0.55).
narrative_ontology:measurement(arti_su_t80, article17_erasure_right__competitive_moat_reading, suppression_requirement, 80, 0.57).
narrative_ontology:measurement(arti_su_t96, article17_erasure_right__competitive_moat_reading, suppression_requirement, 96, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__competitive_moat_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__competitive_moat_reading, 0.12).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the article17_erasure_right kernel, each authored as a separate constraint file per the epsilon-invariance principle. privacy_fundamental_reading treats the same text as instantiating individual data sovereignty (data subjects as beneficiary class, corporate data holders as target class, low epsilon from that reading's lights). censorship_mechanism_reading treats the same text as a vector for strategic content suppression via erasure requests weaponized against speech and archival interests (publishers/archivists as victim class, requesters exploiting the mechanism as beneficiaries). This file (competitive_moat_reading) treats the text as producing an incumbent-advantaging compliance cost asymmetry (dominant platforms and compliance vendors as beneficiaries, resource-constrained entrants as victims). Each reading has its own epsilon, its own stakeholder set, and its own classification; they are linked here rather than merged into one story with a hidden measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
