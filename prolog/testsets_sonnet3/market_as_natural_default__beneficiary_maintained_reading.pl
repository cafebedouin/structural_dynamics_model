% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__beneficiary_maintained_reading, []).

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
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market Naturalization as Beneficiary-Maintained Closure
 *   domain: political_economy/ideology
 *
 * SUMMARY:
 *   This story instantiates the beneficiary-maintained reading of the
 *   market_as_natural_default kernel: the claim that market allocation's
 *   status as the unmarked institutional default is not a residue of
 *   forgotten history but an actively engineered and continuously defended
 *   closure, maintained by identifiable beneficiary classes (finance,
 *   multinational corporate leadership, and the professional economics
 *   establishment) through funded research, curriculum control, media
 *   relationships, and lobbying. Two sibling readings of the same kernel are
 *   NOT part of this constraint: the lapsed_alternative_reading holds that
 *   market dominance results from historical forgetting rather than active
 *   defense (much lower extraction, no identifiable maintaining beneficiary),
 *   and the hybrid_amnesia_reading holds that an initial lapse created
 *   conditions later exploited by beneficiaries. This story's ε (0.47)
 *   reflects sustained, identifiable, institutionally-funded maintenance work
 *   — closer to engineered closure than to passive amnesia.
 *
 * KEY AGENTS:
 *   - finance_sector_incumbents: institutional beneficiary and co-agenda-setter, arbitrage exit — funds and benefits from naturalization narrative
 *   - multinational_corporate_leadership: institutional beneficiary and co-agenda-setter, arbitrage exit — lobbies and funds curriculum/media reinforcing default frame
 *   - market_economics_professional_establishment: institutional agenda-setter, constrained exit — controls gatekeeping infrastructure certifying market-default as settled
 *   - cooperative_and_mutual_sector_advocates: moderate power payer, constrained exit — bears capital and legal cost of alternative-form dismissal
 *   - public_ownership_constituencies: powerless payer, trapped exit — bears service costs without institutional pathway to contest the frame
 *   - labor_organizations_advocating_alternatives: organized payer, constrained exit — proposals structurally excluded despite organizational capacity
 *   - economic_historians_documenting_alternatives: analytical observer — documents the contested historical record the naturalization narrative obscures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.47).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.58).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market Naturalization as Beneficiary-Maintained Closure").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, 'd81438a7-dbdd-4b48-8251-e432e8535329').
narrative_ontology:cs_kernel_codification('d81438a7-dbdd-4b48-8251-e432e8535329', distributed).
narrative_ontology:cs_authority_grounding('d81438a7-dbdd-4b48-8251-e432e8535329', extraction).
narrative_ontology:cs_interpretation_layer_present('d81438a7-dbdd-4b48-8251-e432e8535329').
narrative_ontology:cs_reading_relation('d81438a7-dbdd-4b48-8251-e432e8535329', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('d81438a7-dbdd-4b48-8251-e432e8535329', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('d81438a7-dbdd-4b48-8251-e432e8535329', foundational, naturalization_is_engineered_not_forgotten).
narrative_ontology:cs_axiom_status(naturalization_is_engineered_not_forgotten, holdable).
narrative_ontology:cs_axiom_grounding('d81438a7-dbdd-4b48-8251-e432e8535329', naturalization_is_engineered_not_forgotten, empirically_contingent).
narrative_ontology:cs_axiom('d81438a7-dbdd-4b48-8251-e432e8535329', foundational, identifiable_beneficiary_classes_actively_defend_default_status).
narrative_ontology:cs_axiom_status(identifiable_beneficiary_classes_actively_defend_default_status, holdable).
narrative_ontology:cs_axiom_grounding('d81438a7-dbdd-4b48-8251-e432e8535329', identifiable_beneficiary_classes_actively_defend_default_status, empirically_contingent).
narrative_ontology:cs_reference_frame('d81438a7-dbdd-4b48-8251-e432e8535329', postwar_mixed_economy_contestation).
narrative_ontology:cs_drift_state('d81438a7-dbdd-4b48-8251-e432e8535329', post_1980s_deregulation_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d81438a7-dbdd-4b48-8251-e432e8535329', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, finance_sector_incumbents).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, multinational_corporate_leadership).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, market_economics_professional_establishment).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, cooperative_and_mutual_sector_advocates).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, public_ownership_constituencies).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, labor_organizations_advocating_alternatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold concentrated capital positions whose value depends on the market allocation model being treated as the only serious institutional design. Fund think tanks, business schools, and financial press coverage that frame market mechanisms as inevitable and alternatives as naive or discredited. Have arbitrage-grade exit from any given jurisdiction's regulatory environment, which lets them shape naturalization narratives without bearing local political risk.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, finance_sector_incumbents, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, finance_sector_incumbents, agenda_setter).

% Direct lobbying operations, trade associations, and corporate philanthropy toward reinforcing the naturalness frame — commissioning studies, sponsoring economics curricula, and funding electoral campaigns that keep market-default policy off the table for revision. Their profits depend on continued treatment of market allocation as pre-political common sense rather than one contestable design among several.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, multinational_corporate_leadership, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, multinational_corporate_leadership, agenda_setter).

% Controls curricula, journal gatekeeping, and policy-advisory pipelines that certify which allocation mechanisms count as rigorous versus fringe. Career advancement is tied to treating market mechanisms as baseline; those who study institutional alternatives face reduced publication venues and funding. The field's authority depends on the naturalization frame being seen as settled science rather than active advocacy.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, market_economics_professional_establishment, agenda_setter,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, market_economics_professional_establishment, beneficiary).

% Operate cooperative and mutual enterprises that must compete for capital, legal recognition, and favorable regulation against a policy environment tilted toward market-default assumptions. Face higher borrowing costs, weaker legal protections, and dismissive treatment in policy circles precisely because their institutional form is treated as a historical curiosity rather than a live alternative.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, cooperative_and_mutual_sector_advocates, payer,
    moderate, biographical, constrained, national).

% Communities and workers who would benefit from public or municipal ownership of utilities, housing, or infrastructure find such proposals repeatedly dismissed in mainstream policy discourse as ideologically fringe or economically illiterate, regardless of comparative performance data. They bear the costs of privatized or marketized services without meaningful institutional pathway to contest the underlying allocation model.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, public_ownership_constituencies, payer,
    powerless, biographical, trapped, national).

% Unions and labor federations that propose sectoral bargaining, worker ownership, or planning mechanisms find these proposals structurally excluded from serious policy consideration, often characterized in funded media and academic outlets as economically naive prior to any substantive engagement. Their organized power secures a hearing but rarely a genuine reconsideration of the default frame.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, labor_organizations_advocating_alternatives, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, labor_organizations_advocating_alternatives, excluded).

% Would be positioned to interrogate the naturalization frame directly but operate inside outlets substantially dependent on advertising, ownership structures, and access relationships tied to the same corporate and finance incumbents who benefit from the frame. Genuine critical coverage of market-default naturalization carries career and institutional risk.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, policy_journalists_and_economic_commentators, excluded,
    moderate, immediate, constrained, national).

% Document the historical record of contested institutional choices — the actual political battles over market versus planned, cooperative, or mixed models in the twentieth century. Their scholarship demonstrates that market dominance was a contested outcome, not an unopposed default, but this scholarship circulates in specialist venues with limited influence on the mainstream naturalization narrative.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, economic_historians_documenting_alternatives, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__beneficiary_maintained_reading, diffuse).
narrative_ontology:fixing_cost_class(market_as_natural_default__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Market allocation genuinely does solve real coordination problems — price signals, decentralized information aggregation, and resource allocation without centralized planning bottlenecks. The naturalization narrative can piggyback on this real coordination value.
% TRANSFER_FUNCTION: Moves policy attention, research funding, legal protection, and capital access away from cooperative, public, and planned alternatives and toward market-based institutional forms, regardless of comparative merit in specific cases; moves reputational and career costs onto advocates and scholars of alternatives.
% ABSENT_VOICES: Cooperative movement historians, worker-ownership researchers, and public-utility advocates would contest the naturalization frame directly if given equal institutional standing in economics curricula, financial press, and policy-advisory bodies — they are present in specialist venues but structurally excluded from the mainstream framing apparatus that manufactures the sense of inevitability.
% DISAPPEARANCE_RATIONALE: If the active maintenance apparatus — funded think tanks, curriculum gatekeeping, sympathetic policy press, campaign finance directed at market-default policy — were withdrawn overnight, alternative institutional proposals (cooperatives, public ownership, sectoral planning) would gain a materially fairer hearing in policy and academic venues; capital costs and legal treatment for non-market institutional forms would likely shift within a policy cycle.
% FOUNDING_PROBLEM: Historically, market mechanisms were adopted and defended to solve genuine coordination failures under central planning (information problems, incentive misalignment, scarcity signaling) — a real problem that market allocation addressed better than some rival mechanisms in specific historical episodes.
% FOUNDING_PROBLEM_CORROBORATION: Finance and corporate beneficiaries attest the market-default framing reflects settled economic consensus about efficiency. Economic historians and comparative-institutions researchers outside the beneficiary set attest that the empirical case for market-default as uniquely efficient across all sectors is considerably weaker than the naturalization narrative suggests, and that the continued dominance of the frame owes more to sustained institutional maintenance than to unrebutted comparative performance.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.47, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.47) sits at the upper-middle of the delta band because active maintenance work (funding, gatekeeping, media capture) diverts real resources and attention away from alternative institutional forms without those forms being demonstrably inferior across the board — this is directed rent-protection dressed as settled science. Theater ratio (0.62, rising) is high and rising because an increasing share of the naturalization defense is performative: citing 'consensus' and 'common sense' rather than engaging comparative institutional evidence, consistent with a defense that has hardened into ritual as challengers accumulate credible counter-evidence. Suppression (0.58) is substantial but not overwhelming — cooperative and public alternatives are not illegal, merely structurally disadvantaged in funding, curriculum, and press access, which is real but non-absolute suppression. Accessibility collapse (0.5) is moderate: alternatives remain technically visible and documented (see economic historians) but functionally difficult to institutionalize. Resistance (0.55) reflects genuine and organized pushback from labor and cooperative advocates, which the beneficiary-maintained reading predicts (active defense implies active contestation) — this differs sharply from what the lapsed_alternative_reading would predict (low resistance, since if alternatives were merely forgotten there would be little organized memory to resist erasure).
 *
 * DIRECTIONALITY LOGIC:
 *   Finance incumbents and corporate leadership sit near the full-beneficiary end: they fund the machinery, collect the policy and reputational benefits, and hold arbitrage-grade exit that lets them escape any single jurisdiction's political consequences. The professional economics establishment is a beneficiary-agenda-setter hybrid — its authority is itself a product of the naturalization frame, giving it a self-reinforcing stake distinct from direct financial capture. Cooperative advocates, public ownership constituencies, and labor organizations sit toward the target end: they bear diverted capital, foreclosed policy options, and reputational costs (being cast as 'ideological' rather than 'rigorous') as the direct output of the same maintenance apparatus that benefits the incumbents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (market coordination solving real information and incentive problems under specific historical conditions) is contested rather than flatly dead — market mechanisms retain genuine coordination value in many domains. What has drifted is the SCOPE claim: from 'markets solve certain coordination problems well' to 'market allocation is the unmarked, pre-political default against which all alternatives must justify themselves.' That scope inflation is exactly what active beneficiary maintenance would produce and exactly what a merely-lapsed alternative would not reliably produce on its own — this is the key discriminator between this reading and its siblings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_defense_vs_amnesia_discriminator,
    'Is the observed persistence of market-default framing better explained by continuous active institutional maintenance (this reading) or by an initial historical forgetting that then went unchallenged (the lapsed_alternative_reading), or by a sequenced combination of both (hybrid_amnesia_reading)?',
    'Trace the funding and institutional history of specific naturalization episodes (e.g., business-school curriculum reform, specific think-tank founding dates and funders, media ownership changes) against the timeline of alternative-institution decline. If funding and gatekeeping interventions precede or coincide with periods of alternative-institution decline, active maintenance is supported; if alternatives declined first for independent reasons and funded advocacy arrived later merely to prevent revival, hybrid_amnesia is supported; if no organized funding pattern is found at all, lapsed_alternative is supported.',
    'If lapsed_alternative is correct, this story''s beneficiary declarations and elevated ε are mismeasured — the correct sibling constraint would show much lower extraction and no meaningful agenda_setter beneficiary class. If hybrid_amnesia is correct, ε should be phase-dependent, low in an early window and rising sharply once beneficiary capture begins, which would require re-authoring this story''s temporal grid to show that inflection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_defense_vs_amnesia_discriminator, empirical, 'Whether persistence is due to active beneficiary defense, historical amnesia, or a sequenced combination — the core discriminator among the three kernel readings.').

omega_variable(
    coordination_value_boundary_of_naturalization,
    'Where does the genuine coordination value of market mechanisms end and the engineered ideological closure begin? Is there a principled boundary, or is the naturalization narrative applied uniformly regardless of sector-specific comparative performance?',
    'Sector-by-sector comparative institutional analysis (utilities, healthcare, housing, finance itself) measuring whether market-default policy prescriptions track empirical performance data or track beneficiary interest independent of performance.',
    'A sector-by-sector performance correlation would support that naturalization tracks genuine efficiency in some domains (partially mitigating the tangled_rope classification toward rope in those domains) while a uniform prescription regardless of comparative data would support pure engineered closure (pushing toward snare in those domains).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_value_boundary_of_naturalization, conceptual, 'Whether the coordination function is genuine and bounded or merely a cover story extended uniformly beyond its actual domain of validity.').

omega_variable(
    diffuse_capture_beneficiary_concentration,
    'Is the extraction captured concentratedly by specific finance and corporate actors, or is it genuinely diffuse — a self-reinforcing institutional equilibrium that no single actor centrally directs, even though multiple actors benefit incidentally?',
    'Trace whether specific coordinated campaigns (documented funding chains, shared personnel across think tanks and lobbying bodies) exist, versus whether the pattern is better explained by independently-acting beneficiaries responding to shared incentives without central coordination.',
    'If concentrated coordination is demonstrated, gain_flow should be re-authored to name a specific capturing seat (likely finance_sector_incumbents) rather than ''diffuse'', which would strengthen the snare-adjacent reading of this constraint. If genuinely diffuse and uncoordinated, the current tangled_rope classification with diffuse gain_flow remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_capture_beneficiary_concentration, empirical, 'Whether the beneficiary-maintenance apparatus is coordinated and concentrated or diffuse and structurally emergent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(mark_tr_t8, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 8, 0.46).
narrative_ontology:measurement(mark_tr_t16, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 16, 0.52).
narrative_ontology:measurement(mark_tr_t24, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 24, 0.57).
narrative_ontology:measurement(mark_tr_t32, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 32, 0.6).
narrative_ontology:measurement(mark_tr_t40, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mark_be_t8, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(mark_be_t16, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(mark_be_t24, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(mark_be_t32, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 32, 0.46).
narrative_ontology:measurement(mark_be_t40, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 40, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(mark_su_t8, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(mark_su_t16, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(mark_su_t24, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(mark_su_t32, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 32, 0.57).
narrative_ontology:measurement(mark_su_t40, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__beneficiary_maintained_reading, 0.1).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% Three-story constraint family decomposing the market_as_natural_default kernel by mechanism: this story (beneficiary_maintained_reading, ε≈0.47, tangled_rope) claims active engineered defense by identifiable beneficiaries; the lapsed_alternative_reading sibling claims mere historical forgetting with no maintaining beneficiary class and correspondingly much lower ε; the hybrid_amnesia_reading sibling claims a sequenced two-phase account (lapse then capture) with phase-dependent ε. Each story shares the same kernel (why market allocation holds unmarked-default status) but instantiates structurally distinct claims about mechanism, beneficiary presence, and extraction level — following the ε-invariance principle, they are authored as separate stories rather than one story with a mechanism parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
