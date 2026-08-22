% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA/USMCA Jurisdictional Boundary — Embedded Liberalism Reading (Market Access Balanced Against Legitimate Domestic Policy Space)
 *   domain: International Trade Law / Political Economy / Regulatory Federalism
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested NAFTA/USMCA
 *   jurisdictional-boundary kernel: the embedded-liberalism reading, under
 *   which trade agreement text functions as a framework balancing market
 *   access against legitimate domestic policy space, and environmental/labor
 *   standards are treated as compatible with trade obligations when applied
 *   non-discriminatorily. This is not a description of the treaty text itself
 *   but of one party's structural reading of what that text does. Under this
 *   reading, regulatory agencies retain genuine defensive authority bounded
 *   by a 'legitimate objectives' test, producing partial jurisdictional
 *   overlap rather than either full subordination of domestic law (the
 *   capital-supremacy reading) or full domestic sovereignty (the
 *   sovereignty-primacy reading). Moderate extraction arises specifically
 *   from litigation costs and the differential capacity of regulatory
 *   agencies and public-interest litigants to defend measures within that
 *   boundary — not from the trade obligation itself being extractive.
 *
 * KEY AGENTS:
 *   - exporting_industries: organized beneficiary of predictable market access under the non-discrimination framework
 *   - domestic_regulatory_agencies: agenda_setter administering and defending the legitimate-objectives boundary
 *   - consumer_and_environmental_advocates: beneficiary relying on the boundary to defend non-discriminatory protective standards
 *   - smaller_regulatory_agencies: payer bearing disproportionate defense costs relative to capacity
 *   - public_interest_litigants: payer absorbing asymmetric litigation burden
 *   - communities_facing_regulatory_chill: powerless payer bearing the cost of standards never adopted
 *   - capital_mobile_multinationals: excluded from domestic rulemaking, prefer a different reading of the same text
 *   - trade_dispute_panels: analytical observer whose case-by-case rulings continuously redraw the boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.42).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.4).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA/USMCA Jurisdictional Boundary — Embedded Liberalism Reading (Market Access Balanced Against Legitimate Domestic Policy Space)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "International Trade Law / Political Economy / Regulatory Federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'b7c88fca-4d05-4916-a490-425aa88d4a07').
narrative_ontology:cs_kernel_codification('b7c88fca-4d05-4916-a490-425aa88d4a07', fixed_text).
narrative_ontology:cs_authority_grounding('b7c88fca-4d05-4916-a490-425aa88d4a07', practice).
narrative_ontology:cs_interpretation_layer_present('b7c88fca-4d05-4916-a490-425aa88d4a07').
narrative_ontology:cs_reading_relation('b7c88fca-4d05-4916-a490-425aa88d4a07', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7c88fca-4d05-4916-a490-425aa88d4a07', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('b7c88fca-4d05-4916-a490-425aa88d4a07', foundational, non_discrimination_reconciles_trade_and_policy_space).
narrative_ontology:cs_axiom_status(non_discrimination_reconciles_trade_and_policy_space, holdable).
narrative_ontology:cs_axiom_grounding('b7c88fca-4d05-4916-a490-425aa88d4a07', non_discrimination_reconciles_trade_and_policy_space, conventional).
narrative_ontology:cs_axiom('b7c88fca-4d05-4916-a490-425aa88d4a07', secondary, regulatory_authority_survives_when_evenhandedly_applied).
narrative_ontology:cs_axiom_status(regulatory_authority_survives_when_evenhandedly_applied, holdable).
narrative_ontology:cs_axiom_grounding('b7c88fca-4d05-4916-a490-425aa88d4a07', regulatory_authority_survives_when_evenhandedly_applied, instrumental).
narrative_ontology:cs_reference_frame('b7c88fca-4d05-4916-a490-425aa88d4a07', gatt_embedded_liberalism_compromise).
narrative_ontology:cs_drift_state('b7c88fca-4d05-4916-a490-425aa88d4a07', post_usmca_dispute_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7c88fca-4d05-4916-a490-425aa88d4a07', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, exporting_industries).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, consumer_and_environmental_advocates).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, smaller_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, public_interest_litigants).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, communities_facing_regulatory_chill).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, non_discrimination_as_the_organizing_principle_of_trade_law).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, legitimate_objectives_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain predictable market access across the trade bloc under a framework that presumes non-discriminatory domestic regulation is compatible with treaty obligations. Can restructure supply chains across the three jurisdictions and lobby domestic regulators, but must still design compliance around whatever legitimate-objectives standards each government adopts.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, exporting_industries, beneficiary,
    organized, generational, mobile, continental).

% Retain authority to set environmental, labor, and health standards so long as they are non-discriminatory, science-based, and not more trade-restrictive than necessary. Administer the boundary itself — writing regulations with an eye toward the legitimate-objectives test — and defend those regulations when challenged. Well-resourced agencies can build the evidentiary record dispute panels expect; the boundary's defensibility depends on agency capacity.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, beneficiary).

% Rely on the embedded-liberalism reading to argue that new environmental and labor protections survive trade scrutiny as long as they are applied evenhandedly. Can point to actual panel decisions upholding non-discriminatory measures as precedent, but must litigate or lobby each time a measure is challenged rather than having categorical immunity.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, consumer_and_environmental_advocates, beneficiary,
    organized, generational, constrained, national).

% Lack the technical and legal capacity to build the scientific record and litigation defense the legitimate-objectives boundary demands. Face de facto pressure to avoid or dilute new standards because a challenge they cannot adequately defend risks an adverse ruling and reputational cost, even though the framework formally preserves their authority.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, smaller_regulatory_agencies, payer,
    moderate, biographical, trapped, national).

% Must intervene in trade disputes or domestic rulemaking to defend regulations on non-discrimination grounds, absorbing significant legal costs that better-resourced industry challengers do not bear symmetrically. Their participation is possible under this reading but expensive and uncertain.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, public_interest_litigants, payer,
    moderate, biographical, constrained, national).

% Live with the practical consequence when local or regional regulators decline to adopt or enforce protective standards out of anticipated litigation risk under the trade framework, even though the framework itself does not forbid the standard. Bear the health, labor, or environmental cost of the chilled regulation without a seat at either the trade panel or the rulemaking table.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, communities_facing_regulatory_chill, payer,
    powerless, biographical, trapped, local).

% Would prefer the capital-supremacy reading in which domestic standards are subordinated more categorically to market access obligations; under the embedded-liberalism reading they must accept that non-discriminatory regulation survives scrutiny, which narrows but does not eliminate their leverage. Not part of the domestic rulemaking conversation that sets the standards they must comply with.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_mobile_multinationals, excluded,
    powerful, biographical, arbitrage, continental).

% Adjudicate whether a challenged domestic measure is genuinely non-discriminatory and no more trade-restrictive than necessary. Apply and thereby continuously re-draw the legitimate-objectives boundary case by case, generating precedent that stabilizes or destabilizes the reading depending on how consistently they defer to domestic regulatory judgment.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_dispute_panels, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__embedded_liberalism_reading, diffuse).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__embedded_liberalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework letting three jurisdictions with different regulatory traditions extend predictable market access to each other's producers while each retains the ability to regulate health, safety, labor, and environmental matters domestically, so long as the regulation does not function as disguised protectionism.
% TRANSFER_FUNCTION: Moves compliance and litigation burden from industry (which gets predictable market access and a non-discrimination shield against arbitrary local protectionism) toward the regulatory agencies and public-interest litigants who must build and defend the evidentiary case that a given domestic standard is genuinely non-discriminatory and appropriately tailored — a burden that falls disproportionately on lower-capacity agencies and under-resourced communities.
% ABSENT_VOICES: Communities facing regulatory chill are not present in either the trade panel proceedings or, often, the domestic rulemaking process that quietly declines to adopt a standard rather than risk a challenge; capital-mobile multinationals are excluded from setting the domestic standards they must comply with, and would prefer a different reading of the same text.
% DISAPPEARANCE_RATIONALE: If the embedded-liberalism boundary vanished and were replaced by pure capital-supremacy or pure sovereignty-primacy readings of the same treaty text, the practical balance now negotiated case-by-case in dispute panels would collapse toward one extreme or the other: either domestic environmental/labor regulation would face categorical subordination to market-access obligations, or trade discipline over disguised protectionism would disappear entirely. Regulatory agencies, exporters, and advocacy groups would all reorganize their strategies around whichever pole replaced the current negotiated middle.
% FOUNDING_PROBLEM: GATT/NAFTA-era negotiators needed language that would open markets and discipline disguised protectionism without requiring states to surrender the domestic political capacity to regulate health, safety, labor, and the environment — a problem born of trying to secure trade liberalization's benefits while preserving the postwar settlement's tolerance for domestic policy autonomy (the 'embedded liberalism' compromise, in Ruggie's phrase).
% FOUNDING_PROBLEM_CORROBORATION: Trade law scholars outside industry and outside the advocacy movements (e.g., academic analyses of WTO/NAFTA Article XX-style jurisprudence and dispute panel rulings) corroborate that panels have in fact upheld non-discriminatory environmental and labor measures in specific cases, supporting that the boundary is not merely rhetorical. But the same scholarship documents a persistent 'regulatory chill' effect reported independently by domestic regulatory agencies and public health researchers, suggesting the founding balance is only partially realized in practice rather than settled.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 by 2024) because this reading holds that the trade framework does NOT categorically override domestic regulatory authority — extraction here is confined to the litigation and compliance-capacity costs of operating within the legitimate-objectives boundary, not to wholesale subordination of policy space. Suppression is likewise moderate (0.40): the boundary is real and defensible, but under-resourced agencies and communities experience a chilling effect that functions as a soft suppression mechanism even without a formal prohibition. Theater ratio rises gradually (0.18 to 0.28) reflecting a growing gap between the framework's stated balance and the practical asymmetry in who can actually litigate that balance successfully over three decades of dispute practice.
 *
 * PERSPECTIVAL GAP:
 *   From the domestic-regulatory-agency seat under this reading, the arrangement looks like a workable rope: real coordination (predictable market access) paired with real retained authority. From the seat of an under-resourced agency or an affected community, the same structural arrangement computes closer to tangled rope or even snare-adjacent, because the formal retention of authority does not translate into practical capacity to exercise it. This divergence is exactly what the engine is built to surface from the same authored structural data, not something this story should smooth over.
 *
 * DIRECTIONALITY LOGIC:
 *   Exporting industries and better-resourced regulatory agencies and advocacy groups sit near the beneficiary end: they can use the predictable framework and the non-discrimination shield to their advantage. Smaller agencies, public-interest litigants, and especially regulatory-chill-affected communities sit toward the target end: they bear the compliance/litigation cost of operating within a boundary whose defense requires resources they may lack, even though the boundary formally protects their interests too. Capital-mobile multinationals are excluded rather than coordinated under this specific reading — they would rather occupy the capital-supremacy reading's more favorable terrain.
 *
 * MANDATROPHY ANALYSIS:
 *   The embedded-liberalism reading's founding problem — reconciling trade liberalization with domestic policy autonomy — remains partially live: dispute panels do periodically uphold non-discriminatory measures, so the reading is not purely a legitimating myth. But the corroborated regulatory-chill effect indicates the reading's practical function has partially drifted from its founding balance toward a system where formal authority exists but capacity-gated litigation risk suppresses its exercise for less-resourced actors. This is not full mandatrophy (the founding problem is not simply dead) but a contested, partial one — captured in the founding_problem_status of 'contested' rather than 'dead.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_stability_across_panels,
    'Does the ''legitimate objectives'' boundary this reading depends on hold consistently across dispute panels and time, or is it panel-composition-dependent and therefore less stable than the embedded-liberalism reading assumes?',
    'Systematic coding of dispute panel rulings on environmental/labor measures over the treaty''s life, tracking whether non-discriminatory measures are upheld at a stable rate or whether outcomes cluster by panel composition, era, or challenged party.',
    'If the boundary is panel-dependent rather than doctrinally stable, this reading''s claim that domestic policy space is reliably protected weakens, and the constraint drifts closer to the capital-supremacy reading''s practical operation despite retaining this reading''s formal doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_stability_across_panels, empirical, 'Whether the legitimate-objectives boundary is a stable doctrine or panel-dependent in practice.').

omega_variable(
    chill_versus_genuine_deference,
    'Is the observed pattern of regulatory restraint by smaller agencies genuine deference to a real and correctly-perceived risk of losing a challenge, or an over-cautious chilling effect exceeding the actual litigation risk?',
    'Compare agencies'' internal risk assessments and legal counsel memos (where available) against actual panel outcomes for comparable measures, to see whether perceived risk matches realized risk.',
    'If restraint substantially exceeds actual risk, the extraction measured here is partly a self-reinforcing perception effect rather than a structural feature of the treaty framework itself — which would argue for authoring a lower suppression value in a future revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chill_versus_genuine_deference, empirical, 'Whether regulatory chill reflects accurate risk assessment or excess caution.').

omega_variable(
    reading_selection_and_committer_identity,
    'Which real-world actors actually hold the embedded-liberalism reading as their operative interpretation, versus using its language while functionally applying a different reading?',
    'Discourse analysis of government trade negotiators'', regulatory agencies'', and dispute panel members'' actual argumentative moves across cases, distinguishing rhetorical invocation of ''balance'' from doctrinal commitments that track the capital-supremacy or sovereignty-primacy readings instead.',
    'If key institutional actors invoke embedded-liberalism language while making capital-supremacy arguments in practice, this reading is less a live commitment and more a legitimating vocabulary layered over a different operative reading — which would sharpen the divergence between this story''s claimed_type and its true structural operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_committer_identity, conceptual, 'Whether the embedded-liberalism reading is a genuinely held commitment or primarily rhetorical cover for a different operative reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 1994, 0.18).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2006, 0.22).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2012, 0.24).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 1994, 0.3).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2006, 0.37).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2012, 0.39).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 1994, 0.32).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2000, 0.34).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2006, 0.36).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2012, 0.38).
narrative_ontology:measurement(naft_su_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2018, 0.39).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.12).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language concept 'NAFTA/USMCA jurisdictional boundary between trade obligations and domestic regulatory authority.' The three readings share the same treaty text but author structurally distinct claims about what that text does, with different epsilon values, different beneficiary/victim structures, and different classifications: capital_supremacy_reading (higher extraction, treats text as supreme law subordinating domestic regulation), embedded_liberalism_reading (this story — moderate extraction, treats text as a balanced framework with defensible domestic policy space), and sovereignty_primacy_reading (near-zero extraction, treats text as coordination subordinate to domestic law). Per the epsilon-invariance principle, these are three separate constraints, not one constraint measured three ways, and are linked here via affects_constraints because they contest the same underlying kernel and shift each other's legitimacy conditions through case law and negotiation practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
