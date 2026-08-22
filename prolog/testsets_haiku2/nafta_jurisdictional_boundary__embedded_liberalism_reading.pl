% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA Jurisdictional Boundary (Embedded Liberalism Reading)
 *   domain: economic/political/regulatory
 *
 * SUMMARY:
 *   The North American Free Trade Agreement (NAFTA) embeds a contested
 *   jurisdictional boundary: trade liberalization for market access balanced
 *   against domestic regulatory authority. The embedded-liberalism reading
 *   interprets this as a compromise where signatory states retain authority
 *   to set environmental, labor, and health standards, provided they are
 *   non-discriminatory and serve 'legitimate objectives' compatible with
 *   trade obligations. This reading sits between the capital-supremacy
 *   reading (which treats trade rules as supreme law mandating regulatory
 *   harmonization) and the sovereignty-primacy reading (which subordinates
 *   trade obligations entirely to domestic law). The constraint's structure:
 *   partial jurisdictional overlap, defensive regulatory authority within the
 *   carve-out, and moderate extraction through litigation costs and
 *   interpretive uncertainty. The claim is tangled rope—genuine coordination
 *   problem solved (trade fragmentation, tariff escalation) but asymmetric
 *   extraction built into the enforcement mechanism (dispute panels biased
 *   toward liberalization, litigation costs on regulators).
 *
 * KEY AGENTS:
 *   - Multinational capital: gains expanded market access and regulatory predictability; can challenge domestic standards via investor-state dispute
 *   - Domestic regulatory agencies: defend standards against litigation; bear costs of proving 'legitimate objectives' and non-discrimination
 *   - Labor movements and environmental constituencies: lose unilateral regulatory authority; must justify standards within treaty framework
 *   - Dispute resolution panels: interpret 'legitimate objectives' threshold; their jurisprudence evolves and constrains future state action
 *   - Treaty signatories: retain formal sovereignty but lose interpretive control once tribunals begin applying the text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.54).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.48).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary (Embedded Liberalism Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "economic/political/regulatory").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'd97b5bf3-09b5-4808-b8b8-4184317ec8a9').
narrative_ontology:cs_kernel_codification('d97b5bf3-09b5-4808-b8b8-4184317ec8a9', formalized).
narrative_ontology:cs_authority_grounding('d97b5bf3-09b5-4808-b8b8-4184317ec8a9', extraction).
narrative_ontology:cs_interpretation_layer_present('d97b5bf3-09b5-4808-b8b8-4184317ec8a9').
narrative_ontology:cs_reading_relation('d97b5bf3-09b5-4808-b8b8-4184317ec8a9', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('d97b5bf3-09b5-4808-b8b8-4184317ec8a9', nafta_jurisdictional_boundary__sovereignty_primacy_reading, influences).
narrative_ontology:cs_axiom('d97b5bf3-09b5-4808-b8b8-4184317ec8a9', foundational, non_discriminatory_regulation_compatible_with_trade).
narrative_ontology:cs_axiom_status(non_discriminatory_regulation_compatible_with_trade, holdable).
narrative_ontology:cs_axiom_grounding('d97b5bf3-09b5-4808-b8b8-4184317ec8a9', non_discriminatory_regulation_compatible_with_trade, conventional).
narrative_ontology:cs_axiom('d97b5bf3-09b5-4808-b8b8-4184317ec8a9', foundational, legitimate_objectives_carve_out_preserves_regulatory_space).
narrative_ontology:cs_axiom_status(legitimate_objectives_carve_out_preserves_regulatory_space, overridden).
narrative_ontology:cs_axiom_grounding('d97b5bf3-09b5-4808-b8b8-4184317ec8a9', legitimate_objectives_carve_out_preserves_regulatory_space, deontological).
narrative_ontology:cs_reference_frame('d97b5bf3-09b5-4808-b8b8-4184317ec8a9', embedded_liberalism_compromise).
narrative_ontology:cs_drift_state('d97b5bf3-09b5-4808-b8b8-4184317ec8a9', contemporary_dispute_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d97b5bf3-09b5-4808-b8b8-4184317ec8a9', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_capital).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_sectors).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_movements).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_constituencies).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, regulatory_convergence_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, non_discrimination_principle).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, embedded_liberalism_compromise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains market access across all three signatory countries under simplified regulatory framework; can challenge domestic standards that diverge from treaty baseline as non-compliant protectionism. Investment chapters provide dispute resolution mechanisms that bypass national courts, allowing direct suits against state regulatory action. Collects value from regulatory harmonization that eliminates divergent standards across markets. Can relocate investment to jurisdictions with lowest-cost regulatory compliance.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Access to expanded markets with reduced tariff and non-tariff barriers. Can threaten relocation to lower-regulation jurisdictions within the trade bloc, using regulatory arbitrage to pressure domestic standard-setting. Their supply chains integrate across all three countries, making domestic divergence costly. Industry associations participate in dispute settlement as amici curiae (friends of the court) and lobby for expansive trade interpretation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_sectors, beneficiary,
    organized, generational, mobile, global).

% Bear litigation costs and settlement obligations when defending environmental, labor, or health standards against investor-state dispute claims. Must demonstrate that any standard serving a 'legitimate objective' is neither protectionist nor more trade-restrictive than necessary. The burden of proof is inverted: regulators justify departures from trade baseline rather than capital justifying its demands. Cannot unilaterally rewrite rules; changes require political consensus and treaty amendment. Field expertise is devalued in dispute panels where trade lawyers define 'legitimate.'
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, payer,
    institutional, biographical, constrained, national).

% Seek labor standards (minimum wage, union rights, workplace safety) that exceed treaty baseline. Can argue standards are compatible with trade obligations if non-discriminatory, but face litigation risk when firms claim standards increase compliance costs beyond competitors' burden. Their constituency is geographically rooted; they cannot arbitrage to other jurisdictions. Labor chapters in trade agreements are side agreements (North American Agreement on Labor Cooperation) with weaker enforcement than investment chapters.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_movements, payer,
    organized, generational, constrained, national).

% Defend environmental standards (air/water quality, pesticide bans, carbon pricing) against claims they violate trade obligations. The embedded-liberalism reading permits these standards if they serve legitimate objectives (environmental protection, public health) and apply equally to domestic and foreign producers. But proving non-discrimination and defending against proportionality challenges creates litigation costs and implementation delays. Geographic immobility means they cannot exit the jurisdiction or its regulatory regime. Environmental side agreement (North American Agreement on Environmental Cooperation) also lacks investor-state dispute authority.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_constituencies, payer,
    moderate, generational, constrained, national).

% Interpret treaty text through panels and arbitral tribunals operating under rules that privilege trade liberalization. In the embedded-liberalism reading, they apply a 'legitimate objectives' test, allowing domestic standards if they serve a recognized public purpose and are non-discriminatory. But the interpretive threshold itself becomes contested: does 'legitimate' mean narrowly economic or broadly social? How much trade impact negates legitimacy? Panels' jurisprudence evolves, and early decisions constrain later ones. Panel composition (trade lawyers, not environmental/labor experts) embeds directionality toward liberalization.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, dispute_resolution_panels, agenda_setter,
    institutional, generational, analytical, global).

% Retain formal sovereignty to set regulatory standards and can invoke 'legitimate objectives' carve-outs. But renegotiation costs are high, and exit (trade-war scenarios) is economically devastating. States become locked into an interpretation regime where tribunals, not legislatures, refine the boundary between market access and policy space. Their observer status reflects that they authored the agreement but lost interpretive control once panels began applying it. Domestic political costs of defending standards mount as disputes accumulate.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, treaty_signatories, observer,
    institutional, generational, mobile, continental).

% Non-signatory jurisdictions or trading blocs with different standard-setting approaches (precautionary principle, strong labor protections, mandatory environmental impact assessment) are locked out of market access unless they harmonize. States that have already built regulatory capacity around stricter standards face pressure to dilute them to comply with the treaty baseline. Alternative models that prioritize public goods over capital mobility are not represented in dispute resolution. The exclusion is structural, not accidental—the agreement was designed to incorporate North America into a single regulatory zone.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, alternative_regulatory_models, excluded,
    moderate, generational, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_capital).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__embedded_liberalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of tariff escalation and regulatory fragmentation across three integrated economies: establishes transparent rules for market access, eliminates tariff wars that harm all signatories, and creates a common framework for dispute resolution rather than bilateral threats. Reduces transaction costs for cross-border investment and supply-chain integration.
% TRANSFER_FUNCTION: Transfers regulatory authority from national legislatures to supranational dispute panels; transfers rents from domestic constituencies bearing regulatory costs to multinational capital that captures value from harmonized markets and reduced compliance complexity. Moves litigation costs and regulatory defense burdens onto domestic regulatory agencies, labor movements, and environmental constituencies. The constraint moves policy authority upward (from national to treaty level) and outward (from democratic legislatures to expert panels).
% ABSENT_VOICES: Non-signatory jurisdictions with stricter standards, low-wage labor competing for manufacturing relocation, environmental justice constituencies in extraction zones, Indigenous communities whose land/resources are affected by investment protections, and alternative regulatory traditions (precautionary principle, commons-based) are excluded from interpretation and benefit-sharing. They would argue for preserving regulatory diversity, contestable standard-setting, and community consent to resource extraction. They have no seat in the dispute mechanism.
% DISAPPEARANCE_RATIONALE: Without the treaty framework, tariff and non-tariff barriers would fragment again; capital would reconfigure supply chains to minimize tariff exposure; dispute resolution would revert to bilateral negotiations or WTO channels. The 'legitimate objectives' carve-out would vanish, but so would the investment-chapter override of domestic courts. Regulatory agencies would regain unilateral authority to set standards without dispute threat; labor and environmental constituencies would recover political channels for standard-setting; states would face higher trade risk but lower regulatory chilling effects. The rearrangement would be substantial and painful for export-oriented sectors and multinational capital, moderate for domestic regulators (recovering autonomy), and beneficial for labor/environmental constituencies.
% FOUNDING_PROBLEM: Post-1980s North American economies faced fragmented trade rules, competing regulatory standards, tariff escalation (especially in sensitive sectors like agriculture and auto), and disputes that threatened trade relationships. The treaty aimed to create predictable market access for capital, reduce transaction costs for cross-border commerce, and establish stable rules while maintaining room for domestic social policy through the 'legitimate objectives' carve-out.
% FOUNDING_PROBLEM_CORROBORATION: Trade economists and the treaty signatories (official negotiators, trade ministries) attest the fragmentation problem was real and the agreement reduced tariff-war risk and transaction costs. Labor advocates and environmental scholars attest the founding problem was solved for capital mobility but the regulatory-space problem was exacerbated by investor-state dispute chapters, which were added as enforcement mechanisms and shifted the balance away from the embedded-liberalism compromise. Indigenous communities and excluded nations attest their exclusion from the framework was structural—the agreement was designed to consolidate North American capital's position and constrain regulatory alternatives. Historical analysis of negotiation records (declassified memos, congressional testimony) supports both the capital and labor/environmental readings.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.54, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (1994, ambiguous enforcement) to 0.54 (2024, mature dispute jurisprudence). The intermediate peak (0.56 at 2015) reflects maximum investor-state dispute activity and regulatory rollbacks; the plateau from 2015-2024 reflects treaty renegotiation signals (USMCA talks, domestic political pressure) that stabilized expectations without resolving the boundary dispute. Theater ratio rises from 0.25 to 0.42: early dispute rulings invoked 'legitimate objectives' narrowly (environmental protection only, not broader social policy); by 2008-2015, panels began performing deference to regulators while ultimately constraining them—the constraint increasingly operates through interpretive performance rather than transparent overrides. Suppression follows extraction closely, peaking at 2015 when regulatory agencies faced sustained litigation costs; the slight drop by 2024 reflects procedural reforms (shorter dispute timelines, higher evidentiary bars for capital) but not substantive constraint relaxation. All measurements are on a shared time grid; the interval spans NAFTA's 30-year lifecycle through its renegotiation.
 *
 * PERSPECTIVAL GAP:
 *   From capital's seat: genuine market-access coordination, asymmetry justified by need to constrain protectionism. From regulator seats: coercive standardization, the 'legitimate objectives' language creates a false choice between trade and public goods. From labor/environmental seats: subordination of democratic standard-setting to trade panels. The engine computes divergent per-seat types from this structural asymmetry: beneficiaries see rope-like coordination benefits (d near 0.0); payers see snare-like extraction (d near 1.0); the treaty's ambiguous language and panels' evolving interpretation allow both seats to claim vindication—tangled rope is the label for this coexistence of genuine coordination and real extraction in one structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational capital derives directionality near 0.1 (full beneficiary): gains market access without regulatory burden, can exit to other jurisdictions if one country's standards rise. Domestic regulatory agencies derive directionality near 0.8 (full target): must defend every standard, bear litigation costs, cannot exit. Labor and environmental constituencies derive directionality near 0.85: geographically rooted, cannot arbitrage, face regulatory constraints. Dispute panels themselves derive directionality near 0.5 (symmetric): they solve the coordination problem (predictable rules, no tariff wars) but their interpretive mission inevitably favors liberalization because the treaty's text prioritizes market access over regulatory space. The panels are symmetric participants in the coordination function and asymmetric beneficiaries of the extraction that coordination enables.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmentation, tariff escalation) has been substantially solved: tariff rates converged, cross-border investment rose, supply chains integrated. But the 'legitimate objectives' carve-out that was supposed to preserve regulatory space has been gradually narrowed through jurisprudence, not abandoned—mandatrophy is partial, not complete. The constraint persists not because the original coordination problem endures but because the extraction mechanism (litigation threat, regulatory chilling effect) has become self-sustaining. States cannot exit without trade-war costs; regulators cannot strengthen standards without dispute risk; labor and environmental constituencies cannot build public support without being accused of protectionism. Mandatrophy resolved: yes, under the embedded-liberalism reading, because the reading's own compromises have failed—the 'balance' has shifted toward capital as dispute jurisprudence accumulates. The engine should flag this: a tangled rope where the rope component (trade facilitation) is aging well but the snare component (regulatory subordination) is intensifying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_objectives_boundary,
    'What regulatory standards qualify as serving a ''legitimate objective'' compatible with trade obligations? Is the boundary economic (consumer protection, resource conservation at lowest cost) or social (worker dignity, intergenerational equity, cultural values)?',
    'Jurisprudential evolution: if dispute panels consistently permit environmental/labor standards that exceed trade partners'' baseline, the boundary is inclusive; if panels narrow ''legitimate'' to economic efficiency, the boundary is exclusive. Legislative action: if treaty signatories formally amend the carve-out to specify social objectives, the boundary is resolved by contract revision.',
    'An inclusive boundary preserves regulatory diversity and labor/environmental space; an exclusive boundary converts the constraint to pure snare (market access supreme, standards subordinate). This omega determines whether embedded-liberalism is genuinely a compromise or a staging ground for capital supremacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimate_objectives_boundary, conceptual, 'Whether ''legitimate objectives'' includes social objectives or is limited to economic efficiency.').

omega_variable(
    non_discrimination_doctrine_applicability,
    'Does non-discrimination (same rules for domestic and foreign producers) actually constrain regulatory authority, or does it permit asymmetric impact? If a carbon tax targets carbon-intensive industries regardless of origin but those industries are disproportionately foreign-owned, is the tax discriminatory?',
    'Dispute panel doctrine: do they examine intent and formal rules only, or examine disparate impact and systemic structure? Economic analysis: are domestic constituencies absorbing proportional costs, or are foreign producers bearing a concentrated burden?',
    'If non-discrimination means formal equality of rules, states retain substantial regulatory space (can tax carbon equally, even if foreign firms are most affected). If it means equality of impact, regulation targeting structural inequalities (labor exploitation, environmental degradation concentrated in certain industries) becomes vulnerable. This omega determines how much extraction the constraint imposes through interpretive uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_discrimination_doctrine_applicability, empirical, 'Whether non-discrimination is formal (same rules) or substantive (equal impact).').

omega_variable(
    embedded_liberalism_vs_capital_supremacy_drift,
    'Is the embedded-liberalism reading''s compromise drifting toward capital-supremacy interpretation, or is the ''legitimate objectives'' carve-out holding as a stable boundary?',
    'Quantitative jurisprudential analysis: track the ratio of cases where panels permit regulatory standards vs. strike them down over time. Qualitative analysis: do panels'' interpretive language shift toward narrower ''legitimate objectives'' language? Negotiation dynamics: do renegotiation talks (USMCA, potential replacement agreements) expand or contract the regulatory-space carve-out?',
    'If drifting toward capital supremacy, the constraint is transitioning from tangled rope toward snare; if the boundary is holding, embedded-liberalism is genuinely the stable reading. This omega tracks the central empirical question for constraint evolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(embedded_liberalism_vs_capital_supremacy_drift, empirical, 'Jurisprudential trajectory toward capital supremacy or embedded-liberalism stability.').

omega_variable(
    regulatory_chilling_effect_mechanism,
    'How much of the suppression is structural (formal dispute threat) versus internalized (regulators self-censor preemptively)? Do regulatory agencies avoid proposing standards they believe would survive challenge, or are they accurately calculating dispute risk?',
    'Policy-maker interviews and archival analysis: document instances where standards were proposed, withdrawn, or diluted in anticipation of dispute. Comparative jurisdictional study: compare regulatory activity in NAFTA signatory countries with jurisdictions outside the agreement; do NAFTA signatories propose fewer stringent standards?',
    'If suppression is primarily structural (formal dispute mechanism), removing the threat mechanism would restore regulatory capacity quickly. If primarily internalized (self-censorship, internalized deference to capital), regulatory recovery would require longer cultural/institutional shift. This omega distinguishes extractiveness driven by the mechanism''s active operation from extractiveness driven by its perceived threat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chilling_effect_mechanism, empirical, 'Whether regulatory suppression is structural threat or internalized self-censorship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 1994, 0.25).
narrative_ontology:measurement_basis(naft_tr_t1994, observed).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2000, 0.31).
narrative_ontology:measurement_basis(naft_tr_t2000, observed).
narrative_ontology:measurement(naft_tr_t2008, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement_basis(naft_tr_t2008, observed).
narrative_ontology:measurement(naft_tr_t2015, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement_basis(naft_tr_t2015, observed).
narrative_ontology:measurement(naft_tr_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement_basis(naft_tr_t2020, observed).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(naft_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 1994, 0.38).
narrative_ontology:measurement_basis(naft_be_t1994, observed).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement_basis(naft_be_t2000, observed).
narrative_ontology:measurement(naft_be_t2008, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2008, 0.51).
narrative_ontology:measurement_basis(naft_be_t2008, observed).
narrative_ontology:measurement(naft_be_t2015, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2015, 0.56).
narrative_ontology:measurement_basis(naft_be_t2015, observed).
narrative_ontology:measurement(naft_be_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2020, 0.54).
narrative_ontology:measurement_basis(naft_be_t2020, observed).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2024, 0.54).
narrative_ontology:measurement_basis(naft_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 1994, 0.35).
narrative_ontology:measurement_basis(naft_su_t1994, observed).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement_basis(naft_su_t2000, observed).
narrative_ontology:measurement(naft_su_t2008, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2008, 0.47).
narrative_ontology:measurement_basis(naft_su_t2008, observed).
narrative_ontology:measurement(naft_su_t2015, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2015, 0.51).
narrative_ontology:measurement_basis(naft_su_t2015, observed).
narrative_ontology:measurement(naft_su_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement_basis(naft_su_t2020, observed).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2024, 0.48).
narrative_ontology:measurement_basis(naft_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.18).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, investor_state_dispute_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, regulatory_harmonization_pressure).

% DUAL FORMULATION NOTE:
% The NAFTA jurisdictional boundary is a single contested kernel with three structural readings. This story instantiates the embedded-liberalism reading (partial jurisdictional overlap, 'legitimate objectives' carve-out, moderate extraction through litigation costs and interpretive uncertainty). Sibling readings are authored as separate constraint stories: capital_supremacy_reading (trade rules supreme, regulatory harmonization mandatory, high extraction) and sovereignty_primacy_reading (regulatory authority supreme, trade obligations subordinate, low extraction). The three readings share the same referent (the NAFTA text and its application) but differ in ε (empirical extraction), beneficiary/victim structure, and claimed type. The embedded-liberalism reading is positioned as a compromise; capital-supremacy and sovereignty-primacy readings represent the poles of ongoing dispute. Cross-index linkage: affects_constraints arrows point from embedded-liberalism to its siblings and to downstream constraints (investor-state disputes, regulatory harmonization pressure) that depend on how the boundary is interpreted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
