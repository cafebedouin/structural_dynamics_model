% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__market_access_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: gdpr_article_3_scope__market_access_reading
 *   human_readable: GDPR Article 3 Scope — Market Access Reading (Brussels Effect Standard-Setting)
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   GDPR Article 3(2) extends the regulation's protections to processing
 *   activities targeting or monitoring EU residents, regardless of where the
 *   controller is established. The 'BGS'-style ambiguity here is that the
 *   same statutory text supports at least three structurally distinct
 *   readings of what kind of claim this extension makes: an effects-based
 *   jurisdictional test, a straightforward extraterritorial sovereignty
 *   overreach, or — the reading instantiated in this story — a conditional
 *   market-access requirement whose global reach is a side effect of firms
 *   voluntarily choosing to meet a standard in order to sell into a valuable
 *   market. Under the market-access reading, no firm is coerced into
 *   compliance in the sovereignty sense; a firm can always exit the EU market
 *   and be released from the obligation. The 'extraterritoriality'
 *   commentators describe is better understood as the Brussels Effect: EU
 *   standards diffuse globally because it is cheaper for multinational firms
 *   to build one compliant system than to fragment infrastructure by
 *   jurisdiction. This story deliberately does NOT adjudicate the
 *   effects_jurisdiction_reading or territorial_sovereignty_reading — those
 *   are separate constraints with separate ε values, victim structures, and
 *   enforcement postures, linked here only via network edges and the omega
 *   variables below.
 *
 * KEY AGENTS:
 *   - eu_regulatory_apparatus: agenda_setter/beneficiary (institutional/analytical) — sets the conditional standard and gains global normative leverage
 *   - non_eu_firms_serving_eu_market: payer (powerful/mobile) — bears compliance cost but retains a real exit option (withdraw from EU market)
 *   - eu_resident_data_subjects: beneficiary (moderate/constrained) — receives protection as the intended effect of the condition
 *   - small_non_eu_firms_without_arbitrage: payer/excluded (moderate/constrained) — lacks the scale to treat exit as costless, rarely heard in the policy debate
 *   - non_eu_sovereign_regulators: observer (institutional/analytical) — watches standard diffusion reshape domestic regulatory agendas
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.42).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.3).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3 Scope — Market Access Reading (Brussels Effect Standard-Setting)").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology_governance/international_law/privacy_regulation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, 'cb4087d9-5e6d-4d02-805b-50e499cf23d1').
narrative_ontology:cs_kernel_codification('cb4087d9-5e6d-4d02-805b-50e499cf23d1', fixed_text).
narrative_ontology:cs_authority_grounding('cb4087d9-5e6d-4d02-805b-50e499cf23d1', extraction).
narrative_ontology:cs_interpretation_layer_present('cb4087d9-5e6d-4d02-805b-50e499cf23d1').
narrative_ontology:cs_reading_relation('cb4087d9-5e6d-4d02-805b-50e499cf23d1', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb4087d9-5e6d-4d02-805b-50e499cf23d1', gdpr_article_3_scope__territorial_sovereignty_reading, influences).
narrative_ontology:cs_axiom('cb4087d9-5e6d-4d02-805b-50e499cf23d1', foundational, market_access_is_conditional_not_coercive).
narrative_ontology:cs_axiom_status(market_access_is_conditional_not_coercive, holdable).
narrative_ontology:cs_axiom_grounding('cb4087d9-5e6d-4d02-805b-50e499cf23d1', market_access_is_conditional_not_coercive, conventional).
narrative_ontology:cs_axiom('cb4087d9-5e6d-4d02-805b-50e499cf23d1', secondary, exit_from_regulated_market_dissolves_obligation).
narrative_ontology:cs_axiom_status(exit_from_regulated_market_dissolves_obligation, holdable).
narrative_ontology:cs_axiom_grounding('cb4087d9-5e6d-4d02-805b-50e499cf23d1', exit_from_regulated_market_dissolves_obligation, instrumental).
narrative_ontology:cs_reference_frame('cb4087d9-5e6d-4d02-805b-50e499cf23d1', conditional_market_access_baseline).
narrative_ontology:cs_drift_state('cb4087d9-5e6d-4d02-805b-50e499cf23d1', post_brussels_effect_diffusion, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('cb4087d9-5e6d-4d02-805b-50e499cf23d1', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_resident_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_based_compliant_firms).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, global_privacy_conscious_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, non_eu_firms_serving_eu_market).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, small_non_eu_firms_without_arbitrage).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, brussels_effect_standard_diffusion_thesis).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, market_access_conditionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted Article 3 so that any firm wanting to offer goods or services to, or monitor, EU residents must meet EU data protection standards regardless of where the firm is incorporated or where its servers sit. Under this reading, the mechanism is not a claim to police foreign territory but a condition of market entry: firms choose to comply because the EU market is valuable, not because Brussels asserts sovereign authority over Ohio or Osaka. The apparatus gains outsized influence over global privacy norms because global firms find it cheaper to build one compliant system than to run parallel regimes.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_regulatory_apparatus, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, eu_regulatory_apparatus, beneficiary).

% Receive protection over their personal data whenever a firm targets or monitors them, no matter where the firm operates. Under the market-access reading, this protection is a side effect of a conditional-access rule rather than an extraterritorial reach claim: the firm chose to serve this market and accepted the condition attached.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_resident_data_subjects, beneficiary,
    moderate, biographical, constrained, regional).

% Already bear compliance costs as a condition of operating in their home market. Benefit competitively when foreign firms wishing to access the same market must match those costs — the conditional-access framing levels a playing field that would otherwise favor unregulated foreign entrants.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_based_compliant_firms, beneficiary,
    organized, biographical, constrained, regional).

% Choose whether to build GDPR-compliant systems in order to access EU customers, or to decline that market and avoid the obligation entirely. Under this reading their exit option is real and structurally available — withdrawing from the EU market ends the obligation — which is the central factual claim that distinguishes market-access framing from a jurisdictional-assertion framing where withdrawal would not dissolve the claim of authority. Many choose global compliance because building one system is cheaper than segmenting infrastructure by market, which is what produces the Brussels Effect.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_firms_serving_eu_market, payer,
    powerful, biographical, mobile, global).

% Benefit incidentally outside the EU when firms adopt GDPR-equivalent practices globally rather than segmenting by jurisdiction, absorbing EU-level protections as a byproduct of the firm's engineering economics rather than through any legal entitlement of their own.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, global_privacy_conscious_consumers, beneficiary,
    powerless, generational, analytical, global).

% Watch domestic firms adopt EU-derived standards as de facto defaults, sometimes preempting or shaping their own domestic legislative agendas. They can validate or contest the market-access framing depending on whether they treat the EU's leverage as a legitimate market condition or an erosion of their own regulatory primacy.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_sovereign_regulators, observer,
    institutional, generational, analytical, national).

% Lack the scale to segment infrastructure by jurisdiction the way large multinationals do, so the notionally free 'exit the EU market' option is costlier in practice than for a firm that can build regional variants. They comply or forgo EU customers outright, and rarely appear in the policy conversation about whether the extraterritorial mechanism is legitimate.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, small_non_eu_firms_without_arbitrage, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, small_non_eu_firms_without_arbitrage, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a race-to-the-bottom problem in global data protection: without a conditional-access mechanism, firms could offer weaker protections to whichever jurisdiction demands least, undermining protections everywhere. Tying market access to a floor standard coordinates firms around one global baseline instead of fragmenting into jurisdiction-shopping regimes.
% TRANSFER_FUNCTION: Moves compliance costs from EU taxpayers/regulators (who would otherwise need to police cross-border data flows piecemeal) onto firms wishing to serve the EU market, and moves de facto standard-setting authority from fragmented national regimes toward the EU as first mover.
% ABSENT_VOICES: Small non-EU firms without the scale to geo-fence their EU exposure are rarely represented in the debate over Article 3's reach — trade associations and large multinational counsel dominate the public record, while firms too small to litigate or lobby simply comply or exit silently.
% DISAPPEARANCE_RATIONALE: EU beneficiaries and regulators would say the world rearranges substantially: without conditional market access, global privacy standards would fragment and enforcement leverage over foreign firms would evaporate. Sovereigntist critics would say enforcement against non-EU firms with no EU presence is already largely symbolic, so removing Article 3(2)'s extraterritorial language would change little in practice — hence contested rather than settled either way.
% FOUNDING_PROBLEM: Digital services could target and profile EU residents from servers and headquarters entirely outside the EU, making the EU's own domestic data protection law trivially avoidable by relocating infrastructure rather than changing practices.
% FOUNDING_PROBLEM_CORROBORATION: Independent trade-law scholars and non-EU competition authorities corroborate that the avoidance problem was real prior to Article 3(2) and that infrastructure relocation was a documented compliance-avoidance strategy; this corroboration comes from academic and foreign-regulator sources outside the EU regulatory apparatus itself, though sovereigntist governments dispute whether the chosen remedy is proportionate to the problem it names.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, contested).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__market_access_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__market_access_reading_tests).
:- end_tests(gdpr_article_3_scope__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at interval end) and rising only gradually: under the market-access framing, most of what looks like extraction from outside observers is compliance cost willingly incurred in exchange for market access, not coerced rent extraction. Suppression is authored low-moderate (0.30) because the defining feature of this reading is that exit is real — a firm can withdraw from the EU market and dissolve the obligation, which is precisely what distinguishes this reading from the effects_jurisdiction and territorial_sovereignty readings where exit is either irrelevant to the jurisdictional claim or contested as illegitimate reach. Theater ratio is low (0.20) because the compliance apparatus (DPOs, audits, breach notification) performs a genuinely substantive function under this reading, not mere performance. Resistance is moderate (0.38): large multinationals resist compliance costs through lobbying and litigation, but resistance to the market-access framing itself (as opposed to resistance to GDPR's substantive content) is comparatively muted since the conditionality logic is intuitive to firms used to meeting import standards.
 *
 * PERSPECTIVAL GAP:
 *   From the EU regulatory apparatus's seat, Article 3(2) looks like rope: a voluntary, conditional standard that firms opt into for market access, producing beneficial global diffusion. From a large non-EU firm's seat with genuine market alternatives, it also computes close to rope or a mild tangled_rope — real coordination benefit (one global compliance system, avoided duplicate infrastructure) at a real but chosen cost. From a small non-EU firm without arbitrage capacity, the same structure computes closer to tangled_rope or even snare-adjacent, because their nominal exit option does not function as a genuine off-ramp. The engine should surface this seat divergence directly from the differentiated exit_options and power atoms authored above, without this story asserting any single verdict for all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The EU regulatory apparatus sits at the beneficiary end: it sets a condition and gains outsized global standard-setting influence without needing to claim sovereign authority abroad. EU resident data subjects and EU-based compliant firms are beneficiaries — the former receive the intended protection, the latter gain competitive parity. Non-EU firms serving the EU market are payers, but crucially their directionality is damped relative to what a jurisdictional-assertion reading would assign, because their exit option (mobile — withdraw from the EU market) is real and structurally available, not merely theoretical. Small non-EU firms without arbitrage capacity are also payers, but their functional exit option is costlier than the nominal 'mobile' category suggests, which is why they carry a secondary excluded role — the story's stakeholder surface distinguishes firms who can genuinely walk away from firms who can only nominally do so.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — infrastructure relocation as a trivial way to evade EU data protection law — remains live: this is not an arrangement whose function has expired while the mandate persists. Because the founding problem is corroborated as live by academic and foreign-regulator sources rather than solely by the EU apparatus itself, this reading resists a mandatrophy diagnosis; the disagreement recorded in the six_questions is about proportionality and legitimacy of remedy, not about whether the underlying coordination problem still exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_compliance_or_disguised_coercion,
    'Is the ''choice'' to comply with GDPR to access the EU market genuinely voluntary for all firm sizes, or does the practical unavailability of segmentation for small/mid firms convert what is nominally a conditional-access choice into de facto coercion?',
    'Empirical study of compliance-cost-to-revenue ratios and actual market-exit rates by firm size following GDPR enforcement actions; compare exit rates of large multinationals vs. small foreign firms.',
    'If small-firm exit is empirically near-zero despite high compliance burden relative to EU revenue, the market-access framing understates effective extraction for that segment and the constraint may need decomposition by firm size rather than treatment as one uniform reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_compliance_or_disguised_coercion, empirical, 'Whether market-access conditionality is genuinely optional across all firm scales or only for firms large enough to segment infrastructure.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the market_access_reading the dominant lived reality of Article 3(2), or is it a legitimating narrative that coexists with — and partly obscures — the effects_jurisdiction_reading''s doctrinal claim that EU authority genuinely extends to non-EU processing activities regardless of any market-access framing?',
    'Analysis of enforcement actions against firms with zero EU market presence but processing EU resident data (e.g., academic researchers, non-commercial actors) — if enforcement occurs there, the market-access framing (which presumes a market transaction as the triggering condition) cannot fully explain the doctrine in practice.',
    'If enforcement extends meaningfully beyond market-participating firms, the market_access_reading''s central premise (that exit from the EU market dissolves the obligation) is empirically narrower than claimed, and the effects_jurisdiction_reading better describes actual practice — this would not change this story''s ε (which describes only the market-access reading as a distinct constraint) but would affect which reading analysts should treat as descriptively dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the market-access framing is the operative logic of enforcement or a favorable narrative layered over broader effects-based jurisdiction.').

omega_variable(
    brussels_effect_beneficiary_durability,
    'Is the EU''s gain in global standard-setting influence via the Brussels Effect a stable, self-sustaining beneficiary position, or is it contingent on the continued absence of a comparable competing standard (e.g., a US federal privacy law or a China-centric data regime) that could displace GDPR''s default status?',
    'Track adoption trajectories of competing comprehensive privacy frameworks over the next decade and whether multinational firms begin building to multiple defaults rather than a single GDPR-derived baseline.',
    'If a competing standard achieves comparable market leverage, the EU''s beneficiary position weakens and the coordination-function story (one global baseline avoiding fragmentation) may fail, converting today''s rope-adjacent reading into a more contested, tangled arrangement as firms face genuine multi-standard fragmentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brussels_effect_beneficiary_durability, empirical, 'Durability of EU standard-setting leverage against future competing regulatory regimes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__market_access_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__market_access_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__market_access_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(gdpr_tr_t12, gdpr_article_3_scope__market_access_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(gdpr_tr_t16, gdpr_article_3_scope__market_access_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__market_access_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(gdpr_tr_t24, gdpr_article_3_scope__market_access_reading, theater_ratio, 24, 0.2).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__market_access_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__market_access_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__market_access_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(gdpr_be_t12, gdpr_article_3_scope__market_access_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(gdpr_be_t16, gdpr_article_3_scope__market_access_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__market_access_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(gdpr_be_t24, gdpr_article_3_scope__market_access_reading, base_extractiveness, 24, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gdpr_article_3_scope__market_access_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__market_access_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__territorial_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'GDPR Article 3 extraterritorial scope.' Each reading is authored as a separate constraint with its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle: the market_access_reading (this story) treats the mechanism as conditional market access with real exit and low suppression; the effects_jurisdiction_reading treats it as a doctrinal jurisdictional test triggered by targeting/monitoring effects, independent of any market transaction; the territorial_sovereignty_reading treats extraterritorial application as exceeding legitimate regulatory authority, naming affected non-EU firms and governments as victims of overreach. The three are linked here so that contamination/coupling analysis across the family is possible without collapsing them into one averaged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
