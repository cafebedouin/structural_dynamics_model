% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__orthodox_price_stability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Article 127 Orthodox Price Stability Mandate (Narrow Reading)
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   Article 127(1) of the Treaty on the Functioning of the European Union
 *   (TFEU) mandates that the ECB's 'primary objective shall be to maintain
 *   price stability,' with secondary objectives (employment, sustainable
 *   development) explicitly subordinate ('without prejudice to' the primary
 *   objective). The orthodox reading instantiates this as an exclusive focus:
 *   the ECB must pursue a 2% inflation target with no operational constraint
 *   from climate, employment, or distributional concerns. This constraint
 *   story models the orthodox reading as a specific institutional choice that
 *   benefits creditors and savers, externalizes climate risk to national
 *   fiscal budgets, and suppresses operational pursuit of secondary EU
 *   objectives. The constraint exhibits classic snare characteristics from
 *   the perspective of climate and employment mandates: trapped
 *   subordination, high suppression, and significant extraction to the
 *   beneficiary class. The orthodox reading competes with two sibling
 *   readings: (1) the expansive reading that operationalizes secondary
 *   objectives as co-equal constraints, and (2) the climate-incorporation
 *   reading that reweights the hierarchy to treat climate stability as
 *   co-primary. This story generates only the orthodox reading, treating its
 *   competitors as structurally distinct constraints in separate JSON files.
 *
 * KEY AGENTS:
 *   - ECB (Institutional Decision-Maker): Possesses legal authority to interpret Article 127 and operationalize the mandate. In the orthodox reading, the ECB has entrenched the narrow interpretation as institutional identity. (institutional/arbitrage)
 *   - Creditors and Savers: Primary beneficiaries experiencing the mandate as coordination. Wealth protection through inflation discipline. (institutional/arbitrage)
 *   - Climate Policy Mandates: Legally subordinate secondary objective. Trapped in a hierarchy where price stability veto blocks climate-related monetary support. (powerless/trapped)
 *   - Employment & Social Policy: Legally subordinate secondary objective. Cannot be operationally pursued when inflation deviates from 2%. (powerless/trapped)
 *   - Fiscal Authorities (Member States): Constrained by monetary ceiling. Must compensate for ECB's non-responsiveness to employment and climate through fiscal policy, creating political pressure and fiscal dominance. (moderate/constrained)
 *   - Climate-Incorporating Coalition: Organized actors (European Commission, environmental parties, southern member states) seeking to amend or reinterpret the mandate. (organized/constrained to mobile)
 *   - Treaty Reform Movement: Political coalitions advocating Article 48 amendment to operationalize secondary objectives. (organized/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.58).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.68).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, snare).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Article 127 Orthodox Price Stability Mandate (Narrow Reading)").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, 'c0af8578-48be-487a-8b1a-35e46e5c32bd').
narrative_ontology:cs_kernel_codification('c0af8578-48be-487a-8b1a-35e46e5c32bd', fixed_text).
narrative_ontology:cs_authority_grounding('c0af8578-48be-487a-8b1a-35e46e5c32bd', extraction).
narrative_ontology:cs_interpretation_layer_present('c0af8578-48be-487a-8b1a-35e46e5c32bd').
narrative_ontology:cs_reading_relation('c0af8578-48be-487a-8b1a-35e46e5c32bd', ecb_mandate_article_127__expansive_secondary_objectives, forecloses).
narrative_ontology:cs_reading_relation('c0af8578-48be-487a-8b1a-35e46e5c32bd', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('c0af8578-48be-487a-8b1a-35e46e5c32bd', foundational, price_stability_primary_veto).
narrative_ontology:cs_axiom_status(price_stability_primary_veto, holdable).
narrative_ontology:cs_axiom_grounding('c0af8578-48be-487a-8b1a-35e46e5c32bd', price_stability_primary_veto, deontological).
narrative_ontology:cs_axiom('c0af8578-48be-487a-8b1a-35e46e5c32bd', secondary, secondary_objectives_subordinate_rhetoric).
narrative_ontology:cs_axiom_status(secondary_objectives_subordinate_rhetoric, overridden).
narrative_ontology:cs_axiom_grounding('c0af8578-48be-487a-8b1a-35e46e5c32bd', secondary_objectives_subordinate_rhetoric, empirically_contingent).
narrative_ontology:cs_reference_frame('c0af8578-48be-487a-8b1a-35e46e5c32bd', price_stability_hierarchy_immutable).
narrative_ontology:cs_drift_state('c0af8578-48be-487a-8b1a-35e46e5c32bd', contemporary_climate_and_employment_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c0af8578-48be-487a-8b1a-35e46e5c32bd', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, creditors_savers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, inflation_averse_constituencies).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_policy_mandate).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, employment_sustainability).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, fiscal_space_subordinate_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECONDARY MANDATES (SNARE) — Cannot exit the subordination regime. Article 127(1) makes secondary objectives 'without prejudice to' price stability, meaning the ECB legally cannot pursue them if any inflation deviation exists. Trapped in a hierarchy where price stability always wins. Maximum extraction: secondary mandates are acknowledged in the treaty but operationally voiceless. The engine derives high d (target of suppression) from victim status + trapped exit.
constraint_indexing:constraint_classification(ecb_mandate_article_127__orthodox_price_stability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: FISCAL AUTHORITY & SUBORDINATE MEMBER STATES (SNARE) — Constrained by ECB orthodoxy but cannot fully escape it. Member states that prioritize employment, climate, or redistributive fiscal policy face a monetary ceiling: the ECB's exclusive focus on 2% inflation limits room for fiscal support, constrains credit availability during climate transitions, and blocks demand-side employment support. High suppression from the mandate's legal hierarchy. Significant extraction: member states must align fiscal policy with orthodox monetary constraints or face market discipline. Organized coalitions (southern eurozone states) can exit partially through fiscal sovereignty, but ECB monetary dominance limits that exit to constrained rather than mobile.
constraint_indexing:constraint_classification(ecb_mandate_article_127__orthodox_price_stability, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CREDITORS & SAVERS (ROPE) — Beneficiaries experiencing the mandate as coordination. The orthodox reading stabilizes the real value of savings, keeps interest rates from collapsing below risk-free rates, and creates institutional certainty around anti-inflation credibility. No extraction; the constraint protects their wealth. Arbitrage exit (can move capital across eurozone boundaries) but no incentive to do so — the mandate defends their interests. The engine derives low d (beneficiary + arbitrage) → negative f(d) → negative chi.
constraint_indexing:constraint_classification(ecb_mandate_article_127__orthodox_price_stability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GREEN DEAL COALITION & CLIMATE MANDATES (TANGLED ROPE) — Organized actors (France, Germany, European Commission climate directorate) see the mandate as both coordination AND extraction. Coordination: the ECB's inflation discipline anchors expectations, enabling lower long-term borrowing costs. Extraction: the exclusive price-stability focus constrains green finance, blocks ECB support for climate-transition lending, and externalizes climate risk to the fiscal budget. These actors have constrained exit: they cannot formally override the ECB, but they can build alternative financing structures (European Investment Bank, national green banks). Medium-high extraction. The engine derives moderate-high d from mixed beneficiary/victim status + constrained exit.
constraint_indexing:constraint_classification(ecb_mandate_article_127__orthodox_price_stability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: TREATY REFORM & MANDATE EXPANSION ADVOCATES (SCAFFOLD) — Organized political coalitions (left-wing parties, climate movements, employment advocates) see the mandate as a temporary coordination problem with a sunset: Article 127 could be amended to operationalize secondary objectives or reweight the hierarchy. Low effective extraction (χ) because this group has agency and perceives a path to exit. The sunset clause is political/constitutional rather than institutional — it depends on triggering Article 48 (treaty amendment). Barriers are substantial but not permanent.
constraint_indexing:constraint_classification(ecb_mandate_article_127__orthodox_price_stability, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ECB INSTITUTIONAL IDENTITY (PITON) — The ECB has performatively internalized the orthodox reading as its institutional identity. 'Price stability' has become the ECB's self-concept, defended ritualized in speeches, monetary policy frameworks, and staff recruitment. Yet the theater ratio indicates the pure price-stability constraint is increasingly theatrical: the ECB conducts QE (which is not purely price-stability oriented), uses forward guidance (which shapes expectations beyond price levels), and operationally considers financial stability (which violates the strict secondary subordination). The institutional inertia — 'we are the price-stability central bank' — persists despite the functional degradation of the pure constraint. Piton derives from theater gate: high ritualism (0.55 theater ratio captures this), low functional constraint.
constraint_indexing:constraint_classification(ecb_mandate_article_127__orthodox_price_stability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL / ORTHODOX NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, price stability is presented as an immutable principle: hyperinflation destroys economies, savers cannot plan, markets collapse, central banks must prioritize nominal anchors. This perspective sees Article 127's exclusive focus as natural law — the only scientifically defensible monetary constitution. However, the structural data (identified beneficiaries, externalized climate risks, suppression of secondary mandates) reveals this as a false summit: the 'naturalness' of orthodox monetary policy reflects post-WWII institutional memory and creditor-class interests, not a timeless economic law. The engine will detect this as FSM (false summit mountain) because beneficiaries are declared.
constraint_indexing:constraint_classification(ecb_mandate_article_127__orthodox_price_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ecb_mandate_article_127__orthodox_price_stability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ecb_mandate_article_127__orthodox_price_stability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, TR),
    TR >= 0.70.

:- end_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The orthodox reading benefits creditors through inflation discipline and real-value preservation, while imposing costs on climate policy, employment support, and fiscal-space-constrained member states. The extraction is not maximal because some coordination value exists (inflation anchoring enables long-term planning) and because alternative monetary regimes could provide similar anchoring without suppressing secondary objectives. The measurement trajectory shows rising extractiveness from 0.42 (1999, post-launch) to 0.62 (2014, post-financial-crisis, maximum subordination of employment to austerity) and stabilizing at 0.58 (2019-2024, after some Green Deal political pressure). Suppression (0.68): High. The mandate legally prevents operationalization of secondary objectives ('without prejudice to' price stability means secondary objectives have no independent operational force). Suppression mechanisms include: legal hierarchy (primary veto), institutional culture (the ECB's self-concept as price-stability guardian), market discipline (capital flight if secondary objectives constrain anti-inflation commitment), and credibility dynamics (any deviation signals weakness, triggering rate expectations). Theater ratio (0.55): Moderate. The constraint has functional content (price-stability targeting does anchor expectations and reduce inflation volatility) but increasingly performative elements. Post-2008 QE expanded the ECB's functional scope beyond 2% targeting (financial stability, credit conditions) while maintaining rhetorical commitment to price-stability exclusivity. The ECB's verbal framing of QE as 'supporting price stability' (when it was functionally employed/output-stabilizing) represents theater — a gap between stated mandate and actual mechanism. Theater increased post-2019 as green finance pressures mounted and the ECB began discussing climate risks without formally amending the mandate.
 *
 * PERSPECTIVAL GAP:
 *   The orthodox reading produces a stark perspectival gap. The beneficiary class (creditors, savers) experiences Rope — coordination that protects their interests. The organized climate coalition experiences Tangled Rope — they benefit from inflation discipline but suffer extraction from mandate exclusivity. The fiscal authorities experience Snare — they are constrained by the monetary ceiling and cannot operationally pursue secondary EU objectives. The climate and employment mandates themselves experience pure Snare — legally subordinate, operationally voiceless, bearing the costs of externalized climate and employment risks. The ECB as an institution experiences Piton — it has ritualized price-stability identity while functionally managing wider concerns (financial stability, credit conditions) that violate the pure mandate. The analytical observer risks seeing Mountain (price stability as immutable law) but the engine detects false summit (beneficiaries are identified, showing the 'naturalness' is institutional choice). The perspectival gap reveals that the orthodoxy is NOT a neutral technical constraint but a political choice that concentrates benefits on creditors while externalizing costs to climate, employment, and fiscal authorities.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural position: their power level, exit options, and relationship to the extraction flow. Creditors (institutional/arbitrage) have low d (~0.10, beneficiary + arbitrage) → f(d) ≈ -0.05 → negative chi (protection, not extraction). Climate mandates (powerless/trapped) have high d (~0.95) → f(d) ≈ 1.42 → high chi (maximum extraction experience). Fiscal authorities (moderate/constrained) have moderate-high d (~0.75) → f(d) ≈ 1.15 → high chi (constrained targets of suppression). Climate coalition (organized/constrained) has moderate d (~0.65) → f(d) ≈ 1.00 → moderate chi (organized but constrained). Treaty reform movement (organized/mobile) has lower d (~0.45) → f(d) ≈ 0.40 → low chi (mobile agents perceive exit path). The engine automatically derives these values from beneficiary/victim declarations and exit options; the commentary confirms the logic is sound given the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the orthodox reading is one of three competing interpretations of the same Article 127 kernel, not THE correct reading. The mandatrophy question is: 'Is exclusive price-stability focus a coherent monetary constitution, or does it degrade when confronted with climate and employment realities?' The answer is both: the orthodox reading is internally coherent as a creditor-protection regime but incoherent as a complete monetary constitution because (1) it externalizes real costs (climate risk, employment slack) to other policy domains without operational means to address them, and (2) the ECB itself violates it in practice (QE, forward guidance, financial-stability considerations). The snare classification for climate and employment mandates indicates that subordination is not a neutral technical choice but an extractive institutional design that concentrates benefits on one class (creditors) while imposing uncompensated costs on others (climate policy, employment policy, fiscal authorities). Resolution requires either: (1) genuine operationalization of secondary objectives (expansive reading), (2) reweighting the hierarchy to include climate stability (climate-incorporation reading), or (3) acceptance that the ECB's actual practice (which does pursue secondary objectives informally through QE) should be formalized rather than denied. The engine's false-summit detection will flag that the 'naturalness' of the orthodox reading is not given but constructed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secondary_objectives_latent_functionality,
    'Do the subordinated secondary objectives (employment, sustainable development) have latent operational force that the narrow reading suppresses, or are they genuinely non-functional as written?',
    'Comparative analysis: ECB behavior under the narrow orthodox reading vs. hypothetical behavior under operationalized secondary objectives. Historical counterfactual reconstruction of ECB decisions under climate and employment constraints. Simulation of alternative mandate texts.',
    'If secondary objectives have real operational force: the extractiveness estimate (0.58) underestimates harm from subordination, and the constraint is more severe than snare classification suggests (approaches Snare → Piton boundary). If genuinely non-functional: classification confirmed, but raises question of why they appear in the treaty at all (regulatory theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_objectives_latent_functionality, empirical, 'Whether subordinated secondary objectives have latent operational functionality').

omega_variable(
    climate_externalization_mechanism,
    'Is climate risk externalization to the fiscal budget a design feature of the orthodox mandate or an unintended consequence of post-2008 monetary expansion?',
    'Historical analysis of Article 127 drafting (1992), pre-financial-crisis ECB doctrine, and post-2008 practice divergence. Examination of ECB legal memoranda defending QE under orthodox interpretation vs. functional green finance support.',
    'If design feature: beneficiary class (creditors, savers) intentionally externalizes climate costs; constraint is more extractive than the 0.58 estimate. If unintended consequence: the orthodox reading has degraded in practice, and Piton classification may be more apt than Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_externalization_mechanism, conceptual, 'Whether climate externalization is designed or consequential').

omega_variable(
    price_stability_definition_instability,
    'Is the 2% inflation target itself stable, or does its operationalization (headline vs core inflation, asset prices, labor market tightness) constitute a shifting definition that undermines the orthodoxy''s own naturalness claim?',
    'Long-term analysis of ECB inflation measurement changes, asset price considerations in monetary policy, financial stability concerns intruding into price-stability calculus. Documentation of how the definition has expanded in practice.',
    'If definition is unstable: the mountain classification is weakened (the constraint is not immutable if its core term is shifting). If definition is stable: the mountain claim is more defensible, but requires explaining why secondary objectives cannot be similarly operationalized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(price_stability_definition_instability, empirical, 'Stability of 2% inflation definition and measurement').

omega_variable(
    reading_kernel_ambiguity,
    'Which reading of Article 127 is the authentic kernel reading, and which are interpretive glosses? Is the orthodox reading the intended original meaning, or a post-Maastricht institutional consolidation?',
    'Textual and archival analysis: ECB founding documents, Maastricht Treaty travaux préparatoires, central banker statements 1999-2008 vs. 2008-present. Comparative analysis with other central bank mandates (Federal Reserve, Bank of England post-2021 amendments).',
    'If orthodox is original intent: the reading is kerneled and other readings are reinterpretations (influences/coexists_with). If orthodox is post-hoc consolidation: the kernel is more ambiguous than the narrow reading claims, and sibling readings have equal standing (coexists_with stronger claim).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Authenticity and originality of orthodox reading vs. institutional consolidation').

omega_variable(
    false_summit_natural_law_claim,
    'Is price-stability exclusivity a natural economic law (immutable across institutional contexts), or a contingent institutional choice that benefits identifiable agents?',
    'Cross-cultural monetary history: non-euro central banks with operationalized secondary objectives (Fed dual mandate, Bank of England secondary financial stability mandate post-crisis). Analysis of economies that have pursued employment-first and climate-first monetary policies without hyperinflation or currency collapse. Documentation of the distributional consequences of orthodox subordination.',
    'If natural law: mountain classification holds, FSM does not fire, beneficiaries are incidental. If contingent choice: FSM fires, constraint reclassifies as tangled_rope or snare depending on extracted value, natural-law framing is revealed as institutional self-protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, empirical, 'False summit detection: whether price-stability exclusivity is natural law or institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_ortho_theater_1999, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ecb_ortho_theater_2009, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 10, 0.45).
narrative_ontology:measurement(ecb_ortho_theater_2019, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(ecb_ortho_extraction_1999, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ecb_ortho_extraction_2004, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ecb_ortho_extraction_2009, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ecb_ortho_extraction_2014, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(ecb_ortho_extraction_2019, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ecb_ortho_extraction_2024, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ecb_ortho_suppression_1999, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(ecb_ortho_suppression_2011_crisis, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(ecb_ortho_suppression_2024, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 25, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, eurosystem_secondary_market_purchases).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, austerity_fiscal_dominance_eurozone).

% DUAL FORMULATION NOTE:
% Article 127 kernel admits three structurally distinct readings with different extractiveness values and beneficiary/victim sets. The orthodox reading (ε=0.58, Snare) subordinates secondary objectives absolutely. The expansive reading (ε ≤ 0.30, Rope or Tangled Rope) operationalizes them as co-equal. The climate-incorporation reading (ε=0.45-0.55, Tangled Rope) reweights the hierarchy. These three stories are linked via network.affects_constraints; each represents a different institutional choice that would produce different constraint downstream (QE justification, climate finance availability, employment support optionality). The orthodox reading dominates current ECB practice, making the sibling readings counterfactual. All three are valid interpretations of the same kernel text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
