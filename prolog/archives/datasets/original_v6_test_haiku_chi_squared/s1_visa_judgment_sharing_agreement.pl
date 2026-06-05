% ============================================================================
% CONSTRAINT STORY: s1_visa_judgment_sharing_agreement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_s1_visa_judgment_sharing_agreement, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: s1_visa_judgment_sharing_agreement
 *   human_readable: Visa Judgment Sharing Agreement (AMEX Antitrust Case)
 *   domain: legal/economic/antitrust
 *
 * SUMMARY:
 *   The Visa Judgment Sharing Agreement represents a contractual mechanism by
 *   which Visa USA and Visa International require American Express to fund
 *   their antitrust defense in exchange for settlement of AMEX's own
 *   antitrust claims. Originating in the context of AMEX's challenge to
 *   Visa's network restrictions, the agreement creates an extraction
 *   mechanism disguised as coordination: AMEX, nominally settling its claims,
 *   instead becomes obligated to fund the defense of the very restrictions it
 *   sought to challenge. This constraint exhibits the core tension between
 *   legitimate industry coordination (unified legal defense against systemic
 *   antitrust exposure) and asymmetric extraction (one party bearing costs to
 *   defend restraints that harm its own competitive position). The theater
 *   ratio (0.55) reflects moderate performative content: the settlement
 *   language frames the judgment obligation as a coordination mechanism, but
 *   the actual mechanism transfers legal cost burden rather than achieving
 *   substantive agreement on conduct. The extractiveness (0.52) reflects that
 *   Visa captures direct benefits (defense funding, precedent protection)
 *   while AMEX bears direct costs (judgment obligation, suppressed
 *   counterclaims). Suppression (0.68) is high because the agreement
 *   explicitly forecloses AMEX's independent antitrust arguments and forces
 *   AMEX to finance arguments against AMEX's own interests. The temporal
 *   progression (ε rising from 0.38 to 0.52) reflects how the constraint has
 *   become more extractive over time as Visa has leveraged the judgment
 *   obligation to defend increasingly aggressive network practices, while
 *   AMEX's ability to exit has diminished through legal precedent.
 *
 * KEY AGENTS:
 *   - Visa USA: Primary beneficiary (institutional/arbitrage) — receives defense funding and precedent protection; has exit options through independent settlement
 *   - Visa International: Secondary beneficiary (institutional/arbitrage) — benefits from global precedent while US judgment obligation shields global operations
 *   - American Express: Primary victim (powerless/trapped) — forced to fund antitrust defense of competitors' interests with no legitimate exit pathway
 *   - Competing Payment Networks: Secondary victims (moderate/constrained) — cannot develop independent antitrust positions due to resource constraints while dependent on AMEX funding
 *   - Merchant Ecosystem: Indirect victim (powerless/trapped) — suppressed arguments that Visa's network restraints harm merchant economics and payment choice
 *   - Regulatory Reform Coalition: Organized observer (organized/mobile) — antitrust reformers and payment system regulators with capacity to modify underlying constraints
 *   - Federal Reserve / CFPB: Institutional regulators (institutional/arbitrage) — have authority to supersede settlement through direct regulation of network practices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(s1_visa_judgment_sharing_agreement, 0.52).
domain_priors:suppression_score(s1_visa_judgment_sharing_agreement, 0.68).
domain_priors:theater_ratio(s1_visa_judgment_sharing_agreement, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(s1_visa_judgment_sharing_agreement, extractiveness, 0.52).
narrative_ontology:constraint_metric(s1_visa_judgment_sharing_agreement, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(s1_visa_judgment_sharing_agreement, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(s1_visa_judgment_sharing_agreement, tangled_rope).
narrative_ontology:human_readable(s1_visa_judgment_sharing_agreement, "Visa Judgment Sharing Agreement (AMEX Antitrust Case)").
narrative_ontology:topic_domain(s1_visa_judgment_sharing_agreement, "legal/economic/antitrust").

domain_priors:requires_active_enforcement(s1_visa_judgment_sharing_agreement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(s1_visa_judgment_sharing_agreement, visa_usa).
narrative_ontology:constraint_beneficiary(s1_visa_judgment_sharing_agreement, visa_international).
narrative_ontology:constraint_victim(s1_visa_judgment_sharing_agreement, american_express).
narrative_ontology:constraint_victim(s1_visa_judgment_sharing_agreement, competing_networks).
narrative_ontology:constraint_victim(s1_visa_judgment_sharing_agreement, merchant_ecosystem).
narrative_ontology:constraint_victim(s1_visa_judgment_sharing_agreement, consumer_payment_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AMERICAN EXPRESS (SNARE) — Trapped by judgment obligation with no legitimate exit. AMEX must fund Visa's legal defense for claims that directly harm AMEX's competitive interests. Cannot walk away without additional liability. Victim of asymmetric enforcement. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETING NETWORKS (SNARE) — Networks (Discover, smaller issuers) unable to develop independent antitrust defenses due to funding constraints AMEX must bear. Judgment sharing prevents collective action. Constrained exit: can develop arguments but resource constraints mean dependence on AMEX funding. d≈0.85, f(d)≈1.20, σ=1.0 → χ≈0.62.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MERCHANT ECOSYSTEM (SNARE) — Merchants and acquiring banks cannot exit payment network extraction because Visa's antitrust defense is funded by AMEX's judgment obligation. Suppressed argument that Visa's restraints harm merchants. No direct legal standing; trapped in ecosystem effects. d≈0.88, f(d)≈1.30, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: VISA USA (TANGLED ROPE) — Genuine coordination function: unified defense against systemic antitrust claims benefits all card networks through precedent. Also extraction: AMEX bears costs that Visa would otherwise face independently. Arbitrage exit available (could settle independently). d≈0.15, f(d)≈0.02, σ=1.0 → χ≈0.03. Effective χ is low because institutional actor with exit options experiences this as net beneficial coordination, not extraction.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: VISA INTERNATIONAL (TANGLED ROPE) — Benefits from Visa USA's antitrust defense creating global precedent. Coordination function: shared legal precedent reduces per-entity defense costs. Extraction: AMEX judgment obligation subsidizes Visa International's reputational protection. Arbitrage exit available. d≈0.18, f(d)≈0.08, σ=1.1 → χ≈0.05.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGAL PRECEDENT INERTIA (PITON) — Settlement agreements and judgment sharing have become institutional theater: courts enforce payment obligations nominally to 'settle disputes' but the actual dispute resolution function has atrophied. The constraint persists through contractual language and precedent inertia despite structural dysfunction. Theater ratio 0.55 reflects moderate theatrical content — enforcement is partially functional (Visa does get defense funding) but also partly ritual (judgment language masks ongoing extraction). d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.36.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: REGULATORY REFORM COALITION (SCAFFOLD) — Antitrust reformers, merchant advocates, and consumer protection advocates see the judgment sharing agreement as a temporary institutional arrangement vulnerable to regulatory intervention. Sunset mechanism exists: Federal Reserve and CFPB authority to restrict payment network practices could eliminate the underlying antitrust disputes that justify the agreement. Coalition has organized capacity (mobile exit). d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.16. Low effective extraction because reform is visible and has structural pathways.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / PURE COORDINATION VIEW (ROPE) — From a civilizational economic perspective, the judgment sharing agreement solves a real coordination problem: payment networks face systemic antitrust exposure, and unified defense reduces duplicative legal costs. If extracted data (ε=0.52, suppression=0.68) is discounted, the underlying coordination mechanism is legitimate. However, high suppression and moderate extractiveness indicate this frame misses the asymmetric burden allocation. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.60. Engine's false summit detector will flag this as incomplete analysis.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(s1_visa_judgment_sharing_agreement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(s1_visa_judgment_sharing_agreement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(s1_visa_judgment_sharing_agreement, TR),
    TR >= 0.70.

:- end_tests(s1_visa_judgment_sharing_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The agreement transfers legal defense costs from Visa to AMEX, and these costs are substantial (major antitrust defense in a duopoly case). However, extractiveness is not at the snare threshold (0.66) because: (1) AMEX retained some negotiating power in settlement terms; (2) the defense burden, while real, is finite and not open-ended; (3) AMEX does gain some certainty through settlement (eliminates uncertainty of continued litigation). The value reflects that the extraction is real but structurally bounded. Suppression (0.68): High. The agreement explicitly forecloses AMEX's ability to pursue independent antitrust claims, constrains AMEX's ability to make public arguments about network restraints, and creates legal liability for AMEX if it violates settlement terms. Suppression mechanisms include: contractual silence clauses, judicial enforcement of settlement terms, and precedent effects that weaken independent defenses. However, suppression is not total (0.90+) because AMEX retains competitive options outside the payment network sphere, and regulatory intervention could partially unwind the constraints. Theater ratio (0.55): Moderate. The settlement language employs the rhetoric of coordination ('shared defense of industry interests') but the actual mechanism is cost allocation (judgment obligation). The theater has increased over time (from 0.42 to 0.55) as Visa has used the judgment obligation to defend increasingly aggressive practices, requiring AMEX to fund defenses that the original settlement contemplated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a stark perspectival gap between institutional actors and powerless victims. Visa sees pure coordination (Rope) or tangled rope with low extraction (institutional perspective with arbitrage exit). AMEX sees snare-level extraction (powerless/trapped perspective) despite institutional status. This gap reveals that institutional status alone does not determine extraction experience — the structural relationship (beneficiary vs victim) and exit options (arbitrage vs trapped) override raw power level. Visa's institutional power is insufficient to escape the fundamental asymmetry: Visa benefits from the agreement while AMEX bears costs. The analytical observer risks seeing pure coordination (Rope) if the coordination function is emphasized, but the structural data (suppression=0.68, theater=0.55) indicates the coordination frame obscures extraction. The regulatory reform coalition sees a temporary constraint with a sunset (Scaffold) — payment system regulation could eliminate the underlying disputes — but this is aspirational rather than structural. The constraint persists as extraction mechanism independent of whether the underlying antitrust claims are valid.
 *
 * DIRECTIONALITY LOGIC:
 *   Visa USA: Beneficiary + arbitrage exit → d≈0.15, f(d)≈0.02. Net beneficiary with high optionality. Visa International: Beneficiary + arbitrage exit → d≈0.18, f(d)≈0.08. Similar to Visa USA. American Express: Victim + trapped exit → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot exit without reincurring antitrust liability; judgment obligation is mandatory. Competing networks: Victim + constrained exit → d≈0.85, f(d)≈1.20. High extraction but not maximal; can pursue independent competitive strategies but resource constraints mean dependence on AMEX-funded arguments. Merchant ecosystem: Victim + trapped → d≈0.88, f(d)≈1.30. High extraction. No direct legal standing; trapped in ecosystem effects. Regulatory coalition: Organized + mobile → d≈0.35, f(d)≈0.30. Moderate extraction with organized escape routes. Federal Reserve/CFPB: Institutional + constrained → d≈0.50, f(d)≈0.65. Current legal regime constrains direct intervention but regulatory authority is growing.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint centers on the distinction between legitimate industry coordination and asymmetric extraction disguised as coordination. The claimed type is tangled_rope, which requires: (1) genuine coordination function; (2) asymmetric extraction; (3) active enforcement. The constraint satisfies all three gates but on different axes: Visa and AMEX have a genuine shared interest in settling antitrust exposure (coordination function), but the settlement mechanism allocates costs asymmetrically (extraction), and courts actively enforce the judgment obligation (enforcement). However, the structural data (suppression=0.68, ε=0.52) indicates the extraction component dominates over the coordination component. The pure-Rope frame ('shared industry defense') misses the asymmetry. The pure-Snare frame misses the genuine coordination component (both parties do benefit from avoiding continued litigation). The tangled_rope classification captures both: AMEX experiences snare-level χ (0.72) while Visa experiences rope-level χ (0.03), from the same base metrics, because of directionality derivation. The mandatrophy is resolved by showing that classification depends on structural position (beneficiary vs victim) not on the objective metrics alone. From Visa's perspective, the agreement is coordination. From AMEX's perspective, the agreement is extraction. Both perspectives are correct — the constraint IS a tangled rope precisely because it coordinates for some actors while extracting from others. The engine's perspectival framework dissolves the mandatrophy by making it a feature of the analysis, not a bug.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    defense_funding_proportionality,
    'Should judgment obligation for antitrust defense be proportional to each network''s exposure and market share, or should it follow contractual terms that may impose disproportionate burden?',
    'Analysis of counterfactual: independent defense costs for AMEX vs Visa under judgment obligation. Comparison with industry standard settlement language in peer cases.',
    'If proportional: agreement is legitimate tangled rope with coordinated cost-sharing. If disproportionate: agreement is extraction mechanism (snare from AMEX perspective elevates to primary definition).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(defense_funding_proportionality, conceptual, 'Whether judgment obligation imposes proportional burden on AMEX').

omega_variable(
    shared_antitrust_exposure_reality,
    'Do Visa USA and Visa International actually face joint antitrust exposure that justifies unified defense, or are their competitive positions and regulatory jurisdictions sufficiently distinct that unified defense extracts from weaker party?',
    'Empirical analysis of antitrust claims against each entity separately; legal risk modeling under independent vs joint defense strategy.',
    'If genuinely joint: agreement is pure coordination (Rope). If jurisdictionally separable: agreement is extraction mechanism dressed as coordination (Snare primary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shared_antitrust_exposure_reality, empirical, 'Whether unified defense reflects genuine or fabricated joint exposure').

omega_variable(
    regulatory_supersession_timeline,
    'Will Federal Reserve''s payment system regulation or CFPB''s authority over consumer payment practices render the underlying antitrust disputes moot within 5-10 years?',
    'Timeline analysis of regulatory authority expansion; correlation between regulatory interventions and settlement agreement utilization rates.',
    'If regulatory supersession occurs on 5-year timeline: scaffold sunset is real and agreement is temporary coordination layer. If regulatory action is blocked or delayed: constraint persists as extraction mechanism (snare primary classification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_supersession_timeline, empirical, 'Timeline for regulatory authority to supersede antitrust settlement').

omega_variable(
    alternative_defense_mechanisms,
    'Could payment networks achieve equivalent defense coordination through industry association legal advocacy or shared compliance infrastructure without judgment sharing obligation?',
    'Comparative case study: legal defense outcomes for coordinated networks with and without judgment obligations. International examples (EU networks without judgment sharing).',
    'If alternative mechanisms are functionally equivalent: judgment sharing is pure extraction (Snare). If judgment sharing provides significant efficiency gains: agreement is legitimate tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_defense_mechanisms, empirical, 'Whether alternative coordination mechanisms could replace judgment sharing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(s1_visa_judgment_sharing_agreement, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(visa_jsa_tr_t0, s1_visa_judgment_sharing_agreement, theater_ratio, 0, 0.42).
narrative_ontology:measurement(visa_jsa_tr_t7, s1_visa_judgment_sharing_agreement, theater_ratio, 7, 0.5).
narrative_ontology:measurement(visa_jsa_tr_t15, s1_visa_judgment_sharing_agreement, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(visa_jsa_be_t0, s1_visa_judgment_sharing_agreement, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(visa_jsa_be_t7, s1_visa_judgment_sharing_agreement, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(visa_jsa_be_t15, s1_visa_judgment_sharing_agreement, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(s1_visa_judgment_sharing_agreement, enforcement_mechanism).
narrative_ontology:affects_constraint(s1_visa_judgment_sharing_agreement, payment_network_exclusionary_practices).
narrative_ontology:affects_constraint(s1_visa_judgment_sharing_agreement, card_network_interchange_fee_regulation).
narrative_ontology:affects_constraint(s1_visa_judgment_sharing_agreement, amex_market_concentration).

% DUAL FORMULATION NOTE:
% The judgment sharing agreement is downstream of specific antitrust claims (Visa exclusionary practices, interchange fee extraction) but represents a distinct structural constraint. Upstream constraints (payment_network_exclusionary_practices) have their own ε values reflecting empirical evidence of anticompetitive conduct; the judgment sharing agreement has ε=0.52 reflecting the legal cost allocation mechanism that emerges from settlement, not the underlying antitrust violation itself. The agreement is a meta-constraint that influences how the upstream antitrust constraints are litigated and defended.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
