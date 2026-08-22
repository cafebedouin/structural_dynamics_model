% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine ('Goes Too Far' Compensation Requirement)
 *   domain: constitutional law/property rights/regulatory theory
 *
 * SUMMARY:
 *   Since Pennsylvania Coal v. Mahon (1922), American constitutional law has
 *   held that regulation which goes 'too far' in destroying the economic
 *   value of property is itself a taking for which the government must pay.
 *   The arrangement matured slowly: nearly dormant through the New Deal
 *   deference era, rebuilt in 1978 when Penn Central installed a three-factor
 *   ad hoc balancing test (economic impact, investment-backed expectations,
 *   character of government action), extended by categorical rules in the
 *   1990s, and made easier to bring when Knick removed the state-litigation
 *   gate in 2019. Structurally the arrangement does two things at once
 *   through the same machinery: it closes the evasion route by which
 *   government could accomplish confiscation through regulation without
 *   triggering the compensation guarantee, and it channels money, leverage,
 *   and billable complexity toward the actors best positioned to exploit
 *   doctrinal unpredictability. This file instantiates ONE reading of the
 *   takings_clause_boundary kernel — the regulatory_takings_reading — as a
 *   clean, epsilon-invariant constraint; the physical-appropriation and
 *   categorical readings are separate stories linked through the network. The
 *   epsilon referent is the standing compensation-for-severe-diminution
 *   arrangement as this reading frames it, assessed by this reading's own
 *   lights — never the rival readings' arrangements and never this reading's
 *   endorsed ideal.
 *
 * KEY AGENTS:
 *   - - united_states_supreme_court: Agenda setter (institutional/constrained) — articulates and revises the compensation boundary; bound by precedent and constitutional text
 *   - - affected_property_owners: Nominal beneficiary with dual payer position (organized/constrained) — may claim compensation; most absorb losses instead
 *   - - large_land_developers: Concentrated beneficiary (powerful/arbitrage) — captures the largest recoveries and converts liability exposure into negotiating leverage
 *   - - real_estate_litigation_bar: Secondary beneficiary (organized/arbitrage) — collects fees from doctrinal unpredictability
 *   - - general_taxpayers: Diffuse payer (powerless/constrained) — funds judgments and settlements invisibly
 *   - - state_and_local_regulators: Institutional payer (institutional/constrained) — pays in narrowed rules, litigation reserves, and defensive staff time
 *   - - regulatory_beneficiary_communities: Payer and excluded voice (powerless/trapped) — absorb diluted protections with no party status
 *   - - property_law_academics: Analytical observer (moderate/analytical) — maps the doctrine's indeterminacy from outside adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.62).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.55).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine ('Goes Too Far' Compensation Requirement)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional law/property rights/regulatory theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, 'cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0').
narrative_ontology:cs_kernel_codification('cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0', fixed_text).
narrative_ontology:cs_authority_grounding('cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0', lineage).
narrative_ontology:cs_interpretation_layer_present('cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0').
narrative_ontology:cs_reading_relation('cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0', takings_clause_boundary__physical_appropriation_reading, forecloses).
narrative_ontology:cs_reading_relation('cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0', takings_clause_boundary__categorical_takings_reading, influences).
narrative_ontology:cs_axiom('cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0', foundational, severe_diminution_constitutes_taking).
narrative_ontology:cs_axiom_status(severe_diminution_constitutes_taking, holdable).
narrative_ontology:cs_axiom_grounding('cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0', severe_diminution_constitutes_taking, deontological).
narrative_ontology:cs_axiom('cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0', secondary, compensation_requirement_deters_regulatory_confiscation).
narrative_ontology:cs_axiom_status(compensation_requirement_deters_regulatory_confiscation, holdable).
narrative_ontology:cs_axiom_grounding('cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0', compensation_requirement_deters_regulatory_confiscation, instrumental).
narrative_ontology:cs_reference_frame('cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0', full_equivalence_of_regulatory_and_physical_expropriation).
narrative_ontology:cs_drift_state('cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0', contemporary_post_knick_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cbfd97dd-7b11-4f8a-8b1d-ed9089e699c0', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, large_land_developers).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, affected_property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, real_estate_litigation_bar).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, general_taxpayers).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, state_and_local_regulators).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, regulatory_beneficiary_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, affected_property_owners).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, police_power_respects_property_boundaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulates and periodically revises the doctrine that determines when regulation becomes compensable: announced the 'goes too far' principle in 1922, installed the three-factor balancing test in 1978, added categorical rules for total deprivation in 1992, and removed the state-litigation gatekeeping requirement in 2019. Its members divide over how far the compensation guarantee extends; the institution as a whole is bound by precedent and constitutional text and cannot step outside its interpretive role.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, united_states_supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Own homes, farms, or parcels whose permitted uses are curtailed by zoning, environmental, or heritage rules. They may petition for compensation when restrictions erase most of their land's value; few do, because litigation takes years, costs more than most parcels are worth, and succeeds unpredictably. Winners are paid from public treasuries; losers absorb the diminution plus their legal bills. Selling or relocating remains possible but means realizing the already-diminished value.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, affected_property_owners, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__regulatory_takings_reading, affected_property_owners, payer).

% Hold large portfolios and project pipelines across many jurisdictions. They can choose where to seek entitlements, sustain multi-year litigation, and treat compensation exposure as a bargaining chip in negotiations with municipalities. They capture the largest individual recoveries and settlements, and the mere possibility of a claim shifts regulatory negotiations in their favor before any suit is filed.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, large_land_developers, beneficiary,
    powerful, biographical, arbitrage, national).

% Specializes in land-use and constitutional property litigation. The case-by-case balancing test makes every dispute novel, which sustains demand for specialized counsel; repeat players accumulate expertise that compounds their advantage over one-shot claimants. Fees flow to the bar regardless of which side prevails.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, real_estate_litigation_bar, beneficiary,
    organized, biographical, arbitrage, national).

% Fund compensation judgments and settlements through state and municipal budgets. The per-household cost is small and invisible, spread across millions of people who have no role in any individual case and no practical way to decline the obligation short of moving to another jurisdiction.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, general_taxpayers, payer,
    powerless, generational, constrained, national).

% Draft and administer zoning codes, environmental buffers, coastal rules, and preservation ordinances. Every restrictive rule carries a potential future liability, so agencies either narrow protections, maintain litigation reserves, or spend staff-years defending rules in court. Their capacity to regulate is the resource drawn down when liability risk prices into every proposal.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, state_and_local_regulators, payer,
    institutional, generational, constrained, national).

% Residents and neighbors who rely on land-use and environmental rules for open space, wetland protection, flood buffering, and scenic or historic character. When rules are weakened, delayed, or withdrawn under liability pressure, they absorb the resulting losses directly and cannot relocate away from the degraded baseline cheaply. They appear in takings cases, if at all, as amici — never as parties with standing to defend the rule's benefits.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, regulatory_beneficiary_communities, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__regulatory_takings_reading, regulatory_beneficiary_communities, excluded).

% Study and debate the doctrine from law schools: measuring its unpredictability, proposing replacements ranging from bright-line thresholds to outright abolition, and documenting its history. They hold no material stake in any outcome, and their influence runs through argument rather than adjudication.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_law_academics, observer,
    moderate, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__regulatory_takings_reading, large_land_developers).
narrative_ontology:fixing_cost_class(takings_clause_boundary__regulatory_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends the constitutional compensation guarantee to non-physical government action: it closes the route by which a government could accomplish through regulation what it could not do by seizure without paying, and forces the costs of severe regulatory burdens to be internalized somewhere rather than silently shifted onto the regulated owner.
% TRANSFER_FUNCTION: Moves money (compensation awards and settlement payments) from public treasuries to property owners who prove severe value diminution; moves regulatory discretion from elected and administrative bodies toward courts; moves litigation spending toward specialized counsel; and moves regulatory ambition itself — proposals priced as too risky are narrowed or dropped before enactment.
% ABSENT_VOICES: Regulatory beneficiaries are absent from the table: residents protected by zoning, environmental constituencies, and preservation communities bear diluted protections but hold no party status in takings litigation, appearing only as amici. Future generations who inherit weaker regulatory baselines are absent entirely. Their absence is part of why the arrangement's unanimity — property protection framed as costless to everyone except 'the government' — survives.
% DISAPPEARANCE_RATIONALE: If the compensation requirement vanished overnight, governments could restrict land use to the full extent of the police power without payment; land values in regulable areas would adjust downward immediately; development patterns, municipal budgets, and the land-use litigation economy would all reorganize; and the compensation-claim industry built on the doctrine would collapse within a cycle of litigation.
% FOUNDING_PROBLEM: Government was using police-power regulation to accomplish what the Constitution forbids through eminent domain: destroying property value without paying for it. The doctrine was built to close that evasion route — to make the compensation guarantee bind however the government chooses to take.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: state and local governments' own revealed behavior (narrowing or abandoning rules rather than defending them concedes the severity mechanism operates); legislative findings accompanying state-level compensation statutes; and a cross-ideological academic record — including scholars hostile to the doctrine — documenting regulation destroying most of a parcel's value as a recurring real phenomenon. No corroborator attests that the doctrine's current design is the right remedy; several attest the underlying problem is real.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.62 at interval end: the arrangement moves public money to private claimants, generates uncertainty rents that accrue to repeat players, and prices liability risk into every regulatory proposal — net of the genuine protection a small minority of owners receive. Suppression is 0.55: the arrangement does not coerce persons directly, but it suppresses regulatory alternatives by attaching liability to them, and the suppression series tracks the deliberate build-out of enforcement machinery (Penn Central's test, the categorical layer, Knick's removal of the exhaustion gate) rather than any change in extraction alone. Theater is 0.50: roughly half the arrangement's activity is performative — the rhetoric of protecting the family home against overbearing government, deployed in opinions and advocacy, while the actual distribution of recoveries concentrates among sophisticated claimants. Accessibility collapse is 0.40: workable alternatives persist (legislative compensation schemes, narrower drafting, insurance instruments, doctrinal replacement proposals), so understanding the arrangement does not foreclose exits the way a natural limit would. Resistance is 0.60: the doctrine meets continuous, organized resistance — a century of scholarly criticism of its indeterminacy, repeated legislative attempts to cap or channel it, and persistent state-level experimentation. All three tracked metric series run on one shared nine-point grid spanning 1922–2022, so every metric is authored at every examined time point. The claimed type and the metrics are independent authored facts: tangled_rope is what the structure is (a genuine coordination function fused with asymmetric extraction under active enforcement); the metrics describe how it operates.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the developer and litigation-bar seats the arrangement presents as a protective structure they operate fluently: predictable enough to price, lucrative enough to sustain, and a lever in every negotiation with a municipality. From the taxpayer and regulator seats the same structure presents as extraction: invisible fiscal obligations and a regulatory capacity drawn down by liability pricing. From the Court's seat it presents as constitutional fidelity — the careful maintenance of a balance the text demands. The regulatory_beneficiary_communities seat sees a fourth thing: protections discussed in the abstract but defended by no one with standing. The engine computes per-seat classifications from the structural data; the divergence between the beneficiary seats and the payer seats is the expected signature of a hybrid structure, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map cleanly onto the directionality derivation. Three beneficiary groups (large_land_developers, affected_property_owners, real_estate_litigation_bar) derive low d — the arrangement subsidizes them. Three victim groups (general_taxpayers, state_and_local_regulators, regulatory_beneficiary_communities) derive high d — the arrangement extracts from them, amplified for the trapped community seat and the powerless diffuse taxpayer seat. Affected property owners carry a secondary payer role because their benefit is probabilistic and self-funded: most never recover anything, litigation costs fall on them first, and years of delay discount any award. That dual position is expressed structurally through the secondary_role declaration rather than through a directionality override. No overrides are authored: the beneficiary/victim declarations plus the exit atoms already differentiate every seat, and the one candidate for correction (the property owners' mid-range position) is captured by the dual-role declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — regulation as an evasion route around the compensation guarantee — remains live: governments still impose severe burdens, and the severity mechanism the doctrine responds to is documented across jurisdictions by sources outside the benefiting parties. Accordingly no mandatrophy resolution is declared, and the R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie flag. The tangled_rope claim prevents two opposite mislabelings. Reading the arrangement as pure coordination (rope) would hide the concentrated recoveries, the uncertainty rents, and the chill — the extraction half that the temporal series shows accumulating since the balancing test was installed. Reading it as pure extraction (snare) would predict repeal pressure that a century of entrenchment contradicts, and would erase the genuine protection that reaches a small set of owners whom nothing else would compensate. The hybrid classification holds both facts: the coordination function is real, and the same machinery transfers asymmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading of the takings_clause_boundary kernel — what would the sibling readings change structurally?',
    'Comparative classification across the three reading files: the physical_appropriation_reading shrinks the victim set to physically dispossessed owners and deletes the balancing machinery entirely; the categorical_takings_reading adds per se rules that redistribute recoveries toward total-deprivation and occupation cases.',
    'Under the physical-appropriation sibling, epsilon falls sharply because the compensation-for-diminution transfer channel does not exist; under the categorical sibling, extraction concentrates inside the per se categories and the ad hoc uncertainty rents shrink correspondingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Kernel membership and cross-reading structural deltas for the takings boundary family.').

omega_variable(
    too_far_threshold_indeterminacy,
    'Is there any stable metric for ''goes too far,'' or is the severe-diminution threshold irreducibly case-by-case?',
    'Outcome-clustering analysis of decided cases: if outcomes stabilize around identifiable value-diminution bands, a de facto threshold exists; if outcomes track litigant resources and forum selection instead, the threshold is irreducibly ad hoc.',
    'A discoverable threshold would shrink the uncertainty-rent component of extraction and support a rope-leaning recomputation; irreducible ad hoc-ness sustains the hybrid structure with elevated extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(too_far_threshold_indeterminacy, conceptual, 'Indeterminacy of the ''too far'' threshold as the doctrine''s central operating parameter.').

omega_variable(
    regulatory_chill_magnitude,
    'How much protective regulation is actually deterred by compensation liability, net of regulations enacted anyway?',
    'Difference-in-differences across jurisdictions before and after high-profile liability events (First English aftermath, state compensation statutes), plus regulator surveys on self-reported chilling.',
    'High measured chill raises the suppression borne by regulatory_beneficiary_communities and pushes effective extraction upward; negligible chill supports a materially lower-suppression profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_magnitude, empirical, 'Magnitude of the doctrine''s deterrent effect on protective regulation.').

omega_variable(
    recovery_concentration,
    'Do compensation recoveries and settlements concentrate among large, repeat-litigant claimants?',
    'Claims-payment data from state compensation programs and judgment records, tabulated by claimant size and litigation history.',
    'Strong concentration confirms the asymmetric-extraction half of the hybrid structure and validates gain_flow naming large_land_developers; diffuse recovery across small owners would weaken the extraction reading substantially.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recovery_concentration, empirical, 'Distribution of recoveries across claimant classes.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel best framed as the fixed Clause text interpreted by courts (fixed_text under lineage authority), or as the accumulated compensation practice itself (implicit under practice authority)?',
    'Test both framings: under the practice framing the kernel is whatever compensation practice does, and the drift vector re-reads as codification_collapse rather than practice_drift. Signals guiding the chosen framing: the Clause''s canonical text and the Court''s own self-presentation as interpreting that text.',
    'The alternative framing changes the commitment-system classification and re-reads the drift direction; the chosen framing follows the Court''s textualist self-understanding, but the practice framing is coherent and would reclassify the interpretive layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Framing under-determination of the kernel''s codification and authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(taki_tr_t0, observed).
narrative_ontology:measurement(taki_tr_t13, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 13, 0.3).
narrative_ontology:measurement_basis(taki_tr_t13, observed).
narrative_ontology:measurement(taki_tr_t25, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(taki_tr_t25, observed).
narrative_ontology:measurement(taki_tr_t38, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 38, 0.33).
narrative_ontology:measurement_basis(taki_tr_t38, observed).
narrative_ontology:measurement(taki_tr_t50, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(taki_tr_t50, observed).
narrative_ontology:measurement(taki_tr_t63, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 63, 0.43).
narrative_ontology:measurement_basis(taki_tr_t63, observed).
narrative_ontology:measurement(taki_tr_t75, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 75, 0.46).
narrative_ontology:measurement_basis(taki_tr_t75, observed).
narrative_ontology:measurement(taki_tr_t88, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 88, 0.48).
narrative_ontology:measurement_basis(taki_tr_t88, observed).
narrative_ontology:measurement(taki_tr_t100, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 100, 0.5).
narrative_ontology:measurement_basis(taki_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(taki_be_t0, observed).
narrative_ontology:measurement(taki_be_t13, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 13, 0.3).
narrative_ontology:measurement_basis(taki_be_t13, observed).
narrative_ontology:measurement(taki_be_t25, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement_basis(taki_be_t25, observed).
narrative_ontology:measurement(taki_be_t38, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 38, 0.36).
narrative_ontology:measurement_basis(taki_be_t38, observed).
narrative_ontology:measurement(taki_be_t50, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement_basis(taki_be_t50, observed).
narrative_ontology:measurement(taki_be_t63, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 63, 0.52).
narrative_ontology:measurement_basis(taki_be_t63, observed).
narrative_ontology:measurement(taki_be_t75, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 75, 0.56).
narrative_ontology:measurement_basis(taki_be_t75, observed).
narrative_ontology:measurement(taki_be_t88, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 88, 0.59).
narrative_ontology:measurement_basis(taki_be_t88, observed).
narrative_ontology:measurement(taki_be_t100, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement_basis(taki_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(taki_su_t0, observed).
narrative_ontology:measurement(taki_su_t13, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 13, 0.27).
narrative_ontology:measurement_basis(taki_su_t13, observed).
narrative_ontology:measurement(taki_su_t25, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 25, 0.29).
narrative_ontology:measurement_basis(taki_su_t25, observed).
narrative_ontology:measurement(taki_su_t38, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 38, 0.33).
narrative_ontology:measurement_basis(taki_su_t38, observed).
narrative_ontology:measurement(taki_su_t50, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(taki_su_t50, observed).
narrative_ontology:measurement(taki_su_t63, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 63, 0.5).
narrative_ontology:measurement_basis(taki_su_t63, observed).
narrative_ontology:measurement(taki_su_t75, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 75, 0.53).
narrative_ontology:measurement_basis(taki_su_t75, observed).
narrative_ontology:measurement(taki_su_t88, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 88, 0.51).
narrative_ontology:measurement_basis(taki_su_t88, observed).
narrative_ontology:measurement(taki_su_t100, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement_basis(taki_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__categorical_takings_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the Takings Clause' conflates three structurally distinct claims with different victim sets, different failure modes, and different epsilon values. physical_appropriation_reading is the upstream member (oldest, narrowest, lowest extraction); this regulatory reading sits mid-family (introduces the diminution channel and the balancing machinery); categorical_takings_reading is downstream (incorporates the balancing machinery and layers per se discipline on top, redistributing recoveries). Each story carries its own epsilon, beneficiaries, and victims; the family is linked through affects_constraints edges in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
