% ============================================================================
% CONSTRAINT STORY: australian_federation_1901__dismissal_1975_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_australian_federation_1901__dismissal_1975_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: australian_federation_1901__dismissal_1975_reading
 *   human_readable: The 1975 Dismissal: Senate Supply Block and Vice-Regal Override
 *   domain: legal/constitutional/Westminster-hybrid
 *
 * SUMMARY:
 *   The 1975 dismissal of the Whitlam government by Governor-General Sir John
 *   Kerr detonated the Australian federation's buried contradiction: the
 *   hybrid's design contains two incompatible sovereignty doctrines.
 *   Westminster confidence doctrine holds that an elected government with
 *   lower-house confidence cannot be dismissed. Federal deadlock doctrine
 *   holds that an upper house with equal-state representation can block
 *   supply and force an election. The 1975 crisis instantiated both
 *   simultaneously. The Coalition opposition blocked supply in the Senate
 *   (federal power), the Governor-General dismissed the government (reserve
 *   power), and the lower house's electoral mandate was terminated without
 *   its consent (extraction). This reading of the 1901 kernel frames the
 *   crisis as revealing not a legitimate constitutional mechanism but a
 *   structural flaw: the hybrid preserved vice-regal reserve power and
 *   upper-house supply control in direct tension with Westminster confidence.
 *   The constraint exhibits both coordination (federal deadlock resolution)
 *   and asymmetric extraction (bypassing lower-house sovereignty).
 *
 * KEY AGENTS:
 *   - Whitlam Government (Labor): Primary victim — held lower-house majority and confidence but was dismissed through supply blockade and vice-regal discretion
 *   - Coalition Opposition (Liberal-Country Party): Primary beneficiary (institutional/arbitrage) — forced an election through supply blockade; benefited from vice-regal exercise of reserve powers
 *   - Governor-General (John Kerr): Institutional actor (institutional/arbitrage) — exercised reserve powers to dismiss the government; claimed constitutional authority to resolve deadlock
 *   - Senate (upper house): Institutional actor (institutional/arbitrage) — blocked supply; created the deadlock that triggered dismissal
 *   - Lower House (House of Representatives): Primary victim (powerful/mobile) — retained formal position but had its electoral mandate terminated without consent
 *   - Confidence Convention: Victim (analytical) — suppressed by the crisis; no longer protected lower-house sovereignty from upper-house and vice-regal override
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(australian_federation_1901__dismissal_1975_reading, 0.58).
domain_priors:suppression_score(australian_federation_1901__dismissal_1975_reading, 0.72).
domain_priors:theater_ratio(australian_federation_1901__dismissal_1975_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(australian_federation_1901__dismissal_1975_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(australian_federation_1901__dismissal_1975_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(australian_federation_1901__dismissal_1975_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(australian_federation_1901__dismissal_1975_reading, tangled_rope).
narrative_ontology:human_readable(australian_federation_1901__dismissal_1975_reading, "The 1975 Dismissal: Senate Supply Block and Vice-Regal Override").
narrative_ontology:topic_domain(australian_federation_1901__dismissal_1975_reading, "legal/constitutional/Westminster-hybrid").

domain_priors:requires_active_enforcement(australian_federation_1901__dismissal_1975_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(australian_federation_1901__dismissal_1975_reading, 'bb87f83b-10a5-4f48-a9bb-e4988aff9ed4').
narrative_ontology:cs_kernel_codification('bb87f83b-10a5-4f48-a9bb-e4988aff9ed4', fixed_text).
narrative_ontology:cs_authority_grounding('bb87f83b-10a5-4f48-a9bb-e4988aff9ed4', lineage).
narrative_ontology:cs_interpretation_layer_present('bb87f83b-10a5-4f48-a9bb-e4988aff9ed4').
narrative_ontology:cs_reading_relation('bb87f83b-10a5-4f48-a9bb-e4988aff9ed4', australian_federation_1901__referendum_rigidity_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb87f83b-10a5-4f48-a9bb-e4988aff9ed4', australian_federation_1901__washminster_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('bb87f83b-10a5-4f48-a9bb-e4988aff9ed4', foundational, confidence_convention_constrains_dismissal).
narrative_ontology:cs_axiom_status(confidence_convention_constrains_dismissal, holdable).
narrative_ontology:cs_axiom_grounding('bb87f83b-10a5-4f48-a9bb-e4988aff9ed4', confidence_convention_constrains_dismissal, conventional).
narrative_ontology:cs_axiom('bb87f83b-10a5-4f48-a9bb-e4988aff9ed4', foundational, upper_house_supply_control_extractive).
narrative_ontology:cs_axiom_status(upper_house_supply_control_extractive, holdable).
narrative_ontology:cs_axiom_grounding('bb87f83b-10a5-4f48-a9bb-e4988aff9ed4', upper_house_supply_control_extractive, deontological).
narrative_ontology:cs_reference_frame('bb87f83b-10a5-4f48-a9bb-e4988aff9ed4', westminster_lower_house_sovereignty).
narrative_ontology:cs_drift_state('bb87f83b-10a5-4f48-a9bb-e4988aff9ed4', post_1975_dismissal, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('bb87f83b-10a5-4f48-a9bb-e4988aff9ed4', '').
narrative_ontology:cs_kernel_id(australian_federation_1901__dismissal_1975_reading, australian_federation_1901).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(australian_federation_1901__dismissal_1975_reading, coalition_opposition).
narrative_ontology:constraint_beneficiary(australian_federation_1901__dismissal_1975_reading, vice_regal_discretion).
narrative_ontology:constraint_victim(australian_federation_1901__dismissal_1975_reading, dismissed_government).
narrative_ontology:constraint_victim(australian_federation_1901__dismissal_1975_reading, confidence_convention).
narrative_ontology:constraint_victim(australian_federation_1901__dismissal_1975_reading, elected_house_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISMISSED GOVERNMENT (SNARE) — The Whitlam government faced a constitutional crisis manufactured through supply blockade; exit required accepting the confidence convention's abandonment or negotiating with a hostile Senate. No structural escape path existed once the Governor-General exercised reserve powers. Maximum extraction: the government was removed from office despite holding a majority in the lower house.
constraint_indexing:constraint_classification(australian_federation_1901__dismissal_1975_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COALITION OPPOSITION (ROPE) — The blocking opposition benefited from a structural opportunity: the Senate's power to block supply created a coordination mechanism for forcing an election. From this position, the constraint is perceived as enabling legitimate constitutional action — forcing an election when supply fails is presented as democratic recovery, not extraction.
constraint_indexing:constraint_classification(australian_federation_1901__dismissal_1975_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: VICE-REGAL DISCRETION (ROPE) — The Governor-General's reserve powers appear as a coordination mechanism: resolving deadlock when supply is blocked. The discretionary power is experienced as enabling rather than constraining. From the vice-regal perspective, dismissal restores constitutional balance by forcing an election.
constraint_indexing:constraint_classification(australian_federation_1901__dismissal_1975_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LOWER HOUSE / ELECTED CHAMBER (TANGLED ROPE) — The lower house retains nominal sovereignty (members retained their seats; an election was called) but experienced suppression of the confidence convention that normally protects it from upper-house veto. The constraint exhibits both genuine coordination (federal deadlock resolution) and asymmetric extraction (the lower house's mandate was terminated without its consent).
constraint_indexing:constraint_classification(australian_federation_1901__dismissal_1975_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The 1975 dismissal reveals the hybrid's buried contradiction: Westminster confidence doctrine presumes the elected lower house is sovereign over supply, but the federal upper house retained blocking power. This is a genuine coordination problem (how to resolve deadlock between chambers) combined with genuine extraction (bypassing the lower house's electoral mandate). The constraint's true classification is tangled rope: there is a coordination function, there are identifiable beneficiaries and victims, and enforcement requires suppressing the confidence convention.
constraint_indexing:constraint_classification(australian_federation_1901__dismissal_1975_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(australian_federation_1901__dismissal_1975_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(australian_federation_1901__dismissal_1975_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(australian_federation_1901__dismissal_1975_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(australian_federation_1901__dismissal_1975_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(australian_federation_1901__dismissal_1975_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint exhibits significant extraction of office from the government. The lower house retained majority confidence, yet the government was removed through a mechanism (supply blockade + vice-regal dismissal) that bypassed the confidence doctrine. The extraction is not total (an election was called, not a coup staged), but it is substantial. Suppression (0.72): High suppression of the confidence convention itself. The convention that protects elected governments from removal by upper houses was actively suppressed — the blocking opposition and the Governor-General together subordinated Westminster doctrine to federal deadlock mechanics. Theater ratio (0.68): Moderate-to-high. The dismissal was defended as constitutional necessity (resolving a deadlock), but the real mechanism was political: the Opposition forced a crisis, the GG exercised discretion, and the government lost office. The constitutional language provided theater for what was fundamentally a power move. The theater increased over the crisis period as constitutional justifications accumulated. The measurements show suppression and extractiveness rising from pre-crisis baseline (suppression 0.42 → 0.72, extractiveness 0.35 → 0.58) as the crisis developed.
 *
 * PERSPECTIVAL GAP:
 *   The Coalition opposition (rope) and the dismissed government (snare) perceive the same structural event entirely differently. The opposition sees the Senate blockade as a legitimate constitutional check that forced an overdue election. The government sees the blockade as an abuse of upper-house power combined with vice-regal override of electoral mandate. The analytical observer sees the tangled rope: both mechanisms are present (coordination via deadlock resolution, extraction via mandate suppression). The washminster_hybrid_reading coexists with this reading by accepting the hybrid design as intentional — reserve powers and Senate supply control are features, not bugs. This reading treats them as a latent contradiction exposed by crisis. Neither reading forecloses the other; they differ on the interpretation of 1901's design intent.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality differs sharply by agent. The Coalition opposition (institutional/arbitrage) experiences low directionality (d ≈ 0.15): they are beneficiaries who could exit the constraint through negotiation or electoral competition. They faced low cost and extracted office. The dismissed government (powerful/mobile) experiences high directionality (d ≈ 0.85): despite formal power (elected majority), they faced structural barriers to continued office (supply blockade, vice-regal dismissal). The lower house (powerful/mobile) experiences moderate directionality (d ≈ 0.60): it retained seat-holding power but lost its mandate-protection via the suppressed confidence convention. The Governor-General (institutional/arbitrage) experiences low directionality (d ≈ 0.12): as the exerciser of reserve power, the constraint serves their formal authority. The confidence convention itself experiences maximum directionality (d = 1.0): as a victim-principle, it was suppressed entirely. The measured chi varies by perspective: beneficiaries experience negative chi (the constraint subsidizes them), victims experience high chi (extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by explicitly labeling the constraint as tangled rope — there is genuine coordination function (federal deadlock cannot be resolved by lower-house fiat alone) AND asymmetric extraction (the lower house's electoral mandate was terminated without its consent). The constraint is not a coordination mechanism masquerading as extraction, nor extraction masquerading as coordination. Both elements are structurally present. The supply blockade creates a coordination problem: neither chamber can govern if supply is blocked. The vice-regal dismissal resolves the deadlock but does so by extracting the lower house's mandate-protection. The theater (constitutional justification language) increased over the crisis but did not constitute the entire mechanism — the extraction and suppression are real, not performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reserve_powers_legitimacy_boundary,
    'Does the Governor-General''s discretion to dismiss a prime minister upon supply blockade reflect a genuine reserve power for deadlock resolution, or is it an extractive override of the confidence convention?',
    'Comparative analysis of Westminster systems: do other jurisdictions (Canada, UK, New Zealand) treat supply blockade as triggering dismissal or as requiring negotiation? Historical precedent: had the GG ever dismissed a government pre-1975 on supply grounds alone?',
    'If reserve power is legitimate: classification shifts toward rope (coordination mechanism). If override is extractive: classification shifts toward snare (suppression of confidence convention). The 1975 reading instantiates the extractive reading; the washminster_hybrid_reading coexists with it by claiming the hybrid''s design intentionally preserved reserve powers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reserve_powers_legitimacy_boundary, conceptual, 'Whether reserve powers are coordination or extraction mechanism').

omega_variable(
    confidence_convention_authority,
    'Is the Westminster confidence convention (that an elected government cannot be dismissed while it retains lower-house confidence) a constitutional norm or merely a practice subject to override by formal legal power?',
    'Doctrinal analysis: British constitutional authorities pre-1975; examination of whether the Australian Constitution (text) codifies confidence doctrine or leaves it implicit in convention. The 1975 dismissal was defended on grounds that formal constitutional power (the GG''s authority) overrides unwritten convention.',
    'If convention is paramount: the GG''s dismissal violated constitutional law (the law of confidence). If formal power overrides convention: the GG exercised legitimate constitutional authority. This reading holds that convention was suppressed; the washminster_hybrid_reading holds that formal authority intentionally preserved reserve power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confidence_convention_authority, conceptual, 'Status of confidence convention versus formal constitutional power').

omega_variable(
    senate_supply_blockade_purpose,
    'Was the Senate''s blocking of supply in 1975 a legitimate constitutional check on executive overreach, or an abuse of the upper house''s power to force an unwilling election?',
    'Historical record: examination of the government''s conduct leading to the blockade; analysis of whether the Coalition''s justification (supply-cut as enforcement of budgetary discipline) maps to any principle of Senate restraint in other Westminster systems. Did the lower house majority consent to the supply strategy?',
    'If legitimate check: the constraint is a coordination mechanism (rope). If abuse: the constraint is extraction (snare or tangled rope). This reading treats the blockade as an extractive use of Senate power combined with vice-regal extraction. The referendum_rigidity_reading is orthogonal: it does not address the blockade''s legitimacy but rather the Constitution''s resistance to amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(senate_supply_blockade_purpose, conceptual, 'Legitimacy of Senate supply blockade as constitutional mechanism').

omega_variable(
    hybrid_design_intent,
    'Did the 1901 drafters intentionally design a hybrid system that preserved vice-regal reserve powers and upper-house supply control, or did they intend Westminster confidence doctrine to override federal deadlock provisions?',
    'Constitutional history: examination of 1901 Convention Debates; analysis of whether the framers discussed the interaction between Senate supply control and the GG''s dismissal power. The washminster_hybrid_reading claims intentional hybrid design; this reading claims the hybrid was a latent contradiction that 1975 exposed.',
    'If intentional hybrid: the reservation of power is constitutional by design (supports washminster reading). If latent contradiction: the 1975 crisis reveals a flaw (supports this dismissal reading). The two readings coexist because neither forecloses the other — intent in 1901 cannot retroactively determine what the constitution''s binding text actually prescribes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_design_intent, conceptual, 'Whether hybrid design was intentional or latent contradiction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(australian_federation_1901__dismissal_1975_reading, 0, 11).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dismissal_theater_pre_crisis, australian_federation_1901__dismissal_1975_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(dismissal_theater_crisis_onset, australian_federation_1901__dismissal_1975_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(dismissal_theater_peak, australian_federation_1901__dismissal_1975_reading, theater_ratio, 11, 0.68).

% Extraction over time
narrative_ontology:measurement(dismissal_extract_pre_crisis, australian_federation_1901__dismissal_1975_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dismissal_extract_crisis_onset, australian_federation_1901__dismissal_1975_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(dismissal_extract_peak, australian_federation_1901__dismissal_1975_reading, base_extractiveness, 11, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dismissal_suppression_pre_crisis, australian_federation_1901__dismissal_1975_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(dismissal_suppression_crisis_onset, australian_federation_1901__dismissal_1975_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(dismissal_suppression_peak, australian_federation_1901__dismissal_1975_reading, suppression_requirement, 11, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(australian_federation_1901__dismissal_1975_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(australian_federation_1901__dismissal_1975_reading, australian_federation_1901__referendum_rigidity_reading).
narrative_ontology:affects_constraint(australian_federation_1901__dismissal_1975_reading, australian_federation_1901__washminster_hybrid_reading).

% DUAL FORMULATION NOTE:
% The three readings of the australian_federation_1901 kernel are structurally distinct constraints with different ε values and beneficiary/victim structures. dismissal_1975_reading (ε=0.58) frames the 1975 crisis as extractive suppression of confidence convention; washminster_hybrid_reading (ε≈0.30–0.40, estimated as coordination mechanism) frames the hybrid design as intentional deadlock-resolution; referendum_rigidity_reading (ε≈0.25, estimated as constraint on amendment) focuses on the Constitution's structural rigidity. All three are readings of the same text kernel, but they identify different structural problems and beneficiaries. Links via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(australian_federation_1901__dismissal_1975_reading, institutional, 0.15).
constraint_indexing:directionality_override(australian_federation_1901__dismissal_1975_reading, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
