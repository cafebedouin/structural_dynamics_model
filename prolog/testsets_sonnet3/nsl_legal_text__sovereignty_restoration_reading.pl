% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law as Sovereign Restoration Instrument (Sovereignty-Restoration Reading)
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-restoration reading of the
 *   National Security Law kernel: the law as a legitimate exercise of central
 *   sovereign authority to end a genuine security emergency (the sustained
 *   2019 unrest) that the existing SAR legal apparatus could not resolve. On
 *   this reading, the coordination function is real — restoring
 *   administrative continuity, public order, and the conditions for ordinary
 *   governance and commerce — and the people who bear its costs are not a
 *   general population but specifically those whose organizing, journalism,
 *   or officeholding is judged, within the reading's own terms, to have
 *   constituted or abetted the security threat the law targets. This is a
 *   deliberately narrower victim set than the democratic_enclosure_reading
 *   (permanent closure of democratic space generally) or the
 *   jurisdictional_capture_reading (erosion of common law autonomy through
 *   mainland legal transplantation) would author for the same underlying
 *   legal text — those are different constraints, authored separately,
 *   sharing this kernel_id.
 *
 * KEY AGENTS:
 *   - central_peoples_government_authority: drafting and interpretive authority, primary beneficiary
 *   - hong_kong_special_administrative_region_executive: implementing authority, beneficiary
 *   - pro_democracy_activists and protest_participants_2019: bear prosecutorial and mobility costs, cast as security threats under this reading
 *   - opposition_legislators and independent_journalists: bear disqualification and operational costs
 *   - business_and_property_owning_classes: beneficiary of restored commercial stability
 *   - comparative_constitutional_observers: analytical seat assessing the reading on its own terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.48).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.58).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law as Sovereign Restoration Instrument (Sovereignty-Restoration Reading)").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, '17b48ab6-72c2-4d9e-9bbd-c5377146a789').
narrative_ontology:cs_kernel_codification('17b48ab6-72c2-4d9e-9bbd-c5377146a789', formalized).
narrative_ontology:cs_authority_grounding('17b48ab6-72c2-4d9e-9bbd-c5377146a789', extraction).
narrative_ontology:cs_interpretation_layer_present('17b48ab6-72c2-4d9e-9bbd-c5377146a789').
narrative_ontology:cs_reading_relation('17b48ab6-72c2-4d9e-9bbd-c5377146a789', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('17b48ab6-72c2-4d9e-9bbd-c5377146a789', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('17b48ab6-72c2-4d9e-9bbd-c5377146a789', foundational, sovereign_entitled_to_suppress_genuine_security_threat).
narrative_ontology:cs_axiom_status(sovereign_entitled_to_suppress_genuine_security_threat, holdable).
narrative_ontology:cs_axiom_grounding('17b48ab6-72c2-4d9e-9bbd-c5377146a789', sovereign_entitled_to_suppress_genuine_security_threat, empirically_contingent).
narrative_ontology:cs_axiom('17b48ab6-72c2-4d9e-9bbd-c5377146a789', secondary, central_authority_may_directly_legislate_when_local_institutions_fail).
narrative_ontology:cs_axiom_status(central_authority_may_directly_legislate_when_local_institutions_fail, holdable).
narrative_ontology:cs_axiom_grounding('17b48ab6-72c2-4d9e-9bbd-c5377146a789', central_authority_may_directly_legislate_when_local_institutions_fail, conventional).
narrative_ontology:cs_reference_frame('17b48ab6-72c2-4d9e-9bbd-c5377146a789', one_country_two_systems_sovereign_reservation).
narrative_ontology:cs_drift_state('17b48ab6-72c2-4d9e-9bbd-c5377146a789', post_2020_enactment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17b48ab6-72c2-4d9e-9bbd-c5377146a789', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, central_peoples_government_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, hong_kong_special_administrative_region_executive).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, business_and_property_owning_classes).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, pro_beijing_political_bloc).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, protest_participants_2019).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, independent_journalists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, opposition_legislators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and imposed the law directly under Article 18 and Annex III after assessing that the SAR legislature could not pass local security legislation amid the 2019 unrest. Frames the law as a sovereign act restoring order after months of escalating violence, transport disruption, and attacks on state symbols. Retains ultimate interpretive authority over the law's provisions and can direct case handling in matters it designates as involving foreign intervention.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, central_peoples_government_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, central_peoples_government_authority, beneficiary).

% Gains a durable instrument to end the paralysis of 2019, restore administrative function, and prosecute organizers of the unrest. Implements the law through police, prosecutors, and a newly created national security department, and credits it with the sharp drop in street disorder and the resumption of ordinary governance.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hong_kong_special_administrative_region_executive, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, hong_kong_special_administrative_region_executive, agenda_setter).

% Experienced 2019 as a direct threat to commercial continuity, property, and personal safety amid road blockades, arson, and transit shutdowns. Views the law's restoration of public order as protecting the conditions for continued business operation and asset value, and largely supports the security framing regardless of collateral effects on civil liberties.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, business_and_property_owning_classes, beneficiary,
    powerful, biographical, mobile, regional).

% Gains political dominance in a legislature and district structure cleared of many opposition figures either by disqualification, resignation in protest, or prosecution. Credits the law with removing what it characterizes as a foreign-backed destabilization campaign and stabilizing governance.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_beijing_political_bloc, beneficiary,
    organized, generational, arbitrage, regional).

% Face prosecution, asset freezes, or self-exile for activity that under this reading is treated as secession, subversion, or collusion with foreign forces. From the sovereignty-restoration seat, they are not dissenters being silenced but actors whose organized 2019 campaign is judged to have crossed into a genuine security threat the state was entitled to suppress.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists, payer,
    powerless, biographical, trapped, regional).

% Participated in demonstrations, some violent, during 2019; under this reading their organizing and international appeals for sanctions or intervention are treated as the precipitating security threat the law was built to prevent from recurring. Many face retrospective liability or chilled participation in any future mobilization.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, protest_participants_2019, payer,
    powerless, biographical, trapped, local).

% Reporting that under this reading amplified secessionist messaging or foreign collusion narratives is treated as a security-relevant activity subject to scrutiny; some outlets closed or relocated. From the sovereignty-restoration seat this is the necessary containment of information channels judged to have fueled the unrest, not press suppression per se.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, independent_journalists, payer,
    moderate, biographical, constrained, regional).

% Disqualified, prosecuted, or resigned following an oath-of-office and loyalty framework tied to the law's security logic. Under this reading, their removal is treated as excluding actors who used legislative position to advance the same destabilization campaign the law addresses, not as elimination of legitimate political competition.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, opposition_legislators, payer,
    moderate, biographical, trapped, regional).

% Foreign governments, UN human rights bodies, and international NGOs issued sanctions, statements of concern, and monitoring reports. Under this reading their objections are treated as the very external interference the law is designed to exclude from domestic jurisdiction, so their standing to object is not recognized within the reading's own framework.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_governments_and_ngos, excluded,
    institutional, generational, analytical, global).

% Study the law as a case of a central sovereign asserting emergency security authority over a sub-national jurisdiction following civil unrest, comparing it to other post-crisis security legislation elsewhere; assess it on the sovereignty-restoration reading's own terms of order-restoration efficacy and proportionality.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, comparative_constitutional_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__sovereignty_restoration_reading, central_peoples_government_authority).
narrative_ontology:fixing_cost_class(nsl_legal_text__sovereignty_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the central sovereign with a legal instrument to reassert governmental control and administrative continuity in a jurisdiction whose local institutions, on this reading, had proven unable to end sustained civil unrest, transport disruption, and violence against state authority in 2019.
% TRANSFER_FUNCTION: Moves political security and continuity of order to the state and to those whose commercial or institutional interests depend on stability; moves liability, prosecutorial exposure, and organizing capacity away from individuals and groups classified as having engaged in secession, subversion, terrorism, or foreign collusion.
% ABSENT_VOICES: International governments, UN human rights mechanisms, and diaspora advocacy groups raised sustained objections but are treated within this reading's own framework as illegitimate external interference in a domestic sovereign matter, not as parties with standing to be heard on the law's design.
% DISAPPEARANCE_RATIONALE: If the law disappeared, the prosecutorial and administrative apparatus built around it would lose its legal basis; disqualified legislators could seek reinstatement, exiled activists could return, and the security department established under the law would need a new mandate — under this reading, the risk (from the state's perspective) is that the conditions of 2019 could re-emerge without the deterrent and enforcement structure now in place.
% FOUNDING_PROBLEM: Sustained, escalating civil unrest in 2019 involving prolonged transport paralysis, property destruction, attacks on legislative and police infrastructure, and open calls (including from some participants) for foreign sanctions or intervention, which the existing SAR legal framework was assessed as structurally unable to end.
% FOUNDING_PROBLEM_CORROBORATION: The central and SAR governments, and much of the business community, attest the founding problem (unrest amounting to a genuine security emergency) was real and required a sovereign-level response; this is corroborated in part by contemporaneous reporting on transport disruption and violence during 2019. Independent international human rights bodies and many domestic legal scholars dispute the characterization of the unrest as warranting instruments of this scope, and note the law's prosecutorial reach has extended well beyond violent incidents into peaceful organizing and journalism — so corroboration for the founding-problem characterization itself, versus the proportionality of the remedy, comes overwhelmingly from parties who also benefit from the law's continuation.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate (0.48) rather than high because this reading targets a bounded set of political actors judged to have engaged in security-relevant conduct, not the general population — commerce, daily governance, and most residents' lives are not the object of the law's coercive apparatus under this reading. Suppression is authored moderate-high (0.58) and drawn down slightly over the interval (0.65 to 0.58) reflecting an initial period of aggressive early enforcement (arrests, disqualifications, prosecutions establishing the law's reach) followed by a lower-intensity steady state once the deterrent effect and institutional realignment (media closures, legislative composition change) had largely occurred. Theater ratio is low (0.2) and rising slightly — the coordination function (restoring administrative continuity) remains substantially real under this reading, though an increasing share of enforcement visibly serves symbolic reassertion of sovereignty (prosecutions of low-threat symbolic speech) alongside genuine security administration.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter and beneficiary seats, the arrangement computes as coordination: a sovereign restoring order it is entitled to restore, using proportionate legal process against a genuine threat. From the payer seats (activists, journalists, legislators), the identical structural facts — the same law, same enforcement actions — are experienced as targeted extraction of political and expressive capacity. The engine computes these divergent seat classifications from the shared structural data (power, exit_options, beneficiary/victim role) rather than from either seat's self-description; this story authors the sovereignty-restoration seat's version of the facts, not a synthesis.
 *
 * DIRECTIONALITY LOGIC:
 *   The central and SAR authorities sit at the beneficiary end: they hold interpretive and enforcement power and their institutional continuity is what the law protects and extends. Business and pro-Beijing political interests are secondary beneficiaries whose stability and political dominance the restored order secures. Activists, protest participants, journalists, and opposition legislators sit at the target end — under this reading specifically because their organizing, reporting, or officeholding is classified as the security threat, not because of generic political disfavor; the derivation here is deliberately narrower than a democratic-enclosure reading's victim set would be.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (2019 unrest as an unresolvable security emergency) is authored as contested rather than resolved-dead or clearly-live: the state seats treat it as ongoing (citing continued 'soft resistance' and international pressure) while independent legal observers note the acute crisis conditions of 2019 ended years ago, yet enforcement intensity and prosecutorial reach have not correspondingly narrowed to the acute-crisis period. This mismatch — a founding problem the reading's own beneficiaries describe as still live, absent strong outside corroboration for that liveness claim beyond the original 2019 facts — is exactly the signal the R5 genealogy interview is built to surface, without this story itself adjudicating it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_emergency_vs_pretextual_framing,
    'Was the 2019 unrest a genuine, unresolvable-by-existing-means security emergency of the kind the sovereignty-restoration reading describes, or was the emergency framing itself constructed to justify a pre-existing intent to close political space (as the democratic_enclosure_reading holds)?',
    'Comparative analysis of enforcement patterns against the acute-crisis timeline: if prosecutions and restrictions are concentrated in the 2019-2020 unrest period and taper as conditions normalize, that supports the restoration reading; if enforcement intensity and scope continue to expand years after the unrest ended and extend to conduct unconnected to the original violence (ordinary journalism, symbolic speech, historical commemoration), that supports the enclosure reading.',
    'If the founding-problem-status inquiry finds enforcement has decoupled from the original security rationale, this reading''s own coordination-function claim weakens substantially and the structural facts converge toward the democratic_enclosure_reading''s classification even though this story''s authored ε and stakeholder set remain distinct data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_emergency_vs_pretextual_framing, conceptual, 'Whether the security-emergency premise of this reading is empirically sustained by post-2019 enforcement patterns.').

omega_variable(
    kernel_disaggregation_boundary,
    'Where exactly does the sovereignty-restoration reading''s victim set (organizers/threats to security) diverge from the democratic-enclosure reading''s victim set (the general politically active population), and is that boundary stable across cases or does it expand over time?',
    'Case-by-case coding of NSL prosecutions against a threat-proximity scale (organizing violent unrest vs. peaceful assembly vs. commentary vs. historical commemoration), tracked longitudinally.',
    'A stable, narrow boundary supports treating this as a genuinely distinct reading with materially lower ε than the enclosure reading; a boundary that has expanded to swallow peaceful and expressive conduct would indicate the two readings are converging in practice even while remaining analytically distinct kernel readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disaggregation_boundary, empirical, 'Whether the reading''s narrower victim-set claim holds up against actual enforcement scope over time.').

omega_variable(
    external_standing_denial_ambiguity,
    'Is the reading''s treatment of international objection as illegitimate interference a coherent application of sovereign non-interference doctrine, or does it function to insulate the reading from exactly the kind of independent corroboration the R5 genealogy interview calls for?',
    'Compare this reading''s standing-denial logic to how other sovereign security emergencies (declared by other states, evaluated by the same international bodies) are treated by comparative constitutional scholarship, to see if the non-interference framing is applied consistently or selectively.',
    'If applied selectively, the founding_problem_corroboration finding (that outside corroboration is thin) is reinforced structurally rather than incidentally, sharpening the contested status of founding_problem_status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_standing_denial_ambiguity, conceptual, 'Whether excluding international observers as illegitimate interlocutors is principled sovereignty doctrine or a structural insulation move.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(nsl__tr_t36, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 36, 0.18).
narrative_ontology:measurement(nsl__tr_t48, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 48, 0.19).
narrative_ontology:measurement(nsl__tr_t60, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(nsl__be_t36, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 36, 0.46).
narrative_ontology:measurement(nsl__be_t48, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 48, 0.47).
narrative_ontology:measurement(nsl__be_t60, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(nsl__su_t36, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 36, 0.59).
narrative_ontology:measurement(nsl__su_t48, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 48, 0.58).
narrative_ontology:measurement(nsl__su_t60, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the nsl_legal_text kernel. sovereignty_restoration_reading (this file) authors the state's coordination-function framing with a bounded victim set (security-threat-classified individuals) and moderate ε (0.48). democratic_enclosure_reading authors the same legal text with a much broader victim set (the general politically-engaged population) and correspondingly higher ε, treating the security framing itself as pretextual. jurisdictional_capture_reading authors a distinct extraction target (common-law institutional autonomy, judges, and legal professionals) rather than political dissent as such. All three share the kernel_id nsl_legal_text but are separate constraints per the ε-invariance principle — each has its own stable ε, beneficiaries, victims, and classification, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
