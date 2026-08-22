% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__democratic_enclosure_reading, []).

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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: NSL as Democratic Enclosure and Dissent Criminalization Mechanism
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the democratic_enclosure_reading of the
 *   nsl_legal_text kernel. It treats the National Security Law imposed on
 *   Hong Kong in 2020 not as a security measure but as a structural
 *   instrument for the permanent closure of democratic space, criminalization
 *   of dissent, and elimination of autonomous civil society. The kernel
 *   yields three readings: this one (democratic enclosure), a
 *   sovereignty_restoration_reading that frames it as legitimate
 *   order-restoration, and a jurisdictional_capture_reading that focuses on
 *   mainland legal system transplantation. This reading authors high
 *   extractiveness and suppression because the constraint's operation targets
 *   the entire democratic infrastructure for elimination, not merely
 *   regulation.
 *
 * KEY AGENTS:
 *   - Beijing Central Government: Primary agenda-setter (institutional/arbitrage) â designs, interprets, and directs the law's application without external constraint.
 *   - HK Government: Secondary agenda-setter and beneficiary (institutional/constrained) â implements arrests, prosecutions, and disqualifications; politically dependent on Beijing.
 *   - HK Pro-Establishment Camp: Primary beneficiary (powerful/constrained) â gains uncontested political office from the elimination of opposition.
 *   - Pro-Democracy Opposition: Primary target (powerless/trapped) â bears criminalization, remand, exile, and disqualification.
 *   - Civil Society Organizations: Target (powerless/trapped) â organizational dissolution and operational paralysis.
 *   - Independent Media: Target (powerless/trapped) â outlet closure, journalist arrest, and systemic self-censorship.
 *   - HK Legal Profession: Target (moderate/constrained) â professional autonomy erosion through judge vetting and interpretive override.
 *   - International Human Rights Observers: Analytical observer (institutional/analytical) â monitors and documents divergence from international standards.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.88).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.92).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "NSL as Democratic Enclosure and Dissent Criminalization Mechanism").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '5bb54699-f4f3-4614-b450-c9d0b392b571').
narrative_ontology:cs_kernel_codification('5bb54699-f4f3-4614-b450-c9d0b392b571', fixed_text).
narrative_ontology:cs_authority_grounding('5bb54699-f4f3-4614-b450-c9d0b392b571', extraction).
narrative_ontology:cs_interpretation_layer_present('5bb54699-f4f3-4614-b450-c9d0b392b571').
narrative_ontology:cs_reading_relation('5bb54699-f4f3-4614-b450-c9d0b392b571', nsl_legal_text__sovereignty_restoration_reading, influences).
narrative_ontology:cs_reading_relation('5bb54699-f4f3-4614-b450-c9d0b392b571', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('5bb54699-f4f3-4614-b450-c9d0b392b571', foundational, democratic_self_determination_non_negotiable).
narrative_ontology:cs_axiom_status(democratic_self_determination_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('5bb54699-f4f3-4614-b450-c9d0b392b571', democratic_self_determination_non_negotiable, deontological).
narrative_ontology:cs_axiom('5bb54699-f4f3-4614-b450-c9d0b392b571', foundational, political_dissent_not_subversion).
narrative_ontology:cs_axiom_status(political_dissent_not_subversion, holdable).
narrative_ontology:cs_axiom_grounding('5bb54699-f4f3-4614-b450-c9d0b392b571', political_dissent_not_subversion, conventional).
narrative_ontology:cs_reference_frame('5bb54699-f4f3-4614-b450-c9d0b392b571', pre_nsl_liberal_autonomy).
narrative_ontology:cs_drift_state('5bb54699-f4f3-4614-b450-c9d0b392b571', post_nsl_enforcement_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5bb54699-f4f3-4614-b450-c9d0b392b571', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hk_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hk_pro_establishment_camp).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_democracy_opposition).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_media).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hk_legal_profession).
narrative_ontology:constraint_vindicates(nsl_legal_text__democratic_enclosure_reading, national_security_primacy_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__democratic_enclosure_reading, executive_led_governance_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directs the legislative and interpretive agenda for the NSL through the NPCSC and the Hong Kong Liaison Office. Determines the scope of subversion and collusion charges, approves the pool of national security judges, and can issue binding interpretations that override local judicial review. Exit is unconstrained by any external review mechanism.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Implements the NSL through the Department of Justice, the Police National Security Department, and the Chief Executive's judge-selection mechanism. Administratively carries out arrests, prosecutions, disqualifications, and vetting of candidates. Its political survival depends on Beijing's endorsement, which the NSL secures.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, hk_government, beneficiary).

% Politically benefits from the elimination of competitive opposition in legislative and district councils. Gains uncontested access to political office and policy influence as pro-democracy candidates are disqualified or imprisoned. Its electoral viability depends on the absence of a free opposition.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_pro_establishment_camp, beneficiary,
    powerful, generational, constrained, national).

% Faces prosecution, remand, or exile for subversion and collusion charges arising from peaceful political activity. Parties have been disbanded; candidates disqualified; elected legislators removed. Remaining members operate under threat of arrest; exiles face extraterritorial warrants and bounty notices.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_democracy_opposition, payer,
    powerless, immediate, trapped, national).

% Organizations disbanded or self-dissolved after directors were arrested or bank accounts frozen. Remaining groups operate under strict red lines regarding foreign funding and advocacy. The sector's independent funding, organizing capacity, and public advocacy space have collapsed.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations, payer,
    powerless, immediate, trapped, national).

% Outlets shut down after editors and journalists were arrested or assets frozen. Journalists face collusion charges for reporting that allegedly invites foreign sanctions. The remaining press operates under extensive self-censorship; investigative reporting on governance is effectively extinguished.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_media, payer,
    powerless, immediate, trapped, national).

% Barristers and solicitors face professional consequences for representing defendants or criticizing proceedings. Foreign counsel restrictions, judge vetting, and NPCSC interpretive overrides have constrained the judiciary's autonomy. The common law tradition of procedural fairness is eroded by national security procedure.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_legal_profession, payer,
    moderate, biographical, constrained, national).

% Document prosecutions, issue statements, and advocate for sanctions or treaty accountability. They do not participate in the constraint's operation but monitor and report on its divergence from international human rights law standards.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_human_rights_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__democratic_enclosure_reading, beijing_central_government).
narrative_ontology:fixing_cost_class(nsl_legal_text__democratic_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement is presented as coordinating public safety against subversion, terrorism, secession, and collusion, but from this reading's structural analysis it coordinates only the suppression of political opposition and the dismantling of civil society autonomy.
% TRANSFER_FUNCTION: Moves political authority from elected representatives, civil society, and the judiciary to executive security organs and Beijing-directed institutions; moves compliance and silence from the population to the state via threat of prosecution, remand, and disqualification.
% ABSENT_VOICES: Pro-democracy exiles, imprisoned activists, and disbanded civil society groups are structurally absent from the legislative and interpretive process; they would contest the security framing and demand democratic accountability but are excluded by arrest warrants, disbarment, organizational dissolution, and extraterritorial threats.
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, the entire architecture of political control in Hong Kong would destabilize: opposition parties would reorganize, independent media would resume operation, civil society would reconstitute, and the electoral system would re-open to competitive contestation. The current political equilibrium depends entirely on the constraint's suppressive force.
% FOUNDING_PROBLEM: Restoring public order and ending violent unrest in Hong Kong following the 2019 anti-extradition protests.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and the HK government assert the problem is live, citing ongoing 'black violence' and 'foreign interference.' Independent historians, UN Special Rapporteurs, and the exiled opposition attest that large-scale unrest subsided in 2020 and the law's subsequent application targets peaceful political activity, corroborating the dead-problem reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__democratic_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__democratic_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.88 because the constraint extracts not merely economic rents but the totality of political opposition capacity â it eliminates alternative power centers rather than taxing them. Suppression is 0.92 because the constraint depends on active, ongoing enforcement: arrests, frozen accounts, extraterritorial warrants, and disqualification orders. Theater_ratio at 0.45 reflects the mix of genuine punitive impact (lengthy remands, convictions) with performative elements (televised confessions, show trials, public denunciations). Accessibility_collapse is 0.88 because once the constraint is understood, alternative political organization, free reporting, and independent advocacy collapse almost completely. Resistance at 0.70 captures persistent international condemnation, diaspora activism, and underground cultural resistance, though domestic open resistance has been largely extinguished. The temporal series show a ratchet: extraction deepened as the law's scope expanded from street protest to civil society to electoral candidacy; theater rose as the prosecutions became spectacle; suppression intensified as the enforcement machinery matured and extraterritorial reach was asserted.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (Beijing, HK government) experience the constraint as a restoration of governability and a legitimate assertion of sovereign authority; the payer seats (opposition, civil society, media, legal profession) experience the identical legal text as totalizing political extraction. The engine computes this divergence from the structural data: agenda-setters are beneficiaries with high power and arbitrage-grade exit; payers are powerless or moderate with trapped or constrained exit. The per-seat classification will reflect this asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the beneficiary/victim declarations and exit options. Beijing and the HK pro-establishment camp are declared beneficiaries with high power and relatively open exit (arbitrage for Beijing, constrained but politically secure for the local camp), placing them near the full-beneficiary end (low d). The democratic opposition, civil society, and independent media are declared victims with powerless status and trapped exit, placing them near the full-target end (high d). The legal profession is a victim but retains moderate power and constrained rather than trapped exit, yielding an intermediate high d. International observers are analytical with analytical exit, producing a neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â the 2019 unrest â is assessed as dead in this reading, yet the arrangement persists and has expanded its scope well beyond the initial street confrontations. This mismatch (dead founding problem + world_rearranges disappearance verdict) flags mandatrophy. However, the constraint is not a piton because concentrated beneficiaries (the Beijing authority and HK establishment) actively maintain and profit from its continuation; the persistence is beneficiary-driven extraction, not inertial theatrical maintenance. The constraint therefore classifies as snare rather than degraded rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_narrative_sincerity,
    'Is the national security threat the NSL claims to address genuine, and if so, does the law''s actual application proportionally target that threat rather than peaceful opposition?',
    'Independent empirical audit of prosecution evidence and sentencing data against the statutory definitions of subversion, secession, terrorism, and collusion; comparison with pre-NSL public order incident rates.',
    'If the threat is genuine and the application proportional, the extractiveness score would require downward revision toward tangled_rope; if the threat is pretextual, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_narrative_sincerity, empirical, 'Whether the security framing is sincere or pretextual for democratic enclosure.').

omega_variable(
    extraterritorial_suppression_scope,
    'How extensive is the NSL''s extraterritorial suppression, and does it represent structural enforcement capacity or theatrical deterrence?',
    'Catalogue of extraterritorial warrants, bounty notices, transnational rendition attempts, and foreign institution self-censorship; assessment of actual successful extraterritorial detentions versus threatened ones.',
    'If extraterritorial reach is largely theatrical, suppression and theater_ratio would shift; if structurally effective, extraction extends beyond the territorial scope and amplifies effective extraction for the diaspora.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraterritorial_suppression_scope, empirical, 'Structural versus theatrical nature of extraterritorial enforcement.').

omega_variable(
    judicial_residual_autonomy,
    'Does any residual judicial autonomy in NSL cases meaningfully constrain executive extraction, or has interpretation collapsed entirely to executive will?',
    'Quantitative analysis of bail grant rates, acquittal rates, and sentencing severity in NSL cases versus common-law precedent; review of NPCSC interpretive override frequency.',
    'If residual autonomy exists, extraction is partially damped and directionality for the legal profession shifts; if interpretation has collapsed, the accessibility_collapse and suppression metrics are confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_residual_autonomy, empirical, 'Whether residual judicial autonomy moderates executive extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_de_tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nsl_de_tr_t12, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(nsl_de_tr_t24, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(nsl_de_tr_t36, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 36, 0.45).
narrative_ontology:measurement(nsl_de_tr_t48, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 48, 0.5).

% Extraction over time
narrative_ontology:measurement(nsl_de_be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(nsl_de_be_t12, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 12, 0.8).
narrative_ontology:measurement(nsl_de_be_t24, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(nsl_de_be_t36, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 36, 0.86).
narrative_ontology:measurement(nsl_de_be_t48, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 48, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(nsl_de_su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(nsl_de_su_t12, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 12, 0.88).
narrative_ontology:measurement(nsl_de_su_t24, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 24, 0.91).
narrative_ontology:measurement(nsl_de_su_t36, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 36, 0.93).
narrative_ontology:measurement(nsl_de_su_t48, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 48, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, hk_electoral_system).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, hk_press_freedom).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, hk_common_law_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the nsl_legal_text kernel, which decomposes into three structurally distinct constraints: democratic_enclosure_reading (this file), jurisdictional_capture_reading, and sovereignty_restoration_reading. Each carries a distinct epsilon, stakeholder set, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
