% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Jurisdiction â Hybrid Complementarity Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute's jurisdiction regime is a contested kernel with three
 *   dominant readings. This constraint instantiates the hybrid
 *   complementarity reading: the ICC possesses residual universal authority
 *   operative through treaty ratification, active nationality,
 *   territoriality, and UNSC referral, but is operationally constrained by
 *   the complementarity principle deferring to genuine national proceedings.
 *   The resulting arrangement coordinates international prosecution of
 *   atrocities while extracting sovereignty costs from states and liberty
 *   costs from accused individuals. The constraint is claimed as tangled_rope
 *   â genuine coordination layered with asymmetric extraction â with
 *   metrics authored independently to reflect selective enforcement, state
 *   cooperation dependency, and institutional theater.
 *
 * KEY AGENTS:
 *   - otp (institutional/constrained): Agenda-setter â administers complementarity assessments and prosecutorial strategy
 *   - state_parties (institutional/constrained): Dual beneficiary/payer â fund and legitimize the court while ceding jurisdictional primacy
 *   - accused_individuals (powerless/trapped): Primary target â bear direct extraction of liberty and legal jeopardy
 *   - un_security_council (institutional/arbitrage): External agenda-setter â expands jurisdiction beyond consent via referral while shielding permanent members
 *   - referred_non_party_states (moderate/trapped): Sovereignty targets â subjected to jurisdiction without treaty consent
 *   - powerful_non_party_states (powerful/arbitrage): Excluded observers â shape the constraint while remaining shielded from it
 *   - african_states_collective (organized/constrained): Disproportionate payer bloc â experience selective enforcement as sovereignty infringement
 *   - victim_communities (powerless/constrained): Beneficiaries â gain symbolic and material justice but depend on prosecutorial discretion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.72).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.8).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Jurisdiction â Hybrid Complementarity Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, 'd7b71960-fc97-47f6-b1a8-a808814f19ea').
narrative_ontology:cs_kernel_codification('d7b71960-fc97-47f6-b1a8-a808814f19ea', formalized).
narrative_ontology:cs_authority_grounding('d7b71960-fc97-47f6-b1a8-a808814f19ea', lineage).
narrative_ontology:cs_interpretation_layer_present('d7b71960-fc97-47f6-b1a8-a808814f19ea').
narrative_ontology:cs_reading_relation('d7b71960-fc97-47f6-b1a8-a808814f19ea', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7b71960-fc97-47f6-b1a8-a808814f19ea', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_axiom('d7b71960-fc97-47f6-b1a8-a808814f19ea', foundational, residual_universal_jurisdiction).
narrative_ontology:cs_axiom_status(residual_universal_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('d7b71960-fc97-47f6-b1a8-a808814f19ea', residual_universal_jurisdiction, conventional).
narrative_ontology:cs_axiom('d7b71960-fc97-47f6-b1a8-a808814f19ea', foundational, complementarity_as_deference_not_barrier).
narrative_ontology:cs_axiom_status(complementarity_as_deference_not_barrier, holdable).
narrative_ontology:cs_axiom_grounding('d7b71960-fc97-47f6-b1a8-a808814f19ea', complementarity_as_deference_not_barrier, deontological).
narrative_ontology:cs_reference_frame('d7b71960-fc97-47f6-b1a8-a808814f19ea', complementarity_hybrid_framework).
narrative_ontology:cs_drift_state('d7b71960-fc97-47f6-b1a8-a808814f19ea', post_ukraine_referral_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d7b71960-fc97-47f6-b1a8-a808814f19ea', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, victim_communities).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, otp).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_individuals).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, referred_non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, african_states_collective).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers prosecutorial strategy and evaluates whether national jurisdictions are unwilling or unable to act genuinely under the complementarity framework. Depends on state cooperation to execute arrest warrants, secure evidence, and enforce sentences. Benefits from institutional mandate, budget, and expanding jurisprudence, but cannot unilaterally compel cooperation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, otp, agenda_setter,
    institutional, generational, constrained, global).

% Consent to the Rome Statute and fund the Court through assessed contributions. Benefit from a rules-based backstop against impunity and from the legitimacy of participating in international criminal justice. Cede primary jurisdictional primacy and face admissibility scrutiny if national proceedings are deemed inadequate; withdrawal is legally possible but carries reputational and diplomatic costs.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties, payer).

% Subject to arrest warrants, surrender, trial, and potential imprisonment by the ICC once targeted. Cannot opt out of jurisdiction or choose an alternative forum. Bear the direct personal cost of liberty, legal defense, and prolonged pre-trial detention, even when fugitive indictees from more powerful contexts remain at large.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_individuals, payer,
    powerless, biographical, trapped, global).

% Access reparations, participate in proceedings through legal representatives, and benefit symbolically from accountability narratives. Cannot opt out of the justice process when the OTP opens an investigation in their situation; outcomes depend on prosecutorial discretion and state cooperation rather than victim agency.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, victim_communities, beneficiary,
    powerless, generational, constrained, local).

% Can refer situations involving non-party states to the ICC, effectively expanding jurisdiction beyond treaty consent. Uses the Court as a geopolitical tool while retaining power to block referrals through permanent member vetoes. Not bound by the Rome Statute but shapes its scope and legitimacy through selective engagement.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, un_security_council, agenda_setter,
    institutional, immediate, arbitrage, global).

% Subject to ICC jurisdiction through UNSC referral despite not consenting to the Rome Statute. Bear sovereignty costs, obligations to cooperate, and reputational damage without having ratified the treaty. Limited exit options once referred; non-cooperation risks sanctions but does not terminate jurisdiction.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, referred_non_party_states, payer,
    moderate, biographical, trapped, national).

% Remain outside the Rome Statute through non-ratification and shield nationals from ICC jurisdiction using geopolitical leverage, bilateral immunity agreements, and UNSC veto power. Influence the constraint's operation through referral politics and funding pressure while avoiding direct application to themselves.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, powerful_non_party_states, excluded,
    powerful, civilizational, arbitrage, global).

% Disproportionately subject to ICC investigations and prosecutions. Experience the constraint as an infringement on post-colonial sovereignty and as selective enforcement that targets weaker states while powerful non-parties escape scrutiny. Have threatened coordinated withdrawal and established regional alternative mechanisms, but most remain state parties.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, african_states_collective, payer,
    organized, biographical, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the prosecution of genocide, crimes against humanity, and war crimes by establishing a residual international court that activates only when national jurisdictions are unwilling or unable to act genuinely, thereby addressing collective action failures in ending impunity while preserving a formal role for sovereign legal systems.
% TRANSFER_FUNCTION: Moves prosecutorial authority and jurisdictional primacy from national legal systems to an international body under specific conditions of state failure or absence, and transfers the costs of trial, detention, sovereignty cession, and reputational harm from accused individuals, referred non-party states, and disproportionately targeted regions to the international justice mechanism.
% ABSENT_VOICES: Populations of powerful non-party states whose nationals are shielded from ICC jurisdiction by geopolitical non-ratification and UNSC veto; accused individuals from non-referred non-party states who would face prosecution under a universalist reading but are protected by the hybrid reading's consent-based limits; state parties seeking withdrawal without legacy jurisdiction exposure for crimes committed during membership.
% DISAPPEARANCE_RATIONALE: If the hybrid complementarity mechanism vanished overnight, state parties would revert to pure sovereign jurisdiction over atrocities, the UNSC would lose a referral tool for non-party situations, victim communities would lose the reparations and participation framework, and the OTP would cease to exist â the global architecture for international criminal justice would fragment back toward ad hoc tribunals and purely national courts.
% FOUNDING_PROBLEM: Endemic impunity for the most serious international crimes when national courts were unwilling or unable to prosecute due to bias, collapse, capture, or the absence of capable legal infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations and international law scholars outside the direct beneficiary set attest the problem remains live in conflict zones and authoritarian regimes. Conversely, the African Union and some non-party states attest that the mechanism has been captured by geopolitical selectivity and no longer solves the founding problem impartially. The UN Secretary-General has acknowledged both the continued need for accountability and the systemic cooperation deficits.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is substantial because the constraint systematically moves prosecutorial authority and individual liberty from national, state, and individual seats to the international level, with enforcement depending on state cooperation that tracks political power rather than purely legal criteria. Suppression (0.80) reflects the escalating diplomatic, legal, and institutional coercion required to secure state cooperation, arrest warrants, and surrender in the face of rising non-cooperation from targeted states and powerful actors. Theater ratio (0.45) captures the performative dimension: arrest warrants issued against high-profile uncooperative figures the Court cannot enforce, lengthy proceedings for detained individuals while major fugitives remain at large, and complementarity rhetoric that partially masks geopolitical selectivity. Accessibility collapse (0.55) is moderate-high because the Rome Statute has become the default institutional framework, partially collapsing alternative pathways such as ad hoc tribunals and purely national accountability, though hybrid and regional alternatives persist. Resistance (0.60) is significant, evidenced by African Union opposition, Burundi's withdrawal, US sanctions on ICC personnel, and Russian non-cooperation. The temporal series show rising extraction, theater, and suppression as the Court has expanded its docket into geopolitically sensitive situations where enforcement gaps and power asymmetries become more visible.
 *
 * PERSPECTIVAL GAP:
 *   The OTP and state parties experience the constraint as a genuine coordination mechanism for ending impunity; from accused individuals, referred non-party states, and disproportionately targeted African states, it operates as asymmetric extraction of sovereignty and liberty. The UNSC experiences it as an arbitrage tool. The engine computes this divergence from the structural data: identical nominal power levels (e.g., state_parties vs referred_non_party_states) have radically different directionality due to treaty consent versus UNSC referral, and the powerful_non_party_states sit entirely outside the cost structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state_parties, victim_communities) sit near the low-d end: they invited the constraint and collect legitimacy and justice goods from it. The OTP sits as both agenda-setter and institutional beneficiary (low-to-mid d). Payers (accused_individuals, referred_non_party_states, african_states_collective) sit near high-d: they bear costs without proportional influence or, in the case of referred non-parties, without consent. Powerful non-party states sit at low-d excluded: they avoid costs entirely while shaping the constraint outward. The UNSC sits as arbitrageur: directing the constraint toward others while remaining shielded.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy and victim declarations, this could be misread as a rope (consensual treaty regime) or scaffold (transitional justice mechanism). The mandatrophy check reveals the founding problem â impunity â is contested in its current liveness, while the disappearance verdict is world_rearranges. The presence of non-consensual targets (referred non-party states) and trapped accused individuals prevents classification as pure coordination. The rising theater ratio and suppression trajectory signal that performative maintenance and coercive enforcement have partially supplanted the original coordination function, but genuine prosecutions continue, preventing pure snare classification. Hence tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_authority_stability,
    'Can a hybrid authority grounded simultaneously in treaty consent and universal aspiration remain stable, or does operational pressure force collapse toward either sovereigntist or universalist poles?',
    'Longitudinal analysis of state cooperation rates and UNSC referral patterns; if cooperation consistently tracks geopolitical alignment rather than legal criteria, the hybrid is unstable and may be a transitional scaffold.',
    'If unstable, the constraint is a scaffold rather than a permanent tangled rope; if stable, the hybrid reading is structurally coherent as a distinct commitment system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_authority_stability, conceptual, 'Whether the hybrid reading is a stable equilibrium or transitional.').

omega_variable(
    complementarity_as_cover,
    'Does the complementarity mechanism function as a genuine filter for national incapacity, or as a procedural cover for selective international intervention targeting weaker states?',
    'Quantitative analysis of admissibility decisions: ratio of cases deferred to national jurisdictions versus cases pursued; correlation between state power metrics and deferral outcomes.',
    'If complementarity is applied more rigorously against weak states, the coordination function is veneer over extraction; if applied impartially, the extraction is the necessary cost of residual jurisdiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_as_cover, empirical, 'Whether complementarity is impartial filter or selective cover.').

omega_variable(
    enforcement_as_extraction,
    'Is the constraint''s persistence driven by genuine state consent and perceived mutual benefit, or by coercion through diplomatic isolation, aid conditionality, and UNSC leverage?',
    'Examination of state party accession patterns pre- and post-ICC intervention threats; analysis of UNSC referral dynamics and bilateral pressure on non-cooperating states.',
    'If consent is heavily coerced, the constraint is closer to a snare for weaker states; if consent is autonomous, the coordination is genuine despite asymmetric costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_as_extraction, empirical, 'Whether state participation is consensual or coerced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(rome_tr_t4, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(rome_tr_t8, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(rome_tr_t12, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(rome_tr_t16, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(rome_tr_t24, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(rome_be_t4, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(rome_be_t8, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(rome_be_t12, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(rome_be_t16, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(rome_be_t24, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t0, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(rome_su_t4, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(rome_su_t8, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(rome_su_t12, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(rome_su_t16, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(rome_su_t20, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(rome_su_t24, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 24, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Rome Statute jurisdiction' conflates three structurally distinct readings: a universalist claim of transcendent mandate, a sovereigntist claim of strict consent, and this hybrid claim of residual universal authority operationally constrained by complementarity. Each reading has a different epsilon, beneficiary structure, and classification. They form a constraint family linked by shared kernel but independent classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
