% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Orthodox Restitution Claim on Hagia Sophia Substrate
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   The Orthodox restitution reading claims Hagia Sophia's legitimacy derives
 *   exclusively from its 537 CE founding as a Christian cathedral, demanding
 *   return to Orthodox ecclesiastical control or neutral status. This reading
 *   is one of three contesting the Hagia Sophia substrate kernel. It has no
 *   enforcement mechanism — Turkey exercises full sovereign control — but
 *   generates persistent diplomatic friction and symbolic mobilization. The
 *   claim functions as a piton: a former coordination structure (Byzantine
 *   ecclesiastical authority) that atrophied in 1453, persists as
 *   performative grievance, and is maintained theatrically by diaspora and
 *   state actors who benefit from its symbolic capital.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.15).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.25).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, piton).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Orthodox Restitution Claim on Hagia Sophia Substrate").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage/sovereignty/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, '2b129a1f-8781-471a-bcb6-b8bba62f99a6').
narrative_ontology:cs_kernel_codification('2b129a1f-8781-471a-bcb6-b8bba62f99a6', fixed_text).
narrative_ontology:cs_authority_grounding('2b129a1f-8781-471a-bcb6-b8bba62f99a6', lineage).
narrative_ontology:cs_interpretation_layer_present('2b129a1f-8781-471a-bcb6-b8bba62f99a6').
narrative_ontology:cs_reading_relation('2b129a1f-8781-471a-bcb6-b8bba62f99a6', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b129a1f-8781-471a-bcb6-b8bba62f99a6', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('2b129a1f-8781-471a-bcb6-b8bba62f99a6', foundational, christian_founding_primacy).
narrative_ontology:cs_axiom_status(christian_founding_primacy, holdable).
narrative_ontology:cs_axiom_grounding('2b129a1f-8781-471a-bcb6-b8bba62f99a6', christian_founding_primacy, deontological).
narrative_ontology:cs_axiom('2b129a1f-8781-471a-bcb6-b8bba62f99a6', foundational, byzantine_ecclesiastical_continuity).
narrative_ontology:cs_axiom_status(byzantine_ecclesiastical_continuity, holdable).
narrative_ontology:cs_axiom_grounding('2b129a1f-8781-471a-bcb6-b8bba62f99a6', byzantine_ecclesiastical_continuity, deontological).
narrative_ontology:cs_reference_frame('2b129a1f-8781-471a-bcb6-b8bba62f99a6', byzantine_ecclesiastical_authority_537).
narrative_ontology:cs_drift_state('2b129a1f-8781-471a-bcb6-b8bba62f99a6', post_2020_reconversion, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('2b129a1f-8781-471a-bcb6-b8bba62f99a6', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Global Orthodox communities (Greek, Russian, Serbian, Romanian, etc.) treat the restitution claim as a symbolic and spiritual anchor. The claim sustains collective identity and mobilizes diplomatic attention, but no constituency can enforce return. Their exit from the claim would mean abandoning a core grievance narrative.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    organized, generational, constrained, global).

% The Greek state advances the restitution claim in bilateral and multilateral forums (UNESCO, EU, Council of Europe) as diplomatic leverage against Turkey. It does not expect material return but uses the claim to frame Turkey as a violator of cultural heritage norms. The claim is a tool, not a terminal goal.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, agenda_setter).

% The Turkish state bears the diplomatic cost of the claim: recurring UNESCO scrutiny, European Parliament resolutions, and reputational friction in NATO/EU contexts. It cannot exit the claim without conceding the substrate's contested status, which would undermine the 1453 conquest narrative and waqf continuity.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty, payer,
    institutional, generational, trapped, national).

% Continuous Islamic worship since 1453 (interrupted only 1934–2020) is the living substrate of the site. The restitution claim treats this continuity as an interruption to be corrected, symbolically re-interrupting it. The Muslim faithful and waqf administrators are identity-locked to the site's Islamic status; exit would mean abandoning 570 years of sacralized presence.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity, payer,
    organized, generational, identity_locked, global).

% Monitors the site's conservation under the 1985 World Heritage inscription. Receives state party reports from Turkey and reactive monitoring missions. The restitution claim appears as a risk factor for outstanding universal value; UNESCO's mandate is conservation, not adjudication of sovereignty or religious primacy.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, unesco_world_heritage_committee, observer,
    institutional, generational, analytical, global).

% The Patriarchate, physically located in Istanbul, has the strongest institutional claim to Orthodox ecclesiastical authority over the site. It is excluded from operational decisions (the 2020 reconversion was a Turkish state act). Its voice is filtered through Turkish state permission; it cannot independently advance restitution without risking its remaining institutional position in Turkey.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate_constantinople, excluded,
    moderate, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The claim coordinates a transnational Orthodox grievance narrative and gives the Greek state a persistent diplomatic instrument vis-à-vis Turkey. It sustains a shared object of mobilization across diaspora communities and state actors without requiring material implementation.
% TRANSFER_FUNCTION: Transfers symbolic capital and diplomatic attention from Turkish sovereignty and Islamic continuity to Orthodox diaspora identity and Greek diplomatic leverage. No material resources flow; the transfer is reputational and normative.
% ABSENT_VOICES: The Ecumenical Patriarchate of Constantinople — the institutional heir to the Byzantine church — is physically present but politically excluded from the claim's advancement. Local Muslim worshippers in Istanbul who experience the site as living mosque, not monument, are absent from the restitution framing. Turkish civil society voices that might support a shared or neutral stewardship model are not represented in the binary claim.
% DISAPPEARANCE_RATIONALE: If the restitution claim vanished overnight, the material status of Hagia Sophia (functioning mosque, World Heritage site, Turkish sovereign territory) would not change. The claim has no enforcement pathway and no material implementation mechanism. The diplomatic friction it generates would diminish, but the underlying sovereignty and worship arrangements would persist.
% FOUNDING_PROBLEM: The 1453 conversion and 1934 secularization are framed as historical injustices that severed the site from its founding Christian ecclesiastical purpose. The arrangement was built to maintain a permanent normative claim that the substrate's legitimacy originates in its 537 consecration as a cathedral, not in conquest or secular decree.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox hierarchs and Greek state officials attest the founding injustice remains live. Turkish state, Diyanet (Presidency of Religious Affairs), and UNESCO attest the founding problem is resolved by the 1453 conquest, 1923 Lausanne Treaty, 1934 secularization, and 2020 reconversion — each a sovereign act. Independent legal scholars (e.g., Francesco Francioni, Ana Filipa Vrdoljak) note the claim has no standing in international law; the corroboration split maps exactly to the beneficiary/victim divide.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, world_unchanged).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).
:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the claim extracts no material resources and has no implementation pathway — its 'extraction' is purely reputational, imposing diplomatic costs on Turkey. Suppression is low (0.25) because the claim does not actively coerce; it operates through normative pressure in international forums. Theater ratio is high (0.55) because the claim's primary function is performative: sustaining grievance narrative and diplomatic leverage, not achieving restitution. Accessibility collapse is moderate (0.35) — alternative framings (Islamic sovereignty, universal heritage) remain live and institutionally entrenched. Resistance is moderate (0.45) — the claim faces active counter-narratives from Turkish state, Diyanet, and UNESCO.
 *
 * PERSPECTIVAL GAP:
 *   From the Orthodox diaspora seat, the claim is a sacred obligation (coordination of memory). From the Greek state seat, it is a diplomatic instrument (coordination of pressure). From the Turkish sovereignty seat, it is an external interference (extraction of reputational capital). From the Islamic worship seat, it is a denial of living continuity (erasure). The engine computes these divergences from the structural data; the claim's piton classification reflects that no seat experiences it as functional coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Eastern Orthodox diaspora and Greek state are beneficiaries (d ~0.2–0.3): they collect symbolic capital and diplomatic leverage without bearing enforcement costs. Turkish sovereignty and Islamic worship continuity are victims/payers (d ~0.7–0.8): they bear diplomatic costs and symbolic erasure without consent. The Ecumenical Patriarchate is excluded (d ~0.5): it has the strongest institutional claim but is structurally silenced by Turkish state permission requirements. UNESCO is analytical observer (d ~0.5). The claim's dormancy means directionality is almost entirely symbolic — no material transfer occurs.
 *
 * MANDATROPHY ANALYSIS:
 *   The claim's founding mandate (restore Byzantine ecclesiastical authority) died in 1453. The arrangement persists as a zombie constraint — maintained by beneficiaries who profit from its non-resolution (diaspora identity, Greek diplomatic leverage). Mandatrophy is resolved: the mandate is dead, the constraint persists as theater. The piton classification captures this: atrophied function, theatrical maintenance, no concentrated beneficiary capturing material extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_implementation_pathway,
    'Could any conceivable geopolitical shift create a material implementation pathway for Orthodox restitution, transforming the claim from symbolic to extractive?',
    'Scenario analysis of Turkey-EU relations, NATO cohesion, and domestic Turkish politics; historical analogies (e.g., Hagia Irene, Chora Church).',
    'If a pathway emerged, extractiveness would rise sharply and the constraint would reclassify from piton to snare (active extraction with enforcement). Current low ε depends entirely on practical dormancy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(material_implementation_pathway, empirical, 'Whether the claim''s dormancy is permanent or contingent on current geopolitical equilibrium.').

omega_variable(
    patriarchate_agency_ambiguity,
    'Does the Ecumenical Patriarchate genuinely support the restitution claim, or is its silence coerced by Turkish state pressure?',
    'Analysis of Patriarchal encyclicals, private communications (if disclosed), and comparative behavior on other contested sites (e.g., Hagia Irene, Chora).',
    'If the Patriarchate is a coerced non-participant, the ''excluded'' stakeholder is actually a suppressed victim, raising suppression and shifting the constraint toward snare. If it quietly acquiesces, the claim is diaspora/state theater without ecclesiastical backing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patriarchate_agency_ambiguity, conceptual, 'Whether the institutional heir to Byzantine authority is a silent beneficiary or a silenced victim of the restitution claim.').

omega_variable(
    symbolic_extraction_nature,
    'Is the diplomatic/reputational cost imposed on Turkey by this claim properly modeled as extraction, or is it the normal friction of contested heritage?',
    'Comparative analysis of UNESCO reactive monitoring frequency and diplomatic resolutions for other contested World Heritage sites (Jerusalem Old City, Kosovo monasteries, Palmyra).',
    'If the cost is baseline heritage politics, extractiveness is overestimated and the constraint is closer to rope (coordination of grievance). If the cost is disproportionate and instrumentally imposed, extractiveness is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_extraction_nature, empirical, 'Whether the claim''s diplomatic externalities constitute extraction or normal multilateral friction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 1934, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1934, 0.2).
narrative_ontology:measurement(hagi_tr_t1960, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(hagi_tr_t1985, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1985, 0.45).
narrative_ontology:measurement(hagi_tr_t2000, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(hagi_tr_t2010, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2010, 0.52).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2020, 0.55).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1934, 0.05).
narrative_ontology:measurement(hagi_be_t1960, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1960, 0.08).
narrative_ontology:measurement(hagi_be_t1985, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1985, 0.12).
narrative_ontology:measurement(hagi_be_t2000, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(hagi_be_t2010, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1934, 0.1).
narrative_ontology:measurement(hagi_su_t1960, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1960, 0.15).
narrative_ontology:measurement(hagi_su_t1985, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1985, 0.2).
narrative_ontology:measurement(hagi_su_t2000, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2000, 0.22).
narrative_ontology:measurement(hagi_su_t2010, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2010, 0.24).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2020, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__orthodox_restitution_reading, 0.08).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This reading is one of three in the Hagia Sophia substrate constraint family. The kernel 'hagia_sophia_substrate' decomposes into three structurally distinct constraints with different beneficiary/victim structures and extractiveness profiles. This reading (orthodox_restitution) has low material ε but high symbolic generativity; the Islamic sovereignty reading has high material ε (active enforcement, waqf revenue) and low symbolic generativity; the universal heritage reading has near-zero ε but high institutional capture by UNESCO bureaucracy. All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__orthodox_restitution_reading, institutional, 0.25).
constraint_indexing:directionality_override(hagia_sophia_substrate__orthodox_restitution_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
