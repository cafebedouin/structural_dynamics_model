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
 *   human_readable: NSL Democratic Enclosure Reading
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the democratic_enclosure_reading of
 *   the nsl_legal_text kernel. The National Security Law imposed on Hong Kong
 *   in 2020 is read here not as a security instrument but as a mechanism for
 *   the permanent closure of democratic space and the criminalization of
 *   dissent. The law's broad definitions of subversion, secession, terrorism,
 *   and collusion with foreign forces are interpreted functionally as tools
 *   to disqualify opposition, dismantle civil society, and capture the legal
 *   system. The Îµ-invariance principle requires this reading to be authored
 *   as a separate constraint from its sibling readings
 *   (sovereignty_restoration_reading and jurisdictional_capture_reading),
 *   because the beneficiary/victim structure and extractiveness differ
 *   structurally.
 *
 * KEY AGENTS:
 *   - Beijing/HK establishment (agenda_setter/institutional/arbitrage): Controls promulgation and enforcement; captures political authority and eliminates electoral competition.
 *   - Civil society activists (payer/powerless/trapped): Primary targets of NSL prosecutions for organizing and advocacy.
 *   - Press journalists (payer/moderate/trapped): Professional activity criminalized; newsroom raids and editor arrests.
 *   - Pro-democracy opposition (payer/moderate/trapped): Disqualified, arrested, or exiled; democratic infrastructure dismantled.
 *   - Ordinary HK residents (payer/moderate/constrained): Bear diffuse costs of democratic closure and chilled speech.
 *   - International human rights observers (observer/institutional/analytical): External analytical seat documenting divergence from international norms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.92).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.93).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.93).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "NSL Democratic Enclosure Reading").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '51c00865-ed18-47b0-97f7-0591a7344af3').
narrative_ontology:cs_kernel_codification('51c00865-ed18-47b0-97f7-0591a7344af3', formalized).
narrative_ontology:cs_authority_grounding('51c00865-ed18-47b0-97f7-0591a7344af3', extraction).
narrative_ontology:cs_interpretation_layer_present('51c00865-ed18-47b0-97f7-0591a7344af3').
narrative_ontology:cs_reading_relation('51c00865-ed18-47b0-97f7-0591a7344af3', nsl_legal_text__sovereignty_restoration_reading, forecloses).
narrative_ontology:cs_reading_relation('51c00865-ed18-47b0-97f7-0591a7344af3', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('51c00865-ed18-47b0-97f7-0591a7344af3', foundational, dissent_is_constitutional_right_not_subversion).
narrative_ontology:cs_axiom_status(dissent_is_constitutional_right_not_subversion, holdable).
narrative_ontology:cs_axiom_grounding('51c00865-ed18-47b0-97f7-0591a7344af3', dissent_is_constitutional_right_not_subversion, deontological).
narrative_ontology:cs_axiom('51c00865-ed18-47b0-97f7-0591a7344af3', foundational, national_security_law_as_democratic_closure).
narrative_ontology:cs_axiom_status(national_security_law_as_democratic_closure, holdable).
narrative_ontology:cs_axiom_grounding('51c00865-ed18-47b0-97f7-0591a7344af3', national_security_law_as_democratic_closure, empirically_contingent).
narrative_ontology:cs_reference_frame('51c00865-ed18-47b0-97f7-0591a7344af3', liberal_constitutional_order_with_dissent_protections).
narrative_ontology:cs_drift_state('51c00865-ed18-47b0-97f7-0591a7344af3', contemporary_post_2020, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('51c00865-ed18-47b0-97f7-0591a7344af3', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_hk_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_activists).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, press_journalists).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_democracy_opposition).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, ordinary_hk_residents).
narrative_ontology:constraint_vindicates(nsl_legal_text__democratic_enclosure_reading, national_security_supremacy_over_dissent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the promulgation, interpretation, and enforcement of the NSL. Defines national security broadly to encompass dissent and democratic advocacy. Uses the law to disqualify opposition, reshape legislative and electoral bodies, and criminalize previously protected political activity. Benefits from consolidated political control and elimination of viable electoral opposition.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_hk_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Previously operated within a protected space of advocacy and assembly. Now face arrest, pre-trial detention, and prosecution under NSL charges for organizing forums, publishing commentary, or participating in primaries. Exit means exile or complete silence; many lack the resources or documentation to leave.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, civil_society_activists, payer,
    powerless, biographical, trapped, local).

% Newsrooms have been raided, editors arrested, and outlets shuttered under NSL-related investigations. Reporting on opposition figures or advocating democracy is treated as subversion. Self-censorship is widespread but insufficient to guarantee safety. Professional identity is directly targeted by the constraint.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, press_journalists, payer,
    moderate, biographical, trapped, local).

% Elected legislators, district councillors, and party organizers have been disqualified, arrested, or forced into exile. The primary mechanism of democratic opposition has been criminalized. Those who remain face surveillance, asset freezes, and long pre-trial detention.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_democracy_opposition, payer,
    moderate, biographical, trapped, local).

% Benefited from a prior system with press freedom, judicial independence, and competitive elections. Now experience a chilled public sphere where political speech carries legal risk. Voting choices are constrained by candidate vetting. Exit options include emigration, which requires capital and foreign visas.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, ordinary_hk_residents, payer,
    moderate, biographical, constrained, local).

% Monitor and document NSL prosecutions, publish reports on fair trial violations, and advocate for contractual and diplomatic pressure. They do not experience the constraint directly but analyze its divergence from international law norms.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_human_rights_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__democratic_enclosure_reading, beijing_hk_establishment).
narrative_ontology:fixing_cost_class(nsl_legal_text__democratic_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The claimed coordination function is the protection of national security and restoration of public order after the 2019 unrest. In this reading, that claim is cover. Structurally, the arrangement coordinates political compliance by eliminating non-state centers of democratic authority and replacing competitive politics with vetted candidates.
% TRANSFER_FUNCTION: Moves political authority, civic space, and expressive freedom from opposition-aligned civil society, press, and the general public to the Beijing-HK establishment, using criminal law as the transfer mechanism.
% ABSENT_VOICES: Pro-democracy exiles, arrested activists in pre-trial detention, and shuttered media entities are structurally excluded from the public conversation about the law's legitimacy; their absence is enforced by the law itself.
% DISAPPEARANCE_RATIONALE: If the NSL and its enforcement apparatus vanished overnight, opposition parties would reorganize, press censorship would lift, civil society would remobilize, and the electoral system would revert to competitive contestation. The political economy of Hong Kong would rearrange around restored democratic channels.
% FOUNDING_PROBLEM: The 2019 protests and associated unrest, which the sovereignty-restoration reading frames as a breakdown of public order requiring security legislation; this reading frames the 'problem' as a pretext manufactured to justify pre-planned democratic closure.
% FOUNDING_PROBLEM_CORROBORATION: International human rights observers, academic legal scholars outside the Beijing beneficiary set, and exiled opposition figures attest that the 2019 unrest had subsided before the NSL was imposed and that the law's scope exceeds any security justification. The beneficiary set (Beijing/HK establishment) alone asserts the problem is live.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.92, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.92) is near-ceiling because the constraint extracts the entire democratic infrastructureâelectoral competition, press freedom, assembly rightsâand transfers it to the establishment. Suppression (0.93) is similarly high because persistence depends on active prosecution, pre-trial detention, and extraterritorial threats; alternatives (free elections, independent media) are structurally eliminated, not merely disadvantaged. Theater ratio (0.58) reflects significant performative enforcement: televised raids, staged confessions, and prosecutions designed to chill rather than merely punish. Accessibility collapse (0.88) captures the near-total disappearance of legal alternatives for dissent once the law is understood. Resistance (0.72) reflects sustained international condemnation, exile networks, and pre-imposition protests, though largely suppressed. Metrics are authored independently of the claimed snare classification; the engine measures the gap if any exists.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Beijing/HK establishment) experiences the constraint as a restoration of order and legitimate security coordination; the payer seats (activists, press, opposition, residents) experience it as pure extraction with no reciprocal benefit. The engine computes this divergence from structural data: the establishment has arbitrage-grade exit and institutional power, while targets are trapped at local scope with biographical horizons. The per-seat computed type should diverge sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive directionality: beijing_hk_establishment is the sole declared beneficiary (d near 0.0, subsidized by the constraint). All other named stakeholders are declared victims (d near 1.0, full targets). The international observer seat is analytical (d neutral). The structural derivation is straightforward and requires no override: the law's enforcement pattern concentrates extraction on powerless and moderate agents at local scope while the institutional beneficiary operates at national scope with mobile exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as snare prevents mislabeling this as coordination (rope) or transitional support (scaffold). The claimed security coordination function is structurally uncoupled from the extraction: the suppression of democratic speech does not solve a genuine collective-action problem for the governed, and the arrangement has no sunset. It is not a tangled rope because there is no genuine coordination function mixed with extraction in this readingâthe coordination story is cover. The dead founding problem (2019 unrest) combined with world_rearranges disappearance flags the constraint as a zombie snare: it persists beyond its nominal mandate through coercion and inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state enforcement capacity) or internalized (self-censorship, anticipatory obedience by civil society)?',
    'Compare prosecution rates to speech volume indices; if speech collapses without proportional increase in prosecutions, suppression is largely internalized.',
    'If internalized, the constraint''s effective extraction exceeds the structural measureâthe constraint operates through distributed self-policing rather than centralized enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    kernel_reading_contest,
    'Does the NSL text inherently support the democratic enclosure reading, or is the enclosure produced by selective enforcement and interpretive guidance that a different reading could reverse?',
    'Comparative analysis of the statutory text against charge sheets and judicial interpretations; if the text''s breadth is necessary for the observed pattern, the reading is textually grounded; if prosecutions exceed textual authorization, the reading targets enforcement practice rather than the kernel itself.',
    'If the text is sufficient for enclosure, the kernel itself is the constraint and sibling readings are false summits; if enforcement practice drives the outcome, the kernel is ambiguous and the constraint is the enforcement apparatus layered upon it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether democratic enclosure is in the text or the practice').

omega_variable(
    genuine_security_coordination,
    'Is there any genuine national security coordination function that would survive if the democratic enclosure function were removed?',
    'Counterfactual analysis of a narrowed NSL that excludes speech and assembly crimes: would the remaining espionage and sabotage provisions satisfy a security need?',
    'If a narrower law suffices, the current breadth is extractive surplus; if not, the constraint has a non-extractive core that complicates the snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_security_coordination, conceptual, 'Whether a non-extractive security core exists').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nsl__tr_t8, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(nsl__tr_t16, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(nsl__tr_t32, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 32, 0.53).
narrative_ontology:measurement(nsl__tr_t40, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(nsl__tr_t48, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 48, 0.58).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(nsl__be_t8, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 8, 0.82).
narrative_ontology:measurement(nsl__be_t16, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 16, 0.85).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 24, 0.87).
narrative_ontology:measurement(nsl__be_t32, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 32, 0.89).
narrative_ontology:measurement(nsl__be_t40, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 40, 0.91).
narrative_ontology:measurement(nsl__be_t48, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 48, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(nsl__su_t8, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 8, 0.83).
narrative_ontology:measurement(nsl__su_t16, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 16, 0.85).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 24, 0.87).
narrative_ontology:measurement(nsl__su_t32, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 32, 0.89).
narrative_ontology:measurement(nsl__su_t40, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 40, 0.91).
narrative_ontology:measurement(nsl__su_t48, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 48, 0.93).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% This constraint is the democratic enclosure reading of the nsl_legal_text kernel, decomposed from the colloquial label 'National Security Law' per the Îµ-invariance principle. The kernel decomposes into three structurally distinct constraints: democratic_enclosure_reading (high extraction, victims are civil society), jurisdictional_capture_reading (common law erosion), and sovereignty_restoration_reading (legitimate security instrument). Each has distinct Îµ values, beneficiary/victim structures, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
