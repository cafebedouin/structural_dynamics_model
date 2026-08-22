% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Catastrophe Memory Survival via Hybrid Symbolic-Competence Encoding
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes catastrophe-survivor ritual as operating on
 *   unified dual registers: symbolic boundary-maintenance (identity
 *   continuity, historical trauma remembrance) AND embedded practical
 *   knowledge (survival protocols, resource management, adaptation
 *   heuristics). The constraint is generated from the
 *   HYBRID_ENCODING_READING, which asserts that ritual's survival mechanism
 *   depends on BOTH registers remaining fused in performance—separation into
 *   'religion' and 'practical training' is an analytical imposition that
 *   destroys the constraint's efficacy. The claim and metrics are
 *   intentionally independent: CLAIMED as rope (real coordination solving the
 *   dual problem), while AUTHORED metrics show extractiveness rising from
 *   0.18 (early survival phase) to 0.42 (institutional analysis phase) as
 *   external analysts impose binary classification, suppressing the hybrid
 *   reality. This divergence is the signal the engine measures—a rope under
 *   analytical pressure toward snare classification.
 *
 * KEY AGENTS:
 *   - survivor_communities: primary beneficiary (identity-locked, generational time horizon) — maintain ritual as unified encoding mechanism
 *   - transmission_elders: secondary beneficiary (moderate power, identity-locked) — hold both symbolic narratives and embedded protocols; face pressure to bifurcate
 *   - external_analysts: payer (institutional power, mobile exit) — impose binary classification, suppressing the hybrid register
 *   - diaspora_youth: beneficiary with constrained access (constrained exit, biographical horizon) — inherit both registers but receive bifurcated instruction
 *   - institutional_religious_authority: excluded (constrained exit) — would participate in symbolic register but lack authority over competence register
 *   - policy_makers: observer (analytical seat) — design memory infrastructure based on separated models
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.28).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Catastrophe Memory Survival via Hybrid Symbolic-Competence Encoding").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '5d291018-e76a-4576-bbcb-d643096270ff').
narrative_ontology:cs_kernel_codification('5d291018-e76a-4576-bbcb-d643096270ff', implicit).
narrative_ontology:cs_authority_grounding('5d291018-e76a-4576-bbcb-d643096270ff', practice).
narrative_ontology:cs_interpretation_layer_present('5d291018-e76a-4576-bbcb-d643096270ff').
narrative_ontology:cs_reading_relation('5d291018-e76a-4576-bbcb-d643096270ff', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d291018-e76a-4576-bbcb-d643096270ff', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('5d291018-e76a-4576-bbcb-d643096270ff', foundational, dual_register_inseparability).
narrative_ontology:cs_axiom_status(dual_register_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('5d291018-e76a-4576-bbcb-d643096270ff', dual_register_inseparability, instrumental).
narrative_ontology:cs_axiom('5d291018-e76a-4576-bbcb-d643096270ff', foundational, unified_performance_necessity).
narrative_ontology:cs_axiom_status(unified_performance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('5d291018-e76a-4576-bbcb-d643096270ff', unified_performance_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('5d291018-e76a-4576-bbcb-d643096270ff', unified_ritual_performance_post_catastrophe).
narrative_ontology:cs_drift_state('5d291018-e76a-4576-bbcb-d643096270ff', contemporary_institutional_analysis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5d291018-e76a-4576-bbcb-d643096270ff', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, survivor_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, transmission_elders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, diaspora_youth).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, external_analysts).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__hybrid_encoding_reading, dual_register_necessity_thesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__hybrid_encoding_reading, symbolic_competence_inseparability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities maintaining ritual practice after catastrophe: Holocaust survivors' Passover adaptations, Armenian Apostolic Church post-genocide remembrance, Navajo Diné ceremonies post-Long Walk, Rwandan memorial rituals. They perform rituals that simultaneously encode historical trauma (symbolic register) and transmit survival protocols (competence register—food security, family reunion logistics, identification recovery). The ritual IS their boundary-maintenance and their practical transmission mechanism unified. Exit from ritual performance means identity-erasure and loss of embedded know-how.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, survivor_communities, beneficiary,
    organized, civilizational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, survivor_communities, agenda_setter).

% Ethnographers, historians, religious scholars, policy makers who observe and classify rituals. They impose a forcing binary: ritual is EITHER symbolic boundary-maintenance (validating the identity-preservation reading) OR embedded practical transmission (validating the competence reading). This binary classification demands communities articulate their practice in one epistemic register, suppressing the hybrid reality. Communities that insist on both simultaneously are read as incoherent or therapeutic theatre, not as genuine knowledge systems.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, external_analysts, payer,
    institutional, biographical, mobile, regional).

% Knowledge-keepers who hold both the symbolic narratives and the embedded survival protocols. They teach through ritual performance: a Passover Seder encodes both historical trauma remembrance AND distributed knowledge of food preservation, family signal-sharing, and diaspora navigation. The two registers are inseparable in their transmission practice—the metaphor IS the mnemonic. They face pressure to separate the registers for easier teaching to outsiders or youth in diaspora.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, transmission_elders, beneficiary,
    moderate, generational, identity_locked, local).

% Second and third-generation members of survivor communities learning ritual from elders. They inherit both registers but often receive instruction bifurcated: the symbolic meaning from formal religious education, the practical knowledge as disconnected 'culture' or family custom. They navigate the tension between unified ritual performance (experienced directly) and fragmented instruction (separated by institutional domains—religion vs. cultural anthropology vs. family oral history).
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, diaspora_youth, beneficiary,
    moderate, biographical, constrained, global).

% Formal religious hierarchies (denominational bodies, seminary curricula, authoritative texts) that often treat ritual as primarily symbolic or devotional, leaving embedded practical knowledge outside their interpretive frame. They would contribute to transmission but lack authority over the competence register and may actively suppress it as secular knowledge conflicting with doctrinal clarity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, institutional_religious_authority, excluded,
    institutional, generational, constrained, regional).

% Government bodies, museums, education ministries designing memory infrastructure, genocide education, and intergenerational trauma protocols. They design based on one-register models: either symbolic witnessing (monuments, memorial services) or practical resilience training (disaster preparedness, family communication protocols). A hybrid model would reshape what counts as legitimate cultural transmission and survival strategy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, policy_makers_memory_infrastructure, observer,
    powerful, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__hybrid_encoding_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__hybrid_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and transmits both symbolic identity-continuity (community boundary-maintenance, historical trauma remembrance, meaning-making) AND embedded practical knowledge (survival protocols, resource management, family reunion logistics, adaptive strategies) through unified ritual performance. The coordination solves the dual problem: how do survivor communities preserve identity integrity while ensuring practical survival knowledge reaches the next generation?
% TRANSFER_FUNCTION: Moves two asymmetric goods through ritual: (1) Symbolic capital—shared narrative, identity markers, historical continuity—flows to all participants; (2) Practical embedded knowledge—timing protocols, signaling systems, adaptation heuristics—flows selectively to those with relational authority to transmit (elders to youth, initiated to initiated). Both flows depend on ritual performance remaining unified and unparsed.
% ABSENT_VOICES: Non-survivors and external institutional authorities (academic disciplines, religious hierarchies, policy frameworks) are partially excluded because the ritual's dual-register nature is invisible to frameworks that require categorical purity. Theorists demanding separation of symbolic from practical (for clarity, rigor, or institutional coherence) are structurally unable to validate the hybrid form without reframing their own disciplinary categories.
% DISAPPEARANCE_RATIONALE: If the unified ritual practice disappeared (replaced by separated symbolic ceremonies + secular practical training), survivor communities would lose both identity continuity (symbolic register becomes decontextualized theater) and practical transmission efficacy (competence becomes abstracted, divorced from emotional-narrative embedding that makes it memorable and binding). The hybrid form is not decoration—it is the mechanism that makes both registers stick.
% FOUNDING_PROBLEM: After catastrophic violence, survivor communities must simultaneously preserve identity continuity (symbolic/spiritual need) and transmit survival knowledge (practical/physical need). Separation into 'religion' and 'practical training' was not available during catastrophe—the community learned and taught through unified practice. Post-catastrophe, that unified practice IS what survives and what works.
% FOUNDING_PROBLEM_CORROBORATION: Survivor communities themselves attest the founding problem is live: repeated witness testimony from Holocaust survivor families, Armenian Apostolic Church communities, Navajo Nation, Rwandan memorial communities that ritual IS their survival mechanism and identity anchor. Cognitive science (memory studies) corroborates that emotion-laden narrative encoding makes practical knowledge more durable than abstract instruction alone. Ethnographic evidence from Livingston, Nora, Confino, and Landau documents the unified register structure across multiple catastrophe traditions. External analysts who demand separation are themselves testifying to the founding problem by raising it as a problem.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).
:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.18 → 0.42) because institutional pressure for categorical clarity increases: academia demands epistemological separation, policy frameworks bifurcate 'religion' and 'practical training', formal religious instruction divorces symbolic meaning from embedded know-how. Theater rises (0.08 → 0.38) as ritual performance becomes increasingly performative for external audiences—younger generations learn ritual stripped of practical context, perform it symbolically for institutional validation (museums, memorial services, academic study) rather than as lived transmission of dual knowledge. Suppression rises (0.08 → 0.28) as analytical frameworks actively demand the hybrid register justify itself in separated terms—communities cannot articulate 'ritual IS both symbolic and practical' within institutional languages that require purity. Accessibility_collapse stays moderate (0.62) because the dual-register knowledge is embedded, not accessible to those outside the community, yet alternatives (formal religion education + secular training) are widely available and actively promoted. Resistance (0.45) reflects that communities continue ritual practice despite institutional pressure, but resistance is diffuse (no organized counter-movement, mostly quiet maintenance) rather than organized opposition. The shared time grid tracks one narrative: the gradual institutional extraction of the hybrid form into separated analytical categories.
 *
 * PERSPECTIVAL GAP:
 *   Survivor communities and transmission elders perceive the constraint as rope: genuine coordination solving the dual problem, with ritual performance as the mechanism that makes both symbolic and practical transmission work. External analysts perceive it as either pure symbolic (identity-preservation reading) or pure competence-transmission (competence-transmission reading)—the hybrid framing is incoherent within disciplinary categories. Policy makers perceive it as coordination failure requiring bifurcation into proper religion + proper training. The engine computes these divergent d values from the structural data: communities are beneficiaries with identity-locked exit (d near 0.0); analysts are payers imposing classification (d near 1.0, extracting the ability to articulate hybrid knowledge); youth are beneficiaries with constrained exit but limited authority to maintain the unified form. The perspectival gap IS the structure the engine measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Survivor communities benefit from unified ritual (identity continuity, embedded knowledge transmission, boundary-maintenance, collective meaning-making)—they are the primary beneficiary seat, d approaches 0.0. Transmission elders also benefit but face pressure to bifurcate their knowledge for institutional audiences—secondary beneficiary with higher d due to constrained articulation. Diaspora youth inherit benefit but receive bifurcated instruction, fragmenting the original unified form—secondary beneficiary with higher d due to compromised transmission. External analysts impose binary classification, treating hybrid articulation as incoherent or evasive—they are payers in the structural sense: they extract the form by forcing it into categorical separation. The institutional suppression required to maintain analytical separation is the source of rising suppression_requirement in the measurements: communities must increasingly hide or defend the hybrid nature against institutional pressure to separate and clarify. Identity-lock for survivor communities means exit from ritual performance means cultural death; this creates the asymmetry between beneficiary communities (trapped in the arrangement, not because it extracts from them but because leaving means identity-erasure) and analysts (mobile, able to shift frameworks, not bound to the practice).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dual survival need post-catastrophe) is live: survivors continue to experience the need for both symbolic and practical transmission unified. The constraint (unified ritual) addresses this live founding problem directly and effectively—communities maintain it across generations despite institutional pressure. There is NO mandatrophy here—the constraint has not outlived its function. What EXISTS is analytical suppression: analysts whose disciplinary categories require separation read the constraint as outdated 'magical thinking' (confusing symbol with competence) rather than as sophisticated dual-register knowledge engineering. This analytical suppression is tracked in the rising extraction metrics: the constraint survives functionally but faces delegitimation, which creates pressure toward bifurcation (institutional 'reform' that would 'clarify' the constraint by separating it). The constraint should NOT be classified as piton (theatrical maintenance of lost function) because the function is demonstrably live. It SHOULD be classified as rope under pressure toward snare (as analytical authority demands submission to categorical separation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_register_necessity,
    'Is the symbolic-competence unification structurally necessary for survivor ritual efficacy, or are the registers functionally separable with different pedagogical presentation?',
    'Generational cognitive outcome studies comparing: (A) communities maintaining unified ritual; (B) communities where symbolic and practical elements are taught separately; measuring both identity continuity and practical survival knowledge retention across generations.',
    'If unified form is structurally necessary (practical knowledge loses retention/binding without emotional-narrative embedding, or symbolic meaning becomes theater without practical grounding), this reading''s rope classification stands. If separable, the hybrid form is psychological preference rather than functional requirement, reclassifying toward snare (extraction of unified form via analytical suppression even though separated form would work as well).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_register_necessity, empirical, 'Whether dual register unification is functionally necessary or analytically imposed.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression structural (external institutional pressure demanding categorical separation) or internalized (communities have come to doubt their own hybrid articulation and experience shame about ''mixing'' symbol and competence)?',
    'Post-institutional-pressure ethnography: contexts where external analytical pressure is absent (small insulated communities) versus contexts with high external pressure (communities in academic centers, policy oversight); measuring whether communities articulate dual registers confidently in low-pressure contexts.',
    'If internalized suppression is substantial, the constraint''s effective extraction is higher than measured suppression suggests—communities carry the suppression internally even when external pressure is reduced. Institutional remedies (validation from authority figures, reframing of dual registers as sophisticated knowledge) could reduce internalized component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Suppression mechanism: structural external pressure vs. internalized epistemic doubt').

omega_variable(
    reading_foreclosure_ambiguity,
    'Do the three sibling readings coexist as genuinely live alternatives held by different communities/interpreters, or does one reading logically foreclose the others within a coherent survivor-centered framework?',
    'Qualitative research: do survivor communities and elders actually hold all three readings as possible, or does lived practice commit them to one while other readings are imposed by external analysts? Does the hybrid reading emerge organically from survivor practice, or is it a researcher reconstruction?',
    'If all three coexist in live practice, the relations are coexists_with. If the hybrid reading is logically entailed by survivor practice and the other readings are analytical impositions, this reading influences (creates pressure on) the other readings but does not foreclose them—they remain live in academic discourse but not in community praxis. If one reading actually forecloses the others within survivor framework, that relation holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Relationship between sibling readings: coexistence or logical derivation from survivor praxis').

omega_variable(
    identity_lock_escape_route,
    'For diaspora youth inheriting both registers but receiving bifurcated instruction, is the identity-lock escape route genuinely closed, or do they have the option to consciously reintegrate the registers through deliberate study (academic interdisciplinary work, elder mentorship)?',
    'Longitudinal tracking of diaspora cohorts: how many consciously reintegrate registers; what barriers prevent it; do successful reintegrators have access to mentorship, institutional permission, or time/resources?',
    'If reintegration is achievable through deliberate choice, diaspora youth exit_options upgrade from ''identity_locked'' toward ''constrained''—they can reclaim the unified form but at significant cost (time, social positioning). This would lower their d value and reshape the distribution of the constraint''s asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_escape_route, empirical, 'Whether diaspora youth can escape bifurcated instruction and consciously reintegrate dual registers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(cata_tr_t60, projected).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement_basis(cata_tr_t80, projected).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 60, 0.41).
narrative_ontology:measurement_basis(cata_be_t60, projected).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 80, 0.42).
narrative_ontology:measurement_basis(cata_be_t80, projected).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 60, 0.28).
narrative_ontology:measurement_basis(cata_su_t60, projected).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 80, 0.28).
narrative_ontology:measurement_basis(cata_su_t80, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__hybrid_encoding_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_survival kernel decomposes into three constraint readings, each with different ε and beneficiary/victim structures. The hybrid_encoding_reading (this file) asserts ritual's survival mechanism depends on unified symbolic-competence performance. The symbol_survival_reading isolates symbolic boundary-maintenance as primary and treats practical knowledge as secondary (victim set shifts to analysts forcing competence-reduction). The competence_transmission_reading isolates practical knowledge-encoding and treats symbolic dimensions as ornamental (victim set differs again). All three share the founding problem (survivor communities need both symbolic and practical transmission post-catastrophe) but offer different answers to whether the registers are separable. The three readings are linked via reading_relations (coexists_with or influences relations documented in cs_structure). Each reading should be generated as a complete, self-contained constraint story with its own omegas addressing the reading-specific ambiguities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
