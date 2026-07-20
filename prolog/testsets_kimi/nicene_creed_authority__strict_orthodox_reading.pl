% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed Strict Orthodox Authority
 *   domain: systematic_theology/ecclesiology
 *
 * SUMMARY:
 *   This constraint instantiates the strict_orthodox_reading of the
 *   nicene_creed_authority kernel: the Niceno-Constantinopolitan Creed is
 *   treated as metaphysically binding divine law, deviation from which
 *   constitutes heresy warranting ecclesiastical and historically civil
 *   sanction. The magisterial hierarchy claims exclusive interpretive
 *   authority, deriving institutional power from this monopoly. Heterodox
 *   communities and lay interpreters bear the costs of exclusion and
 *   censorship. The constraint is claimed by its beneficiaries as necessary
 *   guardianship of apostolic truth; its victims experience it as arbitrary
 *   power enforcing a specific metaphysical ontology. This story authors the
 *   structural data for that divergence without reconciling it.
 *
 * KEY AGENTS:
 *   - Hierarchical clergy (institutional/agenda_setter+beneficiary): Administers and enforces the creed, derives interpretive monopoly and status
 *   - Ordinary believers (moderate/beneficiary): Receive liturgical coordination and communal identity, bear diffuse costs of constrained inquiry
 *   - Heterodox communities (organized/payer): Condemned theological minorities targeted by enforcement machinery
 *   - Lay interpreters (moderate/payer): Individual theological agents constrained by censorship and inquisitorial threat
 *   - Historical theologians (analytical/observer): Document the gap between transcendent claims and historical power structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.7).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.75).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed Strict Orthodox Authority").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic_theology/ecclesiology").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '7b84af86-2448-4935-a037-03c50599f3a3').
narrative_ontology:cs_kernel_codification('7b84af86-2448-4935-a037-03c50599f3a3', fixed_text).
narrative_ontology:cs_authority_grounding('7b84af86-2448-4935-a037-03c50599f3a3', lineage).
narrative_ontology:cs_interpretation_layer_present('7b84af86-2448-4935-a037-03c50599f3a3').
narrative_ontology:cs_reading_relation('7b84af86-2448-4935-a037-03c50599f3a3', nicene_creed_authority__symbolic_confessional_reading, forecloses).
narrative_ontology:cs_reading_relation('7b84af86-2448-4935-a037-03c50599f3a3', nicene_creed_authority__liturgical_habituation_reading, influences).
narrative_ontology:cs_axiom('7b84af86-2448-4935-a037-03c50599f3a3', foundational, creed_as_divinely_binding_metaphysics).
narrative_ontology:cs_axiom_status(creed_as_divinely_binding_metaphysics, holdable).
narrative_ontology:cs_axiom_grounding('7b84af86-2448-4935-a037-03c50599f3a3', creed_as_divinely_binding_metaphysics, theological).
narrative_ontology:cs_axiom('7b84af86-2448-4935-a037-03c50599f3a3', foundational, magisterium_as_exclusive_interpreter).
narrative_ontology:cs_axiom_status(magisterium_as_exclusive_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('7b84af86-2448-4935-a037-03c50599f3a3', magisterium_as_exclusive_interpreter, conventional).
narrative_ontology:cs_reference_frame('7b84af86-2448-4935-a037-03c50599f3a3', conciliar_orthodoxy_as_revealed_truth).
narrative_ontology:cs_drift_state('7b84af86-2448-4935-a037-03c50599f3a3', post_enlightenment_secularization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7b84af86-2448-4935-a037-03c50599f3a3', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, ordinary_believers).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the creed as binding divine law through conciliar decrees, magisterial teaching, and pastoral discipline. Derives institutional authority, employment, sacramental status, and social legitimacy from the monopoly on valid theological interpretation. Exit means abandoning a sacred vocation and an identity formed through years of formation and ordination.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, agenda_setter,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, beneficiary).

% Recite the creed in liturgy and are catechized to assent to its metaphysical claims as a condition of full communion. Receive the coordination benefit of a unified translocal religious identity, shared ritual language, and communal boundary clarity. They bear diffuse costs of constrained theological curiosity but are not the primary enforcement target.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, ordinary_believers, beneficiary,
    moderate, biographical, identity_locked, universal).

% Communities holding theological positions condemned by the magisterium (e.g., Arian, Monophysite, Reformational, or modern progressive dissent). Bear costs of excommunication, exclusion from sacraments, social ostracism, and historically state-backed punishment. Their theological difference is the direct object of the constraint's enforcement machinery.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    organized, generational, constrained, regional).

% Individual believers, scholars, or mystics who produce theological reasoning that exceeds magisterially approved boundaries. Subject to censorship, inquisitorial investigation, removal from academic or teaching posts, and ecclesiastical censure. Their intellectual and spiritual inquiry is the constrained resource.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    moderate, biographical, constrained, regional).

% Academic observers who study the creed's historical formation, textual development, and political function. Neither collect from nor pay into the constraint's operation. They document the gap between the creed's claims to transcendent authority and its embeddedness in specific imperial and ecclesiastical power structures.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, historical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:fixing_cost_class(nicene_creed_authority__strict_orthodox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, shared metaphysical and liturgical identity across geographically and culturally dispersed Christian communities, solving the coordination problem of theological fragmentation and communal boundary maintenance in a growing religion.
% TRANSFER_FUNCTION: Moves the authority to define valid Christian belief and practice from local communities, individual conscience, and heterodox traditions to the magisterial hierarchy, transferring institutional control, status, and interpretive monopoly to the clergy while extracting doctrinal assent and suppressing dissent from heterodox communities and lay interpreters.
% ABSENT_VOICES: Condemned heterodox communities expelled from conciliar and communion processes; lay interpreters whose theological insights fall outside magisterial boundaries; feminist, liberation, and postcolonial theologians who challenge the creed's androcentric and imperial frameworks; adherents of the symbolic confessional reading who treat the creed as non-binding witness rather than divine law.
% DISAPPEARANCE_RATIONALE: If the binding metaphysical authority of the creed vanished, the magisterium's exclusive claim to interpretive authority would collapse, heterodox communities would re-enter full communion or restructure the church along conciliar rather than magisterial lines, liturgical practice would fragment into local theologies, and the hierarchical clergy's primary theological grounding for institutional power would dissolve.
% FOUNDING_PROBLEM: Doctrinal fragmentation in the early church â competing Christologies (Arianism, Sabellianism, Apollinarianism, etc.) threatening both communal identity and political stability in the late Roman Empire â required a shared theological boundary marker to unify bishops, liturgy, and imperial policy.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians and non-confessional scholars attest the genuine political and ecclesial fragmentation of the fourth-century church as a historical problem. However, the claim that the specific Nicene solution remains the only viable resolution is attested primarily by the benefiting magisterial parties; modern ecumenical theologians, historical-critical scholars, and non-Christian historians outside the beneficiary set contest that the founding problem persists in its original form or that the Nicene arrangement remains the appropriate response.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.7, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70) is high because the creed transfers interpretive monopoly to the clergy and actively suppresses competing theological voices. Suppression (0.75) is high due to centuries of state-church enforcement, excommunication, and inquisition, though it has moderated in modern secular contexts. Theater ratio (0.45) is moderate-to-high: while the creed coordinates genuine liturgical unity across cultures, a substantial share of its maintenance is performative enforcement of metaphysical assent rather than organic consensus. Accessibility collapse (0.65) is moderately high â within the orthodox frame, heresy becomes cognitively costly and socially unthinkable, though historical alternatives remain visible to outsiders. Resistance (0.50) reflects persistent heterodox movements, reformational challenges, and modern criticism that the constraint has actively suppressed. The temporal series trace enforcement intensification under imperial Christendom, peak coercion in the confessional state, and partial decay under secularization without full dissolution of magisterial authority.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterial seat, the constraint is a necessary Rope â without it, Christianity dissolves into incomprehensible fragmentation and heresy. From the heterodox seat, it is a Snare â a historically contingent power arrangement dressing imperial theology in divine authority to exclude legitimate difference. From the lay interpreter seat, it is a cognitive constraint that extracts intellectual freedom in exchange for communal safety. The engine computes each seat's type from these structural positions; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Hierarchical clergy sit near the full-beneficiary end (low d): the constraint subsidizes their authority, employment, and institutional control. Ordinary believers sit near neutral-to-beneficiary (low-mid d): they receive coordination goods (shared identity, liturgical coherence) without bearing direct extraction costs. Heterodox communities and lay interpreters sit near the full-target end (high d): the constraint extracts voice, status, safety, and intellectual freedom from them. The directionality derivation from beneficiary/victim declarations plus exit modulation produces this spread without override.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as Tangled Rope prevents the false binary of pure coordination vs. pure extraction. The creed genuinely solves a coordination problem â theological fragmentation across a translocal religion requires shared boundary markers â so Snare would mislabel the real communal goods produced. However, Rope would mislabel the asymmetric capture of interpretive authority by the clergy and the active suppression of heterodox and lay voices. The enforcement requirement and the victim set demonstrate that the coordination is parasitized by extraction. Mandatrophy is contested: the magisterium asserts the founding problem (heresy) is eternally live; historical critics argue the arrangement outlived the 4th-century crisis and now primarily serves institutional self-preservation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transcendent_vs_constructed_authority,
    'Is the creed''s binding authority a direct response to divine revelation and metaphysical reality, or a construction of imperial and ecclesiastical power projected backward as eternal truth?',
    'Comparative historical analysis of conciliar politics, imperial involvement in creedal formulation, and the diversity of pre-Nicene Christian theologies; archaeological and textual evidence of the creed''s editorial history.',
    'If the authority is substantially constructed, the constraint''s claim to Mountain-like status collapses and it reclassifies as either Snare or Tangled Rope with a false-summit origin; if genuinely transcendent, the high extraction may be reinterpreted as necessary protective scaffolding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transcendent_vs_constructed_authority, conceptual, 'Whether the creed''s authority derives from divine transcendence or human power construction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of heresy maintained primarily by external enforcement (excommunication, state violence, employment exclusion) or by internalized cognitive conformity (believers self-policing assent, heresy becoming unthinkable)?',
    'Post-exit trajectory analysis: observing whether dissent increases or remains suppressed after external enforcement mechanisms are removed (e.g., in post-Christian secular societies where church courts lack civil power).',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates as a deeper identity lock than visible enforcement suggests; if purely external, suppression may decay rapidly with secularization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in creedal enforcement').

omega_variable(
    coordination_extraction_separability,
    'Can the creed''s genuine coordination function â providing a shared liturgical and theological language across dispersed communities â be separated from its magisterial extraction function, or are they structurally fused in the strict orthodox reading?',
    'Comparative analysis of communions that maintain Nicene liturgical use while rejecting magisterial interpretive monopoly (e.g., some conciliar or progressive Catholic movements, or non-magisterial orthodoxies).',
    'If separable, the extraction is a contingent accretion and the constraint could in principle decouple into Rope coordination; if fused, the coordination is inseparable from the extraction and Tangled Rope remains the correct classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable in this reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 325, 0.1).
narrative_ontology:measurement(nice_tr_t600, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 600, 0.25).
narrative_ontology:measurement(nice_tr_t900, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 900, 0.38).
narrative_ontology:measurement(nice_tr_t1200, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1200, 0.48).
narrative_ontology:measurement(nice_tr_t1500, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1500, 0.55).
narrative_ontology:measurement(nice_tr_t1800, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1800, 0.5).
narrative_ontology:measurement(nice_tr_t2025, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 325, 0.4).
narrative_ontology:measurement(nice_be_t600, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 600, 0.58).
narrative_ontology:measurement(nice_be_t900, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 900, 0.7).
narrative_ontology:measurement(nice_be_t1200, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1200, 0.78).
narrative_ontology:measurement(nice_be_t1500, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1500, 0.82).
narrative_ontology:measurement(nice_be_t1800, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1800, 0.74).
narrative_ontology:measurement(nice_be_t2025, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 2025, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 325, 0.3).
narrative_ontology:measurement(nice_su_t600, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 600, 0.75).
narrative_ontology:measurement(nice_su_t900, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 900, 0.88).
narrative_ontology:measurement(nice_su_t1200, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1200, 0.92).
narrative_ontology:measurement(nice_su_t1500, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1500, 0.9).
narrative_ontology:measurement(nice_su_t1800, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1800, 0.68).
narrative_ontology:measurement(nice_su_t2025, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Nicene Creed authority' conflates three structurally distinct constraints. This story isolates the strict_orthodox_reading, which treats the creed as metaphysically binding divine law enforced by the magisterium. Its siblings are the symbolic_confessional_reading (non-binding historical witness) and the liturgical_habituation_reading (performance-based identity independent of assent). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They form a constraint family linked by shared kernel provenance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
