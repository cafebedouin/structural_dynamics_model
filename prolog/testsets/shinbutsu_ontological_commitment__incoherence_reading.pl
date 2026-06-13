% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Shinbutsu-shugo Ontological Incoherence Institutional Framework
 *   domain: religious/institutional/political
 *
 * SUMMARY:
 *   Between the Tokugawa establishment (1603) and the Meiji restoration
 *   (1868), Japanese institutional religion operated under shinbutsu-shugo—a
 *   framework of coexistence between Buddhism and Shinto that explicitly
 *   tolerated ontological incoherence. Rather than asserting a stable
 *   metaphysics (kami-as-bodhisattvas, kami-as-separate-from-buddhas, or
 *   kami-as-manifestations-of-ultimate-reality), Tokugawa institutions
 *   preserved incoherence itself as a governing principle. This reading
 *   treats shinbutsu-shugo as a SNARE-FLAVORED TANGLED ROPE: genuine
 *   institutional coordination (fragmented religious authority prevented any
 *   single institution from threatening bakufu control) coupled with
 *   systematic extraction from those bearing the cognitive cost of
 *   incoherence. Theophilosophers seeking doctrinal coherence faced
 *   institutional suppression through marginalization rather than refutation.
 *   Practitioners inhabiting the framework carried inescapable identity-lock
 *   (abandoning practice = abandoning community and spiritual identity). The
 *   constraint collapsed catastrophically when the Meiji state dismantled it,
 *   revealing the prior incoherence had been politically maintained, not
 *   ontologically necessary.
 *
 * KEY AGENTS:
 *   - institutional_buddhism: Benefits from shrine networks, ritual authority, and revenue without doctrinal accountability (beneficiary + agenda_setter, institutional power).
 *   - shinto_shrine_networks: Maintains kami authority through Buddhist infrastructure while avoiding coherence costs (beneficiary, powerful).
 *   - tokugawa_bakufu: Sustains incoherence as a political technology keeping religious authority fragmented and state-dependent (agenda_setter, institutional, analytical position).
 *   - doctrinal_coherence_seekers: Bear theoretical suppression cost; coherent proposals threaten institutional utility of incoherence (payer, moderate power, identity_locked via professional commitment).
 *   - devotional_practitioners_identity_confused: Inhabit the incoherent framework without resolution; exit requires abandoning community and spiritual identity (payer, powerless, identity_locked).
 *   - meiji_state_apparatus: External observer seat; their state-building project identifies incoherence as obstacle and forcibly partitions traditions (institutional power, analytical position).
 *   - honji_suijaku_theorists: Develop coherence frameworks marginalized by both institutional Buddhism and Shinto (excluded, moderate power, trapped by institutional indifference).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.67).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-shugo Ontological Incoherence Institutional Framework").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious/institutional/political").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, '44ce1762-d1b2-4289-8b12-01f196a20b5e').
narrative_ontology:cs_kernel_codification('44ce1762-d1b2-4289-8b12-01f196a20b5e', distributed).
narrative_ontology:cs_authority_grounding('44ce1762-d1b2-4289-8b12-01f196a20b5e', extraction).
narrative_ontology:cs_interpretation_layer_present('44ce1762-d1b2-4289-8b12-01f196a20b5e').
narrative_ontology:cs_reading_relation('44ce1762-d1b2-4289-8b12-01f196a20b5e', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('44ce1762-d1b2-4289-8b12-01f196a20b5e', shinbutsu_ontological_commitment__partition_reading, influences).
narrative_ontology:cs_axiom('44ce1762-d1b2-4289-8b12-01f196a20b5e', foundational, no_stable_ontological_commitment_exists).
narrative_ontology:cs_axiom_status(no_stable_ontological_commitment_exists, holdable).
narrative_ontology:cs_axiom_grounding('44ce1762-d1b2-4289-8b12-01f196a20b5e', no_stable_ontological_commitment_exists, empirically_contingent).
narrative_ontology:cs_axiom('44ce1762-d1b2-4289-8b12-01f196a20b5e', foundational, incoherence_enabled_fragmented_authority).
narrative_ontology:cs_axiom_status(incoherence_enabled_fragmented_authority, holdable).
narrative_ontology:cs_axiom_grounding('44ce1762-d1b2-4289-8b12-01f196a20b5e', incoherence_enabled_fragmented_authority, instrumental).
narrative_ontology:cs_reference_frame('44ce1762-d1b2-4289-8b12-01f196a20b5e', tokugawa_dual_institution_coexistence_through_suppressed_coherence).
narrative_ontology:cs_drift_state('44ce1762-d1b2-4289-8b12-01f196a20b5e', meiji_era_shinbutsu_bunri, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('44ce1762-d1b2-4289-8b12-01f196a20b5e', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, institutional_buddhism).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, shinto_shrine_networks).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, doctrinal_coherence_seekers).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, devotional_practitioners_identity_confused).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading instantiates the constraint as INSTITUTIONALLY TOLERATED INCOHERENCE rather than as settled metaphysics (partition_reading) or integrated cosmology (syncretic_reading). Extractiveness begins moderate (0.45 at Tokugawa founding) and rises to 0.67 at interval end as the framework's political utility becomes locked in and institutional reliance on incoherence deepens. Theater rises steeply (0.35→0.78) as the constraint ages: the coordination story (fragmented authority prevents consolidation) never changes, but institutional actors increasingly rely on performative coherence-talk (honji-suijaku recitation, cosmological justifications) without implementing the coherence. Suppression requirement follows extraction (0.42→0.71) because maintaining incoherence requires active suppression of coherence-seeking work—institutional marginalization of systematic theology, blocking of alternative readings from institutional adoption, and (by period's end) Edo-period suppression of Confucian rationalism that might demand religious coherence. The catastrophic drop to zero at 1868 reflects Meiji dismantling: the constraint did not persist, transform, or degrade—it was forcibly severed and revealed as politically maintained, not inevitable. Time grid is intentionally shared across metrics—every measurement point is authored for every metric so temporal analysis has a single timeline. Cyclical dynamics are absent; this is monotonic extraction accumulation until abrupt institutional termination.
 *
 * PERSPECTIVAL GAP:
 *   The bakufu seat and the beneficiary seats (institutional Buddhism, shrine networks) experience the constraint as genuine coordination—a stable framework managing dual-institution coexistence. The payer seats experience it as enforced incoherence: coherence-seekers face institutional rejection of their resolution proposals; practitioners face irresolvable cognitive dissonance tied to identity. The Meiji observer seat reveals the prior gap was not just perceptual—it was structural: the incoherence was administratively maintained, not inevitable. The gap persists even at the end of the interval because institutional beneficiaries never acknowledge the extractive structure; recognition comes only from outside (Meiji state-builders, later historians). This is textbook suppression: the incoherence is known to institutional actors (they maintain it deliberately) but never stated as such in institutional discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional Buddhism and shrine networks are beneficiaries with institutional power and constrained exit (they maintain the framework by institutional choice, not external necessity)—their directionality is low (near 0.15). They benefit from the arrangement and sustain it at minimal cost to themselves. Coherence-seekers are victims with moderate power and identity-locked exit (professional identity committed to doctrinal work; rejection of their proposals leaves them trapped in a field that systematized their suppression)—directionality near 0.75. Practitioners are victims with powerless status and identity-locked exit (abandoning practice = abandoning community, family tradition, and spiritual identity constituted through the constraint)—directionality near 0.90. The bakufu is analytically positioned; their directionality is their analytical frame (0.5 symmetric), not a power seat. Honji-suijaku theorists are excluded with moderate power and trapped exit (their theoretical frameworks are never adopted, institutional indifference = institutional trapping)—directionality near 0.80. No overrides needed: the automatic derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to govern dual-institution coexistence without consolidation) was LIVE through the interval and DELIBERATELY MAINTAINED. It was never solved—it was instrumentally preserved. Meiji dismantling shows this was a choice: when political conditions shifted (nation-building required coherent state religions), the 'problem' was abolished by state fiat, not resolved by institutional Buddhism or Shinto. The mandatrophy marker is present: the founding problem's status is DEAD (not at the end, but revealed-as-dead once Meiji apparatus touched it), yet the constraint persisted until state intervention. This is the signal of a constraint that lost its function but was not dismantled because institutional beneficiaries profited from its maintenance. The incoherence reading specifically posits that the constraint's real function (political fragmentation) was never legitimate as a religious or ontological claim—it was a political technology dressed in theological language. Mandatrophy resolved: the constraint is best understood not as solving a live religious problem (because no such problem exists—the alternatives were always available), but as solving a political problem (fragmentation) that institutional beneficiaries benefited from maintaining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_necessity_vs_political_maintenance,
    'Was shinbutsu-shugo''s incoherence a necessary feature of the religious landscape, or a deliberately maintained political technology?',
    'Examination of honji-suijaku and hongi-suiji theoretical frameworks from the period: if sophisticated coherence theories existed but were marginalized by institutional action (blocked from adoption, not refuted), incoherence was politically maintained. If no coherence theories existed or all were dismissed as incoherent, incoherence was necessary.',
    'If politically maintained, shinbutsu-shugo is a SNARE: institutional beneficiaries suppressed coherence for political gain. If necessary, it is a ROPE: participants accepted incoherence as inevitable. The classification hinges on whether coherence was available but suppressed, or unavailable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_necessity_vs_political_maintenance, empirical, 'Whether incoherence was structurally necessary or politically chosen and defended.').

omega_variable(
    practitioner_cost_internalization,
    'Did practitioners internalize the incoherence (treating it as unproblematic or inevitable) or maintain awareness of the contradiction as a burden?',
    'Analysis of devotional literature, confession practices, doctrinal instruction texts, and oral tradition from village level: do practitioners frame the dual practice as mystically unified, functionally divided, or contradictory-but-unavoidable?',
    'If internalized, suppression is structural (embedded in identity formation); practitioners carry it after exit would be impossible. If burdensome-but-accepted, suppression is more easily dissolved through institutional change. The degree of internalization affects how quickly shinbutsu-bunri could succeed (complete suppression of practitioners'' prior identity would follow high internalization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practitioner_cost_internalization, empirical, 'Whether suppression of contradiction was internalized into practitioners'' identity or maintained as external constraint.').

omega_variable(
    bakufu_deliberation_vs_pragmatism,
    'Did the Tokugawa bakufu explicitly theorize and choose incoherence as a political technology, or did they encounter it as inherited practice and pragmatically preserve it?',
    'Examination of Tokugawa administrative documents, bakufu-issued religious edicts, and internal correspondence: do they show deliberate design of incoherence policy, or post-hoc justification of inherited practice?',
    'If deliberately designed, the beneficiary set is narrower (bakufu + institutional partners in the design), suppression is more intentional, and the constraint is more clearly extractive. If pragmatically inherited, the constraint emerges from institutional accident and reinforcement, less clearly designed. Either way, suppression requirement still rises over the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bakufu_deliberation_vs_pragmatism, empirical, 'Whether incoherence policy was deliberately designed or pragmatically inherited and reinforced.').

omega_variable(
    honji_suijaku_viability_as_institutional_framework,
    'Could honji-suijaku have been institutionalized as the framework if institutional beneficiaries (Buddhism, shrines, bakufu) had chosen to adopt it?',
    'Analysis of honji-suijaku theology to assess internal coherence and institutional compatibility: would adoption have required institutional restructuring or could it coexist with existing power distribution?',
    'If institutionalizable, coherence was available but suppressed—the incoherence reading is strengthened. If not institutionalizable without disrupting existing power, the partition reading is strengthened (institutions had no choice but incoherence given their need to preserve separate authority).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honji_suijaku_viability_as_institutional_framework, conceptual, 'Whether coherence frameworks were institutional alternatives or structurally incompatible with existing power distribution.').

omega_variable(
    reading_committer_kernel_instability,
    'Does the incoherence reading''s assertion of ''no stable ontological commitment'' foreclose the syncretic reading''s honji-suijaku integration claim within a single institutional framework?',
    'Framework compatibility test: if syncretic metaphysics (kami-as-bodhisattva) had been institutionalized instead of incoherence, would it have required different authority structures or different distribution of power? If yes, institutional beneficiaries faced a zero-sum choice and both readings are not simultaneously holdable by a single institutional framework.',
    'If syncretic metaphysics was institutionally incompatible with bakufu fragmentation strategy, the readings are in logical opposition—incoherence forecloses syncretic integration. If syncretic metaphysics could coexist with institutional fragmentation, the readings merely describe different choices and coexist. The foreclosure relation is the kernel_instability signal: if the kernel (kami-buddha relationship) admits no stable commitment that satisfies all institutional actors, the kernel is unstable and multiple readings coexist only through managed disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_kernel_instability, conceptual, 'Committer-frame omega: whether this reading''s incoherence thesis forecloses the syncretic reading or merely coexists with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 1603, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1603, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1603, 0.35).
narrative_ontology:measurement(shin_tr_t1650, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1650, 0.48).
narrative_ontology:measurement(shin_tr_t1720, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1720, 0.65).
narrative_ontology:measurement(shin_tr_t1790, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1790, 0.76).
narrative_ontology:measurement(shin_tr_t1840, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1840, 0.78).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1868, 0.0).

% Extraction over time
narrative_ontology:measurement(shin_be_t1603, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1603, 0.45).
narrative_ontology:measurement(shin_be_t1650, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1650, 0.52).
narrative_ontology:measurement(shin_be_t1720, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1720, 0.62).
narrative_ontology:measurement(shin_be_t1790, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1790, 0.68).
narrative_ontology:measurement(shin_be_t1840, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1840, 0.67).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1868, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1603, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1603, 0.42).
narrative_ontology:measurement(shin_su_t1650, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1650, 0.51).
narrative_ontology:measurement(shin_su_t1720, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1720, 0.61).
narrative_ontology:measurement(shin_su_t1790, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1790, 0.68).
narrative_ontology:measurement(shin_su_t1840, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1840, 0.71).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1868, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__incoherence_reading, 0.18).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, meiji_shinbutsu_bunri_enforcement).

% DUAL FORMULATION NOTE:
% Shinbutsu-shugo constraint family decomposed into three structurally distinct constraint stories per ε-invariance (DP-001): (1) INCOHERENCE_READING asserts no stable ontological commitment; the constraint persists through suppression of coherence-seeking work (ε_high, snare-flavored tangled_rope, extractive from coherence-seekers and practitioners). (2) PARTITION_READING asserts kami and buddhas occupy non-overlapping domains; the constraint is a functional division (ε_low-moderate, rope, genuine coordination with minimal extraction). (3) SYNCRETIC_READING asserts integrated cosmology via honji-suijaku; the constraint is coherent metaphysics (ε_low, rope or mountain, depends on whether metaphysics is treated as discovered or constructed). The three readings instantiate DIFFERENT epsilon values because they measure DIFFERENT constraints: incoherence persists through institutional suppression (high extraction cost); partition achieves coordination through functional division (low extraction cost); syncretic integration dissolves the need for suppression (minimal extraction cost). Each story carries its own ε, its own beneficiary/victim structure, its own type. They affect each other: incoherence reading forecloses syncretic reading if syncretic integration was institutionally viable but suppressed; incoherence reading influences partition reading by explaining why partition was never formally theorized (it threatened the political utility of incoherence). All three are linked to meiji_shinbutsu_bunri_enforcement because Meiji dismantling revealed the prior arrangement was politically maintained, not inevitable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
