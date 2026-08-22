% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__reconstruction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__reconstruction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: classical_latin_standard__reconstruction_reading
 *   human_readable: Humanist Reconstruction Standard of Classical Latin
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   This story authors the reconstruction reading of the contested 'correct
 *   Latin' kernel: the claim that legitimate Latin is exclusively the
 *   Classical form recoverable through philological return to
 *   Ciceronian/Augustan-era manuscripts, with all post-Classical (especially
 *   medieval scholastic, ecclesiastical, and administrative) usage
 *   delegitimized as corruption requiring correction rather than accepted as
 *   valid development. The reading emerges with Renaissance humanism and
 *   hardens over subsequent centuries into an entrenched credentialing and
 *   publishing apparatus. Sibling readings — continuity (medieval Latin as
 *   legitimate living transmission) and hybrid (textual fidelity plus
 *   domain-specific post-Classical legitimacy) — are NOT authored here; they
 *   are separate constraints linked via network and
 *   cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - humanist_philologists: agenda_setter/beneficiary (institutional/arbitrage) — sets and profits from the standard
 *   - renaissance_academies: beneficiary (organized/mobile) — gains prestige from early adoption
 *   - classical_press_publishers: beneficiary (powerful/arbitrage) — commercial interest in the standard's spread
 *   - medieval_notaries_and_clerics: payer (moderate/trapped) — professional competence reclassified as error
 *   - scholastic_universities: payer (institutional/constrained) — centuries of pedagogy delegitimized
 *   - vernacular_educated_clergy: payer (powerless/trapped) — cannot access retraining resources
 *   - administrative_latin_users: payer (moderate/constrained) — functional Latin now stylistically stigmatized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, 0.71).
domain_priors:suppression_score(classical_latin_standard__reconstruction_reading, 0.78).
domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__reconstruction_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__reconstruction_reading, "Humanist Reconstruction Standard of Classical Latin").
narrative_ontology:topic_domain(classical_latin_standard__reconstruction_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__reconstruction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__reconstruction_reading, 'ce523ea8-9f58-4ee4-8b18-39384a9a2a4d').
narrative_ontology:cs_kernel_codification('ce523ea8-9f58-4ee4-8b18-39384a9a2a4d', fixed_text).
narrative_ontology:cs_authority_grounding('ce523ea8-9f58-4ee4-8b18-39384a9a2a4d', expertise).
narrative_ontology:cs_interpretation_layer_present('ce523ea8-9f58-4ee4-8b18-39384a9a2a4d').
narrative_ontology:cs_reading_relation('ce523ea8-9f58-4ee4-8b18-39384a9a2a4d', classical_latin_standard__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('ce523ea8-9f58-4ee4-8b18-39384a9a2a4d', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('ce523ea8-9f58-4ee4-8b18-39384a9a2a4d', foundational, post_classical_drift_is_corruption_not_development).
narrative_ontology:cs_axiom_status(post_classical_drift_is_corruption_not_development, holdable).
narrative_ontology:cs_axiom_grounding('ce523ea8-9f58-4ee4-8b18-39384a9a2a4d', post_classical_drift_is_corruption_not_development, conventional).
narrative_ontology:cs_axiom('ce523ea8-9f58-4ee4-8b18-39384a9a2a4d', foundational, textual_recovery_supersedes_practice_based_transmission).
narrative_ontology:cs_axiom_status(textual_recovery_supersedes_practice_based_transmission, holdable).
narrative_ontology:cs_axiom_grounding('ce523ea8-9f58-4ee4-8b18-39384a9a2a4d', textual_recovery_supersedes_practice_based_transmission, instrumental).
narrative_ontology:cs_reference_frame('ce523ea8-9f58-4ee4-8b18-39384a9a2a4d', ciceronian_augustan_textual_corpus).
narrative_ontology:cs_drift_state('ce523ea8-9f58-4ee4-8b18-39384a9a2a4d', high_medieval_scholastic_consolidation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ce523ea8-9f58-4ee4-8b18-39384a9a2a4d', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__reconstruction_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, renaissance_academies).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, classical_press_publishers).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, medieval_notaries_and_clerics).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, scholastic_universities).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, vernacular_educated_clergy).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, administrative_latin_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Positions itself as the sole legitimate arbiter of correct Latin by appeal to recovered Ciceronian and Augustan manuscripts, training a new class of textual specialists who certify usage against classical exemplars. Controls the standard by controlling access to the corrected texts and the philological method for reading them; sets curricula and orthographic norms other institutions must now adopt or be publicly shamed as 'barbarous'.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, humanist_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__reconstruction_reading, humanist_philologists, beneficiary).

% Gains prestige and patronage by adopting the reconstruction standard early, positioning itself against older scholastic institutions as culturally superior. Benefits from the new hierarchy of taste without bearing the retraining costs imposed on existing practitioners.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, renaissance_academies, beneficiary,
    organized, generational, mobile, continental).

% Profits from printing corrected classical editions, grammars, and dictionaries that only the new standard requires; has a direct commercial interest in the standard's spread and in medieval Latin editions being deemed obsolete or corrupt.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, classical_press_publishers, beneficiary,
    powerful, biographical, arbitrage, continental).

% Have spent careers mastering a functional, living Latin adequate for legal, administrative, and liturgical purposes. Under the reconstruction standard their competence is reclassified as ignorance or corruption overnight; they cannot easily retrain in philological method and their professional authority is undermined by younger scholars trained in the new norms.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, medieval_notaries_and_clerics, payer,
    moderate, biographical, trapped, regional).

% Built centuries of pedagogy, disputation method, and Latin usage now dismissed as degenerate scholastic jargon. Faces pressure to reform curricula around humanist textual standards or lose students and prestige to reformed institutions; reform is costly and slow given entrenched faculties and texts.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, scholastic_universities, payer,
    institutional, generational, constrained, continental).

% Learned functional liturgical and pastoral Latin through practice and rote transmission rather than classical texts. Cannot access the manuscripts, tutors, or leisure required to retrain in philological method; risk being marked as unlearned by newly credentialed peers even though their Latin served its communicative purpose adequately.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, vernacular_educated_clergy, payer,
    powerless, biographical, trapped, local).

% Chancery clerks, notaries, and civic officials whose working Latin was shaped by centuries of legal formula and drift. Their documents and forms become targets of stylistic ridicule under the new standard, creating pressure to hire humanist-trained secretaries even where the older forms functioned perfectly well administratively.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, administrative_latin_users, payer,
    moderate, biographical, constrained, regional).

% The surviving classical manuscripts themselves function as the contested evidentiary base; they do not act but are invoked by all parties as authority, their availability and condition shaping what philological archaeology can even recover.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, textual_transmission_manuscripts, observer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(classical_latin_standard__reconstruction_reading, textual_transmission_manuscripts).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:fixing_cost_class(classical_latin_standard__reconstruction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, textually anchored reference standard for Latin usage across a fragmented Europe of divergent regional and institutional Latin dialects, in principle allowing scholars anywhere to converge on a common, verifiable form by appeal to shared classical sources rather than local custom.
% TRANSFER_FUNCTION: Moves prestige, employment, and interpretive authority away from institutions and individuals whose Latin competence was built through administrative, liturgical, and scholastic practice, toward a new credentialing class trained in philological method and manuscript recovery; also moves market demand toward humanist presses and grammars.
% ABSENT_VOICES: The medieval clerics, notaries, and scholastic faculties whose Latin is being delegitimized are rarely invited to adjudicate the standard themselves — the debate over 'correctness' is conducted largely among humanists and their patrons, with practitioners of the older register cast as the objects of correction rather than participants in defining it.
% DISAPPEARANCE_RATIONALE: If the reconstruction standard vanished, the humanist credentialing apparatus, the classical press market, and the associated academic hierarchies built on 'correct' classical usage would lose their basis for distinguishing themselves from scholastic and administrative Latin; institutions currently under pressure to reform would face no external standard demanding it, and existing practice-based Latin competence would remain fully functional and unchallenged.
% FOUNDING_PROBLEM: Humanist scholars diagnosed genuine linguistic drift and corruption in medieval Latin manuscripts and sought a principled method — return to classical sources — to recover a stable, historically grounded form of the language rather than relying on centuries of uncontrolled scribal and regional variation.
% FOUNDING_PROBLEM_CORROBORATION: Manuscript philologists and textual historians outside the immediate humanist patronage networks (later comparative linguists studying medieval Latin as a legitimate, internally coherent register) attest that medieval Latin functioned as a stable, rule-governed living language for its purposes, not a degraded corruption of Classical Latin — corroboration that comes from outside the beneficiary group and undercuts the reconstruction reading's own founding narrative of decay requiring correction.
narrative_ontology:disappearance_verdict(classical_latin_standard__reconstruction_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__reconstruction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__reconstruction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__reconstruction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__reconstruction_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__reconstruction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__reconstruction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.71 by interval end) because the standard's operation systematically transfers interpretive and professional authority away from practice-based users toward a philologically credentialed elite, without those users having consented to or benefited from the redefinition. Suppression is authored even higher (0.78) because the reading's persistence depends on actively delegitimizing alternatives — medieval Latin is not merely disfavored but redescribed as 'barbarous' or 'corrupt,' foreclosing its status as a legitimate register rather than simply competing with it. Theater ratio is moderate (0.42): genuine philological recovery work occurs, but a substantial and growing share of enforcement activity (orthographic policing, public shaming of 'incorrect' usage) serves boundary-maintenance for the new credentialing class rather than any communicative or historical-recovery function. All three metrics share one time grid across the ~two-century consolidation interval.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist agenda-setter seat, this reads as a coordination triumph: a single, verifiable, historically grounded standard replacing centuries of undisciplined drift — a rope, if considered from that seat alone. From the payer seats (scholastic universities, vernacular clergy, administrative users) the identical structure operates as enforced devaluation of functioning competence, imposed by a small credentialed class that captures the resulting prestige and market. The engine computes these as different seat-level types from the same structural data; the claimed_type of tangled_rope is authored precisely because both a real coordination function (a common textual reference point) and a real asymmetric extraction (delegitimization of existing practice-holders for the benefit of a new gatekeeping class) are structurally present simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philologists and their patron academies are structural beneficiaries: they control the interpretive method, the manuscript access, and the resulting prestige economy — d sits near the beneficiary end. Classical press publishers benefit commercially from manufactured demand for corrected texts. Medieval notaries, scholastic faculties, vernacular clergy, and administrative users are the targets: their existing competence is the resource being devalued, and their exit options range from constrained (institutions that can slowly reform) to trapped (individual clerics with no access to retraining) — d sits near the full-target end for these groups, especially the powerless vernacular clergy who have neither the leisure nor the manuscripts to requalify.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine, uncontrolled scribal drift creating communicative fragmentation — was real at the outset, which is why this is authored as tangled_rope rather than snare: there was a coordination problem to solve. But the corroborating outside evidence (later comparative philology treating medieval Latin as internally coherent, not degraded) suggests the founding problem's 'corruption' framing was overstated from the start and the standard's persistence increasingly serves the credentialing apparatus's own reproduction rather than any live communicative failure — the founding_problem_status is authored as contested rather than dead specifically because reasonable outside observers disagree about whether the coordination need was ever what it claimed to be.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reconstruction_reading_identity,
    'Is the reconstruction reading a defensible historical-linguistic claim about textual fidelity, or is it primarily a legitimating narrative for a new humanist gatekeeping class?',
    'Comparative analysis of whether the humanist standard''s specific corrections track genuine scribal corruption (attested by manuscript stemma analysis) versus arbitrary stylistic preferences that happen to favor texts and training only the humanist class possesses.',
    'If corrections track genuine corruption, the coordination function is stronger and the classification may sit closer to rope; if corrections are substantially arbitrary or overbroad relative to actual textual corruption, the tangled_rope/extraction reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_reading_identity, conceptual, 'Whether the reconstruction standard is genuine textual recovery or gatekeeping dressed as philology.').

omega_variable(
    sibling_reading_foreclosure,
    'Does adopting the reconstruction reading as an institution''s official standard logically foreclose that institution from also holding the continuity reading, or can both be held in tension by the same actor over time?',
    'Track individual humanist scholars and institutions across careers to see whether reconstruction-standard adoption is ever later reversed or hybridized without institutional rupture.',
    'If institutions can and do shift fluidly between readings, the relationship to sibling readings is better modeled as coexists_with or influences rather than forecloses; if adoption is a one-way institutional commitment, forecloses may be more accurate for at least the continuity_reading pairing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether reconstruction and continuity readings can coexist within one institution''s practice or are mutually exclusive commitments.').

omega_variable(
    victim_coalition_potential,
    'Could scholastic universities, administrative users, and vernacular clergy have formed an effective coalition to resist the reconstruction standard''s spread, and why did this largely fail to materialize?',
    'Historical analysis of instances where scholastic institutions did resist humanist curricular reform successfully versus instances of rapid capitulation, controlling for patronage dependency and market pressure from printing.',
    'If coalition was structurally foreclosed by patronage dependency, this strengthens the tangled_rope/extraction reading; if coalition was available but unexploited, some of the outcome reflects victim coordination failure rather than pure structural suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_coalition_potential, empirical, 'Whether the dispersed victim groups had viable coalition options against the standard''s spread.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__reconstruction_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__reconstruction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clas_tr_t40, classical_latin_standard__reconstruction_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(clas_tr_t80, classical_latin_standard__reconstruction_reading, theater_ratio, 80, 0.33).
narrative_ontology:measurement(clas_tr_t120, classical_latin_standard__reconstruction_reading, theater_ratio, 120, 0.37).
narrative_ontology:measurement(clas_tr_t160, classical_latin_standard__reconstruction_reading, theater_ratio, 160, 0.4).
narrative_ontology:measurement(clas_tr_t200, classical_latin_standard__reconstruction_reading, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__reconstruction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clas_be_t40, classical_latin_standard__reconstruction_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(clas_be_t80, classical_latin_standard__reconstruction_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement(clas_be_t120, classical_latin_standard__reconstruction_reading, base_extractiveness, 120, 0.65).
narrative_ontology:measurement(clas_be_t160, classical_latin_standard__reconstruction_reading, base_extractiveness, 160, 0.69).
narrative_ontology:measurement(clas_be_t200, classical_latin_standard__reconstruction_reading, base_extractiveness, 200, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__reconstruction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(clas_su_t40, classical_latin_standard__reconstruction_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(clas_su_t80, classical_latin_standard__reconstruction_reading, suppression_requirement, 80, 0.67).
narrative_ontology:measurement(clas_su_t120, classical_latin_standard__reconstruction_reading, suppression_requirement, 120, 0.72).
narrative_ontology:measurement(clas_su_t160, classical_latin_standard__reconstruction_reading, suppression_requirement, 160, 0.76).
narrative_ontology:measurement(clas_su_t200, classical_latin_standard__reconstruction_reading, suppression_requirement, 200, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__reconstruction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__reconstruction_reading, 0.1).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% Part of the classical_latin_standard constraint family (3 readings of one kernel). This story (reconstruction_reading) authors high extraction and suppression from the reading's own delegitimization of medieval practice. The sibling continuity_reading would author low extraction (drift as legitimate development, no victim class created) and the sibling hybrid_reading would author moderate extraction (textual fidelity required in some domains but post-Classical legitimacy recognized in others, narrowing the victim set to only those domains where hybrid accommodation fails). Each story owns a single stable ε per the ε-invariance principle; they are not averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
