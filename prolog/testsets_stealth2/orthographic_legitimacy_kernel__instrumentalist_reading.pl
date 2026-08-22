% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Instrumentalist Orthographic Legitimacy: Script Reform as Literacy-Efficiency Coordination
 *   domain: political/linguistic/state-formation
 *
 * SUMMARY:
 *   A state governing a population literate in an inherited,
 *   morphophonemically opaque orthography legislates a replacement script
 *   engineered for phonetic transparency and rapid classroom acquisition.
 *   Within a few years the new alphabet is compulsory in schools, required on
 *   official documents, and the only script licensed for periodical print;
 *   state-run evening courses carry the adult population across. Legitimacy
 *   for the arrangement is claimed in a single currency: measured literacy
 *   rates and administrative throughput. By that currency the reform performs
 *   — enrollment and print literacy climb steeply within the interval — while
 *   a stratum trained in the old orthography watches its professional capital
 *   become administratively worthless: clerks, scribes, calligraphers, and
 *   religious scholars lose offices, audiences, and markets faster than they
 *   can retrain. The arrangement therefore runs a genuine mass-literacy
 *   coordination function and a simultaneous transfer of status and
 *   livelihood away from the old literate strata through the same legal
 *   machinery. This file instantiates one reading of the
 *   orthographic-legitimacy kernel as a clean, epsilon-invariant constraint;
 *   committer structure is carried in the omega variables and the
 *   kernel_context note.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: Agenda-setter and principal collector (institutional/arbitrage) — designs, enforces, and profits from the arrangement
 *   - newly_literate_mass_public: Primary beneficiary (powerless/constrained) — receives literacy access, locked inside the new system
 *   - arabic_script_literate_ulema: Primary payer (organized/identity_locked) — stranded religious-scholarly capital
 *   - ottoman_scribal_officials: Secondary payer (moderate/constrained) — stranded administrative capital
 *   - calligraphic_arts_practitioners: Secondary payer (powerless/trapped) — stranded artisan capital
 *   - rural_unenrolled_adults: Excluded seat (powerless/trapped) — bears the transition with no voice
 *   - comparative_literacy_historians: Analytical observer — external check on the reform's headline statistics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.44).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.33).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.19).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.33).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.19).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Instrumentalist Orthographic Legitimacy: Script Reform as Literacy-Efficiency Coordination").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political/linguistic/state-formation").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, '46ecc4af-5dab-4ae7-bbdc-f18bf37ad855').
narrative_ontology:cs_kernel_codification('46ecc4af-5dab-4ae7-bbdc-f18bf37ad855', formalized).
narrative_ontology:cs_authority_grounding('46ecc4af-5dab-4ae7-bbdc-f18bf37ad855', expertise).
narrative_ontology:cs_interpretation_layer_present('46ecc4af-5dab-4ae7-bbdc-f18bf37ad855').
narrative_ontology:cs_reading_relation('46ecc4af-5dab-4ae7-bbdc-f18bf37ad855', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_reading_relation('46ecc4af-5dab-4ae7-bbdc-f18bf37ad855', orthographic_legitimacy_kernel__continuity_reading, influences).
narrative_ontology:cs_axiom('46ecc4af-5dab-4ae7-bbdc-f18bf37ad855', foundational, orthographic_value_is_measurable_outcome).
narrative_ontology:cs_axiom_status(orthographic_value_is_measurable_outcome, holdable).
narrative_ontology:cs_axiom_grounding('46ecc4af-5dab-4ae7-bbdc-f18bf37ad855', orthographic_value_is_measurable_outcome, empirically_contingent).
narrative_ontology:cs_axiom('46ecc4af-5dab-4ae7-bbdc-f18bf37ad855', secondary, script_is_instrument_not_identity).
narrative_ontology:cs_axiom_status(script_is_instrument_not_identity, holdable).
narrative_ontology:cs_axiom_grounding('46ecc4af-5dab-4ae7-bbdc-f18bf37ad855', script_is_instrument_not_identity, instrumental).
narrative_ontology:cs_reference_frame('46ecc4af-5dab-4ae7-bbdc-f18bf37ad855', literacy_efficiency_utility_standard).
narrative_ontology:cs_drift_state('46ecc4af-5dab-4ae7-bbdc-f18bf37ad855', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('46ecc4af-5dab-4ae7-bbdc-f18bf37ad855', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_mass_public).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_script_literate_ulema).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, ottoman_scribal_officials).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, calligraphic_arts_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislates the alphabet law, builds the schools and mobile evening courses, licenses printing, and requires the new script on all official documents. Collects administrative uniformity, a legible population for taxation and conscription records, and the public authority vacated by the old scribal and scholarly networks. Ran its own internal correspondence bilingually through the transition years while the public had no such option.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus, beneficiary).

% Children in the new schools and adults in the literacy courses acquire a phonetic script teachable in months rather than years. They gain access to newspapers, school certificates, petitions, and bureaucratic paperwork that previously required a scribe intermediary. Remaining outside the new script is not a live option: instruction, print, and documents all moved together.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_mass_public, beneficiary,
    powerless, biographical, constrained, national).

% Decades of training in Perso-Arabic orthography and the transmitted corpus become administratively worthless almost overnight. Printing in the old script is banned, preaching is pushed into the vernacular, and the school and endowment networks that employed them are displaced by state schools. They retain private literacy but lose public office, income, students, and audience; leaving means abandoning the vocation their self-concept is built on.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_script_literate_ulema, payer,
    organized, generational, identity_locked, national).

% Career clerks and registrars whose promotion rested on mastery of the old orthography are forced into retraining competitions against younger entrants already fluent in the new script. Many take early retirement, accept subordinate posts, or spend their remaining working years repricing skills the state voided by decree.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, ottoman_scribal_officials, payer,
    moderate, biographical, constrained, national).

% Master calligraphers, manuscript copyists, gilders, and bookbinders serve a market constituted by the old script. When official and commercial print moves to Latin type, commissions collapse; the apprenticeship pipeline dries up within a decade. Their skills do not transfer to the new typographic economy.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, calligraphic_arts_practitioners, payer,
    powerless, biographical, trapped, regional).

% Adults outside the course system are never taught the new script and lose the print access that literate intermediaries under the old arrangement provided. They had no seat in the reform deliberations and would have objected to a transition that opened the door for the schooled young and closed it behind the unschooled old.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, rural_unenrolled_adults, excluded,
    powerless, biographical, trapped, regional).

% Assess literacy trajectories across polities that changed script and polities that expanded schooling without changing script, controlling for enrollment spending and economic growth. Supply the external check on whether the reform's headline statistics attribute to the script change or to development generally.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, comparative_literacy_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes one phonetically transparent national orthography, solving once and centrally the problem that an inherited morphophonemically opaque script posed for mass schooling, cheap printing, and uniform record-keeping across a large population.
% TRANSFER_FUNCTION: Moves literacy access toward the mass population and administrative throughput toward the state; simultaneously moves status, employment, and cultural authority away from the Arabic-script-trained strata toward Latin-script-trained cadres, and moves the inherited textual corpus out of general reach.
% ABSENT_VOICES: The ulema and traditional literati objected but entered the process only as managed opposition; rural adults outside the course system had no representation at all; and the future generations who would inherit the severed access to the pre-reform corpus were structurally unrepresented, since the costs fall on people not yet born.
% DISAPPEARANCE_RATIONALE: If the reformed orthography and its enforcement machinery vanished overnight, schooling, print, and administration would have no common script to fall back on: the literate order built on it — textbooks, archives, newspapers, bureaucratic forms, a Latin-literate labor force — would require wholesale reconstruction around either the restored old script or yet another replacement.
% FOUNDING_PROBLEM: Mass illiteracy under an orthographic tradition that took years to master, compounded by an administrative apparatus dependent on a small clerical caste as the bottleneck between citizens and the state.
% FOUNDING_PROBLEM_CORROBORATION: Comparative literacy historians corroborate that measured literacy rose steeply in the reforming polity within the interval, while noting from outside the benefiting parties that non-reforming neighbors expanding schooling showed substantial parallel gains, so the size of the script-specific contribution is disputed. Surviving members of the old literate strata attest the illiteracy problem was real while testifying to the costs the solution imposed on them; no party outside the beneficiary set attests that the founding problem is fully dead.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).
:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type and metrics are authored independently. The structure carries both a real coordination function (one teachable national orthography replacing an opaque inherited one — solved once centrally rather than per-community) and an identifiable paying stratum whose loss ran through the same legal machinery, with enforcement required to hold the switch; that combination is why the claim is tangled_rope. Metrics describe operation: extractiveness ends at 0.44 — moderate, because the coordination delivered what it promised while the transition confiscated the stranded cohort's accumulated capital; suppression ends at 0.33 after an early enforcement peak (publication bans, document law, attendance drives) gave way to habituation; theater stays low (0.19) because the literacy statistics were substantially real, rising only as the statistics became ritual justification after the targets were met. Accessibility collapse is partial (0.52): the old script vanished from public print and administration but survived in private devotion and religious schooling. Resistance (0.41) was real but fragmented — clergy defending tradition, clerks defending careers, and artisans defending markets pursued incompatible remedies and never formed a coalition, which is why a victim set individually lacking power failed to move the arrangement. All three series share one time grid {0,6,12,18,24,30}; base_properties report the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical facts. From the agenda-setting apparatus the arrangement is a completed nation-building project it designed, staffed, and can measure. From the newly literate majority it is simply the medium they think in — school, print, and paperwork all work. From the stranded payers the same statute reads as confiscation: a lifetime of orthographic capital voided by decree, with retraining priced against careers already ending. The excluded rural adults see a third thing — a door that opened for the schooled young and closed behind the unschooled old. Exit posture drives the divergence: the apparatus holds arbitrage-grade exit (it ran bilingual internally through the transition), the ulema are identity_locked, the artisans trapped, the new literates constrained inside the system that defines their literacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the mass public and the apparatus near the beneficiary end (low d): the arrangement subsidizes their literacy and throughput. Victim declarations place the ulema, scribal officials, and calligraphers near the target end (high d), amplified by exit posture — identity lock for the clergy, trap for the artisans, constraint for the clerks — so effective extraction concentrates hardest on those least able to reprice their loss. The apparatus sits near the beneficiary end despite administering the arrangement because it collects the efficiency gains and the vacated authority; national scope scales the extractive side modestly upward per engine arithmetic. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass illiteracy under an opaque orthography plus a clerical bottleneck in administration — was substantially addressed inside the interval, and the arrangement persists not as a vestige but as working infrastructure: the reformed orthography now carries ordinary schooling, print, and record-keeping load. Mandatrophy is therefore not resolved; nothing here is maintained theatrically. The classification guards both mislabels. Reading the arrangement as pure coordination ignores that its enactment confiscated a stratum's capital under legal compulsion; reading it as pure extraction ignores that the coordination was real, delivered, and still functioning, and that its suppressive force decayed rather than ratcheted. The temporal signature — extraction peaking mid-interval as the stranded cohort's losses crested, then declining as those cohorts aged out and enforcement relaxed — is what separates this from a hardening snare: the extraction was concentrated in the transition, not in the steady state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_attribution,
    'Is the observed arrangement actually driven by the instrumentalist legitimacy premise, or do the sibling readings share causal responsibility for the same statutes?',
    'Archival reconstruction of reform deliberations: if literacy projections and cost-of-clericalism arguments dominate the decision record, the instrumentalist attribution holds; if civilizational-rupture rhetoric carries the decisive votes, attribution shifts toward the modernist reading''s story.',
    'If a sibling reading drives the arrangement, this story''s epsilon and beneficiary/victim structure mis-describe the operative constraint and the classification belongs to the sibling''s file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_attribution, conceptual, 'Whether this reading is the operative legitimacy source for the observed arrangement.').

omega_variable(
    literacy_gain_attribution,
    'How much of the measured literacy gain is attributable to script simplification rather than concurrent schooling expansion, economic growth, and demographic change?',
    'Comparative analysis of polities that expanded schooling without changing script over the same period, controlling for enrollment spending; difference-in-differences across script-reform and non-reform neighbors.',
    'If the script''s marginal contribution is small, the coordination-function justification thins while the stranded cohort''s losses stand unchanged — epsilon rises and the arrangement drifts toward pure-extraction territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_gain_attribution, empirical, 'Attribution of literacy gains to the script change versus confounding development.').

omega_variable(
    transition_cost_vs_recurring_extraction,
    'Is the old stratum''s loss a one-time transition cost that decays as cohorts age out, or a recurring extraction renewed each generation denied direct access to the inherited corpus?',
    'Track corpus-access rates and heritage literacy across generations; if each cohort''s inability to read the pre-reform corpus persists as a standing cultural exclusion with present-day parties positioned where the old stratum stood, the extraction recurs.',
    'If recurring, the decay trajectory reverses and the arrangement trends toward snare-flavored persistence on the heritage-access dimension; if one-time, the decline series confirms a transition-bounded tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost_vs_recurring_extraction, empirical, 'Whether extraction was bounded to the transition or renews across generations.').

omega_variable(
    efficiency_frame_as_centralization_cover,
    'Was administrative efficiency the operative goal, or the legitimating frame for a centralization project that valued uniform legibility of the population over literacy itself?',
    'Compare administrative-throughput gains against literacy gains in the reform record; examine whether measures targeting state legibility (tax rolls, conscription lists, property registries) preceded and outpaced measures targeting popular literacy.',
    'If centralization dominated, part of the measured coordination benefit is control capacity and epsilon rises accordingly; if literacy led, the instrumentalist frame is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficiency_frame_as_centralization_cover, empirical, 'Efficiency goal versus centralization cover under the same statistics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(orth_tr_t6, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(orth_tr_t12, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(orth_tr_t18, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement(orth_tr_t24, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(orth_tr_t30, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 30, 0.19).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(orth_be_t6, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(orth_be_t12, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(orth_be_t18, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 18, 0.51).
narrative_ontology:measurement(orth_be_t24, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(orth_be_t30, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 30, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(orth_su_t6, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(orth_su_t12, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(orth_su_t18, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 18, 0.43).
narrative_ontology:measurement(orth_su_t24, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 24, 0.37).
narrative_ontology:measurement(orth_su_t30, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 30, 0.33).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, information_standard).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'what makes this script legitimate?' decomposes into three structurally distinct claims per the epsilon-invariance principle — instrumentalist_reading (this file: legitimacy as measurable literacy/efficiency outcome; moderate epsilon; stranded-literate-strata victims), modernist_reading (legitimacy as civilizational alignment and rupture; different beneficiary/victim geometry centered on Western-facing cadres), and continuity_reading (legitimacy as preserved access to the inherited corpus; victims are the mass population excluded from the tradition). Each member gets its own epsilon, stakeholders, and classification; this file links both siblings via affects_constraints. Upstream/downstream: the instrumentalist reading's delivered statistics historically supplied evidentiary support to the modernist reading, while its success erodes the continuity reading's constituency — edges documented in each file's reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
