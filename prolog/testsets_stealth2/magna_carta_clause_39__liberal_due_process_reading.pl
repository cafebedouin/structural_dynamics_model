% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Magna Carta Clause 39 — Liberal Due Process Reading (Universal Individual Rights Against Arbitrary State Power)
 *   domain: legal/political/constitutional
 *
 * SUMMARY:
 *   Clause 39 of Magna Carta (1215) — no free man shall be seized,
 *   imprisoned, dispossessed, outlawed, exiled, or destroyed except by the
 *   lawful judgment of his equals or by the law of the land — is here
 *   instantiated under its liberal due process reading: a universal
 *   individual-rights barrier against arbitrary state power, binding whoever
 *   wields public coercive force and protecting every person within
 *   jurisdiction. This file generates ONE reading of the contested clause-39
 *   kernel as a clean, epsilon-invariant constraint; the
 *   feudal_prerogative_reading and the originalist_limitation_reading are
 *   separate constraints in separate files and are neither described nor
 *   averaged into this one. The claim/metric gap is deliberate: the
 *   arrangement is CLAIMED as tangled_rope (a genuine coordination of legal
 *   order carrying a real, celebrated asymmetry — executives pay what
 *   citizens are spared), while the authored metrics describe heavily
 *   extractive operation against executive discretion, exactly as this
 *   reading's own lights measure it. The engine computes per-seat
 *   classifications from the structural data; the divergence between seats is
 *   the measurement the corpus exists to take. KEY AGENTS (by structural
 *   relationship): - executive_branch_officials: Primary target
 *   (powerful/constrained) — surrenders seizure, detention, and dispossession
 *   discretion to prior lawful judgment - all_persons_within_jurisdiction:
 *   Primary beneficiary (organized/constrained) — holds personal and property
 *   security against the state - criminal_defendants: Sharpest beneficiary
 *   (powerless/trapped) — the requirement binds hardest exactly where their
 *   liberty is at stake - constitutional_judiciary: Agenda-setter
 *   (institutional/identity_locked) — administers writs and review; collects
 *   jurisdictional authority - legislative_majorities: Dual-positioned
 *   beneficiary/payer (institutional/constrained) — legislates within policed
 *   boundaries - supranational_rights_courts: Secondary beneficiary
 *   (institutional/constrained) — draws transnational authority from the
 *   universalized reading - emergency_security_officials: Excluded
 *   (powerful/constrained) — demand unreviewable action; heard only as
 *   derogation petitioners - constitutional_historians: Analytical observer
 *   (analytical/analytical) — sees the full three-reading contest of the
 *   clause-39 kernel
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.84).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.66).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Magna Carta Clause 39 — Liberal Due Process Reading (Universal Individual Rights Against Arbitrary State Power)").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "legal/political/constitutional").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, 'f0019ef5-4dde-4baf-bc3c-380dfff49cf4').
narrative_ontology:cs_kernel_codification('f0019ef5-4dde-4baf-bc3c-380dfff49cf4', fixed_text).
narrative_ontology:cs_authority_grounding('f0019ef5-4dde-4baf-bc3c-380dfff49cf4', lineage).
narrative_ontology:cs_interpretation_layer_present('f0019ef5-4dde-4baf-bc3c-380dfff49cf4').
narrative_ontology:cs_reading_relation('f0019ef5-4dde-4baf-bc3c-380dfff49cf4', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('f0019ef5-4dde-4baf-bc3c-380dfff49cf4', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('f0019ef5-4dde-4baf-bc3c-380dfff49cf4', foundational, individual_rights_bind_state_universally).
narrative_ontology:cs_axiom_status(individual_rights_bind_state_universally, holdable).
narrative_ontology:cs_axiom_grounding('f0019ef5-4dde-4baf-bc3c-380dfff49cf4', individual_rights_bind_state_universally, deontological).
narrative_ontology:cs_axiom('f0019ef5-4dde-4baf-bc3c-380dfff49cf4', foundational, executive_discretion_subordinate_to_general_law).
narrative_ontology:cs_axiom_status(executive_discretion_subordinate_to_general_law, holdable).
narrative_ontology:cs_axiom_grounding('f0019ef5-4dde-4baf-bc3c-380dfff49cf4', executive_discretion_subordinate_to_general_law, deontological).
narrative_ontology:cs_reference_frame('f0019ef5-4dde-4baf-bc3c-380dfff49cf4', universal_lawful_judgment_protection).
narrative_ontology:cs_drift_state('f0019ef5-4dde-4baf-bc3c-380dfff49cf4', contemporary_emergency_administration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0019ef5-4dde-4baf-bc3c-380dfff49cf4', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, all_persons_within_jurisdiction).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, criminal_defendants).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, civil_society_institutions).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, supranational_rights_courts).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, executive_branch_officials).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, legislative_majorities).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, legislative_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a government that may not seize, imprison, or dispossess them without prior lawful judgment. They carry the arrangement's civic obligations and fund its courts through taxes. Their protection is broad but shallow per person — most will never invoke it directly. Leaving means emigration, which few can afford; staying means relying on courts they do not control.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, all_persons_within_jurisdiction, beneficiary,
    organized, biographical, constrained, national).

% Face the state's full coercive apparatus with liberty at stake. The requirements of lawful judgment, counsel, and impartial tribunal bind hardest in their cases. They cannot exit the process — flight forfeits the very protections at issue — and they enter it holding the least power of anyone in the story.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, criminal_defendants, beneficiary,
    powerless, immediate, trapped, national).

% Presses, unions, churches, and domestic firms plan and speak under the assurance that the state cannot punish them without process. They cannot relocate their mission abroad; their security depends entirely on the arrangement holding in the polity where they operate.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, civil_society_institutions, beneficiary,
    organized, biographical, constrained, national).

% Run agencies that must give notice, hold hearings, and answer in court before acting on persons or property. Every lawful instrument remains available to them; what is gone is action without judgment. They experience the arrangement as friction, delay, and lost cases, and they periodically seek exceptions through emergency and security framings. Resignation is their only exit from the obligation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, executive_branch_officials, payer,
    powerful, biographical, constrained, national).

% Administers the arrangement: issues writs, reviews detentions, and strikes statutes and executive acts that bypass judgment. Life tenure and professional formation fuse their identity with guardianship of the requirement — leaving the role would mean leaving the vocation. Their institutional power grows with each expansion of the requirement's reach.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Write statutes inside boundaries the courts police. They benefit from the credible order the arrangement sustains and pay when their enactments are struck down or read narrowly. Elections, not exit, are their recourse.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, legislative_majorities, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__liberal_due_process_reading, legislative_majorities, payer).

% Regional human rights bodies take the universalized reading as their jurisdictional premise and enforce fair-hearing requirements against member states. Their authority depends on the reading remaining live; they cannot abandon it without dissolving themselves.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, supranational_rights_courts, beneficiary,
    institutional, generational, constrained, continental).

% Intelligence and security officers who need speed, secrecy, and unreviewable detention in exceptional circumstances. The arrangement forecloses their preferred operating mode; they appear in the conversation only as petitioners for narrow derogations, and their systematic claims for efficiency go structurally unheard.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, emergency_security_officials, excluded,
    powerful, immediate, constrained, national).

% Study the 1215 text, its medieval setting, and its modern career. They see all three readings of the clause at once and owe allegiance to none; their analyses feed courts, legislatures, and the public record.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__liberal_due_process_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts potentially violent, ad hoc contests between state and person into predictable adjudicated procedure, and makes governmental commitments credible to property holders, contractors, and minorities — enabling investment, planning, and cooperation that arbitrary power cannot support.
% TRANSFER_FUNCTION: Moves decision power over life, liberty, and property from executive officials to legal institutions — courts, juries, counsel. It costs the state speed, secrecy, and discretion, and transfers personal security to every person within jurisdiction.
% ABSENT_VOICES: Officials who need rapid, unreviewable action — intelligence officers, emergency managers — sit outside the conversation except as narrow derogation petitioners; their efficiency and security claims are heard only at the margin. Absolutist theorists have had no seat since the reading universalized. Most acutely: persons detained outside ordinary jurisdiction are governed by the arrangement's edges without any voice in it.
% DISAPPEARANCE_RATIONALE: Overnight disappearance restores arrest at will, detention without charge, and dispossession by fiat. Credit and property regimes collapse as state commitments become incredible; mass resistance or emigration follows; the judiciary loses its core jurisdiction. Nearly every modern legal-economic arrangement depends on the requirement holding.
% FOUNDING_PROBLEM: King John's arbitrary fines, disseisin, and imprisonment of barons without lawful judgment — royal predation on elites under personal rule. The 1215 settlement answered with a procedural wall around the crown's coercive power, which the liberal reading later generalized to every person and every wielder of state force.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the paying parties attest it — executives repeatedly petition for exceptions (emergency powers, streamlined detention), confirming the standing pull of arbitrary action; constitutional scholarship documents due-process collapse preceding broader rights failure across historical and contemporary cases; supranatural and domestic tribunals adjudicate live arbitrary-detention claims yearly. No attestation from inside the citizen-beneficiary set is required, and the persistence of executive exception-seeking is itself the strongest external evidence that the founding problem remains live.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.84 because the liberal reading measures the arrangement against the thing it removes: arbitrary executive action. The removal is near-total in its domain — the state retains every lawful instrument and loses precisely the discretion the reading exists to destroy — and the secular rise from 0.32 (1215, narrow scope, weakly enforced) tracks universalization of the beneficiary set, deepening from procedure to substantive review, and internationalization. Suppression is 0.66 and is authored as a raw structural property, unscaled: the enforcement machinery (writs, judicial review, treaty bodies) compels state compliance while imposing no coercion on citizens; the suppressive force is aimed almost entirely at one seat. Theater is 0.38: the functional core (habeas, impartial trial, counsel) is real and load-bearing, but the performative share has grown with age — declaratory instruments, box-ticking proceduralism in administrative states, and compliance assertion during derogation episodes. Accessibility collapse is 0.55: within the liberal tradition, once the premise is understood, arbitrary-rule alternatives lose justificatory standing, yet they persist empirically worldwide, so alternatives are only partly collapsed. Resistance is 0.62: executive pushback is perpetual — emergency framings, national-security exceptions, administrative-discretion expansion — and never succeeds wholesale but never ceases. The measurement series run on one shared eight-point grid so every tracked metric is authored at every examined time point; the 2001 dip in extractiveness and spike in suppression requirement record the war-on-terror stress test superimposed on the secular rise — a stress-recovery oscillation driven by external crisis, not an intermittent-reinforcement cycle. Identity-lock note: the judiciary's exit is identity_locked because life tenure and professional formation fuse the judicial self with guardianship of the requirement; if that frame broke — judges coming to see themselves as administrators of state policy rather than guardians of lawful judgment — the enforcement seat would decay toward theatrical maintenance and the arrangement would drift piton-ward.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute differently, and the inter-institutional structure guarantees it. From the executive's position the arrangement is friction, delay, and lost cases — a near-pure cost imposed by courts it does not control. From the defendant's position it is the wall between liberty and the state's full apparatus. From the judiciary's position it is empowerment: each expansion of the requirement's reach expands judicial jurisdiction. Legislative majorities occupy a genuinely dual position at the same nominal institutional power as the courts — they benefit from the credible order and pay when struck down — which is why two institutional seats with identical power atoms diverge in directionality. Supranational courts add a fourth institutional reading: they inherit the universalized reading as their jurisdictional premise and cannot exit it without dissolving themselves. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation, and no overrides were needed. Executive_branch_officials are the declared victims with powerful power and constrained exit: the arrangement aims at exactly their discretion, placing them near the full-target end (d approaching 0.9) — effective extraction against them is amplified, which is the reading's whole point. All_persons_within_jurisdiction, criminal_defendants, and civil_society_institutions are declared beneficiaries with constrained or trapped exit: they sit near the beneficiary end (d roughly 0.05-0.15), so effective extraction inverts into protection. Constitutional_judiciary and supranational_rights_courts are listed as beneficiaries because jurisdictional authority genuinely accrues to them — they derive low d as empowered administrators rather than mid-range fallback values. Legislative_majorities, dual-positioned and not listed in either array, fall near symmetric (d around 0.45), matching their mixed experience. Emergency_security_officials are excluded rather than coordinated: their preferred operating mode is the enforcement object itself, so they sit structurally target-adjacent despite appearing in no derivation array.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — royal predation under personal rule — has been generalized far past its 1215 form, but its status is live, not dead: arbitrary state power recurs in every generation in new dress (administrative detention, emergency decree, algorithmic deprivation), and the arrangement's continued operation presupposes the threat it answers. Mandatrophy is therefore not resolved. The tangled_rope claim prevents two symmetrical errors. Calling the arrangement pure rope would erase the real and celebrated asymmetry: executives genuinely pay, continuously, through the same structure that coordinates everyone else. Calling it a snare would erase the genuine coordination: the legal order it sustains is broadly beneficial, exits exist (amendment, election, resignation), and rival constitutional arrangements are not suppressed. The hybrid classification holds both truths at once — and the R5 mismatch consumer should find no zombie flag here, since founding_problem_status is live and disappearance_verdict is world_rearranges, a coherent pair.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading captures Clause 39''s binding content — universal individual rights against arbitrary power (this file), rank-bound feudal procedural guarantee, or a closed catalogue of the abuses documented in 1215?',
    'Convergence of historiography (what the 1215 actors and their successors understood the text to do) and doctrinal uptake (what courts across jurisdictions treat the clause as establishing). Under the committer frame the readings are separable constraints; only their relative institutional uptake is empirically decidable.',
    'Sibling readings change the victim set (crown-versus-barons versus state-versus-all-persons), the temporal reach, and epsilon by wide margins. This file''s epsilon of 0.84 is valid only under the liberal reading''s own lights; under the feudal reading the same text computes near-negligible extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'One kernel, three readings: this constraint is the liberal_due_process_reading instantiation of magna_carta_clause_39.').

omega_variable(
    universality_boundary,
    'Does ''all persons within jurisdiction'' extend to non-citizens, extraterritorial detainees, and enemy combatants — or does the universalization stop at borders and battlefields?',
    'Litigation trajectories on habeas access for offshore detainees, treaty-body jurisprudence, and state practice on extraterritorial detention over the coming decades.',
    'If universalism is bounded, the beneficiary set contracts sharply in the excluded zones and measured extraction against the state falls there; the reading''s core axiom would survive formally while thinning in application, opening space for a de facto fourth reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_boundary, empirical, 'Boundary of the universal beneficiary set under the liberal reading.').

omega_variable(
    functional_theater_bimodality,
    'Does the aggregate theater ratio mask a bimodal world — fully functional due process in strong-court jurisdictions and largely performative compliance in weak ones?',
    'Cross-jurisdictional comparison of habeas grant rates, remedial outcomes, and pre-trial detention durations against formal constitutional text.',
    'If bimodal, single-story metrics misdescribe most of the world''s population: the arrangement is coordination-functional in some polities and inertially theatrical in others, arguing for per-jurisdiction decomposition into separate stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_theater_bimodality, empirical, 'Aggregate versus per-jurisdiction functionality of the due process arrangement.').

omega_variable(
    emergency_derogation_ratchet,
    'Are emergency derogations (war, terror, pandemic) the arrangement''s designed safety valve, or a one-way ratchet converting temporary exceptions into permanent executive discretion?',
    'Track post-emergency restoration: do derogated powers lapse when emergencies end, or persist under renamed statutory authority?',
    'Designed-valve reading keeps suppression stable and the tangled_rope classification secure; ratchet reading predicts rising suppression requirement and creeping snare-formation around targeted classes such as detainees and migrants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_derogation_ratchet, empirical, 'Whether exception powers normalize or expire.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1215, 0.12).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1354, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1354, 0.18).
narrative_ontology:measurement_basis(magn_tr_t1354, observed).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1689, 0.21).
narrative_ontology:measurement_basis(magn_tr_t1689, observed).
narrative_ontology:measurement(magn_tr_t1791, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1791, 0.24).
narrative_ontology:measurement_basis(magn_tr_t1791, observed).
narrative_ontology:measurement(magn_tr_t1948, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1948, 0.29).
narrative_ontology:measurement_basis(magn_tr_t1948, observed).
narrative_ontology:measurement(magn_tr_t1966, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1966, 0.27).
narrative_ontology:measurement_basis(magn_tr_t1966, observed).
narrative_ontology:measurement(magn_tr_t2001, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 2001, 0.34).
narrative_ontology:measurement_basis(magn_tr_t2001, observed).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(magn_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1215, 0.32).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1354, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1354, 0.41).
narrative_ontology:measurement_basis(magn_be_t1354, observed).
narrative_ontology:measurement(magn_be_t1689, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1689, 0.54).
narrative_ontology:measurement_basis(magn_be_t1689, observed).
narrative_ontology:measurement(magn_be_t1791, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1791, 0.66).
narrative_ontology:measurement_basis(magn_be_t1791, observed).
narrative_ontology:measurement(magn_be_t1948, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1948, 0.77).
narrative_ontology:measurement_basis(magn_be_t1948, observed).
narrative_ontology:measurement(magn_be_t1966, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1966, 0.81).
narrative_ontology:measurement_basis(magn_be_t1966, observed).
narrative_ontology:measurement(magn_be_t2001, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 2001, 0.79).
narrative_ontology:measurement_basis(magn_be_t2001, observed).
narrative_ontology:measurement(magn_be_t2025, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 2025, 0.84).
narrative_ontology:measurement_basis(magn_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1215, 0.48).
narrative_ontology:measurement_basis(magn_su_t1215, observed).
narrative_ontology:measurement(magn_su_t1354, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1354, 0.44).
narrative_ontology:measurement_basis(magn_su_t1354, observed).
narrative_ontology:measurement(magn_su_t1689, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1689, 0.52).
narrative_ontology:measurement_basis(magn_su_t1689, observed).
narrative_ontology:measurement(magn_su_t1791, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1791, 0.57).
narrative_ontology:measurement_basis(magn_su_t1791, observed).
narrative_ontology:measurement(magn_su_t1948, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1948, 0.61).
narrative_ontology:measurement_basis(magn_su_t1948, observed).
narrative_ontology:measurement(magn_su_t1966, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1966, 0.63).
narrative_ontology:measurement_basis(magn_su_t1966, observed).
narrative_ontology:measurement(magn_su_t2001, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 2001, 0.71).
narrative_ontology:measurement_basis(magn_su_t2001, observed).
narrative_ontology:measurement(magn_su_t2025, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 2025, 0.66).
narrative_ontology:measurement_basis(magn_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, originalist_limitation_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, habeas_corpus_entrenchment).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, echr_article_6_fair_hearing).

% DUAL FORMULATION NOTE:
% Constraint family: the single 1215 text instantiates three structurally distinct constraints. This file authors the liberal_due_process_reading — universal individual rights, victim set = executive discretion, epsilon 0.84 indexed to the reading's own lights. The feudal_prerogative_reading authors a rank-bound procedural guarantee (small victim set, negligible extraction); the originalist_limitation_reading authors a closed catalogue of documented 1215 abuses (victim set frozen at 1215, moderate static extraction). Historical upstream/downstream: the feudal reading is upstream (the text's original force), while the liberal reading supplies the principle cited as authority for downstream instruments (habeas entrenchment, ECHR Article 6). Each member is a separate file with its own stable epsilon per DP-001; this file does not average across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
