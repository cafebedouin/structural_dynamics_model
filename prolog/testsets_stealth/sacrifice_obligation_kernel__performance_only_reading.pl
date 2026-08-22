% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__performance_only_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Standing Sacrificial Command Requiring Physical Performance (Performance-Only Reading)
 *   domain: religious law / halakhic authority / commitment system dynamics
 *
 * SUMMARY:
 *   This story instantiates ONE reading — performance_only_reading — of the
 *   sacrifice_obligation_kernel: the standing commitment that the sacrificial
 *   commandments bind the Jewish people. Under this reading the command
 *   remains fully in force after the Temple's destruction (70 CE): the
 *   obligation requires physical performance, study of the sacrificial orders
 *   is preparatory — it maintains readiness and memory but does not fulfill
 *   the command — and the community has therefore carried a standing,
 *   undischarged command for roughly nineteen centuries. The epsilon referent
 *   is this standing arrangement (the binding, currently unperformable
 *   command as this reading holds it), never the restoration this reading
 *   awaits. Constraint-family note (epsilon-invariance decomposition): the
 *   colloquial label 'the sacrifice obligation' covers four structurally
 *   distinct claims, authored as four linked stories. This reading authors
 *   the high-epsilon standing-debt claim (about 0.78: an entire people
 *   commanded and unable). The study_as_exercise sibling authors a
 *   low-epsilon arrangement (the debt is discharged through study —
 *   coordination, not extraction). The messianic_suspension sibling authors
 *   near-zero epsilon (no standing debt — the command is divinely paused, not
 *   transformed). The symbolic_archive sibling authors epsilon near zero (a
 *   cultural-historical archive making no halakhic claim). They are linked
 *   via network.affects_constraints; this story does not adjudicate between
 *   them — it authors one reading cleanly. No beneficiaries are declared,
 *   deliberately: no agent, human or divine, collects from the standing debt.
 *   The tradition's own texts deny divine need for the service, and the
 *   interpreting authority receives interpretive standing, not the debt's
 *   proceeds. The absence of a collector is the structural fact that drives
 *   this story's classification.
 *
 * KEY AGENTS:
 *   - halakhic_authority_community: agenda-setter (institutional/identity_locked) — poskim, academies, and liturgy-maintainers who rule the command binding and study non-fulfilling; they administer the interpretation they are also bound by
 *   - jewish_people_commanded_collective: primary target/payer (organized/identity_locked) — the commanded addressee of the communal sacrifices; bears the standing debt; its daily liturgy rehearses the inability to discharge it
 *   - temple_restoration_movements: debt-bearers acting on the debt (moderate/identity_locked) — organized efforts to restore performance capacity, converting longing into engineering
 *   - secular_jewish_community: excluded constituency (organized/mobile) — holds the archive-style reading; outside the halakhic conversation the constraint's maintenance never consults
 *   - academic_historian_of_religion: analytical observer (analytical/analytical) — sees the full four-reading structure and documents the recitation apparatus as memory practice
 *   - the command's author: noted here, deliberately not staked — the tradition itself denies that the command's author collects anything; no power atom accommodates this seat, and declaring it a beneficiary would misstate the structure the delta describes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.78).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.65).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, piton).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Standing Sacrificial Command Requiring Physical Performance (Performance-Only Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious law / halakhic authority / commitment system dynamics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, '892b5177-5d7c-47a0-9009-3fc700b85310').
narrative_ontology:cs_kernel_codification('892b5177-5d7c-47a0-9009-3fc700b85310', fixed_text).
narrative_ontology:cs_authority_grounding('892b5177-5d7c-47a0-9009-3fc700b85310', lineage).
narrative_ontology:cs_interpretation_layer_present('892b5177-5d7c-47a0-9009-3fc700b85310').
narrative_ontology:cs_reading_relation('892b5177-5d7c-47a0-9009-3fc700b85310', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('892b5177-5d7c-47a0-9009-3fc700b85310', sacrifice_obligation_kernel__messianic_suspension_reading, forecloses).
narrative_ontology:cs_reading_relation('892b5177-5d7c-47a0-9009-3fc700b85310', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('892b5177-5d7c-47a0-9009-3fc700b85310', foundational, physical_performance_sole_fulfillment).
narrative_ontology:cs_axiom_status(physical_performance_sole_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('892b5177-5d7c-47a0-9009-3fc700b85310', physical_performance_sole_fulfillment, deontological).
narrative_ontology:cs_axiom('892b5177-5d7c-47a0-9009-3fc700b85310', foundational, obligation_persists_without_venue).
narrative_ontology:cs_axiom_status(obligation_persists_without_venue, holdable).
narrative_ontology:cs_axiom_grounding('892b5177-5d7c-47a0-9009-3fc700b85310', obligation_persists_without_venue, deontological).
narrative_ontology:cs_reference_frame('892b5177-5d7c-47a0-9009-3fc700b85310', sinaitic_command_performance_owed).
narrative_ontology:cs_drift_state('892b5177-5d7c-47a0-9009-3fc700b85310', nineteen_centuries_post_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('892b5177-5d7c-47a0-9009-3fc700b85310', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, jewish_people_commanded_collective).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, halakhic_authority_community).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, temple_restoration_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Poskim, Talmudic academies, and liturgy-maintainers who rule that the sacrificial command remains binding, that study of the sacrificial orders is preparatory remembrance rather than fulfillment, and that the daily recitation carries the debt's memory without discharging it. They composed and maintain the lament liturgy that attests the community's inability. They are themselves members of the commanded collective — bound by what they administer. Adopting a sibling reading would dissolve the interpretive role they occupy; their fidelity to the kernel's standing is constitutive of their authority.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_authority_community, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__performance_only_reading, halakhic_authority_community, payer).

% The addressee of the communal sacrifices — the daily offering and the festival offerings — commanded as a collective and unable to perform for nineteen centuries. Its members recite the sacrificial orders daily, study them on the fixed curriculum, and petition for restoration in every prayer. The debt is carried in the liturgy's own words: obligations that stand unfulfilled. Exit would mean leaving the covenant the obligation is addressed to — not a live option from inside.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, jewish_people_commanded_collective, payer,
    organized, civilizational, identity_locked, global).

% Organized groups preparing restored sacrificial capacity: fabricating vessels, training priests, verifying ritual fitness of candidate animals, mapping the altar site. They experience the standing undischarged command as intolerable and respond by attacking its cause — the absence of a venue — rather than its terms. Their project's success would convert the debt from unpayable to payable.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, temple_restoration_movements, payer,
    moderate, generational, identity_locked, regional).

% Jews outside the halakhic framework who regard the sacrificial law as heritage and history rather than standing command. They would say the archive-style reading is the honest one: the texts preserve identity and memory and make no live claim on anyone. They are not bound by the obligation in their own lights, do not participate in the recitation apparatus, and are not consulted by the framework that maintains the debt's standing.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, secular_jewish_community, excluded,
    organized, biographical, mobile, global).

% Scholars of liturgy and religious history who study the recitation apparatus, the lament calendar, and the restoration movements as observable practice. They see the full four-reading structure of the kernel contest, collect nothing from the arrangement, and owe nothing to it. Their analyses document the apparatus's growth and the drift of the framework's center of gravity toward the comfort readings.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, academic_historian_of_religion, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__performance_only_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__performance_only_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: centralized national worship — one venue, one calendar, one atonement channel, with pilgrimage and provision flows coordinating the whole people. Currently, under this reading: the arrangement coordinates memory, not worship — the daily recitation of the sacrificial orders and the fixed lament calendar keep the entire community oriented to the same lost service and the same restoration claim. The reading itself denies this residual coordination any fulfilling effect.
% TRANSFER_FUNCTION: Currently nothing material moves. The arrangement holds a normative debit in place against the entire commanded collective and draws attentional labor: daily recitation, curriculum study of the sacrificial orders, and liturgical acknowledgment of the inability to discharge — flows of attention and acknowledgment from the community toward the command's standing, which this reading explicitly rules out as payment. Historically the arrangement moved animals, grain, wine, and service labor from the people to the Temple cult.
% ABSENT_VOICES: The secular and academic constituency holding the archive-style reading would object that a nineteen-century unfulfillable command binds no one and that its maintenance is memory-work, not obligation; it is outside the halakhic conversation entirely and the constraint's maintenance never consults it. Inside the framework, the holders of the study-as-exercise comfort are present but subordinated — this reading's logic overrules their discharge claim rather than engaging it. The individual who experiences the standing debt as weight without the longing-frame has no seat in the ruling structure.
% DISAPPEARANCE_RATIONALE: If the performance-only standing obligation vanished overnight — the framework adopting suspension or study-as-exercise — the liturgical lament apparatus would lose its warrant, the restoration movements would lose their halakhic urgency, and a standing-debt category would leave halakhic discourse; but no material flow changes, the recitation would likely continue as custom, and messianic hope is carried independently of this constraint. The reading's own parties hold the covenantal architecture would be genuinely altered; the archive parties hold nothing real ever depended on it. The parties dispute which world we are in — hence contested rather than world_rearranges.
% FOUNDING_PROBLEM: The arrangement was constituted by the command itself rather than built to solve a problem in the policy sense; the nearest founding-problem statement is: providing the divinely-prescribed channel through which the covenant community discharges worship and atonement — a standing service owed by the collective, performable only at the central venue.
% FOUNDING_PROBLEM_CORROBORATION: No beneficiary set exists to self-attest, but the maintaining authority attests from inside, and its attestation is the kernel's own authority structure — flagged here as structural, not dispositive. Outside corroboration: the liturgy's institutionalized lament (composed and maintained by the authority itself, so internal but self-implicating — the maintaining parties daily attest their inability to discharge); academic liturgy scholarship attests the recitation apparatus functions as memory of a lapsed practice, corroborating the gap's reality while bracketing the obligation's standing; and the Reform movement's formal 1885 repudiation of the restoration-and-sacrifice program attests, from outside the reading's community, that the founding problem is not live for repudiating parties. Stated plainly: no in-framework source independent of the interpreting authority attests the obligation's present bindingness — the attesting structure and the maintained constraint are the same structure.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.78: the arrangement levies a standing command the entire commanded collective cannot discharge, for nineteen centuries, with no in-framework discharge path — the reading's own liturgy attests the weight daily in its lament formulas and restoration petitions. This is epsilon authored from the reading's own lights: it holds the command legitimate and eternal, and authors the gap between command and capacity as the burden the arrangement imposes. Suppression 0.65 (a raw structural property, unscaled — the engine scales only effective extraction): the reading's interpretive logic forecloses every in-framework substitute — study does not fulfill, the command is not suspended, no archive status applies — and covenantal identity makes exit unthinkable from inside; there is no coercion because there is nothing enforceable. Theater_ratio 0.85: the entire operative apparatus — daily recitation of the sacrificial orders, fixed curriculum study, remembrance liturgy — is activity the reading itself explicitly denies fulfilling status; this is theater with the curtain open, an atrophied function maintained by acknowledged rehearsal rather than deception. Accessibility_collapse 0.85: within the reading, alternatives collapse completely once the constraint is understood — that collapse is the reading's content. Resistance 0.4: soft in-framework evasion (the mainstream study-as-exercise comfort is precisely a coalition path around the debt, and the framework's center of gravity has drifted toward it), formal repudiation outside the framework (the 1885 platform repudiation of the restoration-and-sacrifice program), acceleration rather than resistance from the restoration movements; little active resistance inside the observant core, where the debt is carried as longing. Claimed type piton, authored independently of the metrics: the sacrificial-Temple system was a functioning coordination (worship, calendar, pilgrimage flows, atonement channel); its primary function has been unexecutable since 70 CE; the constraint persists by interpretive inertia, identity fusion, and acknowledged theatrical maintenance; no party profits enough to maintain it for gain and no party is hurt enough to fix it — the interpreting authority could adopt the study-as-exercise reading, but the fidelity-cost to it exceeds what the standing debt costs the institution. The reading itself would claim mountain-shape (eternal divine command); this story authors the structural truth — constructed, interpreted, contested, inertial — and leaves the divergence to the engine. Coordination type identity_coordination is declared for the residual function: the apparatus synchronizes communal memory and the restoration claim across generations. The identity-framing gaming risk is answered by the reading's own structure — it explicitly denies the identity apparatus any fulfilling effect, so the residual coordination cannot serve as a cover story for the performance demand; the divergence between residual function and standing demand is the piton signature itself. Measurements share one grid (8 points, all 3 metrics at every point); suppression_requirement is tracked because this story specifically traces interpretive-maintenance capacity: the machinery bounding the study-substitute and holding the performance-only line grew from near nothing at the destruction to a settled apparatus.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seat compute differently from identical doctrine. From the commanded collective's position the arrangement is a standing debt it cannot discharge — the structure operates as burden. From the authority's position the same structure is the covenant's fidelity: the debt's undischarged standing IS the framework's integrity, and transforming it (study-as-exercise) or pausing it (suspension) would be the failure, not the relief. From the excluded secular seat the entire structure is memory-work mistaken for obligation — no debt, no burden, only archive. The engine computes these per-seat classifications from the structural data (overlapping power atoms, divergent exit options and directionalities); the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims drive the derivation: the jewish_people_commanded_collective is the declared victim and sits near the full-target end (high d), amplified by identity_locked exit and global scope, which the engine applies when scaling effective extraction from base epsilon. No beneficiary declarations exist anywhere: the command's author collects nothing per the tradition's own texts, and the interpreting authority receives interpretive standing, not the debt's proceeds — hence gain_flow 'diffuse' is an affirmative finding, checked seat by seat, not a default. One override is declared: the halakhic_authority_community (institutional) is authored at d=0.55 because it is dual-positioned — bound by the same command it administers (its members bear the debt) yet buffered by administrative agency and identity-investment in the kernel's stability; the derivation from victim declarations alone cannot see this dual position. The restoration movements sit near the target end with unusual agency: they respond to the debt by attacking its cause (capacity absence) rather than its terms. The secular constituency sits effectively outside the constraint's jurisdiction — the constraint does not reach them, so their d is not target-like despite their rejection of it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding function — providing the performable channel of commanded worship and atonement — died with the venue in 70 CE and has stayed dead for nineteen centuries; the arrangement persists. Mandatrophy is unresolved: the framework refuses both transformation (the study-as-exercise discharge) and suspension (the divine-pause ruling), so the mandate-outlived-function condition holds without the resolution that would end it. The piton classification prevents two mislabelings. It is not a snare: no collector exists, no coercion operates, and the coordination story is not cover — there is no coordination story left at all, only the debt and its rehearsal. It is not a mountain: the constraint is constructed, interpreted, and contested — three sibling readings negate it, its persistence requires continuous interpretive maintenance, and it fails the naturality profile outright. The honest middle is the piton: a dead-mandate arrangement maintained by identity and interpretation, whose fix is prohibitive from inside the framework — restoration is messianic-scale, reinterpretation is fidelity-dissolving. The R5 mismatch is documented rather than resolved: founding status contested (the parties dispute whether the owed service is live), disappearance contested (the parties dispute whether anything real depends on the debt).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status_disagreement,
    'This constraint is one reading (performance_only_reading) of the sacrifice_obligation_kernel. Which present normative status does the framework hold for the sacrificial command: standing and unfulfilled (this reading), divinely suspended until restoration (messianic_suspension_reading), discharged through study (study_as_exercise_reading), or no live halakhic claim at all (symbolic_archive_reading)?',
    'Not resolvable by evidence inside the framework: each reading''s core premise strictly negates the others''. Resolution would be an act of interpretive authority (a ruling), not a discovery. Track institutional adoption of readings over time as the observable proxy.',
    'Sibling adoption collapses this story''s epsilon: suspension removes the standing debt, study-as-exercise discharges it, archive status denies it. This story''s high-extraction profile exists only under this reading; the other three stories author their own epsilon over the same kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_status_disagreement, conceptual, 'Which reading of the sacrifice kernel governs; the disagreement is located at the command''s present normative status and mode of fulfillment.').

omega_variable(
    extraction_without_collector,
    'Does a standing normative debt with no collecting creditor, no enforcement mechanism, and no discharge path constitute extraction (burden borne by victims), or is the gap a structural impossibility closer to a natural-law profile wearing interpretive clothing?',
    'Classify by counterfactual comparison: a debt nobody can collect or pay, enforced by nothing, whose only operative effect is liturgical acknowledgment. If the framework''s own authorities would dissolve rather than collect the debt upon capacity restoration, the debt functions as orientation rather than extraction.',
    'If the gap is impossibility rather than extraction, epsilon falls toward a natural-law profile and the piton claim weakens toward a contested mountain shape; if it is extraction, the piton classification holds and victim declarations stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_without_collector, conceptual, 'Whether the unfulfilled obligation is extractive burden or structural impossibility.').

omega_variable(
    suppression_structural_vs_identity,
    'Is the constraint''s closure of alternatives structural (the reading''s interpretive logic forecloses every in-framework substitute) or internalized (covenantal identity makes the obligation constitutive, so exit is unthinkable even where formally available)?',
    'Post-exit suppression trajectory: individuals who leave the observant framework — does the sense of a standing undischarged obligation persist (internalized) or lapse with the framework (structural)?',
    'If internalized, effective suppression exceeds the structural measure and the debt travels with leavers; if structural, exit fully dissolves it and suppression is confined to the framework''s jurisdiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_identity, conceptual, 'Structural interpretive foreclosure versus internalized covenantal identity as the suppression mechanism.').

omega_variable(
    capacity_restoration_transform,
    'Is the constraint''s inertial character contingent on the venue''s continued absence — would restored sacrificial capacity (a functioning altar, qualified priests, ritually fit offerings) convert the standing debt into an operative performance demand?',
    'Observe the restoration movements'' trajectory: vessel fabrication, altar-readiness work, priestly preparation, ritual fitness verification. Any restored capacity converts the demand from unfulfillable to fulfillable and makes the constraint''s operation observable in performance.',
    'On restoration, this constraint transforms from an inertial standing debt toward an operative worship coordination with real beneficiaries and real costs; the epsilon referent changes with the arrangement, and the story should be re-authored against the new arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_restoration_transform, empirical, 'Whether restored capacity would reactivate the constraint''s original function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(sacr_tr_t400, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 400, 0.5).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 800, 0.62).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1200, 0.72).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1500, 0.78).
narrative_ontology:measurement(sacr_tr_t1800, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1800, 0.83).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1950, 0.85).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 100, 0.6).
narrative_ontology:measurement(sacr_be_t400, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 400, 0.64).
narrative_ontology:measurement(sacr_be_t800, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 800, 0.68).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1200, 0.72).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1500, 0.74).
narrative_ontology:measurement(sacr_be_t1800, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1800, 0.77).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1950, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sacr_su_t100, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 100, 0.36).
narrative_ontology:measurement(sacr_su_t400, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 400, 0.46).
narrative_ontology:measurement(sacr_su_t800, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 800, 0.53).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1200, 0.58).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1500, 0.61).
narrative_ontology:measurement(sacr_su_t1800, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1800, 0.64).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1950, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the sacrifice_obligation_kernel per the epsilon-invariance principle: the colloquial label 'the sacrifice obligation' covers four structurally distinct claims that cannot share one epsilon. This story (performance_only_reading) authors the high-epsilon standing-debt claim. Siblings: study_as_exercise_reading (low epsilon — the arrangement discharges through study and coordinates), messianic_suspension_reading (near-zero epsilon — no standing debt; divinely paused), symbolic_archive_reading (epsilon near zero — archive, no live claim). All four stories link via affects_constraints. There is no upstream/downstream empirical structure among them: each reading cites the same fixed text and diverges at the interpretation layer, so the links record shared-kernel coupling, not causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__performance_only_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
