% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Enforcement of Pre-Egalitarian Gender Rulings as Binding Law (Progressive-Abrogation Reading)
 *   domain: religious/legal/hermeneutic/gender
 *
 * SUMMARY:
 *   This story instantiates the progressive_abrogation reading of the
 *   quranic_gender_verses kernel and classifies the standing arrangement
 *   under contest: the continued enforcement of the Qur'an's earlier
 *   gender-specific rulings (differential inheritance shares in 4:11, reduced
 *   testimony weight in 2:282, marital disciplinary authority in 4:34) as
 *   binding law, assessed by this reading's own lights. On this reading those
 *   rulings were transitional steps in a revealed trajectory whose later,
 *   universal strata (49:13 and cognates) supersede them through naskh; their
 *   continued enforcement is therefore the enforcement of superseded law,
 *   sustained by institutions whose authority depends on denying the
 *   supersession. Epsilon (0.88) is authored for the standing arrangement —
 *   never for the egalitarian arrangement this reading endorses, which would
 *   drive epsilon toward zero and make every advocacy reading vacuous. The
 *   claim (snare) and the metrics are independent authored facts: the claim
 *   is this seat's structural verdict; the metrics describe the arrangement's
 *   observed operation. Sibling readings are separate constraint files linked
 *   through network.affects_constraints; the kernel contest is routed to
 *   omegas, not folded into this classification. Interval t=0..100
 *   approximates the century from post-caliphate codification (1920s) to the
 *   present. KEY AGENTS (by structural relationship): -
 *   women_subject_to_differentiated_rules: primary target
 *   (moderate/constrained) — bear the differentiated shares, testimony
 *   weight, guardianship, and disciplinary authority - male_guardians:
 *   primary beneficiary (powerful/mobile) — receive double estate shares,
 *   contracting authority, and disciplinary license without administering
 *   anything - traditional_juristic_establishment: agenda-setter
 *   (institutional/identity_locked) — certifies what the texts require; its
 *   standing depends on the rules remaining in force -
 *   state_family_law_courts: enforcement arm (institutional/constrained) —
 *   apply codified personal-status law with no discretion to apply an
 *   egalitarian reading - reformist_scholars: suppressed alternative-carriers
 *   (organized/constrained) — advance the abrogation argument from outside
 *   the endowed system - literal_identity_communities: identity-bound
 *   beneficiaries (moderate/identity_locked) — collect continuity and
 *   coherence from the arrangement's persistence -
 *   academic_quranic_studies_scholars: analytical observer
 *   (analytical/analytical) — document chronology and the classical
 *   abrogation debates
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.88).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.75).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, snare).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Enforcement of Pre-Egalitarian Gender Rulings as Binding Law (Progressive-Abrogation Reading)").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/legal/hermeneutic/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, '78c95e4b-715f-4e57-b9f3-cb6bd3bd5471').
narrative_ontology:cs_kernel_codification('78c95e4b-715f-4e57-b9f3-cb6bd3bd5471', fixed_text).
narrative_ontology:cs_authority_grounding('78c95e4b-715f-4e57-b9f3-cb6bd3bd5471', extraction).
narrative_ontology:cs_interpretation_layer_present('78c95e4b-715f-4e57-b9f3-cb6bd3bd5471').
narrative_ontology:cs_reading_relation('78c95e4b-715f-4e57-b9f3-cb6bd3bd5471', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('78c95e4b-715f-4e57-b9f3-cb6bd3bd5471', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_axiom('78c95e4b-715f-4e57-b9f3-cb6bd3bd5471', foundational, quranic_arc_terminates_in_universal_dignity).
narrative_ontology:cs_axiom_status(quranic_arc_terminates_in_universal_dignity, holdable).
narrative_ontology:cs_axiom_grounding('78c95e4b-715f-4e57-b9f3-cb6bd3bd5471', quranic_arc_terminates_in_universal_dignity, theological).
narrative_ontology:cs_axiom('78c95e4b-715f-4e57-b9f3-cb6bd3bd5471', secondary, naskh_extends_to_gender_specific_rulings).
narrative_ontology:cs_axiom_status(naskh_extends_to_gender_specific_rulings, holdable).
narrative_ontology:cs_axiom_grounding('78c95e4b-715f-4e57-b9f3-cb6bd3bd5471', naskh_extends_to_gender_specific_rulings, conventional).
narrative_ontology:cs_reference_frame('78c95e4b-715f-4e57-b9f3-cb6bd3bd5471', progressive_egalitarian_trajectory).
narrative_ontology:cs_drift_state('78c95e4b-715f-4e57-b9f3-cb6bd3bd5471', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('78c95e4b-715f-4e57-b9f3-cb6bd3bd5471', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, traditional_juristic_establishment).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, male_guardians).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, literal_identity_communities).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, women_subject_to_differentiated_rules).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, reformist_scholars).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, timeless_application_doctrine).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, juristic_gatekeeping_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains judges and muftis, certifies juristic competence, and answers legal questions from the revealed corpus through the inherited schools. Its teaching posts, court appointments, and public standing depend on being the body that mediates what the texts require. Recognizing that the gender-specific rulings have been superseded would remove a large province of its jurisdiction, so its councils decline to hear abrogation arguments on these verses and treat those who press them as outside the fold of qualified scholarship.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_juristic_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Register marriages, divide estates, and hear family disputes under personal-status codes that codify the classical shares and guardianship rules. Judges are bound by statute and cannot apply an egalitarian reading however persuaded they are of it; change arrives only when legislatures revise the codes, which has happened rarely and partially.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, state_family_law_courts, agenda_setter,
    institutional, generational, constrained, national).

% As fathers, husbands, and brothers they contract marriages for female relatives, receive twice a daughter's or sister's portion of an estate, and hold disciplinary authority recognized in the classical texts. These entitlements arrive automatically with the rules; no office administers them on their behalf. Opting out is always individually available to them, since the rules confer rather than cost them.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, male_guardians, beneficiary,
    powerful, biographical, mobile, global).

% Live under the rules as their subjects: inherit half a brother's share, testify at half weight in classical doctrine, require a guardian's consent or presence for marriage, and fall under a husband's corrective authority. Exit runs through family rupture, community severance, or emigration, and in several jurisdictions through accusations of abandoning religion. Where reform governments have loosened the codes, many of the same women report preferring the changed arrangements — but they did not author the change and cannot extend it.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, women_subject_to_differentiated_rules, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, women_subject_to_differentiated_rules, excluded).

% Publish chronological studies of revelation and argue that the universal-dignity strata of the text carry its final word on gender. Official councils will not seat them; endowed universities decline to hire them; some have faced innovation or apostasy charges. They work through secular academies, advocacy networks, and presses outside the endowed system, which limits their reach into the communities whose law they seek to change.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, reformist_scholars, excluded,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, reformist_scholars, payer).

% Ordinary worshippers whose prayer, family life, and sense of a transmitted, intact religion are braided together with receiving the texts as their grandparents did. They hold no office and run nothing, but the arrangement assures them that nothing essential has been lost between generations. A court or parliament overturning the rules overnight lands on them as dispossession of inherited certainty, whatever it grants their daughters on paper.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, literal_identity_communities, beneficiary,
    moderate, generational, identity_locked, global).

% Date the revelations, reconstruct the occasions of the verses, and document the classical debates over the scope of abrogation. They hold no enforcement role and collect no revenue from any outcome; their stake is descriptive accuracy about the text and its interpretation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, academic_quranic_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__progressive_abrogation, male_guardians).
narrative_ontology:fixing_cost_class(quranic_gender_verses__progressive_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single revealed source and an inherited method for contracting marriages, dividing estates, and weighing testimony across a vast and diverse community, so that family law does not fragment back into tribal custom.
% TRANSFER_FUNCTION: Moves estate value from daughters and widows to male heirs at a fixed ratio; moves decision authority over women's marriage, movement, and bodily correction to fathers, husbands, and guardians; and moves interpretive authority — and the livelihood attached to it — to the juristic class that certifies what the texts require.
% ABSENT_VOICES: Women governed by the rules sat outside the councils that fixed them and remain largely absent from official fatwa bodies today. Reformist scholars working the abrogation argument are unseated from the endowed institutions whose deliberations decide what the law is. In several jurisdictions, citizens governed by the personal-status codes without consenting to them had no vote in their codification.
% DISAPPEARANCE_RATIONALE: Family law across dozens of jurisdictions would rewrite itself within a generation: estate-division ratios, guardianship requirements, and disciplinary provisions would fall to egalitarian replacements already drafted in reform-era codes; the juristic schools would lose their largest remaining jurisdiction over daily life; and the communities bound to the transmitted reading would undergo a forced, painful renegotiation of inherited certainty rather than a quiet continuation.
% FOUNDING_PROBLEM: Constituting a new community's family law out of revelation while dismantling pre-Islamic practices — female infanticide, unrestricted polygyny, denied inheritance for women — by incremental steps that a seventh-century tribal-property society could absorb.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by academic historians of early Islamic law, who document the gradualist arc and its completion; by Muslim-majority states (Tunisia since 1956, Morocco since 2004) that administer stable, legitimate family law on egalitarian bases; and by the documented preferences of women living under reformed codes. The juristic establishment does not corroborate — it attests the rules as timeless — and that refusal, from the seat that benefits, is itself the signal the genealogy interview looks for.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored very high (0.88) because, from this reading's seat, the arrangement enforces rulings whose binding force has been superseded: the entire asymmetric content is rent with no remaining justification, while the administrative shell around it (registration, dispute resolution) would function identically under an egalitarian code. Suppression (0.75) is structural and uneven: codified statutes bind judges, councils refuse a hearing to abrogation arguments, and scholars adopting the reading pay career-level costs — but reform jurisdictions demonstrate the suppression is political, not absolute. Theater (0.48) tracks the defensive share of juristic activity: apologetics sustaining the rules against the abrogation argument rather than administering law. Accessibility_collapse (0.55): the egalitarian alternative is visible and has operated successfully where adopted, so alternatives do not fully collapse. Resistance (0.62) reflects a century of organized reformist and feminist pressure. The temporal series share one grid (t=0..100) and show a concession-and-recapture cycle: codification hardens enforcement (t0–t20), partial reform relaxes it (t30–t40), revivalist mobilization re-hardens it (t50–t60), a second reform wave relaxes it (t70–t80), and renewed counter-pressure re-tightens it (t90–t100). The oscillation is partly an extraction mechanism in itself: periodic concessions discharge reform pressure without dismantling the structure, after which enforcement recovers — intermittent reinforcement at institutional scale. Receipt: the material gains demonstrably accrue to male_guardians (estate shares, contracting and disciplinary authority), so gain_flow names that seat rather than 'diffuse'; fixing_cost is 'prohibitive' — the jurisdictions positioned to legislate parity have found the identity and legitimacy costs of full repeal to exceed the benefit, settling for partial codes after decades.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the establishment seat the arrangement is continuity itself — the unbroken mediation of revelation — and its identity-locked exit means it experiences any application of abrogation as annihilation rather than correction. From the women's seat the same structure is a fixed levy on inheritance, testimony, and bodily autonomy with exit priced in family rupture. The courts occupy a middle position: bound executors who collect institutional purpose but no discretionary rent. Reformist scholars experience the arrangement primarily as suppression of their persons rather than as rules they live under. The engine derives these divergent per-seat classifications from role, power, and exit data; the authored snare claim is the reading's own structural verdict, not an average over seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (traditional_juristic_establishment, male_guardians, literal_identity_communities) derive low d — the arrangement subsidizes them; the victim (women_subject_to_differentiated_rules) derives high d, amplified by constrained exit. The courts sit near-symmetric: they administer without materially capturing. No directionality_overrides are authored: the derivation already separates the two moderate-power seats that share a power atom (women as constrained-exit victims versus literal_identity_communities as identity-locked beneficiaries), and a power-atom-keyed override would collide across them. Identity locking binds the establishment institutionally (the organization has become its mediating function) and the literal-identity communities ideologically (exit equals losing the transmitted world); the exit atoms encode both directly.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying from this reading's seat as snare rather than tangled_rope keeps the separability finding load-bearing: the administrative shell is genuine coordination that an egalitarian code performs equally well, so the asymmetric content cannot borrow the shell's legitimacy — a tangled_rope label would concede coordination value the reading denies the asymmetric rules possess. Against false-summit mislabeling: the arrangement presents in the natural-law register (divine ordinance, timeless), but it declares identifiable beneficiaries and requires active enforcement — the false-summit signature — so the mountain claim is refused and the naturalness ambiguity is carried in an omega instead. Mandatrophy: the founding problem (transitional ordering of a seventh-century tribal-property community) is dead by this reading's account, yet the arrangement persists and the world still rearranges around it — the dead × world_rearranges mismatch flags zombie/capture operation, cross-checked against the theater series, whose defensive share rises with each reform wave. The classification therefore blocks two symmetrical errors: reading the arrangement as eternal nature (mountain) and reading it as necessary coordination (rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This file instantiates the progressive_abrogation reading of the quranic_gender_verses kernel; how would the constraint''s structure differ under the sibling readings?',
    'Generate literal_hierarchical and contextual_egalitarian as separate stories and compare victim sets, epsilon, and computed types across the family.',
    'literal_hierarchical would author near-zero epsilon from its own seat (rules as legitimate ordinance, women''s legal position reframed as divinely assigned station); contextual_egalitarian would author moderate epsilon (framework retained, meanings revised). Cross-family comparison is the measurement; this file contributes one vertex.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of the gender-verses kernel.').

omega_variable(
    naskh_scope_disagreement,
    'Does the juristic doctrine of naskh extend from ritual and procedural particulars to gender-specific rulings, and does 49:13 operate as an abrogating principle rather than a general exhortation?',
    'Systematic usul-al-fiqh analysis of abrogation''s accepted scope, plus textual-chronological study of the dignity strata''s revelatory sequence relative to 4:11, 2:282, and 4:34.',
    'If naskh''s scope excludes these verses, this reading collapses toward contextual_egalitarian and the standing arrangement regains a reformist-defensible core; if the scope includes them, the arrangement loses all legitimacy from every reformist seat and the snare reading hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_scope_disagreement, conceptual, 'Where the kernel contest is located: the reach of abrogation.').

omega_variable(
    scholar_exit_cost_magnitude,
    'How high are the actual exit costs for scholars who adopt this reading inside traditional institutions — employment, certification, standing, safety?',
    'Career-trajectory data on reformist jurists: hiring and defrocking records, innovation or apostasy charges, and publication-channel shifts out of the endowed system.',
    'High realized costs raise the measured suppression of the arrangement and support its snare classification; low costs would indicate open contestation and pull the classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholar_exit_cost_magnitude, empirical, 'Suppression of the alternative reading, measured on careers.').

omega_variable(
    internalized_vs_structural_compliance,
    'Is women''s compliance with the differentiated rules carried by structural enforcement or by internalized conviction that the rules are divine?',
    'Post-reform preference trajectories in jurisdictions that loosened the codes (Tunisia, Morocco): if expressed preference for equal arrangements persists and stabilizes once legally permitted, the internalized component is smaller than the structural one.',
    'If internalized, effective suppression exceeds the structural measure and legal reversal alone under-delivers; the surplus routes to interpretive-community change rather than statutory change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_compliance, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    epistemic_violence_transition_cost,
    'Does the welfare gain to women from full legal parity outweigh the identity disruption imposed on communities whose transmitted certainty the reversal breaks — and who is entitled to price that trade?',
    'Compare staged transitions (gradual codification with interpretive accompaniment) against abrupt judicial imposition, using longitudinal community-cohesion and women''s-outcome data.',
    'If identity costs dominate the short run, abrupt imposition backfires and staged codification dominates; the classification of the standing arrangement is unchanged, but the remedial path differs sharply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_violence_transition_cost, preference, 'Transition-cost trade-off the reading''s implementation must price.').

omega_variable(
    authority_grounding_framing,
    'Is the juristic establishment''s authority genuinely transmitted lineage, as it claims, or extraction-preserved authority that survives by preventing revision, as this reading diagnoses?',
    'Observe whether the establishment tolerates internal revision on non-gender questions without loss of standing: genuine lineage transmits authority across revisions; extraction-preserved authority contracts to defend the revenue-bearing kernel.',
    'Lineage grounding would soften the commitment-system classification and slow foreclosure computation; extraction grounding accelerates both and aligns the commitment-system layer with the snare metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Framing under-determination in the authority structure''s grounding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(qura_tr_t10, quranic_gender_verses__progressive_abrogation, theater_ratio, 10, 0.32).
narrative_ontology:measurement(qura_tr_t20, quranic_gender_verses__progressive_abrogation, theater_ratio, 20, 0.35).
narrative_ontology:measurement(qura_tr_t30, quranic_gender_verses__progressive_abrogation, theater_ratio, 30, 0.36).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.38).
narrative_ontology:measurement(qura_tr_t50, quranic_gender_verses__progressive_abrogation, theater_ratio, 50, 0.44).
narrative_ontology:measurement(qura_tr_t60, quranic_gender_verses__progressive_abrogation, theater_ratio, 60, 0.47).
narrative_ontology:measurement(qura_tr_t70, quranic_gender_verses__progressive_abrogation, theater_ratio, 70, 0.45).
narrative_ontology:measurement(qura_tr_t80, quranic_gender_verses__progressive_abrogation, theater_ratio, 80, 0.42).
narrative_ontology:measurement(qura_tr_t90, quranic_gender_verses__progressive_abrogation, theater_ratio, 90, 0.46).
narrative_ontology:measurement(qura_tr_t100, quranic_gender_verses__progressive_abrogation, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.76).
narrative_ontology:measurement(qura_be_t10, quranic_gender_verses__progressive_abrogation, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(qura_be_t20, quranic_gender_verses__progressive_abrogation, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(qura_be_t30, quranic_gender_verses__progressive_abrogation, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(qura_be_t50, quranic_gender_verses__progressive_abrogation, base_extractiveness, 50, 0.88).
narrative_ontology:measurement(qura_be_t60, quranic_gender_verses__progressive_abrogation, base_extractiveness, 60, 0.9).
narrative_ontology:measurement(qura_be_t70, quranic_gender_verses__progressive_abrogation, base_extractiveness, 70, 0.86).
narrative_ontology:measurement(qura_be_t80, quranic_gender_verses__progressive_abrogation, base_extractiveness, 80, 0.83).
narrative_ontology:measurement(qura_be_t90, quranic_gender_verses__progressive_abrogation, base_extractiveness, 90, 0.86).
narrative_ontology:measurement(qura_be_t100, quranic_gender_verses__progressive_abrogation, base_extractiveness, 100, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(qura_su_t10, quranic_gender_verses__progressive_abrogation, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(qura_su_t20, quranic_gender_verses__progressive_abrogation, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(qura_su_t30, quranic_gender_verses__progressive_abrogation, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(qura_su_t50, quranic_gender_verses__progressive_abrogation, suppression_requirement, 50, 0.74).
narrative_ontology:measurement(qura_su_t60, quranic_gender_verses__progressive_abrogation, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(qura_su_t70, quranic_gender_verses__progressive_abrogation, suppression_requirement, 70, 0.72).
narrative_ontology:measurement(qura_su_t80, quranic_gender_verses__progressive_abrogation, suppression_requirement, 80, 0.68).
narrative_ontology:measurement(qura_su_t90, quranic_gender_verses__progressive_abrogation, suppression_requirement, 90, 0.72).
narrative_ontology:measurement(qura_su_t100, quranic_gender_verses__progressive_abrogation, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, resource_allocation).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what the Qur'an says about gender' decomposes into three structurally distinct constraints — one per reading of the shared kernel. This file (progressive_abrogation) authors epsilon 0.88 for the standing arrangement as seen from the abrogationist seat; literal_hierarchical authors epsilon from a seat that finds the arrangement legitimate; contextual_egalitarian sits between. The upstream member is literal_hierarchical (historically dominant, cited as settled), which this reading structurally pressures; links run through affects_constraints so legitimacy shifts propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
