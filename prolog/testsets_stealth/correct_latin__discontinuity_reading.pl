% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Discontinuity Reading of Correct Latin: Ancient-Text Standard with Medieval Exclusion
 *   domain: intellectual_history/historical_linguistics
 *
 * SUMMARY:
 *   Between roughly 1450 and 1750 the humanist movement converted a literary
 *   preference into a governing norm: correct Latin is the classical form
 *   attested in ancient texts; the Latin that grew up after antiquity is
 *   deviation; and anyone who would write correctly must rebuild the extinct
 *   form from those texts rather than inherit it from living use. Schools
 *   retooled around Ciceronian imitation, the press rebuilt the ancient
 *   corpus, chancery and scholastic usage lost standing, and a pan-European
 *   profession of philologists became the arbiter of correctness. The epsilon
 *   referent here is this standing discontinuity regime itself, assessed by
 *   the reading's own lights: even granting the humanist premise that its
 *   burdens are restorative repair of a corrupted tongue, the regime's
 *   operation concentrates the power to declare correctness in the
 *   philological complex while the costs fall on students, rival
 *   professionals, and the posthumously condemned. Claim/metric independence:
 *   claimed_type=tangled_rope is my structural judgment (a genuine
 *   coordination function joined to asymmetric extraction under active
 *   enforcement); the metrics are my independent descriptive read of the
 *   regime's operation; the engine computes per-seat classifications from the
 *   structural data, and any divergence between my claim and a computed seat
 *   type is signal, not error. Family note: this is one of three readings of
 *   the correct_latin kernel; the continuity and hybrid readings are separate
 *   files linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - humanist_philologists: Agenda-setter and principal collector (institutional/arbitrage) — defines correctness from the ancient corpus and gathers authority, patronage, and chairs from doing so
 *   - - classical_edition_printers: Secondary beneficiary (organized/mobile) — monetizes demand for corrected texts
 *   - - latin_grammar_schoolmasters: Beneficiary-enforcer (moderate/constrained) — paid to reproduce the standard in each generation
 *   - - antiquarian_manuscript_dealers: Beneficiary (organized/arbitrage) — profits from the scarcity value of attestation sources
 *   - - scholastic_dialecticians: Primary target (organized/identity_locked) — bears condemnation of their working language; exit fuses with self-erasure
 *   - - ecclesiastical_liturgists: Insulated target (institutional/constrained) — absorbs stigma, retains functional autonomy
 *   - - notarial_legal_draftsmen: Captive target (moderate/trapped) — bound to registry-accepted formulas
 *   - - resource_poor_latin_students: Burden-bearing target (powerless/trapped) — pays the acquisition cost without resources
 *   - - condemned_medieval_authors: Absent voice (powerless/trapped) — condemned posthumously, unable to reply
 *   - - modern_historical_linguists: Analytical observer (analytical/analytical) — sees the full structure across the rupture debate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.68).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.65).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Discontinuity Reading of Correct Latin: Ancient-Text Standard with Medieval Exclusion").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "intellectual_history/historical_linguistics").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, 'a72939d7-2495-49a0-8835-4a1d19ad470f').
narrative_ontology:cs_kernel_codification('a72939d7-2495-49a0-8835-4a1d19ad470f', fixed_text).
narrative_ontology:cs_authority_grounding('a72939d7-2495-49a0-8835-4a1d19ad470f', lineage).
narrative_ontology:cs_interpretation_layer_present('a72939d7-2495-49a0-8835-4a1d19ad470f').
narrative_ontology:cs_reading_relation('a72939d7-2495-49a0-8835-4a1d19ad470f', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('a72939d7-2495-49a0-8835-4a1d19ad470f', correct_latin__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('a72939d7-2495-49a0-8835-4a1d19ad470f', foundational, legitimacy_requires_ancient_attestation).
narrative_ontology:cs_axiom_status(legitimacy_requires_ancient_attestation, holdable).
narrative_ontology:cs_axiom_grounding('a72939d7-2495-49a0-8835-4a1d19ad470f', legitimacy_requires_ancient_attestation, empirically_contingent).
narrative_ontology:cs_axiom('a72939d7-2495-49a0-8835-4a1d19ad470f', secondary, reconstruction_over_transmission).
narrative_ontology:cs_axiom_status(reconstruction_over_transmission, holdable).
narrative_ontology:cs_axiom_grounding('a72939d7-2495-49a0-8835-4a1d19ad470f', reconstruction_over_transmission, instrumental).
narrative_ontology:cs_reference_frame('a72939d7-2495-49a0-8835-4a1d19ad470f', ancient_canon_exclusive_norm).
narrative_ontology:cs_drift_state('a72939d7-2495-49a0-8835-4a1d19ad470f', contemporary_historical_linguistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a72939d7-2495-49a0-8835-4a1d19ad470f', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_edition_printers).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, latin_grammar_schoolmasters).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, antiquarian_manuscript_dealers).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, scholastic_dialecticians).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, ecclesiastical_liturgists).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, notarial_legal_draftsmen).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, resource_poor_latin_students).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, ancient_attestation_supremacy).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, rupture_necessity_thesis).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, textual_reconstruction_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and police the standard of correct Latin: they edit ancient texts, publish grammars and dictionaries of classical usage, and pronounce on the propriety of forms offered by others. Their standing in the respublica literaria, their patronage, and their chairs depend on the ancient corpus remaining the sole measure of correctness. Their skills transfer readily to diplomacy, historiography, and secretarial service, so their position is chosen as much as imposed.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, humanist_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, humanist_philologists, beneficiary).

% Profit from the demand the standard creates: every school and library needs corrected editions of Cicero, Quintilian, and the grammarians. They fund manuscript collation because accurate texts sell. They can switch their presses to any genre if the classical market turns, so their commitment is commercial rather than doctrinal.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_edition_printers, beneficiary,
    organized, biographical, mobile, continental).

% Make their living teaching boys to imitate classical models and to avoid the forms pupils bring from home or church. Employment follows the classical curriculum; a master who taught the locally evolved usage would find no parents paying fees. Movement between towns is possible but always within the same curriculum.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, latin_grammar_schoolmasters, beneficiary,
    moderate, biographical, constrained, national).

% Search monasteries and private libraries for ancient codices whose value rises with the demand for attestation. Buy cheap where old books are neglected, sell dear to princely libraries and wealthy scholars. Their position is opportunistic and geographically mobile.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, antiquarian_manuscript_dealers, beneficiary,
    organized, biographical, arbitrage, continental).

% Work inside universities and religious orders whose method, textbooks, and technical vocabulary grew up in the post-classical tradition. The humanist verdict brands their working language barbarous; adopting Ciceronian idiom wholesale would strip their disputations of the precision terms their science runs on. Their professional selves are fused with the method; leaving it means ceasing to be what they are.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, scholastic_dialecticians, payer,
    organized, generational, identity_locked, continental).

% Pray, administer sacraments, and run a worldwide administration in a Latin shaped by centuries of church use. The humanist standard stigmatizes their pronunciation and forms, but the church's size and self-sufficiency let them continue largely on their own authority; they absorb the stigma rather than the curriculum.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, ecclesiastical_liturgists, payer,
    institutional, generational, constrained, global).

% Draw contracts, wills, and court records in formulaic Latin fixed by what courts and registries will accept. The formulas descend from late-antique and medieval practice; a draftsman who substituted classical elegance for the accepted formulas would have his instruments rejected. He writes as the registry demands or not at all.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, notarial_legal_draftsmen, payer,
    moderate, biographical, trapped, national).

% Face the full acquisition cost of the reconstructed standard — years of memorizing inflections, imitating models, and unlearning home forms — without the private tutors, manuscript access, or leisure that wealthy peers command. Their families pay in fees and forgone labor; failure closes off the clerical and professional careers that require Latin.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, resource_poor_latin_students, payer,
    powerless, biographical, trapped, regional).

% Wrote within the living practice of their own centuries, following the norms their teachers handed them. Later readers reclassified their life's work as corruption and their learning as barbarism. They are dead; no forum exists in which their practice could answer the charge, and the verdict travels under their names in every subsequent catalogue.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, condemned_medieval_authors, excluded,
    powerless, generational, trapped, continental).

% Study Latin's development as language change rather than decline, comparing Romance outcomes, scribal habits, and attested variation. They take testimony from all the earlier seats and from the manuscripts themselves, and their findings bear on whether the rupture premise survives scrutiny. They collect nothing from the standard and pay nothing to it.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, modern_historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__discontinuity_reading, humanist_philologists).
narrative_ontology:fixing_cost_class(correct_latin__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors written Latin to a fixed ancient corpus, giving learned communication a stable target that does not drift with local speech; trains readers into the exact forms of the ancient authors so that texts separated by a millennium remain mutually legible; supplies a shared curriculum across political borders.
% TRANSFER_FUNCTION: Moves linguistic legitimacy from existing communities of practice (chanceries, universities, liturgy using evolved forms) to holders of manuscript access and philological training; moves years of student labor into imitation of classical models; moves payment to the editors, printers, and schoolmasters of the reconstructed standard.
% ABSENT_VOICES: The medieval authors themselves — judged corrupt posthumously, with no venue in which their practice could answer; also the working users of legal and liturgical Latin whose competence was redefined as ignorance without their consent. Both sit outside the humanist conversation that defined them.
% DISAPPEARANCE_RATIONALE: If the standard vanished overnight, learned Latin would reorganize around living transregional usage (as neo-Latin in fact dissolved once the vernaculars took over), the philological profession would lose its charter, school curricula and the edition trade would restructure, and the boundary between 'good' and 'barbarous' Latin would dissolve into descriptive variation.
% FOUNDING_PROBLEM: Latin had drifted from its ancient form and varied across regions and generations; ancient texts were read through medieval gloss and miscopied transmission; scholars wanted unmediated contact with antiquity and a learned code stable across centuries.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: modern historical linguistics and codicology corroborate the real half — ancient texts were genuinely misread and miscopied before systematic criticism, and Valla's philological demonstrations convinced parties with no stake in humanist patronage — while rejecting the corruption-deviation framing as a description of normal language change. Scholastic contemporaries disputed the barbarism charge from inside the universities. No party outside the humanist network attests the rupture thesis as stated.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 (interval end): the regime transfers legitimacy, years of student labor, and purchase demand away from existing communities of practice and toward the philological complex, and the transfer widens as schooling institutionalizes. Suppression 0.65 is a raw structural property, deliberately unscaled by power or scope: it reflects the enforcement machinery itself — classroom discipline, style policing, patronage gatekeeping, and the registry rules binding legal draftsmen — not any amplified figure. Theater_ratio 0.30: the editorial and grammatical work is mostly real, but a growing share of activity is performative purity display (Ciceronian contests, anti-barbarism polemic) that polices status rather than improving texts. Accessibility_collapse 0.45: within the reading's frame alternatives do partly collapse — once attestation is the criterion, defending evolved forms as correct is unavailable — but real exits persist (church usage, vernaculars, registry formulas), so collapse falls far short of a natural law's. Resistance 0.55: scholastics answered in kind, the church ignored the curriculum, Italianist and Ciceronian camps fought openly, and vernacular advocates bypassed the fight entirely. All three tracked series share one grid ({0,6,12,18,24,30}); no metric is sampled at a point where another is missing. Time-mapping assumption: T0 is approximately 1450 (the Valla/Elegantiae generation), one step is approximately a decade, T30 approximately 1750.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the philologist seat the arrangement is the recovery of antiquity: its burdens are the price of rescuing a corrupted tongue, and the standard is an achievement. From the scholastic seat — identity-locked, since method and vocabulary constitute the vocation — the same arrangement reads as a posthumous death sentence on a life's work. The ecclesiastical seat, large enough to run its own Latin, experiences mainly stigma; the legal draftsman experiences registry compulsion; the resource-poor student experiences a wall of acquisition cost. The engine derives these divergences from the power, exit, and role data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the philologists, printers, schoolmasters, and dealers near the beneficiary end (d near 0): the standard subsidizes their authority, revenue, employment, and trade. Victim declarations place the four target groups near the target end (d near 1). Exit modulation then separates them: the scholastics' identity lock sits them at the full-target end, since they cannot leave without ceasing to be themselves; the students' poverty traps them near it; the church's institutional self-sufficiency moderates its exposure despite formal victimhood; the printers' mobility and the dealers' arbitrage damp their d below what raw beneficiary listing alone would suggest. No directionality overrides are authored: the derivation chain from role, power, and exit already yields the correct ordering, and an override keyed on a power atom would leak onto unintended seats sharing that atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reliable access to ancient texts and a learned code stable across centuries — was substantially served by print, critical edition, and the school system by the late seventeenth century; what outlived its warrant is the corruption-deviation framing that justified condemning whole communities of practice. Authoring the type as tangled_rope keeps both halves on the record: the coordination credit (a stable transregional standard, recovered texts) is not laundered into pure virtue, and the layered extraction (legitimacy transfer, forced curricula, posthumous condemnation) is not inflated into a pure-extraction reading that would erase the genuine scholarship. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: the arrangement is load-bearing, but the parties dispute whether its founding problem was ever as stated — a contested genealogy, not a dead-mandate zombie flag. Coalition note: the scholastics, the church, and the legal profession each resisted separately and lost ground piecemeal; a coordinated defense spanning university, curia, and chancery was available in principle and never assembled, which is why the powerless student seat never converted numbers into leverage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the discontinuity reading the right instantiation of the correct_latin kernel, or do the continuity and hybrid readings capture the standing arrangement better?',
    'Cross-reading comparison at the engine layer plus historical adjudication: which reading''s victim set, enforcement profile, and epsilon survive contact with the manuscript and sociolinguistic record.',
    'If the continuity reading prevails, this reading''s victim set dissolves (nothing is condemned as corrupt), its extraction drops toward the coordination floor, and classification migrates toward rope; if the hybrid reading prevails, part of the victim set converts into correction subjects rather than the condemned.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the correct_latin kernel this constraint''s structure depends on.').

omega_variable(
    corruption_vs_language_change,
    'Is post-classical divergence from the ancient forms corruption (deviation from a norm that ought to be restored) or ordinary language change (a descriptive fact with no normative remainder)?',
    'Diachronic analysis: regular sound-change correspondences among classical Latin, medieval Latin, and Romance; scribal-practice studies showing rule-governed variation rather than decay.',
    'If ordinary change, the condemnation premise fails on the evidence, the regime''s extraction loses its restorative justification, and classification pressure moves toward pure extraction riding a coordination shell, or toward outright dissolution of the standard''s authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_vs_language_change, empirical, 'Whether the rupture premise is an empirical error rather than a framing choice.').

omega_variable(
    standard_naturalness_ambiguity,
    'Is the classical standard a discovered fact about Latin (correctness simply is ancient attestation, as the reading''s definitional phrasing suggests) or a constructed regime serving identifiable interests?',
    'Counterfactual test: whether any normative force survives once the beneficiary positions (philological authority, edition markets, school employment) are removed; comparative study of standards that anchored to living usage instead.',
    'If constructed, the definitional self-presentation is cover and the regime belongs with enforced standards generally; if discovered, part of the measured burden is the unavoidable price of accuracy rather than imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standard_naturalness_ambiguity, conceptual, 'Natural-fact versus interest-serving construction ambiguity in the standard''s self-presentation.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression structural (career gates, registry compulsion, classroom discipline) or internalized (trained shame at ''barbarisms'', self-policing imitation that persists after institutional pressure lifts)?',
    'Post-exit trajectory: examine writers who left the classical curriculum (vernacular authors, churchmen outside the schools) — if self-policing and purity anxiety persist without enforcement, the internalized share is substantial.',
    'If largely internalized, effective suppression exceeds the structural measure and outlives the institutions; if largely structural, removing the gates and registries collapses it quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized composition of the regime''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__discontinuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t6, correct_latin__discontinuity_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement_basis(corr_tr_t6, observed).
narrative_ontology:measurement(corr_tr_t12, correct_latin__discontinuity_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement_basis(corr_tr_t12, observed).
narrative_ontology:measurement(corr_tr_t18, correct_latin__discontinuity_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement_basis(corr_tr_t18, observed).
narrative_ontology:measurement(corr_tr_t24, correct_latin__discontinuity_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(corr_tr_t24, observed).
narrative_ontology:measurement(corr_tr_t30, correct_latin__discontinuity_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(corr_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__discontinuity_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t6, correct_latin__discontinuity_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(corr_be_t6, observed).
narrative_ontology:measurement(corr_be_t12, correct_latin__discontinuity_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(corr_be_t12, observed).
narrative_ontology:measurement(corr_be_t18, correct_latin__discontinuity_reading, base_extractiveness, 18, 0.61).
narrative_ontology:measurement_basis(corr_be_t18, observed).
narrative_ontology:measurement(corr_be_t24, correct_latin__discontinuity_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(corr_be_t24, observed).
narrative_ontology:measurement(corr_be_t30, correct_latin__discontinuity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(corr_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__discontinuity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(corr_su_t0, observed).
narrative_ontology:measurement(corr_su_t6, correct_latin__discontinuity_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement_basis(corr_su_t6, observed).
narrative_ontology:measurement(corr_su_t12, correct_latin__discontinuity_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(corr_su_t12, observed).
narrative_ontology:measurement(corr_su_t18, correct_latin__discontinuity_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement_basis(corr_su_t18, observed).
narrative_ontology:measurement(corr_su_t24, correct_latin__discontinuity_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement_basis(corr_su_t24, observed).
narrative_ontology:measurement(corr_su_t30, correct_latin__discontinuity_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(corr_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle into a three-story family: continuity_reading (transmission legitimates; negligible condemnation structure), discontinuity_reading (this file; exclusive ancient attestation, rupture declared, reconstruction mandated — substantially extractive with a real coordination core), and hybrid_reading (partial continuity with targeted correction — intermediate victim set and intermediate epsilon). Each member carries its own epsilon, beneficiaries, victims, and enforcement profile; the upstream continuity reading is the pre-humanist default against which this reading reacted, and the hybrid reading mediates. All three files link one another through network.affects_constraints so contamination and legitimacy shifts propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
