% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Unbroken Liturgical Use of Hebrew as the Vitality Kernel (Liturgical Reading)
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This story authors the liturgical reading of the Hebrew-vitality kernel
 *   as a clean, epsilon-invariant constraint. The standing arrangement under
 *   assessment: from roughly the close of the Mishnaic era onward, Hebrew
 *   ceased to be anyone's daily vernacular, yet remained the language of
 *   prayer, scripture recitation, law, and learned correspondence, maintained
 *   without interruption by rabbinic institutions across some fourteen
 *   centuries and every inhabited region of the diaspora. Assessed by this
 *   reading's own lights, that unbroken use IS the language's life: a
 *   Yemenite congregation and a Rhineland academy recite the same texts, a
 *   Cairo merchant drafts a Sicilian colleague in Hebrew, and the chain of
 *   transmission — text, vocalization, rite — never lapses. On this reading
 *   the arrangement is a coordination achievement with custodial
 *   beneficiaries and no victim set: preservation demands devotion and study,
 *   not a transferred burden. The mild structural surplus (interpretive
 *   authority) accrues to the rabbinic custodians whose office the continuity
 *   constitutes. This is one reading of a decomposed kernel; the sibling
 *   readings are separate constraint files (see kernel_context and
 *   network.dual_formulation_note). KEY AGENTS (by structural relationship):
 *   - rabbinic_authorities: custodial agenda-setter
 *   (institutional/identity_locked) — administers rite and curriculum; the
 *   continuity constitutes their office; collects interpretive authority -
 *   diaspora_textual_community: primary beneficiary (organized/constrained) —
 *   prays, studies, and corresponds in the shared language across mutually
 *   unintelligible vernaculars - hebrew_textual_specialists: secondary
 *   beneficiary (moderate/constrained) — masoretes, scribes, grammarians,
 *   liturgical poets, printers who maintain text and recitation -
 *   vernacular_prayer_advocates: excluded voice (moderate/constrained) —
 *   vernacular devotional movements seeking intelligible worship at the
 *   arrangement's margins - medieval_hebrew_grammarians: analytical observer
 *   (analytical/analytical) — document the language's registers from within
 *   the tradition
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.16).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.2).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Unbroken Liturgical Use of Hebrew as the Vitality Kernel (Liturgical Reading)").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, 'f2e341af-bbf0-4bfa-b62a-39301c1c836e').
narrative_ontology:cs_kernel_codification('f2e341af-bbf0-4bfa-b62a-39301c1c836e', fixed_text).
narrative_ontology:cs_authority_grounding('f2e341af-bbf0-4bfa-b62a-39301c1c836e', lineage).
narrative_ontology:cs_interpretation_layer_present('f2e341af-bbf0-4bfa-b62a-39301c1c836e').
narrative_ontology:cs_reading_relation('f2e341af-bbf0-4bfa-b62a-39301c1c836e', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2e341af-bbf0-4bfa-b62a-39301c1c836e', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('f2e341af-bbf0-4bfa-b62a-39301c1c836e', foundational, unbroken_liturgical_use_is_life).
narrative_ontology:cs_axiom_status(unbroken_liturgical_use_is_life, holdable).
narrative_ontology:cs_axiom_grounding('f2e341af-bbf0-4bfa-b62a-39301c1c836e', unbroken_liturgical_use_is_life, deontological).
narrative_ontology:cs_axiom('f2e341af-bbf0-4bfa-b62a-39301c1c836e', secondary, ritual_register_sufficiency).
narrative_ontology:cs_axiom_status(ritual_register_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('f2e341af-bbf0-4bfa-b62a-39301c1c836e', ritual_register_sufficiency, conventional).
narrative_ontology:cs_reference_frame('f2e341af-bbf0-4bfa-b62a-39301c1c836e', unbroken_liturgical_continuum).
narrative_ontology:cs_drift_state('f2e341af-bbf0-4bfa-b62a-39301c1c836e', eve_of_vernacular_revival, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f2e341af-bbf0-4bfa-b62a-39301c1c836e', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, diaspora_textual_community).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, hebrew_textual_specialists).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, unbroken_masoretic_chain).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, lashon_hakodesh_sanctity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fix the liturgical rite, the curriculum of study, and the norms of recitation and pronunciation across the diaspora. Their office is constituted by the chain of transmission they claim to embody: a gaon, a rishon, an aharonida rabbi inherits text, vocalization, and rite from predecessors and passes them on. What flows to them is interpretive authority and custodial standing; what flows from them is the maintained text and rite. Exiting the Hebrew liturgical world would dissolve the office itself rather than relocate its holder.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, rabbinic_authorities, beneficiary).

% Pray, recite scripture, study, correspond, draft contracts, and write poetry in Hebrew across communities whose daily vernaculars — Aramaic, Greek, Arabic, Yiddish, Ladino, Judeo-Arabic, Persian — are mutually unintelligible. The shared language is what lets a Cairo merchant write a Sicilian colleague and a Rhineland student cite a Babylonian academy. Devotion and study hours flow into the Hebrew channel; a shared textual world flows back. Leaving it means leaving the community's normative life, not switching languages.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, diaspora_textual_community, beneficiary,
    organized, generational, constrained, global).

% Masoretes, scribes, grammarians, liturgical poets, and later printers whose craft, livelihood, and standing depend on maintaining the text, its vocalization, and its recitation traditions. Their skill exists only inside the arrangement; their exit would forfeit the profession, not merely a position within it.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, hebrew_textual_specialists, beneficiary,
    moderate, biographical, constrained, continental).

% Those who sought devotion in the spoken tongue: readers and authors of Yiddish devotional literature, Ladino devotional verse, and the nineteenth-century reformers who introduced vernacular prayer services. They worship at the margins of the arrangement or break with communal norms over it. Their objection is that worship should be intelligible to the worshipper; their voice sits largely outside the councils that fix the rite.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, vernacular_prayer_advocates, excluded,
    moderate, biographical, constrained, regional).

% From Saadia Gaon through the Kimhi family, scholars who analyzed the language's structure, vocalization, and history from within the tradition, documenting its registers and their changes without administering the rite. They see the arrangement whole, across centuries, from a descriptive rather than custodial seat.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, medieval_hebrew_grammarians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(hebrew_vitality__liturgical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps one authoritative language for scripture, prayer, law, and learned correspondence across a diaspora of mutually unintelligible vernaculars, so that geographically scattered communities remain participants in a single textual conversation and the sacred texts remain recitable in a fixed, shared form.
% TRANSFER_FUNCTION: Moves devotional attention, study time, and communal deference into the Hebrew channel: worshippers and learners give hours of recitation and memorization to Hebrew, and status and interpretive authority flow to those who master and transmit it — the rabbinic custodians and the textual specialists.
% ABSENT_VOICES: Those who prayed without comprehension — in many communities the women excluded from the Hebrew curriculum — and the vernacular devotional movements would object that worship should be intelligible; their devotional life ran in Yiddish, Ladino, and Judeo-Arabic parallel channels at the arrangement's margins, largely outside the councils that fixed the rite.
% DISAPPEARANCE_RATIONALE: If unbroken Hebrew liturgical use vanished overnight, the diaspora's textual unity would dissolve: prayer and scripture-reading would fragment into vernaculars, cross-communal correspondence and legal responsa would lose their shared medium, the chain of transmission the rabbinic office embodies would break, and the corpus and trained readership that any later revival would in fact draw on would not exist in maintained form.
% FOUNDING_PROBLEM: After Hebrew ceased to be anyone's daily vernacular in late antiquity, the dispersed communities needed a way to keep scripture, law, and prayer in one authoritative language across a population speaking Aramaic, Greek, Arabic, and later Yiddish, Ladino, Judeo-Arabic, and Persian — and a way to remain one community despite that linguistic fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Cairo Geniza letters show Hebrew and Aramaic serving as the working medium of trade and scholarship between communities from India to Sicily — the coordination demand is attested by the correspondence itself; medieval Karaite communities, which rejected rabbinic authority, nonetheless adopted Hebrew for prayer and scholarship, indicating the demand was not an artifact of rabbinic interest; and parallel sacred-language maintenance in other dispersed traditions (Syriac, ecclesiastical Latin, Ge'ez) shows the pattern answers a general multilingual-dispersion problem rather than a locally designed rent structure.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.16 at interval end) because the referent is the standing liturgical arrangement assessed by this reading's lights: devotion and study flow into Hebrew as participation, and the only surplus above coordination cost is custodial status, not a burden transferred from a victim class. Suppression (0.20) is normative rather than coercive — communal expectation, curriculum, and rite — and rises only at the interval's end, when vernacular prayer movements and the nineteenth-century reform liturgies provoke intensified defense of the Hebrew norm; that enforcement arc is why suppression_requirement is tracked on the shared grid rather than left as a static scalar. Theater ratio stays low (0.12) because under this reading the ritual is the function — recitation is not a performance standing in for some other activity — with a slight rise as rote recitation spreads among populations distant from the academies. Accessibility collapse is moderate (0.45): vernacular glosses and parallel devotional channels existed and were used at the margins, so alternatives never fully collapsed. Resistance (0.30) reflects the vernacular devotional movements and later reform liturgies. The claimed type (rope) is authored from the reading's lights; the metrics are authored descriptively of the arrangement's actual operation; the two are independent authored facts. All three tracked metrics run on one shared eight-point grid so the engine samples a consistent temporal surface.
 *
 * PERSPECTIVAL GAP:
 *   The custodial seat and the lay seat compute differently from the same structure: from the rabbinate the arrangement is the chain of transmission they embody — its persistence is their office; from the textual community it is the shared medium that makes scattered vernacular communities one conversation; from the excluded vernacular advocates it is an intelligibility barrier their devotion must route around. The engine computes these per-seat classifications from the power and exit atoms; this story's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for every seated actor: the custodians (agenda_setter/beneficiary, identity_locked) sit near the beneficiary end — the arrangement subsidizes their authority — and the textual community and specialists likewise. No victim set is declared, so no seat derives high directionality. The global spatial scope scales effective extraction modestly upward, but from a low base. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. No directionality overrides were needed: the beneficiary declarations plus exit atoms capture the structure, since every real actor here is a net beneficiary or an excluded outsider rather than a target.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading-indexed referent prevents misclassification in both directions. An observer importing the native-daily frame would count incomprehension as extraction and score the arrangement as extraction from the linguistically excluded; fixing the referent to the standing liturgical arrangement assessed by its own lights keeps epsilon low and makes the coordination claim testable rather than assumed. Conversely, the founding problem — textual unity across a multilingual diaspora — remains live across the whole interval, corroborated from outside the beneficiary set (Geniza correspondence, Karaite Hebrew adoption), so the arrangement is not a zombie mandate: founding_problem_status live paired with disappearance_verdict world_rearranges raises no capture flag. The gentle rise in custodial surplus is tracked temporally so that any post-interval accumulation would surface as drift rather than being baked into the base claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the liturgical_reading of kernel hebrew_vitality: what would the sibling readings change structurally if adopted as the classification''s basis?',
    'The sibling constraint files (hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading) instantiate those readings; cross-file comparison of epsilon, beneficiary sets, and claimed types resolves the structural delta.',
    'The native_daily reading would treat the pre-revival generations as speakers of a dead vernacular, raising epsilon and adding a victim set of the linguistically excluded; the hybrid reading would split the arrangement into a necessary enabler plus an insufficiency, moving the account toward a transitional substrate story. This file''s low-epsilon, no-victim authoring holds only within the liturgical reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a three-reading kernel; the disagreement is located in the definition of vitality and the victim-set question.').

omega_variable(
    custodial_benefit_vs_rent,
    'Is the rabbinic custodial benefit the legitimate return on a real coordination service, or the rent of an interpretive position the arrangement itself maintains?',
    'Comparative evidence: Hebrew adoption by communities outside rabbinic authority (the Karaites), the volume and necessity of cross-diaspora Hebrew correspondence in the Geniza record, and the fate of vernacular liturgy where it was actually attempted.',
    'If the coordination service is real and demanded, the low-epsilon coordination account stands; if the service is replaceable at low cost while custodial authority persists, the arrangement carries a concentrated beneficiary over a replaceable function and the computed classification moves toward a hybrid coordination/extraction account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodial_benefit_vs_rent, empirical, 'Whether custodial benefit tracks service rendered or monopoly maintained.').

omega_variable(
    no_cost_presumption,
    'The reading holds that preservation imposes no cost; does the standing arrangement in fact bear a diffuse cost-bearer — worshippers reciting without comprehension, learners excluded from the Hebrew curriculum — that the reading''s own frame renders invisible?',
    'Devotional-practice evidence: comprehension rates among reciters across the period, the size and persistence of vernacular parallel liturgy (Yiddish devotional literature, Ladino devotional verse), and testimony of those who sought vernacular worship.',
    'A demonstrated diffuse cost-bearer would add a victim set, raise epsilon, and move the computed classification away from pure coordination; its absence would corroborate the reading''s no-cost premise and stabilize the low-epsilon authoring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(no_cost_presumption, conceptual, 'Whether the no-victim-set declaration survives contact with devotional-practice evidence.').

omega_variable(
    kernel_framing_language_vs_authority,
    'Is the kernel the Hebrew language itself (a linguistic entity whose vitality is at issue) or the masoretic authority structure (the institutions claiming to embody unbroken transmission)? The two framings assign different referents to the arrangement under assessment.',
    'Framing test: classify the arrangement with the kernel read as the language (coordination of a dispersed textual community) and again as the authority structure (the custodial chain and its interpretive position); compare the computed classifications.',
    'The language-framing yields a low-extraction coordination account (this file''s authoring); the authority-framing sharpens the custodial-benefit question and could surface capture structure that is invisible under the language framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_language_vs_authority, conceptual, 'CS-framing under-determination: language-entity kernel versus authority-structure kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 500, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t500, hebrew_vitality__liturgical_reading, theater_ratio, 500, 0.06).
narrative_ontology:measurement_basis(hebr_tr_t500, observed).
narrative_ontology:measurement(hebr_tr_t700, hebrew_vitality__liturgical_reading, theater_ratio, 700, 0.07).
narrative_ontology:measurement_basis(hebr_tr_t700, observed).
narrative_ontology:measurement(hebr_tr_t900, hebrew_vitality__liturgical_reading, theater_ratio, 900, 0.08).
narrative_ontology:measurement_basis(hebr_tr_t900, observed).
narrative_ontology:measurement(hebr_tr_t1100, hebrew_vitality__liturgical_reading, theater_ratio, 1100, 0.09).
narrative_ontology:measurement_basis(hebr_tr_t1100, observed).
narrative_ontology:measurement(hebr_tr_t1300, hebrew_vitality__liturgical_reading, theater_ratio, 1300, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t1300, observed).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_vitality__liturgical_reading, theater_ratio, 1500, 0.11).
narrative_ontology:measurement_basis(hebr_tr_t1500, observed).
narrative_ontology:measurement(hebr_tr_t1700, hebrew_vitality__liturgical_reading, theater_ratio, 1700, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t1700, observed).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_vitality__liturgical_reading, theater_ratio, 1880, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t1880, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t500, hebrew_vitality__liturgical_reading, base_extractiveness, 500, 0.1).
narrative_ontology:measurement_basis(hebr_be_t500, observed).
narrative_ontology:measurement(hebr_be_t700, hebrew_vitality__liturgical_reading, base_extractiveness, 700, 0.11).
narrative_ontology:measurement_basis(hebr_be_t700, observed).
narrative_ontology:measurement(hebr_be_t900, hebrew_vitality__liturgical_reading, base_extractiveness, 900, 0.13).
narrative_ontology:measurement_basis(hebr_be_t900, observed).
narrative_ontology:measurement(hebr_be_t1100, hebrew_vitality__liturgical_reading, base_extractiveness, 1100, 0.14).
narrative_ontology:measurement_basis(hebr_be_t1100, observed).
narrative_ontology:measurement(hebr_be_t1300, hebrew_vitality__liturgical_reading, base_extractiveness, 1300, 0.15).
narrative_ontology:measurement_basis(hebr_be_t1300, observed).
narrative_ontology:measurement(hebr_be_t1500, hebrew_vitality__liturgical_reading, base_extractiveness, 1500, 0.16).
narrative_ontology:measurement_basis(hebr_be_t1500, observed).
narrative_ontology:measurement(hebr_be_t1700, hebrew_vitality__liturgical_reading, base_extractiveness, 1700, 0.16).
narrative_ontology:measurement_basis(hebr_be_t1700, observed).
narrative_ontology:measurement(hebr_be_t1880, hebrew_vitality__liturgical_reading, base_extractiveness, 1880, 0.16).
narrative_ontology:measurement_basis(hebr_be_t1880, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t500, hebrew_vitality__liturgical_reading, suppression_requirement, 500, 0.08).
narrative_ontology:measurement_basis(hebr_su_t500, observed).
narrative_ontology:measurement(hebr_su_t700, hebrew_vitality__liturgical_reading, suppression_requirement, 700, 0.09).
narrative_ontology:measurement_basis(hebr_su_t700, observed).
narrative_ontology:measurement(hebr_su_t900, hebrew_vitality__liturgical_reading, suppression_requirement, 900, 0.1).
narrative_ontology:measurement_basis(hebr_su_t900, observed).
narrative_ontology:measurement(hebr_su_t1100, hebrew_vitality__liturgical_reading, suppression_requirement, 1100, 0.11).
narrative_ontology:measurement_basis(hebr_su_t1100, observed).
narrative_ontology:measurement(hebr_su_t1300, hebrew_vitality__liturgical_reading, suppression_requirement, 1300, 0.12).
narrative_ontology:measurement_basis(hebr_su_t1300, observed).
narrative_ontology:measurement(hebr_su_t1500, hebrew_vitality__liturgical_reading, suppression_requirement, 1500, 0.13).
narrative_ontology:measurement_basis(hebr_su_t1500, observed).
narrative_ontology:measurement(hebr_su_t1700, hebrew_vitality__liturgical_reading, suppression_requirement, 1700, 0.16).
narrative_ontology:measurement_basis(hebr_su_t1700, observed).
narrative_ontology:measurement(hebr_su_t1880, hebrew_vitality__liturgical_reading, suppression_requirement, 1880, 0.2).
narrative_ontology:measurement_basis(hebr_su_t1880, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The colloquial commitment 'Hebrew vitality' decomposes into three structurally distinct constraint stories with different epsilon values and different beneficiary/victim structures, per the epsilon-invariance principle. This file authors the liturgical reading: the standing liturgical arrangement assessed by its own lights (low epsilon, custodial beneficiaries, no victim set). hebrew_vitality__native_daily_reading authors the same historical span with a high-epsilon referent (a dead vernacular whose non-speakers bear the cost of the liturgical norm); hebrew_vitality__hybrid_continuity_reading authors the arrangement as necessary-but-insufficient substrate. The family's upstream/downstream structure runs through this reading: the maintained corpus and trained readership this arrangement produced are the substrate both siblings cite. Each file links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
