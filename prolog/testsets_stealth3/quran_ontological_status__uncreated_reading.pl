% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__uncreated_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Uncreated Eternal Divine Speech Doctrine (Kalam Allah Qadim)
 *   domain: religious/doctrinal/political-authority
 *
 * SUMMARY:
 *   SUMMARY: This story authors the uncreated reading of the kernel
 *   quran_ontological_status as a single, epsilon-invariant constraint: the
 *   doctrine that the Qur'an is kalām Allāh qadīm — God's own eternal,
 *   uncreated speech — together with the authority regime that doctrine
 *   sustains. The standing arrangement under contest (epsilon's referent) is
 *   the lived doctrine-and-guild complex: a fixed text whose meaning is
 *   treated as fixed divine fact, guarded by a credentialed jurist class,
 *   enforced successively by dynastic states and then by institutional and
 *   social discipline, and extracting interpretive flexibility from
 *   rationalist theologians, allegorical readers, and reform movements. KEY
 *   AGENTS (by structural relationship): - traditional_jurists: Agenda setter
 *   & principal beneficiary (powerful/identity_locked) — administers
 *   orthodoxy, collects interpretive authority - caliphal_state_apparatus:
 *   Enforcement agenda setter (institutional/arbitrage) — supplies and
 *   withdraws coercive backing - literalist_communities: Beneficiary
 *   (organized/constrained) — receives unmediated certainty -
 *   anti_rationalist_schools: Beneficiary & enforcement muscle
 *   (organized/identity_locked) - rationalist_theologians: Primary target
 *   (organized/identity_locked) — bore office purges and method
 *   delegitimation - metaphorical_interpreters: Target (moderate/constrained)
 *   - reform_movements: Target (organized/constrained) - ordinary_believers:
 *   Diffuse beneficiary with diffuse costs (powerless/identity_locked) -
 *   historical_critical_scholars: Excluded voice (institutional/analytical) -
 *   academic_historians_of_doctrine: Analytical observer — sees the full
 *   structure CLAIM/METRIC INDEPENDENCE AND FSM INTENT: The claim is authored
 *   from the reading's own ontic presentation — this reading asserts the
 *   constraint is an ontic fact coeternal with God, which is the strongest
 *   available mountain claim, hence claimed_type=mountain with
 *   emerges_naturally=true. Beneficiaries are declared INTENTIONALLY:
 *   traditional jurists, literalist communities, and anti-rationalist schools
 *   identifiably profit from the doctrine's social operation while presenting
 *   it as metaphysical necessity — the exact false-summit shape. The metrics
 *   are authored independently from what the historical operation shows: real
 *   coordination function (scriptural anchor for law, worship, identity),
 *   real asymmetric extraction (hermeneutic flexibility taken from targets,
 *   authority concentrated in the guild), and a genuine enforcement history
 *   (office purges after 848, credentialing gates, blasphemy floors). Whether
 *   the mountain claim survives the engine's false-summit evaluation is
 *   precisely the measurement this file is built to take; no reconciliation
 *   between claim and metrics is performed here.
 *
 * KEY AGENTS:
 *   - - traditional_jurists: Agenda setter & primary beneficiary (powerful/identity_locked) — administers the orthodoxy, licenses interpretation, collects the concentration of interpretive authority
 *   - - caliphal_state_apparatus: Enforcement agenda setter (institutional/arbitrage) — post-848 patronage, purges of rationalist officeholders, later piecemeal withdrawal of enforcement
 *   - - literalist_communities: Beneficiary (organized/constrained) — unmediated access to divine command without rationalist gatekeeping
 *   - - anti_rationalist_schools: Beneficiary with enforcement secondary role (organized/identity_locked) — popular enforcement muscle, identity fused with defense of the creed
 *   - - rationalist_theologians: Primary target (organized/identity_locked) — paid in offices, stipends, safety, and methodological legitimacy
 *   - - metaphorical_interpreters: Target (moderate/constrained) — readings delegitimized as corruption of divine speech
 *   - - reform_movements: Target (organized/constrained) — required textual flexibility foreclosed, no rival authority to exit into
 *   - - ordinary_believers: Dual-positioned mass seat (powerless/identity_locked) — devotional benefit, diffuse costs
 *   - - historical_critical_scholars: Excluded voice (institutional/analytical) — outside the credentialing conversation entirely
 *   - - academic_historians_of_doctrine: Analytical observer (institutional/analytical) — sees both rival readings and the enforcement reversals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.62).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.55).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Uncreated Eternal Divine Speech Doctrine (Kalam Allah Qadim)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "religious/doctrinal/political-authority").

domain_priors:requires_active_enforcement(quran_ontological_status__uncreated_reading).
domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, '095d353f-0228-4660-b3f1-2e2650d80417').
narrative_ontology:cs_kernel_codification('095d353f-0228-4660-b3f1-2e2650d80417', fixed_text).
narrative_ontology:cs_authority_grounding('095d353f-0228-4660-b3f1-2e2650d80417', lineage).
narrative_ontology:cs_interpretation_layer_present('095d353f-0228-4660-b3f1-2e2650d80417').
narrative_ontology:cs_reading_relation('095d353f-0228-4660-b3f1-2e2650d80417', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('095d353f-0228-4660-b3f1-2e2650d80417', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('095d353f-0228-4660-b3f1-2e2650d80417', foundational, quran_uncreated_coeternal_with_god).
narrative_ontology:cs_axiom_status(quran_uncreated_coeternal_with_god, holdable).
narrative_ontology:cs_axiom_grounding('095d353f-0228-4660-b3f1-2e2650d80417', quran_uncreated_coeternal_with_god, theological).
narrative_ontology:cs_axiom('095d353f-0228-4660-b3f1-2e2650d80417', secondary, textual_meaning_is_fixed_divine_fact).
narrative_ontology:cs_axiom_status(textual_meaning_is_fixed_divine_fact, holdable).
narrative_ontology:cs_axiom_grounding('095d353f-0228-4660-b3f1-2e2650d80417', textual_meaning_is_fixed_divine_fact, theological).
narrative_ontology:cs_reference_frame('095d353f-0228-4660-b3f1-2e2650d80417', eternal_coeternal_divine_speech).
narrative_ontology:cs_drift_state('095d353f-0228-4660-b3f1-2e2650d80417', contemporary_post_caliphal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('095d353f-0228-4660-b3f1-2e2650d80417', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rationalist_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, ordinary_believers).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, ordinary_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Guard the creed that the recited text is God's own eternal speech; staff courts, schools, and pious endowments; license who may interpret; derive law from the text's fixed meanings. The doctrine concentrates interpretive authority in their credentials while binding their own method to transmitted meanings they may gloss but never historicize. Leaving would mean exiting the class that constitutes them.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, agenda_setter,
    powerful, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, traditional_jurists, beneficiary).

% Dynastic state machinery that, after reversing the inquisition of 833-848, patronized the traditionalist creed, purged rationalist officeholders, and enforced orthodoxy through appointments, oaths, and punishment. Later dynasties inherited the settlement; modern successor states dismantled enforcement piecemeal while keeping educational and criminal-law hooks. Doctrinal alignment can be shifted at low doctrinal cost to the state itself.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, caliphal_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, continental).

% Lay and scholarly currents that treat the text's surface meanings as directly authoritative. They receive certainty and unmediated access to divine command without passing through rationalist gatekeepers, and they depend on the eternity doctrine to defeat rival interpretive elites. They bear little of the doctrine's cost.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_communities, beneficiary,
    organized, biographical, constrained, continental).

% Tradition-minded lineages (Hanbali and Athari networks) that supplied popular enforcement for the creed: street mobilization, oath campaigns, denunciation of speculative theology. They gained durable standing as guardians of the text's uncreated status, and their collective identity is fused with defending it.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, anti_rationalist_schools, agenda_setter).

% Dialectical theologians (Mu'tazila and their heirs) who subordinate revealed text to demonstrated reason and held judicial and court positions before the purges. The doctrine brands their method impious at its root. Retaining their methodology cost them offices, stipends, and physical safety during and after the inquisition era; exit would require dissolving the school's constitutive commitments.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rationalist_theologians, payer,
    organized, biographical, identity_locked, continental).

% Allegorical and esoteric readers (philosophers, Sufi allegorists, Shi'i batin-oriented currents) who treat anthropomorphic descriptions and legal texts as symbols pointing beyond the surface. Fixing textual meaning as eternal divine fact delegitimizes their readings as corruption of God's own speech. They operate under permanent suspicion and credential exclusion.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, continental).

% Modernist and reform currents that need textual flexibility: reinterpreting inheritance shares, servitude-related rulings, and corporal punishments for changed conditions. The doctrine forecloses treating any verse as a temporally conditioned artifact. They pay in delegitimation, accusations of innovation, and exclusion from orthodoxy-certifying institutions, with no rival authority structure to exit into.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, payer,
    organized, generational, constrained, global).

% Recite the text in worship as God's unmediated eternal word. They receive devotional certainty, cosmic significance, and communal identity. They carry diffuse costs where the fixed-meaning regime settles contested questions (gender, punishment, dissent) against reinterpretive openings they have no standing to authorize.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, ordinary_believers, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, ordinary_believers, payer).

% Academic philologists and historians who treat the text as an artifact with a compositional history. They would contest the eternity premise and its hermeneutic consequences. They stand wholly outside the credentialing system that polices acceptable interpretation, with no seat in the conversation their work bears on.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, historical_critical_scholars, excluded,
    institutional, civilizational, analytical, global).

% Trace the controversy from the inquisition through the Ash'ari synthesis to modern creeds. They see both rival readings, the enforcement reversals, and the beneficiary structure that participants dispute, and they take no side in the creed.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, academic_historians_of_doctrine, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors communal meaning, law, and worship in a single stable transcendent source: one text whose status as divine speech guarantees its normative authority across regions and generations, so that worship, jurisprudence, and identity coordinate around a fixed sacred center rather than renegotiating authority each generation.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal legitimacy from methodologically flexible interpreters (rationalist dialecticians, allegorists, reformers) to the credentialed guardian class; moves certainty and status to adherents; extracts hermeneutic flexibility from anyone whose project requires treating verse meaning as conditionable.
% ABSENT_VOICES: Historical-critical scholars and philosophical allegorists would object that textual meaning is historically conditioned, but they sit outside the orthodox credentialing system that determines acceptable interpretation; their exclusion is maintained by the very doctrine whose status their work contests. Rationalist theologians were physically present in the eighth-century conversation and were removed from it by office purges rather than argumentative defeat alone.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, jurisprudence loses its guarantee that legal texts carry unconditional divine authority, creedal formulas and liturgy lose their object, the ulema's licensing monopoly over meaning collapses toward open contestation, and reform currents gain immediate room to condition previously fixed rulings; the entire authority architecture built on the text's eternal status would reorganize.
% FOUNDING_PROBLEM: Secure the absolute authority of revelation against relativization: if God's speech is a created thing among creatures, temporally contingent, its commands could be weighed and outranked; the doctrine answers by locating the text outside creation, coeternal with God, so that its authority shares in divine eternity.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Mu'tazilite opponents attested the problem's reality even while disputing the solution (their counter-position presupposed the same stakes); academic historians of doctrine document the controversy and its enforcement reversals from outside the tradition; structurally parallel word/Logos controversies in Christian and rabbinic literature show the underlying problem class is not internal to this community's interests.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, ExtMetricName, E),
    domain_priors:suppression_score(quran_ontological_status__uncreated_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62 at interval end) is moderate-high: the doctrine performs real, continuous coordination work — a community-spanning anchor for law, worship, and identity that no rival arrangement replicated — while operating asymmetrically, taking interpretive flexibility from rationalists, allegorists, and reformers and concentrating adjudication of meaning in the jurist guild. Suppression (0.55) is raw and unscaled: it mixes structural mechanisms (credential exclusion, office purges in the classical era, blasphemy statutes in some modern states) with internalized self-policing of doubt as impiety; the scalar cannot split these, which is routed to the suppression_structural_vs_internalized omega. Theater ratio (0.18) is low: the doctrine does real work continuously; creedal affirmation is not performance substituting for function. Accessibility collapse (0.82) is high-but-not-total: within any single commitment framework the rival creation premise is logically unusable (see cs_structure.forecloses edges), yet the created reading persisted historically as a suppressed live position across parties rather than vanishing, so collapse falls short of natural-law completeness. Resistance (0.55) records real, sustained pushback: Hanbali endurance under the inquisition-era persecution of the uncreated camp, Mu'tazilite institutional persistence, and modern reform pressure. TEMPORAL DESIGN: one shared grid (833, 900, 1050, 1258, 1500, 1800, 1924, 2024) carries all three tracked metrics — every metric authored at every examined time point, no substitution of end-states backward. The suppression_requirement series is authored deliberately (not left static) because this story specifically tracks enforcement-capacity change: force applied to hold the uncreated settlement rises sharply after the 848 reversal (purges, oaths, appointment control), plateaus under the classical madrasa and taqlid regimes, and decays after 1800 and especially 1924 as dynastic enforcement ends — a build-up-then-decay arc, not a monotone ratchet. Extractiveness tracks the same consolidation and partial modern loosening; the small 1924-to-2024 uptick reflects renewed political literalism re-tightening the fixed-meaning regime. Theater ticks upward as enforcement fades (creed increasingly ritually affirmed) then dips slightly as contemporary literalist movements re-functionalize it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the traditional_jurists seat the constraint is load-bearing foundation: it is what makes their license to interpret possible and their law derivable, and they experience its binding character as devotion rather than cost. From the rationalist_theologians and reform_movements seats the identical structure operates as a cage: their projects require conditioning textual meaning, which the eternity premise forbids in principle, not merely in administration. The ordinary_believers seat experiences mostly benefit with diffuse, hard-to-trace costs. The caliphal_state_apparatus seat experienced the constraint as an instrument — enforced, reversed, re-enforced — bearing almost no doctrinal cost itself. The engine derives these divergent classifications from the structural data; this commentary only explains why they must diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to the low-d end: traditional_jurists collect the concentration of interpretive authority (their secondary beneficiary role is explicit), literalist_communities receive unmediated certainty, anti_rationalist_schools collect standing as defenders. Victims map toward the high-d end: rationalist_theologians and reform_movements bear the deepest extraction, and their identity_locked exit pushes them toward the full-target pole — they cannot abandon the methodology or the reform project without dissolving what they are. metaphorical_interpreters sit slightly less extreme (constrained, moderate power). ordinary_believers derive near symmetric-low: genuine benefit, diffuse cost, no mobility premium. caliphal_state_apparatus derives near the beneficiary end despite its enforcer role because its arbitrage-grade exit means the doctrine constrains it hardly at all — it enforced, reversed, and re-enforced at will. historical_critical_scholars carry an excluded seat: their objection is recorded, but as commentary-grade absence it feeds the consensus-provenance check, not the directionality arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing scriptural authority against relativization — remains live: the question of what grounds the text's authority persists in every generation, and the doctrine remains the majority answer, so no obsolescence flag is warranted (status live, verdict world_rearranges, no mismatch). The mandatrophy analysis guards against misclassification in both directions. Reading the constraint as a pure snare fails: the coordination function is genuine and irreplaceable — a transregional, transtemporal anchor for meaning and law that the community demonstrably uses — so the extraction rides on real function. Reading it as a pure natural mountain fails on inspection: identifiable parties collect concentrated rents (interpretive monopoly, credential gating), the settlement required armed enforcement to establish and maintain, and the rival reading survived as a live position under suppression rather than collapsing like a falsified physical hypothesis. Hence the mountain claim stands as the reading's own assertion while the declared beneficiaries route the file through false-summit evaluation — the correct instrument for a constraint that presents as metaphysical necessity while functioning with a beneficiary-victim asymmetry. No sunset clause exists or could: the doctrine claims eternity, which is precisely what makes it unavailable to scaffold treatment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_constraint,
    'Is the uncreated-status claim a genuine metaphysical mountain — a fact about divine reality that would hold regardless of who defends it or whether anyone enforces it — or a constructed constraint whose naturality presentation serves identifiable interpretive-authority interests?',
    'Comparative analysis of structurally parallel word/Logos doctrines across traditions (Christian Logos christology, rabbinic Torah-preexistence motifs) plus the internal test of whether the doctrine''s social operation changes when enforcement capacity vanishes; the declared-beneficiary structure already routes this file through false-summit evaluation.',
    'If constructed, the mountain claim recomputes as tangled rope — jurists coordinated, rationalists and reformers paying through the same structure — and the doctrine''s ''naturality'' is exposed as the guild''s load-bearing presentation; if genuine, the mountain classification stands and the beneficiary declarations mark incidental adjacency rather than design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_constraint, conceptual, 'Natural-law versus constructed-constraint ambiguity for the uncreated doctrine').

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of kernel quran_ontological_status; what structurally changes under the sibling readings, and where exactly is the disagreement located?',
    'Author the sibling files (created_reading, state_enforced_creation_reading) and diff their beneficiary/victim maps, epsilon, and enforcement profiles; the disagreement locus is the modal property assigned to divine speech (created artifact versus eternal attribute), from which all downstream authority structure differs.',
    'Under created_reading the directionality map inverts — rationalist patrons become beneficiaries and traditionalist classes become targets — so this file''s epsilon and classification do not transfer across the family; cross-reading comparisons are valid only per-seat, never per-topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer-frame routing: sibling-reading structural delta and disagreement locus').

omega_variable(
    essence_vs_articulation_boundary,
    'Does the classical Ash''ari synthesis (eternal inner speech, created articulated recitation) place the recited text itself inside or outside this constraint?',
    'School-by-school creed comparison (Athari, Ash''ari, Maturidi, Twelver) coding which layer each declares uncreated, then testing whether extraction attributed to the eternity claim tracks the essence layer or the recitation layer.',
    'If the articulation layer is conceded created, part of the measured extraction migrates out of this constraint into a hybrid position, lowering effective epsilon and reshuffling the beneficiary map (Ash''ari jurists partially exit the uncreated beneficiary set); if the recited text is included wholesale, the constraint''s scope and extraction widen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essence_vs_articulation_boundary, conceptual, 'Layer ambiguity between eternal essence and recited articulation').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the constraint''s present-day suppression is structural (credential exclusion, blasphemy statutes, appointment gates) versus internalized (self-policing of doubt as impiety that persists without external enforcement)?',
    'Post-exit suppression trajectory: compare doubters who remain inside orthodox institutions with those who exit to secular contexts; survey and ethnographic data on concealed disbelief and on interpretive self-censorship among clergy.',
    'If the internalized share is large, effective suppression exceeds the structural 0.55 measure — the constraint travels with its targets after institutional exit — and the post-1924 enforcement-decay segment of the suppression series understates the constraint''s real persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism split').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 833, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quran_uncreated_tr_t833, quran_ontological_status__uncreated_reading, theater_ratio, 833, 0.1).
narrative_ontology:measurement(quran_uncreated_tr_t900, quran_ontological_status__uncreated_reading, theater_ratio, 900, 0.12).
narrative_ontology:measurement(quran_uncreated_tr_t1050, quran_ontological_status__uncreated_reading, theater_ratio, 1050, 0.13).
narrative_ontology:measurement(quran_uncreated_tr_t1258, quran_ontological_status__uncreated_reading, theater_ratio, 1258, 0.14).
narrative_ontology:measurement(quran_uncreated_tr_t1500, quran_ontological_status__uncreated_reading, theater_ratio, 1500, 0.16).
narrative_ontology:measurement(quran_uncreated_tr_t1800, quran_ontological_status__uncreated_reading, theater_ratio, 1800, 0.19).
narrative_ontology:measurement(quran_uncreated_tr_t1924, quran_ontological_status__uncreated_reading, theater_ratio, 1924, 0.22).
narrative_ontology:measurement(quran_uncreated_tr_t2024, quran_ontological_status__uncreated_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(quran_uncreated_be_t833, quran_ontological_status__uncreated_reading, base_extractiveness, 833, 0.28).
narrative_ontology:measurement(quran_uncreated_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.44).
narrative_ontology:measurement(quran_uncreated_be_t1050, quran_ontological_status__uncreated_reading, base_extractiveness, 1050, 0.58).
narrative_ontology:measurement(quran_uncreated_be_t1258, quran_ontological_status__uncreated_reading, base_extractiveness, 1258, 0.63).
narrative_ontology:measurement(quran_uncreated_be_t1500, quran_ontological_status__uncreated_reading, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement(quran_uncreated_be_t1800, quran_ontological_status__uncreated_reading, base_extractiveness, 1800, 0.64).
narrative_ontology:measurement(quran_uncreated_be_t1924, quran_ontological_status__uncreated_reading, base_extractiveness, 1924, 0.59).
narrative_ontology:measurement(quran_uncreated_be_t2024, quran_ontological_status__uncreated_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(quran_uncreated_su_t833, quran_ontological_status__uncreated_reading, suppression_requirement, 833, 0.15).
narrative_ontology:measurement(quran_uncreated_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.68).
narrative_ontology:measurement(quran_uncreated_su_t1050, quran_ontological_status__uncreated_reading, suppression_requirement, 1050, 0.74).
narrative_ontology:measurement(quran_uncreated_su_t1258, quran_ontological_status__uncreated_reading, suppression_requirement, 1258, 0.76).
narrative_ontology:measurement(quran_uncreated_su_t1500, quran_ontological_status__uncreated_reading, suppression_requirement, 1500, 0.79).
narrative_ontology:measurement(quran_uncreated_su_t1800, quran_ontological_status__uncreated_reading, suppression_requirement, 1800, 0.68).
narrative_ontology:measurement(quran_uncreated_su_t1924, quran_ontological_status__uncreated_reading, suppression_requirement, 1924, 0.58).
narrative_ontology:measurement(quran_uncreated_su_t2024, quran_ontological_status__uncreated_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: kernel quran_ontological_status decomposes into three epsilon-distinct readings, linked pairwise via affects_constraints. This file authors ONLY the uncreated reading. created_reading inverts the beneficiary/victim map (rationalist patrons gain, traditionalist classes lose); state_enforced_creation_reading layers a coercive enforcement apparatus onto the creation premise, so its epsilon additionally prices inquisition costs. The upstream/downstream gradient runs through enforcement history: the uncreated reading's post-848 victory structurally shaped what the state-enforced variant could attempt afterward, and each reading cites the others' failures in its own apologetics. Edges are preserved for contamination-propagation analysis across the family; classification outputs remain strictly per-file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
