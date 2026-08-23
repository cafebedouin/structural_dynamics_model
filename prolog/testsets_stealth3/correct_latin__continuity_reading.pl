% ============================================================================
% CONSTRAINT STORY: correct_latin__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__continuity_reading, []).

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
 *   constraint_id: correct_latin__continuity_reading
 *   human_readable: Continuity Reading of Correct Latin: Living Transmission as the Standard of Legitimacy
 *   domain: historical linguistics/philology/intellectual history
 *
 * SUMMARY:
 *   A single clean instantiation of one legitimacy rule: correct Latin is the
 *   form transmitted through continuous living practice; medieval Latin is
 *   legitimate evolved Classical Latin; reform, where admitted, is internal
 *   adjustment rather than external reconstruction. This rule governed the
 *   adjudication of Latinity across Latin Christendom from the eleventh
 *   century until humanist philology displaced it. Its operation: correctness
 *   is certified by lineage rather than by evidence, so the act of
 *   transmission confers truth on what is transmitted, and textual testimony
 *   from antiquity is inadmissible as a standard. KEY AGENTS (by structural
 *   relationship): cathedral_school_masters: transmission-lineage incumbents
 *   (organized / identity_locked) who enforce daily correction and hold
 *   standing by the chain; university_arts_faculties: agenda setters atop the
 *   licensing pyramid (institutional / constrained); curial_chancery_clerks:
 *   administrative beneficiaries consuming the standard's product
 *   (institutional / constrained); textual_correctors_and_humanists: primary
 *   targets whose corrective labor is ruled inadmissible (organized /
 *   mobile); direct_text_emulators: secondary targets classicizing outside
 *   the classroom line (moderate / constrained);
 *   peripheral_regional_latinists: diffuse targets whose equally old usages
 *   are ruled faulty (powerless / trapped); vernacular_literati: excluded
 *   defectors (moderate / mobile); classical_canon_authors: non-agent witness
 *   entity barred from adjudication; intellectual_history_observers:
 *   analytical seat. Claim/metric independence: the claimed type
 *   (tangled_rope) is what I believe structurally true of the arrangement — a
 *   real coordination function fused with lineage-serving extraction and
 *   active enforcement — while the metric values are what I believe
 *   descriptively true of its operation over 1100 to 1500; the engine
 *   computes per-seat classifications from the structural data, and
 *   divergence between the claim and computed types is the measurement the
 *   corpus exists to take.
 *
 * KEY AGENTS:
 *   - cathedral_school_masters: transmission-lineage incumbents (organized / identity_locked) — enforce daily correction, hold standing by the chain
 *   - university_arts_faculties: agenda setters atop the licensing pyramid (institutional / constrained)
 *   - curial_chancery_clerks: administrative beneficiaries of the standard's product (institutional / constrained)
 *   - textual_correctors_and_humanists: primary targets — corrective labor ruled inadmissible (organized / mobile)
 *   - direct_text_emulators: secondary targets — classicizers outside the classroom line (moderate / constrained)
 *   - peripheral_regional_latinists: diffuse targets — equally old usages ruled faulty (powerless / trapped)
 *   - vernacular_literati: excluded defectors shrinking the governed constituency (moderate / mobile)
 *   - classical_canon_authors: non-agent witness entity — the barred adjudicator (agent: false)
 *   - intellectual_history_observers: analytical seat tracing the rule's operation and displacement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__continuity_reading, 0.62).
domain_priors:suppression_score(correct_latin__continuity_reading, 0.75).
domain_priors:theater_ratio(correct_latin__continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__continuity_reading, "Continuity Reading of Correct Latin: Living Transmission as the Standard of Legitimacy").
narrative_ontology:topic_domain(correct_latin__continuity_reading, "historical linguistics/philology/intellectual history").

domain_priors:requires_active_enforcement(correct_latin__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__continuity_reading, '954d1da4-9bf1-4eb0-aaf6-4982b860f16f').
narrative_ontology:cs_kernel_codification('954d1da4-9bf1-4eb0-aaf6-4982b860f16f', implicit).
narrative_ontology:cs_authority_grounding('954d1da4-9bf1-4eb0-aaf6-4982b860f16f', practice).
narrative_ontology:cs_interpretation_layer_present('954d1da4-9bf1-4eb0-aaf6-4982b860f16f').
narrative_ontology:cs_reading_relation('954d1da4-9bf1-4eb0-aaf6-4982b860f16f', correct_latin__discontinuity_reading, forecloses).
narrative_ontology:cs_reading_relation('954d1da4-9bf1-4eb0-aaf6-4982b860f16f', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('954d1da4-9bf1-4eb0-aaf6-4982b860f16f', foundational, no_rupture_evolutionary_continuity).
narrative_ontology:cs_axiom_status(no_rupture_evolutionary_continuity, holdable).
narrative_ontology:cs_axiom_grounding('954d1da4-9bf1-4eb0-aaf6-4982b860f16f', no_rupture_evolutionary_continuity, conventional).
narrative_ontology:cs_axiom('954d1da4-9bf1-4eb0-aaf6-4982b860f16f', foundational, transmission_preserves_authenticity).
narrative_ontology:cs_axiom_status(transmission_preserves_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('954d1da4-9bf1-4eb0-aaf6-4982b860f16f', transmission_preserves_authenticity, empirically_contingent).
narrative_ontology:cs_reference_frame('954d1da4-9bf1-4eb0-aaf6-4982b860f16f', continuous_living_transmission).
narrative_ontology:cs_drift_state('954d1da4-9bf1-4eb0-aaf6-4982b860f16f', humanist_manuscript_recovery, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('954d1da4-9bf1-4eb0-aaf6-4982b860f16f', '').
narrative_ontology:cs_kernel_id(correct_latin__continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, cathedral_school_masters).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, curial_chancery_clerks).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, university_arts_faculties).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, textual_correctors_and_humanists).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, direct_text_emulators).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, peripheral_regional_latinists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach grammar and composition in cathedral and monastic schools, passing on the forms they received from their own teachers and correcting pupils toward currently accepted usage. Their professional standing rests entirely on being links in the chain: what they transmit is correct by construction under the prevailing rule. Leaving would mean repudiating the authority of everything they have ever taught.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, cathedral_school_masters, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, cathedral_school_masters, agenda_setter).

% License teachers, set arts curricula, and examine candidates for fluency in the received usage; their statutes define which books and practices confer standing. Fees, enrollment, and doctrinal authority flow to faculties that keep the definition of correctness in-house. They could in principle rebuild examinations around ancient texts, but doing so would discard the accumulated value of their licensing apparatus.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, university_arts_faculties, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, university_arts_faculties, beneficiary).

% Draft bulls, letters, and administrative records in the received style. Their careers reward command of the living standard and require no independent textual scholarship; their stylistic credentials are certified simply by having been formed inside the tradition.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, curial_chancery_clerks, beneficiary,
    institutional, biographical, constrained, continental).

% Collate manuscripts, restore ancient spellings and idioms, and argue that composition be judged against recovered classical usage. Under the prevailing rule their findings are not corrections but faults, and their reputations depend on patrons willing to host a rival standard. Exit is comparatively open: they can move between courts, take up vernacular writing, or attach themselves to new printing ventures.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, textual_correctors_and_humanists, payer,
    organized, biographical, mobile, continental).

% Readers who form their Latin directly from ancient authors without passing through the schools. The prevailing rule marks their archaisms as barbarisms regardless of fidelity to the originals; admission to official literacy runs through the classroom line they bypassed.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, direct_text_emulators, payer,
    moderate, biographical, constrained, continental).

% Monks, parish clergy, and scribes in regional houses whose local usages are as old as anyone's transmission but fall outside the metropolitan line. Visiting metropolitan-trained correctors mark their forms as faulty; they lack standing to defend them and there is no alternative Latin world to relocate to.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, peripheral_regional_latinists, payer,
    powerless, biographical, trapped, regional).

% Writers increasingly composing in Tuscan, French, and other vernaculars. They have left the Latin conversation altogether and so figure in none of its deliberations, though their defection steadily shrinks the constituency the rule governs.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, vernacular_literati, excluded,
    moderate, biographical, mobile, continental).

% Non-agent entity: the ancient authors as transmitted witnesses. Their texts are read constantly for content and style, but under the prevailing rule they are barred from adjudicating correctness; admitting their testimony as standard-setting would dissolve the rule itself.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, classical_canon_authors, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(correct_latin__continuity_reading, classical_canon_authors).

% Later historians and philologists of the humanist quarrel who trace how correctness was defined, enforced, and eventually displaced; they hold no stake in the period's adjudications.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, intellectual_history_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__continuity_reading, university_arts_faculties).
narrative_ontology:fixing_cost_class(correct_latin__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single mutually intelligible written Latin across Latin Christendom without print or centralized academies: by defining correct composition as conformity to the continuously taught and copied practice, every schoolroom, scriptorium, and chancery reproduces the same standard, so a letter drafted in York reads as proper Latin in Krakow.
% TRANSFER_FUNCTION: Moves professional standing and the authority to certify Latinity away from whoever masters ancient texts or local usage and toward holders of the metropolitan teaching lineage; moves the products of textual scholarship (emendations, restored idioms) from admissible evidence to rejectable error.
% ABSENT_VOICES: The ancient authors themselves, whose recovered words would adjudicate correctness but are barred from testifying by the very rule under which correctness is decided; regional Latinists without access to metropolitan schools; vernacular writers already outside the conversation, whose defection is registered nowhere in the rule's deliberations.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, correctness adjudication would fall back on whatever evidence lay at hand, chiefly the ancient texts already circulating; schools, chanceries, and examinations would reorganize around textual standards within a generation, and the standing of everyone certified under the old rule would be renegotiated.
% FOUNDING_PROBLEM: After the Western Empire's administrative collapse, written Latin threatened to fragment into mutually unintelligible regional speech while surviving texts were few and copies multiplied errors; keeping a continent-wide learned medium alive required treating the living chain of teaching and copying, not scarce and corrupt manuscripts, as the standard of correctness.
% FOUNDING_PROBLEM_CORROBORATION: Humanist philologists and early printer-editors attest, from outside the benefiting parties, that manuscript recovery and print removed the scarcity that had made living transmission the only usable standard; histories of education corroborate that the rule persisted past its rationale chiefly through incumbent interest. The transmission lineage's own masters dispute the obsolescence claim and assert continuing necessity; the dispute is recorded in the humanist polemics themselves.
narrative_ontology:disappearance_verdict(correct_latin__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__continuity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon (0.62) is assessed on the standing arrangement under this reading's own lights: a rule that certifies usage by lineage rather than evidence. Through the twelfth and thirteenth centuries the rule's operation approaches a working standard — it solves a genuine coordination problem and its costs resemble ordinary standardization overhead — so early-series extraction sits near 0.25. Extraction accumulates from the mid-fourteenth century onward as recovered manuscripts make textual standards feasible and defense of the certification monopoly becomes the rule's principal activity; the series reaches 0.62 by 1500. Suppression (0.75) is authored as a raw structural property — licensure, correction regimes, examination control — and is deliberately NOT scaled by directionality or scope; only extractiveness is scaled in the engine's computation, amplified here by the continental scope that makes verifying competing standards slow. Theater ratio (0.42 at interval end) tracks the growing share of activity that invokes the tradition ritually rather than performing transmission: by 1500 a large fraction of the rule's defense consists of appeals to continuity as a credential rather than acts of teaching and copying. Accessibility collapse is moderate (0.38): the rival evidence base — ancient manuscripts — never disappeared, it multiplied, so the alternative did not collapse so much as get delegitimized. Resistance is high (0.65): an organized, patronage-backed corrective movement met the rule head-on. All three tracked metrics share one nine-point time grid (1100 to 1500, half-century steps) so no metric row borrows another's endpoints; the trajectories are monotonic rather than cyclical, driven by the exogenous arrival of manuscript recovery and print rather than by any intermittent-reinforcement cycle. Identity lock operates on the schoolmaster seat: professional identity fused with the transmission chain makes exit equivalent to self-repudiation, which is why the beneficiary seats hold the rule longest after its rationale lapses. Inter-institutionally, universities, cathedrals, and the curia experience the rule differently — faculties administer it, chanceries consume its product, regional houses absorb its corrections — and among same-level actors, metropolitan and regional Latinists of comparable learning receive opposite verdicts on equally ancient usages, differentiated solely by proximity to the licensed line.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from identical structural facts. To a cathedral schoolmaster the rule is simply what teaching letters means — his classroom performance is the standard, and questioning it attacks his life's work; his seat computes near-pure coordination. To a humanist collating manuscripts, the same rule is a refusal to look at evidence, sustained by licensure he does not control; his seat computes enforced extraction. The curial clerk experiences neither pole: certified by formation, taxed by nothing, he collects quietly. Peripheral regionalists bear the corrections without the compensation of belonging to the certifying center. The engine derives these per-seat classifications from power, exit, and directional placement; the divergence is the datum, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Schoolmasters, faculties, and chancery clerks sit near the beneficiary end: the rule subsidizes their standing and their certification authority, with identity lock pinning the masters nearest full subsidy. Humanist correctors and direct-text emulators sit near the target end: the rule converts their best evidence into fault and their labor into error, with mobility moderating but not removing the exposure. Peripheral regionalists are trapped targets — highest effective extraction per unit of power, since they bear corrections with no exit and no coalition channel beyond their houses. Vernacular literati have exited the constraint's jurisdiction entirely; their directionality decays toward irrelevance as they leave. The continental spatial scope raises verification difficulty for anyone attempting to demonstrate that an alternative standard works, which amplifies effective extraction on the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification keeps two misreadings apart. Calling the rule a snare erases its genuine four-century achievement — a continent-wide written medium maintained without print — and misattributes to malice what began as the only workable answer to text scarcity. Calling it a rope launders the post-1450 period, when persistence served incumbent certification interests long after print and recovered manuscripts had removed the scarcity rationale. The founding problem — keeping a unified learned medium alive amid scarce, corrupt texts — is dead at interval end while the arrangement still demonstrably rearranges the world; that mismatch is the capture/zombie signature, cross-checked here against a theater ratio of 0.42 showing heavy performative defense but not yet full inertial neglect: the rule was still fought over, not merely maintained. Mandatrophy resolution: the mandate outlived its problem, and the constraint persisted by incumbent identity lock rather than by function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    correct_latin_kernel_reading_indexicality,
    'This story instantiates one reading of the contested kernel correct_latin — the continuity reading. How would classification shift if the discontinuity_reading or hybrid_reading governed instead?',
    'Adjudication-history analysis: establish which legitimacy rule actually governed correction decisions in a given period and place, then classify under that reading''s structural data.',
    'Under the discontinuity reading the beneficiary and target sets invert — transmission-holders become the corrected party and textual scholars become standard-setters — so per-seat types and aggregate extraction differ sharply across readings sharing one colloquial label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(correct_latin_kernel_reading_indexicality, conceptual, 'Kernel indexicality: one label, three structurally distinct constraints.').

omega_variable(
    authority_source_locus,
    'Where exactly is the disagreement between the readings located?',
    'Compare the readings'' declared warrants — living practice, ancient texts, or a managed mix — and locate the disputed element as the evidentiary basis of correctness rather than the existence of a standard.',
    'All three readings agree that a correctness standard should exist; classification differences turn entirely on which source confers legitimacy. Mislocating the dispute as being about standards per se would misread every seat''s position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_source_locus, conceptual, 'Disagreement locus: the evidentiary basis of linguistic legitimacy.').

omega_variable(
    transmission_fidelity_empirics,
    'Does continuously transmitted practice actually preserve ancient Classical usage, as the continuity warrant assumes?',
    'Compare transmitted medieval forms against recovered ancient texts and against stemmatic evidence of copyist corruption — precisely the comparison the humanists performed.',
    'Demonstrated divergence strips the continuity reading of its epistemic warrant and drives axiom_overriding drift toward engine-computed foreclosure by the discontinuity position; confirmed convergence would instead lower effective extraction by validating the coordination story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transmission_fidelity_empirics, empirical, 'Whether the reading''s empirical warrant survives textual comparison.').

omega_variable(
    organic_practice_vs_constructed_gate,
    'Is the continuity norm an organic feature of language communities — usage naturally validating current practice — or a constructed institutional gate that identifiable actors maintain for advantage?',
    'Compare settings where transmission ran without licensure (informal scriptoria, private tutoring) against licensed settings: if the same legitimacy rule arises without gatekeepers it leans organic; if it tracks licensure boundaries, it is constructed.',
    'If constructed, the beneficiary declarations stand as authored and any naturalness framing fails; if organic, part of the measured suppression reflects ordinary language-community conservatism rather than institutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organic_practice_vs_constructed_gate, empirical, 'Naturalness ambiguity of the transmission-legitimacy rule.').

omega_variable(
    internalized_traditionalist_suppression,
    'Is the measured suppression structural (licensure, correction regimes, examination control) or internalized (masters'' sincere conviction that transmitted usage is right, fused with professional identity)?',
    'Post-displacement trajectory: after humanist standards prevailed, did former masters continue marking restored classical forms as faults in the absence of institutional backing?',
    'If internalized, effective suppression outlasts the institutions and the rule''s decay lags enforcement collapse; if purely structural, removing licensure collapses the rule quickly. Roughly seventy percent structural, thirty percent internalized on the available record, with the internalized share concentrated in the schoolmaster seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_traditionalist_suppression, empirical, 'Structural versus internalized component of enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__continuity_reading, 1100, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t1100, correct_latin__continuity_reading, theater_ratio, 1100, 0.08).
narrative_ontology:measurement(corr_tr_t1150, correct_latin__continuity_reading, theater_ratio, 1150, 0.09).
narrative_ontology:measurement(corr_tr_t1200, correct_latin__continuity_reading, theater_ratio, 1200, 0.11).
narrative_ontology:measurement(corr_tr_t1250, correct_latin__continuity_reading, theater_ratio, 1250, 0.13).
narrative_ontology:measurement(corr_tr_t1300, correct_latin__continuity_reading, theater_ratio, 1300, 0.17).
narrative_ontology:measurement(corr_tr_t1350, correct_latin__continuity_reading, theater_ratio, 1350, 0.21).
narrative_ontology:measurement(corr_tr_t1400, correct_latin__continuity_reading, theater_ratio, 1400, 0.27).
narrative_ontology:measurement(corr_tr_t1450, correct_latin__continuity_reading, theater_ratio, 1450, 0.34).
narrative_ontology:measurement(corr_tr_t1500, correct_latin__continuity_reading, theater_ratio, 1500, 0.42).

% Extraction over time
narrative_ontology:measurement(corr_be_t1100, correct_latin__continuity_reading, base_extractiveness, 1100, 0.22).
narrative_ontology:measurement(corr_be_t1150, correct_latin__continuity_reading, base_extractiveness, 1150, 0.24).
narrative_ontology:measurement(corr_be_t1200, correct_latin__continuity_reading, base_extractiveness, 1200, 0.26).
narrative_ontology:measurement(corr_be_t1250, correct_latin__continuity_reading, base_extractiveness, 1250, 0.29).
narrative_ontology:measurement(corr_be_t1300, correct_latin__continuity_reading, base_extractiveness, 1300, 0.33).
narrative_ontology:measurement(corr_be_t1350, correct_latin__continuity_reading, base_extractiveness, 1350, 0.37).
narrative_ontology:measurement(corr_be_t1400, correct_latin__continuity_reading, base_extractiveness, 1400, 0.44).
narrative_ontology:measurement(corr_be_t1450, correct_latin__continuity_reading, base_extractiveness, 1450, 0.53).
narrative_ontology:measurement(corr_be_t1500, correct_latin__continuity_reading, base_extractiveness, 1500, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t1100, correct_latin__continuity_reading, suppression_requirement, 1100, 0.35).
narrative_ontology:measurement(corr_su_t1150, correct_latin__continuity_reading, suppression_requirement, 1150, 0.36).
narrative_ontology:measurement(corr_su_t1200, correct_latin__continuity_reading, suppression_requirement, 1200, 0.4).
narrative_ontology:measurement(corr_su_t1250, correct_latin__continuity_reading, suppression_requirement, 1250, 0.43).
narrative_ontology:measurement(corr_su_t1300, correct_latin__continuity_reading, suppression_requirement, 1300, 0.47).
narrative_ontology:measurement(corr_su_t1350, correct_latin__continuity_reading, suppression_requirement, 1350, 0.52).
narrative_ontology:measurement(corr_su_t1400, correct_latin__continuity_reading, suppression_requirement, 1400, 0.6).
narrative_ontology:measurement(corr_su_t1450, correct_latin__continuity_reading, suppression_requirement, 1450, 0.68).
narrative_ontology:measurement(corr_su_t1500, correct_latin__continuity_reading, suppression_requirement, 1500, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' decomposes into a constraint family of three readings of one kernel: correct_latin__continuity_reading (this file — living transmission confers legitimacy), correct_latin__discontinuity_reading (ancient texts alone confer legitimacy; beneficiary and target sets invert), and correct_latin__hybrid_reading (partial continuity with targeted textual reform). Each carries its own epsilon, its own beneficiary/victim structure, and its own classification. The family is linked through affects_constraints because the readings compete for the same adjudication surface, and this reading's defensive concessions shaped the hybrid position downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
