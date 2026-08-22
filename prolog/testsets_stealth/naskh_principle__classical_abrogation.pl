% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Quranic Abrogation Doctrine (Naskh): Chronological Supersession Hierarchy
 *   domain: religious/legal/hermeneutical
 *
 * SUMMARY:
 *   A fixed, closed scripture revealed over roughly twenty-three years
 *   contains commands that vary across its own surface — on intoxicants, on
 *   fighting, on inheritance, on divorce. The classical abrogation doctrine
 *   (naskh) resolves this by chronological supersession: a later verse on the
 *   same topic replaces an earlier one's legal force, while the earlier verse
 *   remains recited, memorized, and historically valued. Systematized by
 *   al-Shafi'i (d. 820 CE), who confined abrogation to Quran-abrogating-Quran
 *   and built the determinations into legal theory, the doctrine grew into a
 *   curricular science, was catalogued in the classical reference works of
 *   al-Zarkashi and al-Suyuti, and is enforced through juristic gatekeeping:
 *   orthodoxy over interpretive method, control of the chronology reports,
 *   and delegitimation of readings that deny supersession. This file
 *   instantiates the classical_abrogation reading of the naskh_principle
 *   kernel (see kernel_context); the sibling readings are separate
 *   constraints in the same family, linked via network.affects_constraints.
 *   The claim/metrics split is deliberate: claimed_type records the authoring
 *   seat's structural judgment (tangled_rope — a real coordination core with
 *   asymmetric, enforced extraction); the metrics record the arrangement's
 *   operation as described. Epsilon's referent is the standing classical
 *   arrangement itself — the supersession hierarchy as administered by the
 *   juristic class — assessed within the reading's own premises: granting
 *   that supersession is real and the hierarchy necessary, what does the
 *   arrangement cost those it governs? KEY AGENTS (by structural
 *   relationship): - classical_juristic_class: Agenda-setter and principal
 *   beneficiary (institutional/identity_locked) — administers the
 *   supersession hierarchy, collects interpretive authority -
 *   sharia_court_apparatus: Secondary beneficiary (institutional/constrained)
 *   — receives determinate law, defers enforcement costs upward -
 *   lay_quran_readers: Primary payer (powerless/identity_locked) — bears
 *   mediated access; direct coherent reading foreclosed -
 *   contextualist_interpreters: Payer and excluded (moderate/mobile) — rival
 *   hermeneutics delegitimized, exit to academic spaces -
 *   comparative_hermeneutics_scholars: Analytical observer — attests the
 *   structure from outside the beneficiary set
 *
 * KEY AGENTS:
 *   - classical_juristic_class: agenda-setter and principal beneficiary (institutional/identity_locked) — administers the supersession hierarchy and collects interpretive authority
 *   - sharia_court_apparatus: secondary beneficiary (institutional/constrained) — receives determinate law from the doctrine's output
 *   - lay_quran_readers: primary payer (powerless/identity_locked) — bears mediated access; direct unmediated legal reading foreclosed
 *   - contextualist_interpreters: payer and excluded (moderate/mobile) — holders of the rival readings, delegitimized within orthodoxy
 *   - comparative_hermeneutics_scholars: analytical observer (analytical/analytical) — maps the arrangement from outside the tradition's authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.58).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.6).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.58).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Quranic Abrogation Doctrine (Naskh): Chronological Supersession Hierarchy").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious/legal/hermeneutical").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, 'f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62').
narrative_ontology:cs_kernel_codification('f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62', fixed_text).
narrative_ontology:cs_authority_grounding('f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62', lineage).
narrative_ontology:cs_interpretation_layer_present('f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62').
narrative_ontology:cs_reading_relation('f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_reading_relation('f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62', naskh_principle__progressive_restriction, forecloses).
narrative_ontology:cs_axiom('f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62', foundational, later_revelation_supersedes_earlier_legal_force).
narrative_ontology:cs_axiom_status(later_revelation_supersedes_earlier_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62', later_revelation_supersedes_earlier_legal_force, theological).
narrative_ontology:cs_axiom('f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62', secondary, operative_law_requires_chronological_hierarchy).
narrative_ontology:cs_axiom_status(operative_law_requires_chronological_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62', operative_law_requires_chronological_hierarchy, instrumental).
narrative_ontology:cs_reference_frame('f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62', revelation_order_supremacy_framework).
narrative_ontology:cs_drift_state('f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62', contemporary_hermeneutic_plurality, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f8f9e7ec-b16d-4e17-afe9-2a4c2a454f62', '2026-06-20T09:15:00Z').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_juristic_class).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, sharia_court_apparatus).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, lay_quran_readers).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, contextualist_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, lay_quran_readers).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, chronological_supersession_doctrine).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, necessity_of_scholarly_mediation).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, fixed_text_determinate_law_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines which verses supersede which through the science of the abrogating and the abrogated, maintains the chronology-of-revelation reports and occasions-of-revelation literature the determinations rest on, trains each generation of jurists in the abrogation curriculum, and rules on which interpretive approaches are orthodox. Access to the operative meaning of the text runs through this class: a reader cannot tell from the text's surface order which commands remain in force. Members' scholarly standing, teaching posts, and judicial appointments are constituted by command of this apparatus; renouncing it would dissolve both the class's authority and their own place in it.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_juristic_class, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, classical_juristic_class, beneficiary).

% Applies the doctrine's output: one operative rule per contested topic, delivered with a citation chain through the supersession hierarchy. Gains predictability — judges and litigants share a determinate rulebook — and defers to the juristic class on which verses remain in force. Bears the cost of enforcing outcomes whose plain textual wording points the other way, and of litigants who quote a superseded verse back at the court.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, sharia_court_apparatus, beneficiary,
    institutional, generational, constrained, continental).

% Read the text as direct divine speech and encounter commands that appear to conflict — on wine, on fighting, on inheritance shares, on divorce waiting periods. They are taught that the conflict is resolved by supersession they cannot verify: the revelation chronology is not recoverable from the text itself, and the determinations require the scholarly apparatus. Their direct reading of the whole text as presently operative law is foreclosed within orthodoxy; leaving the textual tradition altogether carries the cost of leaving the faith and community they are constituted by. They receive settled rules in exchange for mediated access.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, lay_quran_readers, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, lay_quran_readers, beneficiary).

% Hold that the verses remain valid within their revelatory situations and that apparent conflicts dissolve under contextual specification, or that the permissive-to-restrictive sequence is pedagogy rather than invalidation. The classical apparatus classifies these positions as error or innovation; holders historically held institutional posts in the rationalist theological schools and were later pushed out; modern holders largely work in universities and reform movements outside the traditional institutions' authority. Their alternative is to carry the work into academic and translation-accessible spaces where the classical gatekeeping does not reach.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, contextualist_interpreters, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, contextualist_interpreters, excluded).

% Map the doctrine's consolidation and its disputes from outside the tradition's authority structures: count the abrogation lists (which range from a handful to several hundred depending on school and era), document the pre-classical dissent, the rationalist debates, and the modernist challenge, and attest the structure of the arrangement to readers with no stake in any reading's victory.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, comparative_hermeneutics_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__classical_abrogation, classical_juristic_class).
narrative_ontology:fixing_cost_class(naskh_principle__classical_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves apparent conflicts among rulings in a fixed, closed revelation — wine, fighting, inheritance shares, divorce waiting periods, prayer concessions — so that courts and jurists can derive one operative rule per topic without re-litigating the text's internal variation case by case; the chronological tiebreaker is checkable by any trained jurist.
% TRANSFER_FUNCTION: Moves interpretive authority from direct readers of the text to the juristic class: knowing which verses remain legally operative requires the apparatus of revelation-chronology reports, occasions-of-revelation literature, and the science of the abrogating and the abrogated — none of it recoverable from the text's surface order.
% ABSENT_VOICES: Contextualist interpreters and progressive-restriction readers sit outside the classical apparatus's orthodoxy; lay readers have no seat — no one in the classical conversation represents the reader who encounters the text sequentially and finds it coherent without supersession; historically, rationalist theologians who denied the Quran's self-abrogation were marginalized rather than answered on the merits.
% DISAPPEARANCE_RATIONALE: Courts would face directly conflicting commands on inheritance, intoxicants, and fighting with no tiebreaker; the juristic monopoly on operative meaning would dissolve; the hermeneutical field would reorganize around the rival readings; and roughly twelve centuries of legal determinations keyed to abrogation lists would need re-derivation from scratch.
% FOUNDING_PROBLEM: The Quran was revealed over roughly twenty-three years to a changing community, and later rulings modified earlier ones on the same topics; the founding problem was how a single fixed text can serve as determinate law when it records its own revision — which command governs now, and who may say so.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: academic historians and hermeneutics scholars document the problem as generic to closed legal corpora that record internal revision; modernist Muslim scholars who reject the classical solution (the contextualist programs of Rida and Rahman) nonetheless attest the underlying problem of textual variation. The dispute in the literature is over the solution, not over the problem's existence.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 is substantial but not snare-grade. The arrangement takes real, non-recoverable value from those it governs: operative meaning of the text requires the apparatus (revelation chronology and abrogation determinations are not recoverable from the surface text), so direct unmediated reading is foreclosed, and rival hermeneutical frameworks are delegitimized rather than engaged. It is damped from snare levels by the genuine coordination output: courts do receive one operative rule per contested topic, and the tiebreaker is checkable by any trained jurist. Suppression 0.60 is hermeneutical gatekeeping rather than physical coercion: curricular control, orthodoxy policing over method, marginalization of dissenting schools — machinery that must stay active because the rival readings remain live and articulated. Suppression is authored as the raw structural gatekeeping force, unscaled; the engine scales only extraction, by directionality and scope. Theater 0.22 is low: abrogation determinations decide real cases (the wine prohibition's stages, bequest and inheritance rulings, the fighting verses' sequence), though late-classical cataloguing inflated disputed pairs into scholastic display and a share of contemporary curricular reproduction is performance rather than operative adjudication. Accessibility_collapse 0.30: the alternatives do not collapse — the sibling readings are live, articulated, and historically institutionalized (the Zahiri school denied or radically restricted Quranic abrogation from inside Sunni law for centuries). Resistance 0.55: pre-Shafi'i dissent over whether the Quran abrogates itself, the rationalist-school debates, intra-school list disputes (counts run from roughly five to several hundred), and the modernist challenge. The measurement series share one grid. Suppression_requirement is tracked because the story's enforcement history is its central dynamic: build-up through consolidation (800-1400), normalization (1600-1800), re-activation against the modern contextualist revival (1950-2020). Suppression at the producer side is structural (gatekeeping, orthodoxy enforcement); at the consumer side it is partly internalized (mediation accepted as piety) — the omega lay_identity_lock_mechanism carries that ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is the tradition's coherence achievement: without it the text contradicts itself and no court can rule; the jurists who built it see necessity, not extraction. From the lay reader's seat the same structure is a barrier: the text they hold as direct divine speech is legally operative only through a hierarchy they cannot verify, and their piety is recruited into the mediation. From the contextualist seat it is an enforced error with a millennium of institutional investment behind it. Same-level divergence inside the class itself: Hanafi, Shafi'i, and Zahiri jurists stood at the same nominal scholarly standing with radically different abrogation lists — the Zahiris near zero — so position on the doctrine, not global standing, differentiated their experience of it. The engine computes per-seat classifications from the structural data; the divergence between the seats is the measurement, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: the juristic class collects the arrangement's rent — interpretive authority, institutional position, the mediation monopoly — placing it near the beneficiary end, reinforced by identity-locked exit (their professional self is the apparatus). The court apparatus collects determinacy and passes enforcement costs upward (low d, constrained exit). Victim declarations: lay readers bear mediated access with identity-locked exit — the text is constitutive of them, so they cannot exit the arrangement's reach, pushing their effective position toward the full-target end; contextualist interpreters bear delegitimation and exclusion, with mobility (exit to academic and translation-accessible spaces) damping their effective extraction. Scope is continental-to-global: the arrangement operates across the traditionalist Islamic world, and verification of any given abrogation claim requires apparatus access that scales worse with distance from the scholarly centers. Two distinct identity-lock mechanisms bind the two locked seats: the juristic class is locked by professional identity (twelve centuries of scholarly self-understanding is the apparatus; abandoning the doctrine dissolves the class), lay readers by theological identity (the text constitutes them; mediated access is experienced as piety, so the lock survives contact with the alternatives). If the juristic frame broke — a mass juristic movement renouncing supersession for contextual specification — enforcement would collapse quickly, since persistence runs through the class's reproduction rather than lay demand. No directionality overrides were authored: the beneficiary/victim structure is clean and the derivation chain handles the two locked seats through their declared positions and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two mislabelings. Reading the doctrine as pure extraction would erase the coordination function: a closed legal corpus that records its own revision needs a conflict rule, and the arrangement has delivered one that courts operated on for twelve centuries. Reading it as pure coordination would erase the asymmetry: the same apparatus that resolves contradictions concentrates interpretive authority in a self-reproducing class and forecloses lay direct reading — the coordination and the extraction run through the same structure, which is the tangled-rope signature, not two separable functions. On mandate: the founding problem (determinate law from a self-varying fixed text) is live wherever the Quran functions as a legal source, so no dead-mandate declaration is authored; the theater series tracks the scholastic-drift risk (late-classical catalogue inflation; contemporary curricular reproduction) and the omega persistence_function_vs_theater asks whether traditionalist maintenance is operative function or inertia. Fixing is prohibitive for the seat that could fix it: the juristic class would need to re-derive twelve centuries of determinations and dissolve the authority that constitutes it, a cost far exceeding the benefit it would capture from interpretive openness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the naskh_principle kernel — what would the sibling readings change structurally, and where exactly does the disagreement bite?',
    'No empirical resolution; the readings are rival framings of the same textual phenomenon. The disagreement is located in one structural element: whether the Quran''s internal variation constitutes textual invalidation (this reading) or context-bound co-validity / pedagogical progression (the siblings). Adopting a sibling dissolves this story''s victim set (no verse loses force, so no mediation is required to know which commands bind) and redistributes its beneficiary structure (interpretive authority disperses to any competent reader of contexts).',
    'If a sibling reading displaced this one institutionally, the arrangement''s epsilon would fall toward the coordination-cost floor, the juristic class''s beneficiary position would evaporate, and the enforcement machinery would lose its object.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer position: classical_abrogation reading of the naskh_principle kernel, with foreclosure relations to contextual_harmonization and progressive_restriction.').

omega_variable(
    abrogation_list_instability,
    'Which verse pairs are genuinely abrogated? The classical tradition never settled its own list — counts run from roughly five (strict attestation criteria) through the majority working set (around twenty) to the maximal catalogues of several hundred.',
    'Isnad-critical analysis of the occasions-of-revelation and chronology reports against the legal-operative requirement (an abrogation claim must identify the superseding verse, the superseded verse, and the legal topic), with cross-school comparison of the resulting lists.',
    'A minimal list supports the coordination framing (the hierarchy touches few verses; the extraction is narrow); a maximal list inflates the arrangement''s reach (more of the text foreclosed to direct legal reading, more mediation required) and raises effective extraction across the payer seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_list_instability, empirical, 'Intra-reading contest over the abrogation list; the list''s size is the arrangement''s reach.').

omega_variable(
    persistence_function_vs_theater,
    'Is the doctrine''s contemporary persistence maintained by operative legal function (live rulings turning on abrogation determinations) or by institutional inertia and curricular reproduction (the science taught, catalogued, and examined but rarely decisive)?',
    'Survey operative legal practice across traditionalist institutions: count contemporary rulings and fatwas that actually turn on an abrogation determination, against curricular hours devoted to the science.',
    'If reproduction dominates, the arrangement drifts toward the piton signature inside traditionalist institutions — maintained by an agenda-setter class whose cost to fix exceeds what it bears; if operative function dominates, the tangled_rope classification holds steady.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_function_vs_theater, empirical, 'Operative function versus curricular performance in the doctrine''s present-day maintenance.').

omega_variable(
    lay_identity_lock_mechanism,
    'Is the lay readers'' locked position structural (community and apostasy costs, gatekept education) or internalized (the mediation accepted as piety, the text''s authority fused with the apparatus''s authority)?',
    'Post-exit trajectory of readers who acquire independent critical access (mass translation, published chronology scholarship): if resistance to the mediation persists once structural access barriers fall, the lock is substantially internalized; if reading practice reorganizes immediately, the lock was structural.',
    'If internalized, the arrangement''s effective suppression exceeds the structural measure — the mediation survives the gatekeeping''s removal; if structural, opening access collapses the enforcement requirement without any doctrinal change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_identity_lock_mechanism, empirical, 'Structural versus internalized lock on the payer seat with identity-locked exit.').

omega_variable(
    theological_coherence_cost,
    'Does the doctrine impose an intrinsic theological cost — divine speech containing self-superseding rulings — borne by the rationalist theologians who had to defend the coherence of God''s speech, and is that cost avoidable within the reading or constitutive of it?',
    'Analysis of the scholastic-theology literature''s abrogation defenses against the alternatives those defenses rejected: whether every coherent defense concedes the superseded-verse problem or some dissolve it.',
    'If constitutive, part of the arrangement''s cost is paid by a seat (the rationalist theologians) not captured in the victim arrays, and effective extraction is understated; if avoidable, the coherence cost belongs to the contest between readings, not to this arrangement''s operation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_coherence_cost, conceptual, 'Whether the coherence cost of self-superseding divine speech is intrinsic to the classical reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 800, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naskh_classical_tr_t800, naskh_principle__classical_abrogation, theater_ratio, 800, 0.1).
narrative_ontology:measurement_basis(naskh_classical_tr_t800, observed).
narrative_ontology:measurement(naskh_classical_tr_t1000, naskh_principle__classical_abrogation, theater_ratio, 1000, 0.12).
narrative_ontology:measurement_basis(naskh_classical_tr_t1000, observed).
narrative_ontology:measurement(naskh_classical_tr_t1200, naskh_principle__classical_abrogation, theater_ratio, 1200, 0.15).
narrative_ontology:measurement_basis(naskh_classical_tr_t1200, observed).
narrative_ontology:measurement(naskh_classical_tr_t1400, naskh_principle__classical_abrogation, theater_ratio, 1400, 0.22).
narrative_ontology:measurement_basis(naskh_classical_tr_t1400, observed).
narrative_ontology:measurement(naskh_classical_tr_t1600, naskh_principle__classical_abrogation, theater_ratio, 1600, 0.2).
narrative_ontology:measurement_basis(naskh_classical_tr_t1600, observed).
narrative_ontology:measurement(naskh_classical_tr_t1800, naskh_principle__classical_abrogation, theater_ratio, 1800, 0.18).
narrative_ontology:measurement_basis(naskh_classical_tr_t1800, observed).
narrative_ontology:measurement(naskh_classical_tr_t1950, naskh_principle__classical_abrogation, theater_ratio, 1950, 0.2).
narrative_ontology:measurement_basis(naskh_classical_tr_t1950, observed).
narrative_ontology:measurement(naskh_classical_tr_t2020, naskh_principle__classical_abrogation, theater_ratio, 2020, 0.22).
narrative_ontology:measurement_basis(naskh_classical_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(naskh_classical_be_t800, naskh_principle__classical_abrogation, base_extractiveness, 800, 0.35).
narrative_ontology:measurement_basis(naskh_classical_be_t800, observed).
narrative_ontology:measurement(naskh_classical_be_t1000, naskh_principle__classical_abrogation, base_extractiveness, 1000, 0.44).
narrative_ontology:measurement_basis(naskh_classical_be_t1000, observed).
narrative_ontology:measurement(naskh_classical_be_t1200, naskh_principle__classical_abrogation, base_extractiveness, 1200, 0.5).
narrative_ontology:measurement_basis(naskh_classical_be_t1200, observed).
narrative_ontology:measurement(naskh_classical_be_t1400, naskh_principle__classical_abrogation, base_extractiveness, 1400, 0.55).
narrative_ontology:measurement_basis(naskh_classical_be_t1400, observed).
narrative_ontology:measurement(naskh_classical_be_t1600, naskh_principle__classical_abrogation, base_extractiveness, 1600, 0.57).
narrative_ontology:measurement_basis(naskh_classical_be_t1600, observed).
narrative_ontology:measurement(naskh_classical_be_t1800, naskh_principle__classical_abrogation, base_extractiveness, 1800, 0.58).
narrative_ontology:measurement_basis(naskh_classical_be_t1800, observed).
narrative_ontology:measurement(naskh_classical_be_t1950, naskh_principle__classical_abrogation, base_extractiveness, 1950, 0.57).
narrative_ontology:measurement_basis(naskh_classical_be_t1950, observed).
narrative_ontology:measurement(naskh_classical_be_t2020, naskh_principle__classical_abrogation, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement_basis(naskh_classical_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(naskh_classical_su_t800, naskh_principle__classical_abrogation, suppression_requirement, 800, 0.45).
narrative_ontology:measurement_basis(naskh_classical_su_t800, observed).
narrative_ontology:measurement(naskh_classical_su_t1000, naskh_principle__classical_abrogation, suppression_requirement, 1000, 0.55).
narrative_ontology:measurement_basis(naskh_classical_su_t1000, observed).
narrative_ontology:measurement(naskh_classical_su_t1200, naskh_principle__classical_abrogation, suppression_requirement, 1200, 0.65).
narrative_ontology:measurement_basis(naskh_classical_su_t1200, observed).
narrative_ontology:measurement(naskh_classical_su_t1400, naskh_principle__classical_abrogation, suppression_requirement, 1400, 0.68).
narrative_ontology:measurement_basis(naskh_classical_su_t1400, observed).
narrative_ontology:measurement(naskh_classical_su_t1600, naskh_principle__classical_abrogation, suppression_requirement, 1600, 0.6).
narrative_ontology:measurement_basis(naskh_classical_su_t1600, observed).
narrative_ontology:measurement(naskh_classical_su_t1800, naskh_principle__classical_abrogation, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement_basis(naskh_classical_su_t1800, observed).
narrative_ontology:measurement(naskh_classical_su_t1950, naskh_principle__classical_abrogation, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement_basis(naskh_classical_su_t1950, observed).
narrative_ontology:measurement(naskh_classical_su_t2020, naskh_principle__classical_abrogation, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement_basis(naskh_classical_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% Constraint family: the naskh_principle kernel decomposes into three readings with distinct epsilon values because they are distinct constraints, not one constraint under different observables. This story (classical_abrogation) authors the supersession arrangement: epsilon ~0.58 — a real coordination core (contradiction resolution for a closed legal corpus) with asymmetric, enforced extraction (interpretive authority concentrated in the juristic class; lay direct reading foreclosed). The sibling stories author different arrangements: contextual_harmonization (no verse loses force; the mediation requirement dissolves; epsilon near the coordination floor) and progressive_restriction (the sequence itself is the content; no invalidation; epsilon low, pedagogical). The classical reading is upstream: the siblings' catalogues and self-definitions are structured against its enforcement history, and its consolidation created the legitimacy conditions under which they operate as dissent. Edges run from this story to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
