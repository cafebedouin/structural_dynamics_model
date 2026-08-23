% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Classical Naskh: Chronological Supersession of Earlier Revealed Rulings
 *   domain: religious/legal/hermeneutic
 *
 * SUMMARY:
 *   The classical abrogation doctrine holds that where two revealed verses
 *   address the same legal or theological matter incompatibly, the
 *   chronologically later verse supersedes the earlier one's legal force
 *   while the earlier text remains in the recited Book. Administered by a
 *   jurist class trained in revelation chronology and in the evidentiary
 *   criteria for establishing supersession, the doctrine converts textual
 *   development across the revelatory decades into a single determinate law.
 *   Colloquial usage compresses three structurally distinct claims under the
 *   label of naskh: chronological supersession (this story), contextual
 *   specification of every verse within its occasion (sibling
 *   contextual_harmonization), and progressive divine pedagogy without
 *   invalidation (sibling progressive_restriction). Per the
 *   epsilon-invariance principle these are separate constraints with separate
 *   epsilon values, linked through network.affects_constraints; this file
 *   instantiates only the classical reading. The claimed_type is authored
 *   independently of the metrics: the doctrine is claimed tangled_rope
 *   because it simultaneously solves a real coordination problem (determinate
 *   law from a developing text) and extracts asymmetrically (interpretive
 *   authority concentrated in the jurist class; rival hermeneutics
 *   suppressed), while the metrics describe observed operation. The abstract
 *   goods named in the expected delta are carried by actor proxies:
 *   interpretive flexibility by the modernist interpreter seat, theological
 *   coherence by the lay-believer recitation burden and omega
 *   retained_text_coherence_cost; neither abstract good is listed as a
 *   beneficiary or victim.
 *
 * KEY AGENTS:
 *   - classical_jurist_class: agenda-setter and principal beneficiary (organized/identity_locked) — administers the chronology, certifies abrogation, collects interpretive authority and the fatwa economy
 *   - madhhab_institutions: beneficiary (institutional/identity_locked) — codified corpus depends on the supersession spine
 *   - sharia_judges: secondary beneficiary (organized/mobile) — determinacy rents without existential dependence
 *   - modernist_reformist_interpreters: primary target (moderate/constrained) — locked out of re-evaluating superseded rulings
 *   - quranic_scripturalists: primary target (powerless/trapped) — the reading the enforcement machinery excludes outright
 *   - lay_believers: diffuse target with incidental coordination benefit (powerless/constrained)
 *   - speculative_abrogation_critics: excluded historical seat (organized/trapped) — strict-evidence line sidelined from canon-setting
 *   - academic_islamic_studies_scholars: analytical observer — attests the history without a seat in adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.56).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.5).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.56).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Naskh: Chronological Supersession of Earlier Revealed Rulings").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious/legal/hermeneutic").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, '9038ef0b-1445-4b97-b9e6-ef1ae2369136').
narrative_ontology:cs_kernel_codification('9038ef0b-1445-4b97-b9e6-ef1ae2369136', fixed_text).
narrative_ontology:cs_authority_grounding('9038ef0b-1445-4b97-b9e6-ef1ae2369136', lineage).
narrative_ontology:cs_interpretation_layer_present('9038ef0b-1445-4b97-b9e6-ef1ae2369136').
narrative_ontology:cs_reading_relation('9038ef0b-1445-4b97-b9e6-ef1ae2369136', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_reading_relation('9038ef0b-1445-4b97-b9e6-ef1ae2369136', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('9038ef0b-1445-4b97-b9e6-ef1ae2369136', foundational, chronological_supersession_determines_legal_force).
narrative_ontology:cs_axiom_status(chronological_supersession_determines_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('9038ef0b-1445-4b97-b9e6-ef1ae2369136', chronological_supersession_determines_legal_force, theological).
narrative_ontology:cs_axiom('9038ef0b-1445-4b97-b9e6-ef1ae2369136', secondary, abrogation_determination_requires_expert_verification).
narrative_ontology:cs_axiom_status(abrogation_determination_requires_expert_verification, holdable).
narrative_ontology:cs_axiom_grounding('9038ef0b-1445-4b97-b9e6-ef1ae2369136', abrogation_determination_requires_expert_verification, conventional).
narrative_ontology:cs_reference_frame('9038ef0b-1445-4b97-b9e6-ef1ae2369136', revelation_sequence_supremacy).
narrative_ontology:cs_drift_state('9038ef0b-1445-4b97-b9e6-ef1ae2369136', contemporary_reform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9038ef0b-1445-4b97-b9e6-ef1ae2369136', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_jurist_class).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, madhhab_institutions).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, sharia_judges).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, modernist_reformist_interpreters).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, quranic_scripturalists).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, lay_believers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, lay_believers).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, chronological_supersession_validity).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, juristic_mediation_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains in usul al-fiqh, masters the occasions of revelation and the abrogation registers, and certifies which verse governs where texts collide. Scholarly standing, students, court appointments, and fatwa income flow through that certification. Leaving the discipline means forfeiting a standing built over decades, and the discipline's categories constitute the senior scholar's professional self.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_jurist_class, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, classical_jurist_class, beneficiary).

% Codified legal schools whose manuals interlock thousands of rulings through the supersession spine. Abandoning the doctrine would unravel transmitted chapters and the school's self-understanding as a faithful chain of transmission; the institution has become its reconciled corpus.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, madhhab_institutions, beneficiary,
    institutional, civilizational, identity_locked, continental).

% Decides disputes using determinate rules; knowing which text controls shrinks litigation ambiguity. Individual judges can move between jurisdictions or into secular posts, so their dependence is professional rather than existential.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, sharia_judges, beneficiary,
    organized, biographical, mobile, national).

% Seeks to harmonize apparently conflicting verses through context, purpose, and historical specificity. The fixed hierarchy locks rulings they would otherwise re-evaluate, and their publications draw charges of undermining the tradition. Staying inside the scholarly ecosystem costs them standing; leaving it removes the audience they address.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, modernist_reformist_interpreters, payer,
    moderate, generational, constrained, global).

% Rejects the jurist-mediated layer altogether and reads the Quran as self-sufficient. Classical abrogation doctrine marks the position as heresy across much of the Muslim world, and several jurisdictions prosecute it. Exit would mean abandoning the conviction that defines the movement.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, quranic_scripturalists, payer,
    powerless, biographical, trapped, global).

% Receives determinate law without mastering its derivation, which is a genuine convenience, while carrying the delegation of interpretive authority and the recurring puzzle of reciting verses they are told no longer carry legal force. Direct questions about retained-but-inoperative passages are deflected to credentialed authorities.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, lay_believers, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, lay_believers, beneficiary).

% A scholarly line associated with literalist methodology insisting every abrogation claim rest on indisputably mass-transmitted evidence. Its critiques stripped many classical determinations down to a handful of secure cases, but the majority curriculum absorbed the critique selectively and kept the broader tables, leaving the strict line without a seat in canon-setting.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, speculative_abrogation_critics, excluded,
    organized, generational, trapped, continental).

% Studies the doctrine's history, the wide variance of abrogation registers, and the modern contests from university departments; attests the reality of revelatory-period textual development without holding a seat in the tradition's internal adjudication.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, academic_islamic_studies_scholars, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__classical_abrogation, classical_jurist_class).
narrative_ontology:fixing_cost_class(naskh_principle__classical_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate decision procedure for apparent conflicts among revealed rulings: where two verses address the same obligation in incompatible terms, the chronologically later one supplies the operative law, letting judges and jurists across regions and centuries apply a single consistent code without editing the received text.
% TRANSFER_FUNCTION: Moves interpretive authority from the general body of believers to the trained jurist class, since only experts in revelation chronology and abrogation criteria may pronounce on which text governs; moves normative force from earlier to later revelations on shared topics; and moves the explanatory burden for retained-but-legally-void verses onto teachers who must reconcile recited text with operative law for questioning congregations.
% ABSENT_VOICES: Direct-reading laypeople who stumble over retained-but-inoperative verses have no seat in usul canon-setting; contextualist harmonizers inside the tradition publish but rarely staff the committees that certify curricula; the strict-evidence critic line was historically absorbed and then sidelined; and contemporary Muslims most affected by particular superseded rulings (inheritance shares, contested penal provisions) are rarely present when abrogation tables are taught or applied.
% DISAPPEARANCE_RATIONALE: Without the supersession hierarchy, fiqh inherits unresolved textual conflicts on core topics (gradual prohibitions, revised inheritance shares, battlefield conduct); the madhhab legal corpora lose their reconciliation spine; courts would face contradictory authoritative texts; the reformist project would gain immediate ground; and the entire curriculum built around abrogating and abrogated categories would need replacement. The doctrine is load-bearing for the standing legal architecture even where its costs are hotly disputed.
% FOUNDING_PROBLEM: The first generations received a scripture whose rulings developed over roughly twenty-three years: wine moved from tolerated to discouraged to prohibited, inheritance shares were revised, the prayer direction changed. The community needed one operative law at any given time while keeping the full revealed text intact and recited, and needed to explain why an earlier and a later command both stood in the Book.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the beneficiary set by the hadith corpus itself, which records companions revising practice upon later revelation (reports transmitted in Bukhari and Muslim, attested by narrators who collected no rent from the jurist class), and by Western academic historiography of the revelation period. The status is contested: classical institutions attest the problem stays live wherever the reconciled corpus governs, while modernist and contextualist scholars attest it is a closed historical problem whose solution mechanism, not existence, is the real dispute; the sibling readings agree the tension existed while rejecting chronological invalidation as its resolution.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.56, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate-high (0.56 at interval end) because the doctrine concentrates a scarce good — the authority to say which revealed text governs — in one class, and forecloses reinterpretation of whatever that class has ruled superseded; it is damped below snare levels because the doctrine delivers real determinacy to every seat that consumes the law. Suppression (0.50) is predominantly social and institutional (charges of innovation, curriculum control, prosecution of scripturalists in some polities) rather than physical coercion everywhere; it is a raw unscaled structural input, and only extractiveness is scaled by directionality and scope downstream. Theater_ratio (0.38) reflects a growing rote layer — memorized abrogation registers whose entries rarely touch operative law, credentialing rituals detached from adjudication — atop a core function that still works. Accessibility_collapse is low-moderate (0.40) because the alternatives demonstrably survive: the two sibling readings remain live positions, so understanding the doctrine does not close the option space. Resistance (0.62) is sustained: modernist publication, scripturalist refusal, and the historical strict-evidence critique. The three measurement series run on one shared time grid (points spaced at roughly 57-year steps, T0 near the companion generation around 650 CE, T4 near the late-ninth-century codification wave, T8 near classical consolidation, T16 near the imperial-height era, T20 near the nineteenth-century rupture, T24 the present), so every tracked metric is authored at every examined time point. The trajectory is monotonic-drifting except suppression_requirement, whose dip at T20 records the colonial-era dismantling of judicial enforcement and whose partial recovery at T24 records state religious bureaucracy and social enforcement; that enforcement-capacity arc is why suppression_requirement is tracked at all. Victim seats are fragmentation-prone — modernists, scripturalists, and disaffected lay readers share grievances but lack coalition infrastructure (distinct methodologies, mutual suspicion, geographic dispersion), which holds aggregate resistance below what the summed grievance would predict. Fixing the arrangement is prohibitive: replacing the supersession spine would require re-deriving the reconciled corpus of applied law, and the receipts accrue to the class that would have to authorize the repair.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the doctrine is the tradition's own reconciliation machinery, coordination it built, maintains, and teaches; from the scripturalist seat the same machinery operates as enforced exclusion from the text's meaning. Judge seats experience something close to a rope — cheap determinacy purchased at modest professional dependence — while modernist seats experience a ceiling on reinterpretation and lay seats experience a mixed bill of certainty gained and authority delegated. The engine computes these divergent per-seat classifications from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map to directionality as follows: the jurist class sits nearest the beneficiary pole, writing and enforcing the supersession tables from which its authority derives; madhhab institutions and working judges collect determinacy at decreasing intensity. The scripturalist seat sits nearest the full-target pole — the doctrine's enforcement exists precisely to exclude its reading, and its exit is blocked by prosecution and conviction. Modernist interpreters are high-d but retain constrained mobility through publishing and institutional niches. Lay believers derive genuine certainty benefits that damp their d below the scripturalists' while bearing the delegation cost and the recitation-coherence burden. The strict-evidence critic line is excluded rather than coordinated; its historical marginalization is the enforcement object made visible. No directionality overrides are needed: the beneficiary and victim declarations together with the exit-option spread produce the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — producing one operative law from a text whose rulings developed over twenty-three years — is historically bounded: revelation closed in 632 CE. Yet the doctrine's coordination output, determinate law across the reconciled corpus, remains live wherever that corpus governs, so the mandate has not plainly outlived its function and mandatrophy_resolved is deliberately left unset. The rote portions (memorized registers with little remaining legal effect, credentialing detached from adjudication) show piton-grade theatricality, visible in the rising theater_ratio series; the classification prevents both failure modes at once — it refuses to read the whole doctrine as pure extraction (which would erase the live coordination function judges and litigants consume) and refuses to read it as pure coordination (which would erase the documented exclusion of rival hermeneutics). Because founding_problem_status is authored contested rather than dead, the status-by-verdict consumer finds no dead-problem capture flag; the genuinely open questions are routed to abrogation_list_variance, retained_text_coherence_cost, and enforcement_capacity_trajectory rather than forced to a verdict the sources cannot support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_classification_shift,
    'This story instantiates only the classical_abrogation reading of the naskh_principle kernel; how would the classification, the victim set, and epsilon move under the contextual_harmonization or progressive_restriction sibling?',
    'Generate the two sibling stories under the same referent discipline (each assessing the standing arrangement it contests by its own lights) and compare computed per-seat types; the harmonization sibling should reverse which seat is extracted-from, converting the jurist class''s gatekeeping rents into the arrangement''s cost side.',
    'If the siblings compute as rope or scaffold while this reading computes tangled_rope, the contest itself is the extraction surface: the doctrine''s persistence functions to suppress cheaper coordination alternatives, and the kernel''s resolution would reprice every seat in this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_classification_shift, conceptual, 'Reading-indexed classification: one kernel, three constraints, divergent epsilon and victim geometry.').

omega_variable(
    abrogation_list_variance,
    'Which verse-pairs does abrogation actually govern — the extension ranges from a handful of indisputably mass-transmitted cases to hundreds of entries in maximalist madhhab tables?',
    'Comparative collation of the classical abrogation registers, separating the minimal securely-transmitted core from probable and disputed entries, weighted by which entries still touch operative law today.',
    'A minimal core shrinks measured extraction toward pure coordination and lowers the expertise premium; maximal lists raise theater_ratio (maintenance of determinations with little remaining legal effect) and widen the authority gap the jurist class collects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_list_variance, empirical, 'Extensional uncertainty in the doctrine''s own determinations.').

omega_variable(
    retained_text_coherence_cost,
    'Does resolving legal conflict by invalidation preserve or degrade theological coherence for believers who must recite a text containing commands God later replaced?',
    'Survey preaching practice and believer-facing apologetics: measure how often the supersession explanation resolves doubt versus generates it, benchmarked against communities operating under the harmonizing sibling reading.',
    'If the coherence cost is high and rising, the doctrine''s legitimation story decays faster than its legal function, accelerating drift toward the harmonization sibling and raising resistance beyond current levels.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retained_text_coherence_cost, conceptual, 'Whether the abrogation solution trades legal determinacy for a growing theological-coherence debt.').

omega_variable(
    enforcement_capacity_trajectory,
    'Will state religious bureaucracies re-intensify suppression of rival hermeneutics (scripturalist prosecutions, curriculum mandates, platform removal of contextualist work), reversing the colonial-era decay visible in the suppression series?',
    'Track prosecutions, curriculum-control instruments, and censorship events targeting contextualist and scripturalist publication over the coming two decades.',
    'Rising suppression_requirement would push payer-seat computations toward snare-flavored classifications; continued decay would deepen piton symptoms in the rote curriculum while leaving the live adjudication core intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_trajectory, empirical, 'Future path of the enforcement-capacity arc the measurements show dipping at the twentieth-century rupture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(nask_tr_t0, observed).
narrative_ontology:measurement(nask_tr_t4, naskh_principle__classical_abrogation, theater_ratio, 4, 0.18).
narrative_ontology:measurement_basis(nask_tr_t4, observed).
narrative_ontology:measurement(nask_tr_t8, naskh_principle__classical_abrogation, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(nask_tr_t8, observed).
narrative_ontology:measurement(nask_tr_t12, naskh_principle__classical_abrogation, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(nask_tr_t12, observed).
narrative_ontology:measurement(nask_tr_t16, naskh_principle__classical_abrogation, theater_ratio, 16, 0.28).
narrative_ontology:measurement_basis(nask_tr_t16, observed).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__classical_abrogation, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(nask_tr_t20, observed).
narrative_ontology:measurement(nask_tr_t24, naskh_principle__classical_abrogation, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(nask_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(nask_be_t0, observed).
narrative_ontology:measurement(nask_be_t4, naskh_principle__classical_abrogation, base_extractiveness, 4, 0.42).
narrative_ontology:measurement_basis(nask_be_t4, observed).
narrative_ontology:measurement(nask_be_t8, naskh_principle__classical_abrogation, base_extractiveness, 8, 0.49).
narrative_ontology:measurement_basis(nask_be_t8, observed).
narrative_ontology:measurement(nask_be_t12, naskh_principle__classical_abrogation, base_extractiveness, 12, 0.53).
narrative_ontology:measurement_basis(nask_be_t12, observed).
narrative_ontology:measurement(nask_be_t16, naskh_principle__classical_abrogation, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(nask_be_t16, observed).
narrative_ontology:measurement(nask_be_t20, naskh_principle__classical_abrogation, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(nask_be_t20, observed).
narrative_ontology:measurement(nask_be_t24, naskh_principle__classical_abrogation, base_extractiveness, 24, 0.56).
narrative_ontology:measurement_basis(nask_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(nask_su_t0, observed).
narrative_ontology:measurement(nask_su_t4, naskh_principle__classical_abrogation, suppression_requirement, 4, 0.45).
narrative_ontology:measurement_basis(nask_su_t4, observed).
narrative_ontology:measurement(nask_su_t8, naskh_principle__classical_abrogation, suppression_requirement, 8, 0.55).
narrative_ontology:measurement_basis(nask_su_t8, observed).
narrative_ontology:measurement(nask_su_t12, naskh_principle__classical_abrogation, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(nask_su_t12, observed).
narrative_ontology:measurement(nask_su_t16, naskh_principle__classical_abrogation, suppression_requirement, 16, 0.6).
narrative_ontology:measurement_basis(nask_su_t16, observed).
narrative_ontology:measurement(nask_su_t20, naskh_principle__classical_abrogation, suppression_requirement, 20, 0.44).
narrative_ontology:measurement_basis(nask_su_t20, observed).
narrative_ontology:measurement(nask_su_t24, naskh_principle__classical_abrogation, suppression_requirement, 24, 0.5).
narrative_ontology:measurement_basis(nask_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'naskh' per the epsilon-invariance principle. Measuring the doctrine by its legal-certainty output yields low extraction; measuring it by its treatment of contested verse-pairs and rival hermeneutics yields substantial extraction. That observable-dependence signals two-plus distinct claims sharing one label, so the family splits: this file (classical_abrogation, tangled_rope — coordination with asymmetric extraction and active enforcement) links to contextual_harmonization (expected rope-flavored: coordination without a supersession hierarchy, extraction shifted onto the lost determinacy) and progressive_restriction (expected scaffold-or-rope-flavored: pedagogical framing with different victim geometry). Upstream-downstream structure runs from this story to the siblings historically: the classical reading's codified tables are the datum both siblings reframe, so its determinations shape the resource environment in which the alternatives operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
