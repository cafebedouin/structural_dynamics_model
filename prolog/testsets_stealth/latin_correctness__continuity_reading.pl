% ============================================================================
% CONSTRAINT STORY: latin_correctness__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: latin_correctness__continuity_reading
 *   human_readable: Continuity Reading of Latin Correctness: Medieval Latin as Organic Continuation
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   The standing arrangement under contest is the normative regime by which
 *   post-classical Latin is evaluated. This file instantiates ONE reading of
 *   that regime — the continuity reading: medieval Latin is the legitimate
 *   continuation of classical Latin through organic linguistic change, so
 *   vernacular-shifted phonology, expanded vocabulary, and altered syntax are
 *   developmental stages rather than faults. Under this reading the regime
 *   operates as light coordination: a shared evaluative protocol that lets
 *   scholars read, date, localize, and edit a millennium of texts
 *   cumulatively. Beneficiaries are the scholarly communities working on
 *   medieval materials and, retrospectively, the historical authors whose
 *   practice the frame legitimizes; costs are mild and diffuse. There is no
 *   victim set. The sibling readings (rupture_reading, hybrid_reading) are
 *   separate constraint files linked through the network section; this file's
 *   epsilon is indexed to the standing arrangement AS THIS READING SEES IT,
 *   per the kernel-referent rule. Temporal mapping: interval units are years,
 *   t=0 corresponds to 1960 (postwar consolidation of medieval Latin
 *   lexicography and corpus-building) and t=60 to 2020. KEY AGENTS (by
 *   structural relationship): - medievalists: primary beneficiary
 *   (organized/constrained) — the frame makes their archive legible on its
 *   own terms - critical_editors_of_medieval_texts: beneficiary
 *   (organized/mobile) — preservation-over-correction editorial license -
 *   scholastic_theologians: beneficiary (organized/constrained) — scholastic
 *   corpus secured as continuous inheritance - medieval_latin_authorship:
 *   retrospective beneficiary (powerless/trapped) — historical writers
 *   legitimized, unable to respond - classical_philologists: mild payer with
 *   offsetting gain (institutional/mobile) — corrective jurisdiction narrows
 *   to antiquity - rupture_tradition_classicists: excluded
 *   (institutional/mobile) — corrective program finds little purchase where
 *   this reading sets defaults - historical_linguists: analytical observer —
 *   supplies the descriptive apparatus, takes no editorial side
 *
 * KEY AGENTS:
 *   - medievalists: primary beneficiary (organized/constrained) — reads medieval usage as evidence, not fault
 *   - critical_editors_of_medieval_texts: beneficiary (organized/mobile) — preserves manuscript forms instead of normalizing
 *   - scholastic_theologians: beneficiary (organized/constrained) — seven-century corpus kept mutually intelligible
 *   - medieval_latin_authorship: retrospective beneficiary (powerless/trapped) — historical practice legitimized from outside
 *   - classical_philologists: mild payer, secondary beneficiary (institutional/mobile) — lost jurisdiction offset by longer record
 *   - rupture_tradition_classicists: excluded (institutional/mobile) — hold the fixed-standard view outside the governing conversation
 *   - historical_linguists: analytical observer (analytical/analytical) — descriptive apparatus, no editorial stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__continuity_reading, 0.14).
domain_priors:suppression_score(latin_correctness__continuity_reading, 0.08).
domain_priors:theater_ratio(latin_correctness__continuity_reading, 0.11).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, theater_ratio, 0.11).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__continuity_reading, rope).
narrative_ontology:human_readable(latin_correctness__continuity_reading, "Continuity Reading of Latin Correctness: Medieval Latin as Organic Continuation").
narrative_ontology:topic_domain(latin_correctness__continuity_reading, "intellectual_history/philology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__continuity_reading, 'f5f12e6e-0fe6-413c-b3fa-33b41d4eac69').
narrative_ontology:cs_kernel_codification('f5f12e6e-0fe6-413c-b3fa-33b41d4eac69', fixed_text).
narrative_ontology:cs_authority_grounding('f5f12e6e-0fe6-413c-b3fa-33b41d4eac69', lineage).
narrative_ontology:cs_interpretation_layer_present('f5f12e6e-0fe6-413c-b3fa-33b41d4eac69').
narrative_ontology:cs_reading_relation('f5f12e6e-0fe6-413c-b3fa-33b41d4eac69', latin_correctness__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('f5f12e6e-0fe6-413c-b3fa-33b41d4eac69', latin_correctness__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('f5f12e6e-0fe6-413c-b3fa-33b41d4eac69', foundational, unbroken_transmission_confers_legitimacy).
narrative_ontology:cs_axiom_status(unbroken_transmission_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f5f12e6e-0fe6-413c-b3fa-33b41d4eac69', unbroken_transmission_confers_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('f5f12e6e-0fe6-413c-b3fa-33b41d4eac69', foundational, diachronic_deviation_is_development).
narrative_ontology:cs_axiom_status(diachronic_deviation_is_development, holdable).
narrative_ontology:cs_axiom_grounding('f5f12e6e-0fe6-413c-b3fa-33b41d4eac69', diachronic_deviation_is_development, empirically_contingent).
narrative_ontology:cs_reference_frame('f5f12e6e-0fe6-413c-b3fa-33b41d4eac69', living_language_continuum).
narrative_ontology:cs_drift_state('f5f12e6e-0fe6-413c-b3fa-33b41d4eac69', contemporary_sociolinguistic_era, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('f5f12e6e-0fe6-413c-b3fa-33b41d4eac69', '').
narrative_ontology:cs_kernel_id(latin_correctness__continuity_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medievalists).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, critical_editors_of_medieval_texts).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, scholastic_theologians).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_latin_authorship).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, classical_philologists).
narrative_ontology:constraint_victim(latin_correctness__continuity_reading, classical_philologists).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, descriptive_historical_linguistics).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, latin_romance_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research and teach the Latin writing of the Middle Ages. The frame lets them treat manuscript spellings, mixed declensions, new compounds, and shifted word order as evidence for dating, localizing, and attributing texts rather than as faults awaiting repair. Their training, journals, conferences, and the scholarly standing of their archives all presuppose this way of reading; moving to another specialty would mean retraining and abandoning accumulated expertise.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medievalists, beneficiary,
    organized, biographical, constrained, global).

% Produce critical editions and digital corpora of medieval texts. Under the frame they preserve manuscript forms, record variants, and annotate innovations instead of silently normalizing them to classical usage; linguistic difference becomes sortable data. Their skills transfer readily to other philological and documentary projects.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, critical_editors_of_medieval_texts, beneficiary,
    organized, biographical, mobile, global).

% Read Aquinas, canon-law commentaries, liturgical exposition, and university disputations as continuous with patristic and classical inheritance. The frame keeps a seven-century corpus mutually intelligible and authoritative across generations of faculties; their institutions' self-understanding depends on that continuity holding.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, scholastic_theologians, beneficiary,
    organized, generational, constrained, global).

% Wrote charters, theology, science, histories, and letters in the Latin of their own region and century, without classical pronunciation, without the full classical lexicon, and often with late-antique and Christian models rather than Cicero. The frame legitimizes their practice retrospectively. They cannot respond to it, profit from it, or object to it; the frame is applied to their texts by others.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_latin_authorship, beneficiary,
    powerless, generational, trapped, continental).

% Maintain the classical corpus and its norms as the reference point for Latinity. Under the frame their corrective jurisdiction stops at antiquity's edge: they no longer grade a millennium of post-classical prose against Cicero. That narrows their gatekeeping over correct Latin, while freeing them from adjudicating endless medieval variation and handing them a longer diachronic record to work with.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, classical_philologists, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(latin_correctness__continuity_reading, classical_philologists, beneficiary).

% Hold that classical Latin is a fixed textual standard recoverable from ancient sources and that medieval deviation is corruption to be corrected. Where the continuity frame sets editorial defaults — medieval-studies series, corpus projects, graduate training — their corrective program finds little purchase, and their objections circulate mainly in classicist venues outside that conversation.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, rupture_tradition_classicists, excluded,
    institutional, biographical, mobile, global).

% Trace sound change, morphology, and vocabulary from classical Latin through the medieval period into the Romance languages. They supply the descriptive apparatus — comparative method, chronology of shifts, dialect geography — that the continuity frame relies on, and they examine how the norm operates without taking sides in editorial disputes.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, historical_linguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(latin_correctness__continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared evaluative standard under which scholars can read, date, localize, attribute, and edit medieval Latin texts cumulatively: treating the language as one continuing tradition turns lexical, phonological, and syntactic innovation into evidence instead of defect.
% TRANSFER_FUNCTION: Moves interpretive legitimacy and editorial authority: status flows from classical-norm gatekeeping toward the communities that work directly on medieval usage; classical philologists' corrective jurisdiction over post-classical texts narrows correspondingly. Nothing material is extracted — what moves is standing.
% ABSENT_VOICES: Holders of the rupture reading are largely absent from the venues where this frame sets editorial defaults (medieval-studies journals, monograph series, digital corpus projects); their objection — that continuity framing licenses under-correction of genuine error — circulates mainly in classicist outlets. The historical authors and scribes whom the frame legitimizes cannot speak at all; their practice is described by modern specialists who benefit from the frame doing the describing.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished overnight, medieval studies would rearrange around a corrective norm: editions would normalize spelling and syntax to classical usage, dating and localization by linguistic features would lose their evidentiary basis, and a millennium of charters, theology, and scientific writing would be reframed as degraded imitation. The field's infrastructure — dictionaries, corpora, graduate training — would rebuild around reconstruction from ancient sources, and the scholastic corpus's cross-century intelligibility would fracture.
% FOUNDING_PROBLEM: The humanist encounter with medieval texts: applying Ciceronian norms made a millennium of European writing look barbarous and unusable. Scholars needed a standard under which charters, scholastic theology, and scientific Latin could be read as evidence rather than condemned — Du Cange's glossary and, later, historical linguistics supplied it by re-describing difference as development.
% FOUNDING_PROBLEM_CORROBORATION: Legal and social historians who depend on charters and records attest the practical necessity from outside the named beneficiary disciplines: without a continuity frame the documents resist use. Romance linguists independently demonstrate continuous development out of Latin. Even classicist critics concede the descriptive facts while disputing the normative conclusion. Corroboration is disciplinary-adjacent rather than fully external — historians are heavy users of the frame's outputs — and no body outside philology polices the claim; that partial externality is itself signal.
narrative_ontology:disappearance_verdict(latin_correctness__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__continuity_reading, 0.14, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__continuity_reading_tests).
:- end_tests(latin_correctness__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. The claimed type is rope on structural grounds: a genuine collective-action problem (making non-classical Latin usable as evidence) solved with minimal overhead, net beneficiaries, and no suppressed alternative. The metrics describe actual operation: extractiveness 0.14 — residual extraction consists of a mild status transfer away from classicist gatekeeping plus episodic under-correction excused as development; far below any tangled-rope or snare range. Suppression 0.08 is a raw, unscaled structural value: nothing bars anyone from adopting rupture norms; the reading persists by demonstrated usefulness, not coercion. Theater_ratio 0.11 — the work is mostly real (dating, localization, edition policy); ritual invocations of 'organic development' as a credential phrase creep upward slowly, which the measurement series tracks. Accessibility_collapse 0.22 — grasping the frame leaves the rupture alternative fully available; low collapse is rope-consistent. Resistance 0.28 — steady classicist critique and periodic flare-ups (dictionary controversies, diglossia debates), no organized opposition. Both tracked metrics run on one shared seven-point grid (t=0,10,...,60) so every metric is authored at every examined time point; a suppression_requirement series is deliberately omitted because the enforcement picture is static — there is no enforcement machinery to build up or decay — and the scalar suppression carries that fact. Receipt surface: the small extraction dissipates as diffuse status adjustment across the field; no seat administers the reading or collects its effects, so gain_flow is authored 'diffuse' as an affirmative checked claim, not a default. fixing_cost is 'cheap': nothing entrenches the reading — no machinery to dismantle — and a sustained editorial-pedagogical movement could replace it within a generation; its persistence reflects ongoing assent, which is what keeps it a rope rather than a piton.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical global standing. The medievalist seat experiences the frame as liberating: their material becomes legible on its own terms, and their chi sits near the beneficiary end. The classical_philologists seat is near-symmetric: a real but mild cost (jurisdiction narrowed) offset by a real gain (freedom from adjudicating a millennium of variation, plus a longer laboratory). The excluded rupture_tradition_classicists seat would compute the very same texts as corrupted — but that computation belongs to the sibling constraint's file, not this one; within this reading's frame their objection registers only as the resistance metric. Same-power differentiation: classical_philologists and rupture_tradition_classicists share institutional power and mobile exit, yet occupy different relations to the norm — the former accepts its jurisdiction limit, the latter rejects the frame outright — which is exactly the constraint-specific factor the per-seat computation should surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (medievalists, critical editors, scholastic theologians, and retrospectively the historical authorship) derive directionality near the beneficiary end (d roughly 0.05-0.15): the frame subsidizes their practice and their exit costs are ordinary career costs, not trap or identity-lock — medievalists could retrain, at price. Classical_philologists are the only seat bearing a cost, and it is mild: the derivation from their payer role alone would overshoot toward the target end because it cannot see the offsetting benefit (a longer diachronic record, freedom from unbounded correction duty); a directionality override sets institutional-seat d to 0.55 to encode the near-symmetric reality. No agent approaches the full-target end; with base epsilon small and no trapped targets, effective extraction stays low across every seat. Global spatial scope raises verification difficulty modestly, but there is little extraction to amplify.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making a millennium of non-classical Latin usable as evidence rather than condemning it as barbarous — is still live: every newly digitized corpus re-poses it, so the mandate has not outlived its function and no sunset applies. Classifying this as rope prevents two characteristic mislabels. As mountain it would be wrong: the reading is a constructed, revisable scholarly norm that meets real resistance and has a documented history of formation (Du Cange through historical linguistics), not a natural law. As snare it would be equally wrong: there are no victims, no coercion, and no suppressed exit — the rival reading publishes freely. The live drift risk is Goodhart's: theater_ratio rising slowly as 'organic development' hardens from a research posture into a credential phrase; the measurement series watches exactly that slope, and the development_error_boundary omega names the mechanism by which drift would convert into real epistemic cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates only the continuity_reading of the latin_correctness kernel; how would the sibling readings (rupture_reading, hybrid_reading) change the structural classification if compiled as their own constraints?',
    'Compile the sibling files and compare computed per-seat types: the rupture reading should show sharply higher epsilon with a victim set appearing (medieval scribes and scholastics condemned as corrupt); the hybrid reading should land intermediate, splitting legitimacy by domain.',
    'If the siblings compute as expected, the kernel decomposition is confirmed and this file''s low-extraction rope profile is specific to the continuity reading rather than an artifact of the topic; if they converge on this profile, the reading distinction is doing less structural work than the contest suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame omega: this constraint is one reading of a contested kernel; sibling readings are separate files.').

omega_variable(
    organic_vs_schooled_transmission,
    'Was medieval Latin''s development genuinely organic (native-speaker transmission) or school-maintained (a learned second language in diglossia with the emerging Romance vernaculars)?',
    'Sociolinguistic and paleographic evidence on when Latin ceased to be natively acquired: acquisition patterns in Carolingian educational reforms, vernacular glossing density, Romance strata in inscriptions and charters.',
    'If transmission was substantially schooled rather than native, ''organic linguistic change'' needs qualification and the reading drifts toward the hybrid sibling; the legitimacy claim survives but its grounding shifts from natural growth to institutional maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organic_vs_schooled_transmission, empirical, 'Whether the continuity reading''s core ''organic'' premise describes native transmission or schooled maintenance.').

omega_variable(
    development_error_boundary,
    'Where, under this reading, does legitimate development end and individual error begin?',
    'Statistical analysis of variant distributions in manuscript traditions: systematic innovations shared across scribes and regions versus idiosyncratic lapses confined to single witnesses.',
    'If the boundary blurs in practice, the reading carries a hidden epistemic cost — editors under-correct genuine errors by invoking development — and effective extraction rises above the authored base; if the boundary is robust, the residual extraction stays negligible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_error_boundary, conceptual, 'Boundary between sanctioned innovation and unsanctioned error inside the continuity frame.').

omega_variable(
    professional_interest_contamination,
    'Does the reading''s coordination function partly serve medievalist professional self-interest — devaluing the corrective competence of rival specialists?',
    'Compare error rates and emendation quality in editions produced under continuity-framed versus normative-framed editorial protocols on the same corpora.',
    'If contamination is real, the theater_ratio understates performative maintenance and the classification trends toward tangled_rope with classicists as a mild victim seat; if not, the coordination function is clean.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_interest_contamination, empirical, 'Whether the reading''s low extraction conceals interest-driven coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__continuity_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(lati_tr_t0, observed).
narrative_ontology:measurement(lati_tr_t10, latin_correctness__continuity_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(lati_tr_t10, observed).
narrative_ontology:measurement(lati_tr_t20, latin_correctness__continuity_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(lati_tr_t20, observed).
narrative_ontology:measurement(lati_tr_t30, latin_correctness__continuity_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement_basis(lati_tr_t30, observed).
narrative_ontology:measurement(lati_tr_t40, latin_correctness__continuity_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement_basis(lati_tr_t40, observed).
narrative_ontology:measurement(lati_tr_t50, latin_correctness__continuity_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement_basis(lati_tr_t50, observed).
narrative_ontology:measurement(lati_tr_t60, latin_correctness__continuity_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement_basis(lati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(lati_be_t0, observed).
narrative_ontology:measurement(lati_be_t10, latin_correctness__continuity_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement_basis(lati_be_t10, observed).
narrative_ontology:measurement(lati_be_t20, latin_correctness__continuity_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement_basis(lati_be_t20, observed).
narrative_ontology:measurement(lati_be_t30, latin_correctness__continuity_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement_basis(lati_be_t30, observed).
narrative_ontology:measurement(lati_be_t40, latin_correctness__continuity_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(lati_be_t40, observed).
narrative_ontology:measurement(lati_be_t50, latin_correctness__continuity_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement_basis(lati_be_t50, observed).
narrative_ontology:measurement(lati_be_t60, latin_correctness__continuity_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement_basis(lati_be_t60, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(latin_correctness__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__continuity_reading, information_standard).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__rupture_reading).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Latin correctness' covers one thing in ordinary discourse but decomposes into three structurally distinct constraints — continuity_reading (this file), rupture_reading, and hybrid_reading — with different epsilon values, different beneficiary/victim structures, and therefore different classifications; per the epsilon-invariance principle they are authored as separate linked stories rather than one story with a measurement parameter. The rupture reading is historically upstream: humanist reconstruction of the fixed classical standard created the corrective norm to which the other two readings respond. This reading's success exerts downstream pressure on the hybrid reading, since a fully continuous tradition leaves the hybrid's domain-split with less work to do.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(latin_correctness__continuity_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
