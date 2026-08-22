% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__censorship_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__censorship_mechanism_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: article17_erasure_right__censorship_mechanism_reading
 *   human_readable: Article 17 Right to Erasure as Weaponized Speech Suppression
 *   domain: technology_governance/data_protection_law/competition_policy
 *
 * SUMMARY:
 *   Article 17 of the GDPR ('right to be forgotten') grants individuals the
 *   right to request erasure of personal data held by organizations, with
 *   narrow exceptions for public interest and press freedom. This constraint
 *   instantiates ONE reading of that contested kernel: the
 *   censorship_mechanism_reading. From this seat, Article 17 functions as a
 *   weaponized suppression tool, enabling strategic erasure requests that
 *   suppress factual speech about public figures and events, weaponize
 *   privacy rights against journalism and archival, and create a chilling
 *   effect on speech by shifting the burden of proof to defenders of public
 *   records. Bad-faith requesters (individuals and corporations with
 *   resources and legal leverage) benefit by erasing unflattering facts
 *   without contestation. Platforms enforce erasure as risk mitigation and
 *   service. Journalists, archivists, researchers, and the public lose access
 *   to searchable information. The kernel is contested — the
 *   privacy_fundamental_reading reads the same Article 17 as the codification
 *   of individual data sovereignty as a fundamental right; the
 *   competitive_moat_reading reads it as incumbent protection via compliance
 *   cost asymmetry. This constraint story focuses on the censorship function
 *   and its extraction from speech.
 *
 * KEY AGENTS:
 *   - strategic_erasure_requesters: powerful individuals and corporations filing erasure requests to suppress unflattering facts; benefit from the right without contesting truthfulness
 *   - platforms_enforcing_erasure: institutional agenda-setters that comply with erasure requests and benefit through regulatory risk reduction and service differentiation
 *   - journalists_archivists: organized payers who lose speech visibility and archival completeness; subject to chilling effects and fragmented records
 *   - historical_researchers: moderate-power payers who lose access to primary documents and historical facts; unable to verify causation when erasure removes inconvenient records
 *   - public_information_seekers: powerless trapped payers who lose access to factual information without transparency or recourse
 *   - eu_regulators: institutional agenda-setters interpreting and enforcing Article 17; their rulings systematically extend erasure applicability
 *   - legal_defense_actors: excluded stakeholders who could mount defenses but are structurally weakened by reversed burden of proof and regulatory fines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, 0.71).
domain_priors:suppression_score(article17_erasure_right__censorship_mechanism_reading, 0.79).
domain_priors:theater_ratio(article17_erasure_right__censorship_mechanism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__censorship_mechanism_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__censorship_mechanism_reading, "Article 17 Right to Erasure as Weaponized Speech Suppression").
narrative_ontology:topic_domain(article17_erasure_right__censorship_mechanism_reading, "technology_governance/data_protection_law/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__censorship_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__censorship_mechanism_reading, 'c84f65bf-0c85-4caa-9682-ed5e7ec89f01').
narrative_ontology:cs_kernel_codification('c84f65bf-0c85-4caa-9682-ed5e7ec89f01', fixed_text).
narrative_ontology:cs_authority_grounding('c84f65bf-0c85-4caa-9682-ed5e7ec89f01', lineage).
narrative_ontology:cs_interpretation_layer_present('c84f65bf-0c85-4caa-9682-ed5e7ec89f01').
narrative_ontology:cs_reading_relation('c84f65bf-0c85-4caa-9682-ed5e7ec89f01', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('c84f65bf-0c85-4caa-9682-ed5e7ec89f01', article17_erasure_right__competitive_moat_reading, influences).
narrative_ontology:cs_axiom('c84f65bf-0c85-4caa-9682-ed5e7ec89f01', foundational, erasure_weaponizable_against_speech).
narrative_ontology:cs_axiom_status(erasure_weaponizable_against_speech, holdable).
narrative_ontology:cs_axiom_grounding('c84f65bf-0c85-4caa-9682-ed5e7ec89f01', erasure_weaponizable_against_speech, empirically_contingent).
narrative_ontology:cs_axiom('c84f65bf-0c85-4caa-9682-ed5e7ec89f01', foundational, privacy_right_suppression_incompatible).
narrative_ontology:cs_axiom_status(privacy_right_suppression_incompatible, holdable).
narrative_ontology:cs_axiom_grounding('c84f65bf-0c85-4caa-9682-ed5e7ec89f01', privacy_right_suppression_incompatible, deontological).
narrative_ontology:cs_reference_frame('c84f65bf-0c85-4caa-9682-ed5e7ec89f01', privacy_protection_original_mandate).
narrative_ontology:cs_drift_state('c84f65bf-0c85-4caa-9682-ed5e7ec89f01', contemporary_weaponization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c84f65bf-0c85-4caa-9682-ed5e7ec89f01', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, strategic_erasure_requesters).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, platforms_enforcing_erasure).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, journalists_archivists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, historical_researchers).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, public_information_seekers).
narrative_ontology:constraint_vindicates(article17_erasure_right__censorship_mechanism_reading, privacy_as_fundamental_right_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and corporations with resources to hire legal counsel who file Article 17 erasure requests to remove unflattering, factual information about themselves from search results and archives. They benefit from the suppression of speech about them without having to contest its truthfulness or demonstrate actual harm — the right to be forgotten becomes a right to erase inconvenient public records. Their exit from this arrangement is not germane; they are the beneficiaries of its operation.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, strategic_erasure_requesters, beneficiary,
    powerful, biographical, arbitrage, global).

% Search engines and content platforms that implement Article 17 erasure. They benefit by: (1) reducing legal exposure and regulatory fines (compliance as risk mitigation), (2) shifting the burden of erasure decision-making to requesters (outsourcing content curation), (3) in some cases, capturing erasure as a service to powerful entities who can afford to make requests stick. They enforce the right by removing content and delisting URLs at requester demand, with minimal transparency about what was erased or why.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, platforms_enforcing_erasure, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__censorship_mechanism_reading, platforms_enforcing_erasure, agenda_setter).

% Professional information preservers who publish factual reporting and maintain historical archives. They pay by losing the ability to publish (content gets delisted or suppressed) and losing the ability to reference past events (archives become fragmented by erasure). They cannot easily exit because their work inherently depends on platforms' indexing and discoverability, and erasure requests often come from subjects they have covered, creating a chilling effect on future reporting.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, journalists_archivists, payer,
    organized, generational, constrained, global).

% Academic and independent researchers conducting historical analysis, biography, or social research. They pay by losing access to primary documents and public records that have been erased, fragmenting the historical record. Their ability to verify claims and trace causation in historical narrative is compromised when erasure removes inconvenient facts from searchable space.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, historical_researchers, payer,
    moderate, generational, constrained, national).

% The general public accessing information through search and platforms. They pay by losing access to factual information about public figures, corporations, and events — not because the information is false, but because the subject has the resources to request erasure. They have no transparency into what has been erased or why, and no practical recourse to challenge erasure decisions.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, public_information_seekers, payer,
    powerless, immediate, trapped, global).

% EU authorities (CJEU, national DPAs) that interpret and enforce Article 17. They set the rules for when erasure is 'necessary' and adjudicate conflicts between privacy and other rights. They are positioned as neutral arbiters but their rulings consistently extend erasure applicability, creating a structural bias toward suppression and away from the transparency needed to detect weaponization.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, eu_regulators, agenda_setter,
    institutional, generational, analytical, continental).

% Those who might mount a legal defense against erasure (the original publishers, platforms resisting erasure) are structurally weakened: platforms face regulatory fines for non-compliance and bear the burden of proving 'legitimate interest' in retention — a burden reversed from traditional defamation/privacy standards where the complainant must prove harm. Publishers cannot efficiently defend every piece of archival content when erasure comes through legal channels rather than the public sphere.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, legal_defense_actors, excluded,
    moderate, biographical, constrained, global).

% EU lawmakers who drafted GDPR Article 17 with the intent to protect privacy but did not anticipate or adequately guard against weaponization by powerful actors. They observe the constraint's operation but their oversight is limited by the speed of judicial interpretation outpacing legislative amendment.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, eu_legislative_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__censorship_mechanism_reading, strategic_erasure_requesters).
narrative_ontology:fixing_cost_class(article17_erasure_right__censorship_mechanism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article 17 coordinates a single framework for privacy protection: individuals can request erasure of personal data, platforms comply centrally rather than responding to litigation per-datum. It solves the problem of asymmetric data power by giving individuals a lever to control information about themselves.
% TRANSFER_FUNCTION: Moves the right to determine information visibility from publishers and platforms (who retain and index) to data subjects (who can demand erasure). The constraint transfers decisional power over public records from the epistemic commons (transparency, accountability, memory) to individual preference, with financial and reputational benefit flowing to those with resources to make erasure requests stick and to platforms that can use erasure as service differentiation.
% ABSENT_VOICES: Archivists and historians are structurally excluded from erasure proceedings — they have no standing to argue the public interest in retaining information, no participation in the legal process, no transparency into what has been erased. Publishers of original reporting are present only as respondents defending against erasure, not as coordinators of the system. The public whose information access is diminished has no voice.
% DISAPPEARANCE_RATIONALE: If Article 17 erasure enforcement vanished, platforms would cease removing content at individual request, archives would become complete again, journalistic records would not face post-hoc suppression, and the historical record would become searchable. The suppression function that rides on privacy justification would evaporate, and the asymmetry it creates would reverse — powerful actors would lose the lever to erase inconvenient facts.
% FOUNDING_PROBLEM: Large tech corporations retained personal data indefinitely, used it for profiling and targeting, and resisted individual requests to delete it. Privacy advocates sought a mechanism to give individuals control over data about themselves and limit corporate data accumulation.
% FOUNDING_PROBLEM_CORROBORATION: The GDPR Article 17 drafters and privacy advocates (EDPB, privacy NGOs) attest the problem is live — individuals lack meaningful control over corporate data. Data rights researchers and journalists (outside the regulatory/privacy advocacy sphere) attest the founding problem is substantially solved by technical means (data minimization, anonymization) and that Article 17 has metastasized into a suppression tool weaponized against speech; regulatory impact assessments from independent academic sources document the chilling effect on reporting and archival.
narrative_ontology:disappearance_verdict(article17_erasure_right__censorship_mechanism_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__censorship_mechanism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__censorship_mechanism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__censorship_mechanism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__censorship_mechanism_reading, 0.71, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__censorship_mechanism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article17_erasure_right__censorship_mechanism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.71) because the constraint transfers control over public information visibility from the epistemic commons to individual preference, and powerful actors use that transfer to erase inconvenient facts — extraction is the asymmetric benefit of erasure to those with resources. Suppression is higher still (0.79) because enforcement depends on active removal of content, legal threats against platforms, and chilling effects on journalists and archivists — without sustained suppressive force, the constraint would not persist. Theater ratio is moderate (0.42) because the privacy justification is real (individuals do deserve some control over personal data) but an increasing share of erasure activity defends the suppression function rather than protecting genuine privacy. Accessibility collapse is moderate-high (0.68) because journalists face a structured barrier (legal liability for archived content) and researchers lose access to indexed information; alternatives (offline archives, legal databases) are less searchable and less accessible to the public. Resistance is moderate (0.58) because journalist organizations, archivists, and data rights advocates actively contest erasure at institutional and legal levels, but they have fewer resources and less structural power than erasure requesters and platforms. The measurement series track the interval from early GDPR implementation (t=0, modest erasure practice) to contemporary widespread weaponization (t=25, entrenched suppression). Base extractiveness and suppression_requirement both rise monotonically, indicating that the constraint's extractive function has accumulated and enforcement intensity has hardened over time. Theater ratio rises, indicating that compliance rhetoric increasingly decouples from the actual function being enforced.
 *
 * PERSPECTIVAL GAP:
 *   From the strategic requester and platform seats, Article 17 is Rope: genuine coordination solving an asymmetry (individuals couldn't control corporate data, now they can). From the journalist and archivist seats, Article 17 is Snare: pure extraction of speech visibility justified by a privacy narrative that masks suppression. From the regulator seat, Article 17 is Mountain: a natural law following from fundamental rights doctrine, unchosen and inevitable. The engine computes these per-seat divergences from the directionality profile — the authored claimed_type (tangled_rope) is the reading's structural claim, not a consensus. Tangled Rope is appropriate here because the arrangement genuinely coordinates (solves the asymmetric data power problem) AND extracts asymmetrically (suppresses speech from those without resources to challenge erasure); both functions are active and both require enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic erasure requesters sit at the beneficiary end of directionality (d ≈ 0.0–0.15): they collect the benefit of suppression without bearing the constraint's cost; they have arbitrage exit (they can exit by not requesting erasure, but choosing not to is a preference, not a constraint). Platforms enforcing erasure sit at mixed beneficiary/agenda-setter position (d ≈ 0.2–0.35): they benefit through risk mitigation and service differentiation; they have mobile exit (they could refuse erasure but face regulatory penalties; the exit is not truly open). Journalists and archivists sit at the high-target end (d ≈ 0.8–0.95): they bear the cost of suppressed visibility and fragmented records; they have constrained exit (leaving platforms means losing their audience; they cannot exit the constraint without exiting the epistemic commons). Researchers sit at moderate-target (d ≈ 0.65–0.8): they lose access to primary documents; their exit is similarly constrained. The public sits at the trapped-target end (d ≈ 0.85–1.0): they lose information access with no voice or exit. This directionality profile drives the constraint's classification: platforms and requesters are coordinated beneficiaries (the coordination is the arrangement of erasure itself); journalists and archivists are victims of the same arrangement; the constraint requires active enforcement (platforms must delete, must refuse to index, must resist appeals) to hold; therefore Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protect individual privacy, give individuals control over personal data) is live and widely endorsed. But the founding problem (corporate data retention without consent) is substantially solved by technical means (data minimization, anonymization, regulatory data-retention limits) and by market competition (privacy-preserving business models). What persists and has expanded is the suppression function — erasure of factual speech about public figures and historical events. The constraint has metastasized beyond its original scope. Mandatrophy is NOT fully resolved (the privacy mandate still has constituency), but there is a mandate-function mismatch: the original mandate has diminished relevance, the constraint has found a new parasitic function (speech suppression), and that function is what now drives persistence and enforcement. The measurement series show this: theater ratio rising indicates increasing decoupling between privacy rhetoric and suppression practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privacy_vs_speech_kernel_reading,
    'Is Article 17 fundamentally an instantiation of privacy as a foundational right (privacy_fundamental_reading) or does it function as a speech suppression mechanism when weaponized by powerful actors (censorship_mechanism_reading)?',
    'Kernel contest — the two readings share the same text and institutional apparatus but derive incompatible normative conclusions. Empirical resolution comes via measurement of actual erasure patterns: if erasure is predominantly used by powerless individuals to remove non-consensual intimate images, the privacy reading holds; if erasure is predominantly used by powerful actors to remove factual reporting and archival content, the censorship reading holds.',
    'If the censorship reading is correct, Article 17 requires amendment to carve out public interest exceptions (news reporting, archival, historical research). If the privacy reading is correct, current Article 17 is appropriate and erosion is a betrayal of fundamental rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privacy_vs_speech_kernel_reading, conceptual, 'Whether Article 17''s core function is privacy protection or speech suppression — a reading-indexical question about the same constraint.').

omega_variable(
    good_faith_vs_weaponization,
    'Can Article 17 distinguish good-faith erasure requests (removing intimate images, financial data, health information) from bad-faith erasure requests (removing unflattering but truthful reporting)?',
    'Measurement of: (1) erasure request acceptance rates by category (intimate/financial/health vs. news/public figure), (2) legal challenges and their outcomes by requester power level, (3) rate of re-indexing and appeals, (4) longitudinal patterns in platform compliance.',
    'If bad-faith weaponization is substantial but undetectable at the point of decision, the constraint''s enforcement mechanism is structurally corrupted — suppression is occurring under privacy pretext. If good-faith and bad-faith requests are empirically separable, technical or legal modifications could preserve privacy while guarding against suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(good_faith_vs_weaponization, empirical, 'Whether the constraint can operationally distinguish legitimate from weaponized erasure.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (journalists avoiding coverage, archivists self-censoring to avoid legal liability) structural (external erasure threat) or internalized (adopting the premise that erasure is legitimate)?',
    'Comparative study of reporting patterns in EU jurisdictions with strong Article 17 enforcement vs. non-EU jurisdictions; analysis of chilling-effect mechanisms (does self-censorship persist after a journalist''s article is erased, or only while erasure threat is active?).',
    'If suppression is primarily structural, removing or limiting Article 17 would restore reporting rapidly. If internalized, journalists'' and archivists'' practices may remain cautious even if erasure enforcement eases, because the legitimacy of erasure as a tool has been normalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression operates through external threat or has become internalized practice.').

omega_variable(
    platform_compliance_vs_collusion,
    'Are platforms complying with Article 17 because they are legally bound and risk-averse, or because they actively benefit from erasure (service differentiation, complicity with powerful requesters, reduction of moderation burden)?',
    'Forensic analysis of platform erasure implementation: (1) transparency reports and erasure statistics, (2) patterns in which requests are granted vs. denied, (3) speed and cost of erasure vs. appeal process, (4) evidence of platform-requester relationships or commercial arrangements.',
    'If platforms are reluctant enforcers, regulation could reduce compliance incentives. If platforms actively benefit, they become co-conspirators in the suppression mechanism, and the constraint''s effective extraction is higher than the formal Article 17 authority suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_compliance_vs_collusion, empirical, 'Whether platforms are passive rule-followers or active beneficiaries in the erasure regime.').

omega_variable(
    kernel_reading_contest_framing,
    'This constraint is one reading of the article17_erasure_right kernel. Does instantiating it as censorship_mechanism_reading foreclose the privacy_fundamental_reading, or do both readings remain live despite their incompatibility?',
    'Hermeneutic analysis: the two readings share the kernel text and institutional apparatus but draw different normative conclusions. Foreclosure would require that adopting the censorship reading logically entails rejecting the privacy reading within any single coherent framework. Coexistence would require that different parties (privacy advocates, journalists, legal scholars) can simultaneously hold incompatible readings without logical contradiction in their respective frameworks.',
    'If readings foreclose, the kernel is unstable and will resolve toward one or the other. If readings coexist, the kernel is a live site of contestation and both readings will persist as institutional positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, conceptual, 'The structural relationship between this reading and its siblings in the kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__censorship_mechanism_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t3, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement_basis(arti_tr_t3, observed).
narrative_ontology:measurement(arti_tr_t6, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement_basis(arti_tr_t6, observed).
narrative_ontology:measurement(arti_tr_t12, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement_basis(arti_tr_t12, observed).
narrative_ontology:measurement(arti_tr_t18, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 18, 0.39).
narrative_ontology:measurement_basis(arti_tr_t18, observed).
narrative_ontology:measurement(arti_tr_t25, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(arti_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t3, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement_basis(arti_be_t3, observed).
narrative_ontology:measurement(arti_be_t6, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 6, 0.59).
narrative_ontology:measurement_basis(arti_be_t6, observed).
narrative_ontology:measurement(arti_be_t12, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement_basis(arti_be_t12, observed).
narrative_ontology:measurement(arti_be_t18, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 18, 0.69).
narrative_ontology:measurement_basis(arti_be_t18, observed).
narrative_ontology:measurement(arti_be_t25, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 25, 0.71).
narrative_ontology:measurement_basis(arti_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t3, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 3, 0.67).
narrative_ontology:measurement_basis(arti_su_t3, observed).
narrative_ontology:measurement(arti_su_t6, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 6, 0.71).
narrative_ontology:measurement_basis(arti_su_t6, observed).
narrative_ontology:measurement(arti_su_t12, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement_basis(arti_su_t12, observed).
narrative_ontology:measurement(arti_su_t18, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 18, 0.77).
narrative_ontology:measurement_basis(arti_su_t18, observed).
narrative_ontology:measurement(arti_su_t25, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement_basis(arti_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__censorship_mechanism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__censorship_mechanism_reading, 0.12).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__competitive_moat_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the article17_erasure_right kernel. The sibling readings (privacy_fundamental_reading and competitive_moat_reading) describe the same Article 17 institutional apparatus but derive different ε values and different beneficiary/victim structures. Each reading is a separate constraint story linked by network.affects_constraints. The kernel contest is a live hermeneutic dispute; no single reading forecloses the others. Decomposition follows the ε-invariance principle: reading the constraint through the privacy-fundamental lens gives low extraction (privacy protection); reading it through the suppression lens gives high extraction (speech suppression); these are not the same constraint viewed from different angles, they are genuinely different ε values entailed by incompatible interpretive premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, powerful, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
