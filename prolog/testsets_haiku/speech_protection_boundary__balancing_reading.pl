% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Case-by-Case Speech Balancing Under First Amendment
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The balancing reading of the First Amendment holds that speech protection
 *   is not categorical but context-determined: courts weigh First Amendment
 *   interests against competing constitutional values (equal protection,
 *   dignitary harm, public safety, educational mission) and demonstrated
 *   harms on a case-by-case basis. This reading has been the dominant
 *   American constitutional framework since the 1980s (post-Brandenburg,
 *   Rosenberger, Snyder v. Phelps era). It instantiates a constraint because
 *   it distributes interpretive authority to the judiciary, making the
 *   boundary between protected and unprotected speech unpredictable until
 *   courts decide. Beneficiaries are the judicial institutions that gain
 *   gatekeeper authority; victims are speakers denied ex-ante predictability
 *   and members of marginalized classes whose protection depends on
 *   case-by-case judicial favor rather than categorical rules. The constraint
 *   is claimed as tangled_rope (coordination of competing constitutional
 *   values plus extraction of interpretive authority) and the metrics reflect
 *   both the coordination function (real competing values, hence moderate
 *   theater ratio) and the extraction cost (high speaker uncertainty, hence
 *   moderate extractiveness).
 *
 * KEY AGENTS:
 *   - Judicial gatekeeper institutions (Federal and state courts interpreting First Amendment through balancing)
 *   - Speech subjects in marginalized classes (targets of coded speech, lack predictable protection)
 *   - Categorical speakers denied predictability (journalists, advocates, academics seeking clear ex-ante boundaries)
 *   - State government actors (regulators who gain authority under balancing to restrict speech causing demonstrated harm)
 *   - Absolutist advocates (excluded from balancing framework, treated as contestable policy preference)
 *   - Harm-centered advocates (seek protection based on dignity and group subordination, face unpredictable judicial weighting)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.58).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.41).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Case-by-Case Speech Balancing Under First Amendment").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '0a281726-a3ea-4347-8fb9-98a417e42eb4').
narrative_ontology:cs_kernel_codification('0a281726-a3ea-4347-8fb9-98a417e42eb4', fixed_text).
narrative_ontology:cs_authority_grounding('0a281726-a3ea-4347-8fb9-98a417e42eb4', lineage).
narrative_ontology:cs_interpretation_layer_present('0a281726-a3ea-4347-8fb9-98a417e42eb4').
narrative_ontology:cs_reading_relation('0a281726-a3ea-4347-8fb9-98a417e42eb4', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a281726-a3ea-4347-8fb9-98a417e42eb4', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('0a281726-a3ea-4347-8fb9-98a417e42eb4', foundational, competing_constitutional_values_are_coordinate).
narrative_ontology:cs_axiom_status(competing_constitutional_values_are_coordinate, holdable).
narrative_ontology:cs_axiom_grounding('0a281726-a3ea-4347-8fb9-98a417e42eb4', competing_constitutional_values_are_coordinate, deontological).
narrative_ontology:cs_axiom('0a281726-a3ea-4347-8fb9-98a417e42eb4', foundational, context_determines_boundary_legitimacy).
narrative_ontology:cs_axiom_status(context_determines_boundary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0a281726-a3ea-4347-8fb9-98a417e42eb4', context_determines_boundary_legitimacy, conventional).
narrative_ontology:cs_axiom('0a281726-a3ea-4347-8fb9-98a417e42eb4', secondary, judicial_discretion_enables_just_outcomes).
narrative_ontology:cs_axiom_status(judicial_discretion_enables_just_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('0a281726-a3ea-4347-8fb9-98a417e42eb4', judicial_discretion_enables_just_outcomes, instrumental).
narrative_ontology:cs_reference_frame('0a281726-a3ea-4347-8fb9-98a417e42eb4', competing_constitutional_values_equal_footing).
narrative_ontology:cs_drift_state('0a281726-a3ea-4347-8fb9-98a417e42eb4', contemporary_polarized_courts, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('0a281726-a3ea-4347-8fb9-98a417e42eb4', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, judicial_gatekeeper_institutions).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, speech_subjects_in_marginalized_classes).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, categorical_speakers_denied_predictability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, state_government_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal and state courts interpret the First Amendment through context-sensitive balancing, case-by-case. Judges weigh speech interests against government interests (safety, equality, dignity) and demonstrated harms. They retain discretion over which factors dominate in particular categories (student speech, workplace speech, speech targeting protected classes). This authority structure distributes the gatekeeper role across the judiciary rather than through categorical rules; individual judges apply standards that shift with fact patterns and legal evolution.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, judicial_gatekeeper_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Members of historically marginalized groups (racial minorities, religious minorities, LGBTQ+ persons, women) experience cumulative harm from speech targeting their group status, often coded in forms that courts have found difficult to reach under categorical speech doctrines. Balancing gives courts authority to recognize systemic harm and subordination, but also means their protection depends on case-by-case judicial discretion rather than predictable rules. They cannot exit the status that makes them targets; their recourse is petition to courts whose reasoning is unpredictable across jurisdictions and judges.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, speech_subjects_in_marginalized_classes, payer,
    powerless, biographical, trapped, national).

% Speakers who want clear ex-ante boundaries (journalists, academic institutions, civil rights advocates, political organizers) find their protections uncertain under balancing doctrine. A speaker cannot reliably predict whether speech will be protected or restricted until a court weighs the specific context, audience, harm evidence, and competing constitutional values. This creates chilling effects for speakers who cannot afford litigation or cannot tolerate uncertainty. Exit means self-censoring, relocating to jurisdictions with clearer rules, or accepting the cost of unpredictable enforcement.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, categorical_speakers_denied_predictability, payer,
    moderate, biographical, constrained, national).

% State and local governments gain authority to regulate speech in domains where balancing permits it: workplace safety, educational environments, counter-speech against public harm or equality violations. Balancing doctrine allows them to argue that speech causing demonstrated harm to protected groups or public welfare can be restricted proportionally. The courts remain the final arbiters, but balancing gives regulators leverage to justify restrictions that categorical rules might forbid.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, state_government_actors, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__balancing_reading, state_government_actors, agenda_setter).

% Advocates for near-absolute speech protection (libertarian organizations, some civil libertarians, classical First Amendment scholars) are excluded from the balancing framework itself: their core premise (that balancing itself betrays speech protection) is not a seat at the judicial weighing table but a structural critique of that table. They have standing to litigate and file amicus briefs, but the doctrine forecloses their position by treating it as a contestable policy preference, not as a constitutional mandate.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, absolutist_advocates, excluded,
    powerful, generational, constrained, national).

% Advocates centered on protection from dignity harm, group subordination, and systemic harassment find themselves in an uncomfortable position: balancing doctrine nominally permits courts to weigh these harms, but in practice courts have been cautious about restricting speech based on harm to speakers' dignity or equality standing, especially when speakers are powerful. They are excluded because the balancing weight assigned to harms they prioritize is unpredictable and often minimal in actual doctrine.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, harm_centered_advocates, excluded,
    organized, biographical, mobile, national).

% The courts function as the institutional interpreter of how balancing operates in concrete cases. Their role is not merely to apply a pre-given standard but to interpret the First Amendment itself through balancing, which means the judiciary's case-by-case evolution IS the constraint's evolution. No external authority can override this interpretive authority without constitutional amendment.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, judiciary_as_interpreter, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__balancing_reading, judicial_gatekeeper_institutions).
narrative_ontology:fixing_cost_class(speech_protection_boundary__balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a judicial doctrine that permits reconciliation of First Amendment speech protection with competing constitutional values (equal protection, dignitary harm prevention, government safety interests, educational mission) through context-sensitive judgment rather than per se categorical rules. This solves the coordination problem of protecting speech while also protecting equal standing, group dignity, and collective safety—commitments that abstract categorical rules cannot simultaneously honor.
% TRANSFER_FUNCTION: Moves interpretive authority from categorical speech boundaries (where speakers and regulators could know status ex ante) to case-by-case judicial discretion (where the boundary is context-dependent and known only post-hoc after judicial decision). This transfers predictability from speakers and regulators to courts: courts gain flexibility and authority; speakers lose ex-ante certainty about protection; regulators gain authority to argue for speech restrictions when courts weigh competing values in their favor.
% ABSENT_VOICES: Speakers without access to federal litigation (linguistically marginalized, economically precarious, those in hostile jurisdictions) are excluded from the process by which balancing doctrine is interpreted and applied. Non-institutional voices—marginalized speakers without organizational backing, absolutist advocates whose position is treated as contestable policy not constitutional mandate—are excluded from the judiciary's authoritative interpretation. The silenced are those whose harms are not recognized in balancing (e.g., group-targeted harm from coded speech) and those whose speech is deemed harmful but lack resources to litigate.
% DISAPPEARANCE_RATIONALE: If balancing doctrine disappeared, either categorical absolutism or harm-based categorical rules would dominate. Judicial authority would either collapse (under absolutism) or rigidify (under harm-based categorical boundaries). Speakers' legal certainty would either increase dramatically (absolutism) or be replaced by clear harm-thresholds (harm-limited). Which speakers are protected and which are vulnerable would reorganize: absolutism would protect speakers courts currently restrict; harm-limited would restrict speakers absolutism protects. The distribution of gatekeeper authority, the burden of uncertainty, and the victim/beneficiary sets would all shift.
% FOUNDING_PROBLEM: Categorical speech rules inherited from the mid-20th century (clear and present danger, Brandenburg imminent lawless action) treated speech protection as nearly absolute except for narrow, bright-line exceptions. These rules could not adequately address: (1) coded speech and systemic targeting that causes group subordination without meeting the Brandenburg test; (2) cumulative harm from multiple instances of speech that individually might not meet the exception threshold; (3) the conflict between speech protection and equal protection when speech targets protected classes; (4) context-dependent harm (the same words have different effects in a classroom, a workplace, a public forum, a marketplace). Balancing doctrine was adopted to permit courts to weigh these contextual factors rather than requiring categorical per se protection.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary's own case law (Rosenberger v. University of Virginia, R.A.V. v. City of St. Paul, Snyder v. Phelps, Brandenburg v. Ohio historical lineage) documents that courts encountered recurring situations where categorical rules either over-protected hate speech or under-protected speakers addressing legitimate harms. Civil rights scholars and advocates (outside the judiciary) testify that coded speech and group targeting are real phenomena categorical rules miss. Absolutist and libertarian scholars testify that balancing doctrine itself corrupts speech protection. Empirical researchers document both hate speech harms (supporting balancing accommodation of harm concerns) and chilling effects on legitimate speech (supporting absolutist concerns). No single corroborating voice outside constitutional law itself provides independent assessment; the corroboration is inter-sectoral (courts, advocacy, scholarship) but all within the legal/constitutional tradition.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at endpoint) because balancing extracts predictability from speakers while distributing authority to courts, but the extraction is not total: some speakers (those with resources and legal access, those aligned with dominant viewpoints) can litigate uncertainty into clarity. Suppression is moderate (0.41) because the constraint operates through judicial interpretation and discretion rather than through overt coercion; speakers can still speak, but face unpredictable legal consequences. Theater ratio is moderate-low (0.28) because the balancing function is genuine (courts really do weigh competing values and context matters), but an increasing share of the interpretive work goes into justifying outcomes that favor established speakers or powerful governments, which creates space for theater. The measurement series shows stability from year 10 onward: extractiveness and suppression plateau, theater stabilizes, indicating that the doctrine has reached an equilibrium where the distribution of predictability/unpredictability, authority/vulnerability, and protection/exposure are stable. The marginal increase early (years 0-10) reflects doctrinal hardening as courts refined balancing standards.
 *
 * PERSPECTIVAL GAP:
 *   The judicial gatekeepers see the balancing doctrine as a genuine coordination mechanism that reconciles competing constitutional values and permits context-sensitive justice. Categorical speakers and absolutist advocates see it as judicial usurpation of speech protection, an extraction of certainty in service of shifting political agendas. Speech subjects in marginalized classes face a split perspective: balancing nominally permits courts to recognize systemic harm, but in practice their protection is inconsistent and their vulnerability to coded speech persecution remains high. State regulators see balancing as authority to protect public welfare and equality; speakers and libertarians see it as state power to chill speech. The engine computes per-seat directionality and classification from this structural divergence; the authored claim (tangled_rope) reflects the structural asymmetry (coordination + extraction) rather than any seat's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Judicial gatekeepers have directionality near the beneficiary end (d ≈ 0.15-0.25) because they set the rules and maintain interpretive authority. Categorical speakers denied predictability sit closer to symmetric (d ≈ 0.45-0.55) because they face real costs (chilling effects, uncertainty) but retain legal standing and can litigate; their exit is constrained but not trapped. Speech subjects in marginalized classes sit closer to the target end (d ≈ 0.65-0.75) because their protection is discretionary, their status makes them perpetual targets, and their exit options are trapped (they cannot leave their marginalized status). State government actors sit near beneficiary (d ≈ 0.20-0.30) because balancing gives them authority to justify speech restrictions in service of government interests (safety, equality, educational mission), though judicial review limits their discretion. The overrides needed: none declared, as the structural derivation from beneficiary/victim + power + exit captures the intended directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy mislabeling by declaring it tangled_rope (not snare or pure rope). Snare classification would require concealing the genuine coordination function (balancing really does reconcile competing constitutional values—this is not mere cover). Pure rope classification would ignore the extraction of predictability and the gatekeeper authority. Tangled_rope captures both: courts genuinely coordinate competing values (the coordination function is real) AND extract interpretive authority from speakers and limit protection predictability (the extraction is real and active enforcement is required to maintain the gatekeeper role against competing readings of the First Amendment). Mandatrophy would arise only if the constraint were classified purely as coordination (rope) while ignoring the speaker uncertainty costs, or purely as extraction (snare) while ignoring the real competing constitutional values courts balance. The measurement series supports tangled_rope: extractiveness and suppression are moderate, not minimal (rope) or severe (snare); theater ratio is moderate, not high (piton).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_weight_hierarchy_ambiguity,
    'What determines which constitutional values courts prioritize when they conflict? Is there a stable hierarchy (speech protection > other values; or equality > speech)? Or is the hierarchy itself context-dependent?',
    'Meta-empirical study of appellate opinions: document which values were weighed most heavily in similar fact patterns across time and jurisdiction. If weights are stable, an implicit hierarchy exists; if weights vary unpredictably, hierarchy is itself context-dependent.',
    'If hierarchy is stable, balancing doctrine has more predictability than it appears; if hierarchy is unstable or context-dependent, the constraint extracts more uncertainty from speakers than this reading acknowledges. This determines whether balancing is genuine coordination or theater-masked extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balancing_weight_hierarchy_ambiguity, empirical, 'Whether judicial balancing employs a stable value hierarchy or context-dependent weighting.').

omega_variable(
    marginalized_speaker_protection_gap,
    'Do speech subjects in marginalized classes receive materially different protection under balancing compared to speakers in dominant social positions? Does their victimhood depend on case-by-case judicial luck rather than doctrine?',
    'Empirical analysis of First Amendment case outcomes by defendant/speaker status: compare protection rates for coded speech targeting protected classes vs. speech by protected-class members; compare outcomes across judicial circuits and ideological court compositions.',
    'If marginalized speakers receive systematically lower protection despite balancing''s nominally contextual approach, the doctrine extracts disparate harm from victims while claiming to balance harms. This would shift the constraint''s type toward snare (for marginalized speakers) and away from tangled_rope (which requires real coordination across seats).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marginalized_speaker_protection_gap, empirical, 'Whether balancing produces disparate protection outcomes by speaker/target status.').

omega_variable(
    predictability_chilling_tradeoff,
    'Does the loss of ex-ante predictability under balancing produce chilling effects on legitimate speech that exceed the coordination gains from flexible context-sensitivity? What is the chilling-effect threshold where balancing becomes extractive net of coordination benefit?',
    'Empirical study: survey speakers'' (journalists, academics, advocates, activists) reported self-censorship and litigation-avoidance under balancing vs. categorical doctrine. Compare jurisdictions with varying degrees of balancing clarity. Model the relationship between predictability loss and social participation in speech.',
    'If chilling effects are substantial and concentrated in speakers with legitimate speech interests (not hate speakers), the extraction cost of balancing outweighs its coordination benefit, shifting toward snare classification. If chilling effects are minimal or concentrated in harmful speech, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predictability_chilling_tradeoff, empirical, 'Whether balancing''s predictability loss produces net chilling effects exceeding coordination gains.').

omega_variable(
    reading_stability_in_doctrine,
    'Is the balancing reading an entrenched judicial doctrine or a contingent coalition subject to reversal or replacement by absolutist or harm-limited readings?',
    'Doctrinal history and institutional-change analysis: track changes in Supreme Court composition, First Amendment doctrine evolution, and evidence of live contestation from absolutist and harm-centered advocates in current litigation.',
    'If balancing is contingent, it is a constraint whose existence depends on current judicial composition and may not persist; if entrenched, it is a stable element of the legal order. This affects whether the constraint is durable or transient (classification toward scaffold if transient, piton if degraded, tangled_rope if stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_stability_in_doctrine, conceptual, 'Whether balancing doctrine is entrenched or contingent on judicial composition.').

omega_variable(
    kernel_reading_committer_framing_risk,
    'Is the balancing reading''s framing as a genuine constitutional reconciliation (competing values doctrine) or as a compromise doctrine that privileges judicial power over speech protection? Does the answer depend on whether one prioritizes speech as primary or equality/safety as coordinate?',
    'Committer-axis documentation: the framing risk is not empirical but conceptual—different traditions (libertarian, progressive, institutional) arrive at different assessments of whether balancing doctrine genuinely coordinates or privileges power. Resolution requires stating which tradition''s frame is adopted, not more evidence.',
    'If balancing is read through a libertarian frame, it appears as judicial usurpation extracting from speakers; through a progressive frame, it appears as necessary coordination of competing values; through an institutional frame, it appears as legitimate interpretation. This reading (balancing) is authored under the institutional frame (courts as legitimate interpreters); an alternative reading would author the same constraint under a libertarian or progressive frame and would arrive at different ε and classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_framing_risk, conceptual, 'Whether balancing doctrine is a genuine value coordination or a frame-dependent framing of judicial power.').

omega_variable(
    categorical_boundary_collapse_mechanism,
    'What is the structural mechanism by which balancing''s case-by-case approach collapses categorical boundaries that absolutism and harm-limiting readings try to maintain? Is this mechanism beneficial (flexible justice) or extractive (incoherent doctrine)?',
    'Doctrinal analysis: trace how balancing doctrine handles speech that falls outside categorical boundaries (coded speech, systemic targeting, ambiguous harmful intent). Document whether collapse serves marginalized speakers, established speakers, or primarily empowers courts.',
    'If collapse serves marginalized speakers by permitting recognition of systemic harm, it is a coordination benefit; if it primarily serves courts (allowing case-by-case discretion and authority-consolidation), it is extraction. If it serves established interests in unpredictable ways, it is theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_boundary_collapse_mechanism, empirical, 'Whether categorical boundary collapse under balancing serves coordination or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(spee_tr_t5, speech_protection_boundary__balancing_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(spee_tr_t10, speech_protection_boundary__balancing_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(spee_tr_t20, speech_protection_boundary__balancing_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(spee_tr_t30, speech_protection_boundary__balancing_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__balancing_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(spee_be_t5, speech_protection_boundary__balancing_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(spee_be_t10, speech_protection_boundary__balancing_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(spee_be_t20, speech_protection_boundary__balancing_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(spee_be_t30, speech_protection_boundary__balancing_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__balancing_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(spee_su_t5, speech_protection_boundary__balancing_reading, suppression_requirement, 5, 0.39).
narrative_ontology:measurement(spee_su_t10, speech_protection_boundary__balancing_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(spee_su_t20, speech_protection_boundary__balancing_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(spee_su_t30, speech_protection_boundary__balancing_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__balancing_reading, suppression_requirement, 40, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__balancing_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__harm_limited_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel decomposes into three structurally distinct constraints corresponding to three live readings in American constitutional law. The balancing reading instantiated here differs from siblings in value hierarchy, predictability structure, and victim/beneficiary sets. The absolutist reading treats the boundary as categorical (low extractiveness, high predictability, no structural victims—Mountain from speaker seats). The harm_limited reading treats the boundary as harm-responsive (high extractiveness for speech that causes proven harm, high predictability via harm test, no unpredictability cost—Snare or Tangled Rope depending on harm threshold specificity). These three constraints share a kernel (the First Amendment text) and compete via reading conflict; each reading produces different ε and classification. Balancing is authored as moderate extractiveness (0.58) and Tangled Rope because the doctrine both coordinates competing values and extracts interpretive authority/unpredictability. Decomposition follows ε-invariance (DP-001): each reading's referent is the standing arrangement under that reading's own assessment; balancing's referent is the status quo doctrine, assessed as extracting unpredictability from speakers and coordinating constitutional values. No measurement basis is Goodhart (no single ε that works across all three readings); instead each reading authors its own ε-invariant picture and links to siblings via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
