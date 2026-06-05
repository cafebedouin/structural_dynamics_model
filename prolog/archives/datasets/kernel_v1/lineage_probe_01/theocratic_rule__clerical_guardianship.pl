% ============================================================================
% CONSTRAINT STORY: theocratic_rule__clerical_guardianship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_theocratic_rule__clerical_guardianship, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: theocratic_rule__clerical_guardianship
 *   human_readable: Clerical Guardianship: Theocratic Rule as Religious Trusteeship
 *   domain: political/comparative_government
 *
 * SUMMARY:
 *   Velayat-e faqih (Rule of the Jurist) emerged in twentieth-century Islamic
 *   jurisprudence as a theory of theocratic governance: a qualified jurist,
 *   through mastery of Islamic law, temporarily rules a Muslim society
 *   pending the return of the Hidden Imam or the awaited restoration of
 *   perfect Islamic order. This reading of theocracy presents clerical
 *   guardianship as a trusteeship — the faqih administers on behalf of the
 *   religious community and the divine law, not on behalf of personal or
 *   institutional aggrandizement. This constraint story generates the
 *   clerical_guardianship reading of the theocratic_rule kernel, modeling how
 *   this specific juridical form produces structural extraction, suppression,
 *   and contested legitimacy. The constraint is tangled_rope at the core
 *   level (genuine coordination function of translating religious law into
 *   state action, combined with extraction of secular-republican authority
 *   and suppression of alternative Islamic governance models). However, it
 *   instantiates all six types from different perspectives: snare for
 *   republican institutions and suppressed secular claims; rope for the
 *   clerical estate; piton for opposition (who see it as theater-dependent
 *   performance); mountain for the analytical observer who risks naturalizing
 *   it as necessary from Islamic premises; identity_locked for reform-minded
 *   clerical intellectuals whose professional identity prevents exit;
 *   constrained for the professional bureaucracy. The theater ratio (0.58
 *   rising to 0.65) reflects that guardianship increasingly relies on
 *   invocations of Islamic authority, revolutionary principle, and
 *   theological legitimacy — performative mechanisms that mask the actual
 *   extraction of power toward the clerical estate. The suppression metric
 *   (0.72) captures the structural foreclosure of alternative Islamic
 *   governance frameworks and the delegitimization of secular-republican
 *   discourse. The extractiveness metric (0.68) reflects that the faqih and
 *   clerical estate extract significant state authority, resource control,
 *   and institutional power, legitimated through trusteeship framing.
 *
 * KEY AGENTS:
 *   - Guardian Jurist (Velayat-e Faqih): Institutional beneficiary with arbitrage options — holds supreme authority, commands security apparatus, adjudicates all state decisions through religious interpretation. Experiences constraint as pure coordination (Rope).
 *   - Clerical Estate: Institutional beneficiary — religious scholars, seminary network, Islamic jurists who benefit from guardianship's elevation of clerical authority. Share extraction benefit with the faqih; see constraint as coordination of Islamic law application.
 *   - Republican Institutions (Parliament, Cabinet, Judiciary): Powerless/trapped institutional victims — formally exist but subordinated to guardianship veto. Experience maximum extraction (Snare) — formal roles stripped of autonomous authority.
 *   - Secular Sovereignty Claim (Democratic Legitimacy): Structurally suppressed victim — republican and democratic frameworks are illegitimate within guardianship logic. Cannot be voiced as alternative; maximum suppression + extraction of legitimacy.
 *   - Reform-Minded Clerical Intellectuals: Moderate/identity-locked victims — structurally mobile (can leave Iran) but identity-fused with clerical vocation. Cannot exit without abandoning selfhood constituted through Islamic jurisprudence. Extraction mechanism is cognitive: clerical framework prevents recognition of exit as possible.
 *   - Professional Bureaucracy (State Administrators, Judges, Security Officials): Moderate/constrained — benefit from stable hierarchical authority (faqih resolves disputes) but constrained by veto power. Mixed coordination + extraction experience (Tangled Rope).
 *   - Opposition and Civil Society: Organized/constrained — perceive guardianship as theater-dependent (Piton). Strategy focuses on exposing performative legitimation rather than offering alternative governance model.
 *   - Analytical Observer (Civilizational/Universal): Risks seeing Islamic jurisprudence as naturally producing guardianship (Mountain). False summit candidate — the analytical observer's instruments cannot detect that guardianship is a choice among Islamic alternatives, not a necessary derivation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(theocratic_rule__clerical_guardianship, 0.68).
domain_priors:suppression_score(theocratic_rule__clerical_guardianship, 0.72).
domain_priors:theater_ratio(theocratic_rule__clerical_guardianship, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(theocratic_rule__clerical_guardianship, extractiveness, 0.68).
narrative_ontology:constraint_metric(theocratic_rule__clerical_guardianship, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(theocratic_rule__clerical_guardianship, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(theocratic_rule__clerical_guardianship, tangled_rope).
narrative_ontology:human_readable(theocratic_rule__clerical_guardianship, "Clerical Guardianship: Theocratic Rule as Religious Trusteeship").
narrative_ontology:topic_domain(theocratic_rule__clerical_guardianship, "political/comparative_government").

domain_priors:requires_active_enforcement(theocratic_rule__clerical_guardianship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(theocratic_rule__clerical_guardianship, '33f48544-ce02-40bc-99fc-c3fdecc2ff6a').
narrative_ontology:cs_kernel_codification('33f48544-ce02-40bc-99fc-c3fdecc2ff6a', formalized).
narrative_ontology:cs_authority_grounding('33f48544-ce02-40bc-99fc-c3fdecc2ff6a', lineage).
narrative_ontology:cs_interpretation_layer_present('33f48544-ce02-40bc-99fc-c3fdecc2ff6a').
narrative_ontology:cs_reading_relation('33f48544-ce02-40bc-99fc-c3fdecc2ff6a', theocratic_rule__divine_kingship, coexists_with).
narrative_ontology:cs_reading_relation('33f48544-ce02-40bc-99fc-c3fdecc2ff6a', theocratic_rule__scriptural_legalism, influences).
narrative_ontology:cs_axiom('33f48544-ce02-40bc-99fc-c3fdecc2ff6a', foundational, jurist_as_temporary_trustee).
narrative_ontology:cs_axiom_status(jurist_as_temporary_trustee, holdable).
narrative_ontology:cs_axiom_grounding('33f48544-ce02-40bc-99fc-c3fdecc2ff6a', jurist_as_temporary_trustee, theological).
narrative_ontology:cs_axiom('33f48544-ce02-40bc-99fc-c3fdecc2ff6a', secondary, ijtihad_is_judicial_authority).
narrative_ontology:cs_axiom_status(ijtihad_is_judicial_authority, holdable).
narrative_ontology:cs_axiom_grounding('33f48544-ce02-40bc-99fc-c3fdecc2ff6a', ijtihad_is_judicial_authority, deontological).
narrative_ontology:cs_reference_frame('33f48544-ce02-40bc-99fc-c3fdecc2ff6a', divine_law_as_governance_source).
narrative_ontology:cs_drift_state('33f48544-ce02-40bc-99fc-c3fdecc2ff6a', contemporary_nation_state_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('33f48544-ce02-40bc-99fc-c3fdecc2ff6a', '').
narrative_ontology:cs_kernel_id(theocratic_rule__clerical_guardianship, theocratic_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(theocratic_rule__clerical_guardianship, guardian_jurist).
narrative_ontology:constraint_beneficiary(theocratic_rule__clerical_guardianship, clerical_estate).
narrative_ontology:constraint_victim(theocratic_rule__clerical_guardianship, republican_institutions).
narrative_ontology:constraint_victim(theocratic_rule__clerical_guardianship, secular_sovereignty_claim).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REPUBLICAN INSTITUTION (SNARE) — The parliament, cabinet, judiciary operating under velayat-e faqih cannot exit the guardianship framework; their authority is conditioned on the faqih's consent. They experience this as pure extraction: formal roles with subordinated decision power. High suppression (alternatives are foreclosed); high extraction (authority flows to the guardian); no genuine coordination benefit. Trapped agents perceiving an immutable ceiling.
constraint_indexing:constraint_classification(theocratic_rule__clerical_guardianship, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SECULAR SOVEREIGNTY CLAIM (SNARE) — Republican legitimacy (popular sovereignty, legislative supremacy) is structurally suppressed under clerical guardianship. The secular claim cannot be voiced as an alternative framework; making it explicit invokes sanctions. Pure extraction of sovereign authority toward the religious estate; maximum suppression (no legitimate alternative discourse). Trapped — no exit from the constraint without renouncing the entire secular-republican frame.
constraint_indexing:constraint_classification(theocratic_rule__clerical_guardianship, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: GUARDIAN JURIST / CLERICAL ESTATE (ROPE) — Benefits from the guardianship structure: legitimate authority, resource concentration, control over state violence and law-making. Experiences the constraint as pure coordination: the faqih coordinates the state's action on religious principles. Low extraction (they are the beneficiary); no suppression (they can exit or modify the rules). Institutional power with arbitrage options. The trusteeship framing is genuine from this perspective — they solve the coordination problem of translating religious law into state action.
constraint_indexing:constraint_classification(theocratic_rule__clerical_guardianship, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROFESSIONAL BUREAUCRACY (TANGLED ROPE) — State administrators, judges, security officials operate under guardianship but also participate in coordinating state function. They benefit from stable hierarchical authority (the faqih resolves disputes, clarifies policy) but are constrained by the guardianship's veto power. Mixed experience: genuine coordination benefit + extraction of autonomous decision-making. Constrained exit — they could defect but lose career, security clearance, family connections. Moderate power with limited options.
constraint_indexing:constraint_classification(theocratic_rule__clerical_guardianship, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM-MINDED CLERICAL INTELLECTUAL (SNARE) — Structurally mobile (can leave Iran, publish abroad, teach) but identity-fused with clerical vocation and Islamic jurisprudence. Cannot exit the clerical identity without abandoning the intellectual framework that constitutes their selfhood. Trapped by identity lock, not by material barriers. Experiences the guardianship as extraction: their alternative readings (democratic guardianship, constrained clerical authority) are delegitimized; their professional identity is weaponized against their own reform proposals. The extraction mechanism is cognitive — the clerical framework itself prevents seeing exit as possible.
constraint_indexing:constraint_classification(theocratic_rule__clerical_guardianship, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 6: OPPOSITION AND CIVIL SOCIETY (PITON) — Organized resistance (diaspora networks, student movements, women's rights organizations) exists outside and within the guardianship structure. They perceive it as largely performative: the guardianship persists through theatrical legitimation (invocations of Islamic law, appeals to revolutionary principle) rather than through genuine coordination function. Opposition strategy focuses on exposing the theater — showing that guardianship is power masquerading as trusteeship. Theater ratio high (regime relies on narrative legitimacy); constraint persists through inertia and enforcement of the performative narrative.
constraint_indexing:constraint_classification(theocratic_rule__clerical_guardianship, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal analytical position, Islamic jurisprudential tradition treats fiqh (jurisprudence) as derived from immutable sources (Quran, Sunnah, consensus, analogy). The guardian jurist's role emerges as a natural consequence of fiqh's logic: someone must interpret and apply the immutable law. From this view, guardianship is not a contingent political choice but a structural necessity following from the Islamic epistemic commitments. The analytical observer risks seeing this as an unchangeable feature of Islamic governance. However, structural data (beneficiaries, victims, enforcement machinery) reveals this as a false summit — the 'natural' derivation from Islamic law is actually a contested reading of that law, weaponized to suppress alternative jurisprudential and political positions.
constraint_indexing:constraint_classification(theocratic_rule__clerical_guardianship, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(theocratic_rule__clerical_guardianship_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(theocratic_rule__clerical_guardianship, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(theocratic_rule__clerical_guardianship, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(theocratic_rule__clerical_guardianship, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(theocratic_rule__clerical_guardianship, TR),
    TR >= 0.70.

:- end_tests(theocratic_rule__clerical_guardianship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate. The faqih and clerical estate extract significant authority, resource control, and institutional power from the republican institutions they formally coordinate. The extraction is not maximal (0.90+) because genuine coordination function exists — the faqih does solve real problems of Islamic law interpretation and state coherence. The beneficiaries (clerical estate) experience this as coordination and trust administration, not extraction. But from the victims' perspectives (secular-republican institutions, democratic legitimacy), the extraction is severe. The measurement trajectory (0.58 → 0.68 over 15 years) shows rising extraction as the clerical estate consolidates control and suppresses competing institutional centers. Suppression (0.72): Moderate-high. The guardianship structure actively forecloses alternative governance frameworks (secular democracy, shura-based councils, constrained clerical authority) and delegitimizes secular discourse. However, suppression is not absolute (0.90+) — underground opposition exists, diaspora networks articulate alternatives, and even within clerical institutions reform proposals emerge. The suppression trajectory (0.65 → 0.72) reflects increasing enforcement machinery and tighter control over institutional dissent. Theater ratio (0.58 rising to 0.65): Moderate-high. The guardianship increasingly relies on invocations of Islamic authority, revolutionary legitimacy, and theological necessity to maintain its extraction. The theater is not dominant (as it would be for a pure Piton), but it occupies a substantial portion of the constraint's operation. The rise over time reflects that as material integration of clerical authority deepens, the regime depends more heavily on narrative legitimation and theatrical displays of Islamic piety and constitutional procedure. The mechanics: as extraction accumulates, beneficiaries must invest in theater to prevent victims from recognizing the constraint and organizing resistance.
 *
 * PERSPECTIVAL GAP:
 *   The clerical_guardianship constraint exhibits maximum perspectival divergence. The clerical estate sees it as Rope (pure coordination of Islamic law application with no extraction). Republican institutions and suppressed secular claims see it as Snare (pure extraction with no coordination benefit). The professional bureaucracy sees mixed Tangled Rope (benefits from stable authority but constrained by veto). The reform-minded cleric sees it as Snare with identity-lock (structurally mobile but unable to exit because their selfhood is constituted through clerical identity). The opposition sees it as Piton (theater-dependent performance maintained by inertia and enforcement, not by functional necessity). The civilizational analytical observer risks seeing it as Mountain (natural from Islamic jurisprudence) — but structural data reveals this as false summit, naturalization of a contested reading. The perspectival gaps reveal competing readings of Islamic jurisprudence itself: does fiqh naturally produce guardianship, or is guardianship one choice among alternatives? The committer-frame answer is: that question IS the kernel dispute, and clerical_guardianship is one answer to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d-value) is derived from each perspective's structural position: power level, exit options, and relationship to the extraction flow. Guardian jurist (institutional/arbitrage beneficiary): d ≈ 0.10 (full beneficiary with exit options — can modify or exit rules). Republican institutions (powerless/trapped victims): d ≈ 0.92 (maximum target, no exit). Secular sovereignty claim (powerless/trapped, but a frame not an agent): d ≈ 0.95 (completely suppressed). Professional bureaucracy (moderate/constrained, mixed beneficiary-victim): d ≈ 0.55 (symmetric — benefits from coordination, harmed by veto). Reform-minded cleric (moderate/identity-locked, nominally beneficiary but practically victim): d ≈ 0.75 (high target because identity-lock prevents recognizing their structural mobility; the identity-lock mechanism itself is extraction). Opposition (organized/constrained, victim): d ≈ 0.60 (can organize and articulate alternatives, but suppression and enforcement create barriers). Analytical observer (analytical/analytical): d ≈ 0.73 (canonical fallback, reflecting the observer's structural position outside the system). Each d-value feeds the sigmoid f(d) producing the effective extractiveness chi = ε × f(d) × σ(S) experienced by that agent. The clerical beneficiary experiences negative chi (actually experiences subsidy and authority gain); the trapped victim experiences maximum chi; the identity-locked cleric experiences chi amplified by the fact that their framing prevents recognizing they could exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The clerical_guardianship constraint resolves the mandatrophy — the tension between genuine coordination and extraction — by showing that the same mechanism can be both: the faqih genuinely coordinates Islamic law application (coordination function) while simultaneously extracting authority from republican institutions (extraction function). Both are real. The clerical beneficiary experiences the coordination; the republican victim experiences the extraction. The mandatrophy is not 'which is it?' but 'from which perspective?' The tangled_rope classification captures this: the constraint has a genuine coordination kernel (translating religious law into state action) combined with asymmetric extraction (clerical authority dominates; republican authority is subordinated). The false summit risk is the analytical observer's naturalization of this structure as 'necessary from Islamic law.' The structural data (beneficiaries, victims, suppression machinery) reveals that clerical guardianship is a choice among Islamic alternatives, not a necessary derivation. Alternative Islamic governance frameworks exist (shura-based councils, constrained clerical authority, jurist consultation models); they are suppressed through political and theological means, not foreclosed by logic. The mandatrophy resolves: guardianship appears as mountain-like necessity because the suppression and extraction mechanisms are so effective that alternatives become invisible. The false summit detection reveals the construction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiqh_interpretation_boundary,
    'Is the guardian jurist''s authority a necessary derivation from Islamic jurisprudential principles, or a contingent reading that competes with alternative jurisprudential frameworks?',
    'Historical and textual analysis of competing Islamic jurisprudential schools; examination of classical and contemporary fiqh literature for alternative frameworks of governance authority; comparison of velayat-e faqih to other Islamic governance models (jurist consultation, council of scholars, shura-based authority)',
    'If necessary derivation: guardianship appears mountain-like (natural from Islamic premises). If contingent reading: guardianship is a choice among live alternatives within Islamic tradition, revealing the false summit — naturalization of a political decision. Reclassification would move from mountain to tangled_rope or snare depending on how alternative readings are suppressed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiqh_interpretation_boundary, empirical, 'Whether clerical guardianship is necessary or contingent within Islamic jurisprudence').

omega_variable(
    trusteeship_versus_extraction_legitimacy,
    'Does the guardian jurist genuinely coordinate religious law into state action (trusteeship function), or does the trusteeship framing mask extraction of state power toward clerical institutional interests?',
    'Analysis of decisions made under guardianship: Do they align with theological jurisprudential reasoning, or with clerical institutional interests when these diverge? Historical tracking of when the faqih overrode his own theological precedents to maintain power or clerical privilege. Comparison of stated jurisprudential principles to actual enforcement patterns.',
    'If genuine coordination: guardianship is legitimately Rope from clerical perspective (solving a real coordination problem). If institutional extraction: it is Snare masked by theological rhetoric. The classification would shift; more importantly, the false summit becomes visible — what appears as ''natural law'' implementation is actually institutional power accumulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trusteeship_versus_extraction_legitimacy, empirical, 'Whether trusteeship framing reflects genuine coordination or masks institutional extraction').

omega_variable(
    alternative_islamic_governance_suppression,
    'Are alternative Islamic governance frameworks (shura-based councils, constrained clerical authority, jurist consultation models) actively suppressed, passively excluded, or genuinely incompatible with Islamic jurisprudence?',
    'Examination of state response to internal Islamic arguments for alternative governance; analysis of whether dissenters are prosecuted for heresy or for political opposition; study of historical periods when alternative frameworks were entertained within Islamic societies; assessment of whether the suppression is theological or political.',
    'If actively suppressed: high suppression metric is justified (0.72). Clerical guardianship maintains itself through force, not consent. If passively excluded: suppression may be lower; constraint could be Rope rather than Tangled Rope from some perspectives. If genuinely incompatible: mountain-like immutability follows from Islamic premises (but this contradicts omega 1).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_islamic_governance_suppression, empirical, 'Whether alternative Islamic governance models are suppressed, excluded, or incompatible').

omega_variable(
    reading_versus_kernel_distinction,
    'Is velayat-e faqih (Rule of the Jurist) present in the Islamic jurisprudential tradition as a core principle, or did it emerge as a twentieth-century reading of classical fiqh texts?',
    'Textual history of the velayat-e faqih concept: examination of Khomeini''s 1970 lectures introducing it compared to classical Islamic governance texts; analysis of whether classical Islamic law contained the concept or whether Khomeini innovated it; assessment of contemporary Islamic scholars'' views on whether it is extracted from tradition or constructed by Khomeini.',
    'If present in tradition: reading is retrieving an ancient principle (kernelized). If twentieth-century innovation: reading is constructing a new principle by reinterpreting texts (committing the kernel). This affects how we understand the false summit — is guardianship presented as ancient law (mountain) or as modern innovation? The answer shapes whether beneficiaries are accused of hiding innovation or hiding selection from alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_kernel_distinction, empirical, 'Whether velayat-e faqih is classical Islamic principle or twentieth-century reading').

omega_variable(
    identity_locked_versus_constrained_boundary,
    'For clerical intellectuals and religious professionals who dissent from guardianship, is their inability to exit primarily due to material constraints (career loss, security threat, physical confinement) or cognitive identity-fusion with the clerical vocation?',
    'Comparative analysis of exits: clerical defectors who leave Iran vs those who stay and conform vs those who stay and resist internally. Assessment of whether the binding mechanism is threat/loss of status or internalization of clerical identity as non-negotiable. Follow-up study of post-exit identity integration: do defectors rebuild clerical identity in exile, or does the identity dissolve once material constraints are removed?',
    'If primarily constrained (material barriers): exit_options should be ''constrained'' for clerical dissenters. If primarily identity-locked (cognitive fusion): exit_options should be ''identity_locked''. This changes the classification of the reform-minded intellectual perspective from snare (identity-locked) to tangled_rope (constrained). The distinction reveals whether the binding is internal or external — critical for understanding reformation possibilities within clerical institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_versus_constrained_boundary, empirical, 'Whether clerical dissent is constrained by material barriers or identity-fusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(theocratic_rule__clerical_guardianship, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theo_tr_t0, theocratic_rule__clerical_guardianship, theater_ratio, 0, 0.42).
narrative_ontology:measurement(theo_tr_t5, theocratic_rule__clerical_guardianship, theater_ratio, 5, 0.5).
narrative_ontology:measurement(theo_tr_t10, theocratic_rule__clerical_guardianship, theater_ratio, 10, 0.58).
narrative_ontology:measurement(theo_tr_t15, theocratic_rule__clerical_guardianship, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(theo_be_t0, theocratic_rule__clerical_guardianship, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(theo_be_t5, theocratic_rule__clerical_guardianship, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(theo_be_t10, theocratic_rule__clerical_guardianship, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(theo_be_t15, theocratic_rule__clerical_guardianship, base_extractiveness, 15, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(theo_su_t0, theocratic_rule__clerical_guardianship, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(theo_su_t5, theocratic_rule__clerical_guardianship, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(theo_su_t10, theocratic_rule__clerical_guardianship, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(theo_su_t15, theocratic_rule__clerical_guardianship, suppression_requirement, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(theocratic_rule__clerical_guardianship, enforcement_mechanism).
narrative_ontology:affects_constraint(theocratic_rule__clerical_guardianship, theocratic_rule__divine_kingship).
narrative_ontology:affects_constraint(theocratic_rule__clerical_guardianship, theocratic_rule__scriptural_legalism).
narrative_ontology:affects_constraint(theocratic_rule__clerical_guardianship, islamic_jurisprudence_ijtihad_closure).
narrative_ontology:affects_constraint(theocratic_rule__clerical_guardianship, clerical_institutional_capture).

% DUAL FORMULATION NOTE:
% The theocratic_rule kernel has three distinct readings, each generating a separate constraint story with its own ε and classification profile. clerical_guardianship (this story, ε=0.68) models trusteeship-based theocracy; divine_kingship (sibling, ε≈0.55) models cosmic-political fusion; scriptural_legalism (sibling, ε≈0.50) models subordination to immutable law. These are not observables of one constraint but three structurally distinct constraints sharing a kernel. Each story links to the upstream constraint islamic_jurisprudence_ijtihad_closure (the closure of independent jurisprudential interpretation), which enables all three readings by establishing the fiqh as fixed rather than evolving. The downstream constraint clerical_institutional_capture models how guardianship captures republican institutions through the same mechanisms (suppression, extraction, theater) analyzed here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
