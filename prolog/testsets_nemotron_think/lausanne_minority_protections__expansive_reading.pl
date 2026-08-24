% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Minority Protections — Expansive Reading (Institutional Continuity)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The Treaty of Lausanne (1923) Articles 37-45 establish minority
 *   protections for non-Muslim communities in Turkey. The expansive reading
 *   claims these guarantee functional continuity of pre-1923 religious
 *   governance: institutional self-administration (schools, courts, communal
 *   property), property rights (restitution/maintenance of vakıf assets), and
 *   clergy formation (theological schools, notably Halki for Greek Orthodox).
 *   This reading treats the treaty as a living coordination rope — minority
 *   institutions self-govern under state non-interference, in exchange for
 *   loyalty to the Turkish Republic. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as rope (genuine coordination protecting
 *   institutions from absorption) while metrics reflect historical extraction
 *   waves (Varlık Vergisi 1942, Istanbul Pogrom 1955, Halki closure 1971) and
 *   recent partial normalization (2003-2013 reforms, property returns). The
 *   engine measures that divergence; do not reconcile the claim to the
 *   metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.18).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.15).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Minority Protections — Expansive Reading (Institutional Continuity)").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '31edc486-9dce-4ef7-b97e-3b9b017b43ec').
narrative_ontology:cs_kernel_codification('31edc486-9dce-4ef7-b97e-3b9b017b43ec', fixed_text).
narrative_ontology:cs_authority_grounding('31edc486-9dce-4ef7-b97e-3b9b017b43ec', lineage).
narrative_ontology:cs_interpretation_layer_present('31edc486-9dce-4ef7-b97e-3b9b017b43ec').
narrative_ontology:cs_reading_relation('31edc486-9dce-4ef7-b97e-3b9b017b43ec', lausanne_minority_protections__restrictive_reading, coexists_with).
narrative_ontology:cs_reading_relation('31edc486-9dce-4ef7-b97e-3b9b017b43ec', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('31edc486-9dce-4ef7-b97e-3b9b017b43ec', foundational, institutional_continuity_guaranteed).
narrative_ontology:cs_axiom_status(institutional_continuity_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('31edc486-9dce-4ef7-b97e-3b9b017b43ec', institutional_continuity_guaranteed, conventional).
narrative_ontology:cs_axiom('31edc486-9dce-4ef7-b97e-3b9b017b43ec', foundational, theological_education_protected).
narrative_ontology:cs_axiom_status(theological_education_protected, holdable).
narrative_ontology:cs_axiom_grounding('31edc486-9dce-4ef7-b97e-3b9b017b43ec', theological_education_protected, conventional).
narrative_ontology:cs_reference_frame('31edc486-9dce-4ef7-b97e-3b9b017b43ec', treaty_text_as_living_guarantee).
narrative_ontology:cs_drift_state('31edc486-9dce-4ef7-b97e-3b9b017b43ec', contemporary_echr_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('31edc486-9dce-4ef7-b97e-3b9b017b43ec', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, greek_orthodox_patriarchate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, armenian_patriarchate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, jewish_chief_rabbinate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_community_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lausanne_minority_protections__expansive_reading, turkish_state).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, treaty_obligations_bind_successor_states).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, minority_institutional_autonomy_as_international_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims institutional self-administration of schools, churches, communal properties, and clergy formation through the Halki theological school (closed 1971). Depends on treaty text for legal personality and property recognition. Cannot exit the Turkish legal order without losing institutional continuity; identity fused with ecumenical status and Istanbul locus.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, greek_orthodox_patriarchate, beneficiary,
    organized, generational, identity_locked, national).

% Claims parallel protections for Armenian communal schools, churches, hospitals, and clergy formation. Operates under same treaty articles as Greek Orthodox Patriarchate. Faces similar identity lock: the patriarchate's legitimacy derives from continuous operation in Istanbul since 1461 under Ottoman then Turkish law.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, armenian_patriarchate, beneficiary,
    organized, generational, identity_locked, national).

% Claims protections for communal schools, synagogues, hospitals, and religious education. Smaller demographic base but same treaty standing. Identity locked through centuries-old communal structure (hahambaşı) recognized by Ottoman and Turkish states.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, jewish_chief_rabbinate, beneficiary,
    organized, generational, identity_locked, national).

% Individual members of Greek, Armenian, Jewish communities who depend on minority institutions for education, worship, personal status (marriage, inheritance), and cultural continuity. Exit options constrained by citizenship, language, and community ties; emigration possible but severs communal connection.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_community_members, beneficiary,
    moderate, biographical, constrained, national).

% Successor state to Ottoman Empire; bound by Treaty of Lausanne Articles 37-45. Administers minority protections through domestic law (Foundations Law, education regulations). Bears compliance costs (property restitution, school autonomy, non-interference in clergy selection). Can reinterpret or restrict protections through domestic legislation and administrative practice; exit from treaty obligations would carry severe international reputation costs.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__expansive_reading, turkish_state, payer).

% Original signatories (France, UK, Italy, Japan) with guarantee function under Treaty. Diplomatic protection largely atrophied post-WWII; role partially succeeded by EU accession process and ECHR oversight. Monitor compliance but lack enforcement mechanism beyond diplomatic pressure.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, guarantor_powers, observer,
    institutional, generational, analytical, global).

% Supervises Turkey's compliance with ECHR, which incorporates Lausanne obligations through Article 9 (religious freedom) and Article 1 Protocol 1 (property). Issues binding judgments on minority property (e.g., Fener Rum Patrikliği v. Turkey, Bozcaada Kimisis Monastery cases). Interpretation shapes domestic implementation but cannot directly amend treaty scope.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, european_court_of_human_rights, observer,
    institutional, generational, analytical, continental).

% Turkish state officials, nationalist jurists, and some constitutional scholars who argue Lausanne protects only individual worship, not institutional autonomy. Their view shapes administrative practice (school closures, property expropriation, Halki closure) but they are not a formal party to the treaty's minority protection regime — they are the domestic interpretive counter-weight.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, restrictive_interpretation_advocates, excluded,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees functional continuity of pre-1923 minority religious institutions (self-administration, property, clergy formation) against unilateral state absorption, providing a stable legal basis for communal existence within the Turkish Republic.
% TRANSFER_FUNCTION: Transfers the burden of institutional maintenance from minority communities alone to a shared obligation: the Turkish state must respect and facilitate (property return, school autonomy, clergy formation), while minority institutions self-administer. No monetary extraction; the transfer is obligation-for-autonomy.
% ABSENT_VOICES: Descendants of expelled or emigrated community members (Greek, Armenian, Jewish diasporas) who would claim restored property and institutional access but lack standing in Turkish courts. Also, potential future clergy who cannot be formed due to Halki closure — their absence is structural, not incidental.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished overnight, minority institutions would lose treaty-based legal personality, property claims, and school autonomy. They would revert to associations under general Turkish law (Dernek/Vakıf) with no guaranteed self-administration, no protected clergy formation, and no international recourse. The communal infrastructure built over centuries would face immediate legal dissolution.
% FOUNDING_PROBLEM: Post-WWI population transfers and state-building created existential risk for non-Muslim minorities remaining in Anatolia. The Treaty of Lausanne (1923) was designed to prevent their elimination by guaranteeing not just individual worship but the institutional structures (schools, courts, property, clergy) that sustain a people across generations.
% FOUNDING_PROBLEM_CORROBORATION: The expansive reading is attested by minority institutions themselves, the ECHR (Fener Rum Patrikliği v. Turkey, 2007; Samut v. Turkey, 2018), and the Venice Commission (2011 opinion on Foundations Law). The Turkish state and restrictive interpretation advocates contest that the founding problem (physical elimination) is gone and only individual rights remain. No neutral third party corroborates the 'problem solved' claim; demographic collapse of minorities (from ~200k to ~3k Greeks, ~60k Armenians, ~15k Jews) suggests the problem persists in altered form.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.18) because the constraint's current operation is protective, not extractive — the state does not harvest resources from minorities; it restricts their institutional autonomy. The historical peaks (1942 wealth tax, 1955 pogrom, 1971 Halki closure) represent periods when the constraint was violated or reinterpreted restrictively, not the expansive reading's own operation. Theater ratio rose during violation periods (performative compliance while restricting substance) and fell during reform periods. Suppression requirement peaked when the state actively blocked institutional function (school takeovers, property seizures, clergy formation ban) and declined with ECHR jurisprudence and EU-driven reforms. Accessibility collapse is moderate (0.45): alternatives (individual worship under general law) exist but lack institutional continuity. Resistance is moderate-high (0.52): the Turkish state has consistently resisted expansive implementation through administrative obstruction, legal reinterpretation, and property regime manipulation.
 *
 * PERSPECTIVAL GAP:
 *   From the minority institution seats, the constraint is a genuine rope: it coordinates their survival within the Turkish state, providing legal personality and property protection they could not secure alone. From the Turkish state seat, the same constraint appears as a sovereign limitation — a coordination cost that restricts domestic policy space. The engine computes this divergence from the structural data: identity_locked beneficiaries experience near-zero effective extraction (subsidy), while the institutional agenda_setter/payer experiences positive but bounded extraction (compliance cost). The claimed rope type reflects the minority seat's experience; the state seat may compute differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority institutions (patriarchates, rabbinate) are structural beneficiaries: they collect institutional continuity, legal personality, and property protections (d near 0.0). Their exit is identity_locked — the institution's existence is constituted by the treaty recognition; leaving the Turkish legal order means institutional death. Community members are beneficiaries with constrained exit (emigration possible but severs communal ties). The Turkish state is agenda_setter (administers the regime) and payer (bears compliance costs: property restitution, school autonomy, non-interference). Its exit is arbitrage — it could denounce treaty obligations but faces severe international costs. Guarantor powers and ECHR are observers with analytical exit. Restrictive interpretation advocates are excluded from the treaty regime but shape domestic practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing minority elimination) is contested as live vs. solved. If 'solved', the expansive reading becomes a piton — an atrophied coordination mechanism maintained theatrically (property returns without full autonomy restoration). If 'live', it remains a rope. The current classification as rope with moderate theater and low extractiveness reflects the reading's own lights: the coordination function (institutional continuity) is real and unfinished, but the constraint's persistence depends on continuous interpretive contest, not automatic enforcement. No party extracts rents; the vulnerability is interpretive, not extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the expansive_reading a distinct constraint from the restrictive_reading and guarantor_reading, or are they measurement perspectives on one constraint?',
    'Test ε-invariance: if changing the interpretive lens (expansive vs restrictive) changes the constraint''s extractiveness, suppression, or beneficiary structure, they are distinct constraints. The expansive reading has beneficiaries (minority institutions) and low extractiveness; the restrictive reading would have no institutional beneficiaries and higher effective extraction on communities. Distinct ε confirms distinct constraints.',
    'If distinct, each reading gets its own constraint story with own classification. If unified, the kernel is a single constraint with observer-dependent classification — violating ε-invariance. The framework requires decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate separate ε-invariant constraints.').

omega_variable(
    interpretive_contest_as_extraction,
    'Does the interpretive contest itself function as an extraction mechanism — does the state''s restrictive administration extract compliance value from minority institutions'' uncertainty?',
    'Measure whether minority institutions invest disproportionate resources in legal defense, diplomatic advocacy, and property documentation solely to maintain treaty-reading viability — resources that would be unnecessary if the expansive reading were settled.',
    'If yes, the constraint''s effective extractiveness is higher than the base_properties value (which measures only the reading''s internal operation). The contest becomes a tangled_rope: coordination (institutional continuity) + extraction (uncertainty tax).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_contest_as_extraction, empirical, 'Whether interpretive instability operates as a hidden extraction layer.').

omega_variable(
    halki_closure_suppression_mechanism,
    'Is the Halki theological school closure (1971) structural suppression (legal ban) or internalized suppression (community''s acceptance of impossibility)?',
    'Post-exit suppression trajectory: if the community continues to treat Halki as ''closed but reopenable'' and invests in reopening advocacy, suppression is structural. If the community has internally accepted closure and shifted to alternative formation (abroad, informal), suppression has internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure — the target carries the suppression after the formal barrier could be lifted. Relevant for omega resolution if Halki reopens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(halki_closure_suppression_mechanism, empirical, 'Structural vs. internalized suppression in clergy formation blockade.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 1923, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lausanne_expansive_tr_t1923, lausanne_minority_protections__expansive_reading, theater_ratio, 1923, 0.05).
narrative_ontology:measurement(lausanne_expansive_tr_t1942, lausanne_minority_protections__expansive_reading, theater_ratio, 1942, 0.3).
narrative_ontology:measurement(lausanne_expansive_tr_t1955, lausanne_minority_protections__expansive_reading, theater_ratio, 1955, 0.45).
narrative_ontology:measurement(lausanne_expansive_tr_t1971, lausanne_minority_protections__expansive_reading, theater_ratio, 1971, 0.55).
narrative_ontology:measurement(lausanne_expansive_tr_t2003, lausanne_minority_protections__expansive_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement(lausanne_expansive_tr_t2013, lausanne_minority_protections__expansive_reading, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(lausanne_expansive_tr_t2023, lausanne_minority_protections__expansive_reading, theater_ratio, 2023, 0.12).

% Extraction over time
narrative_ontology:measurement(lausanne_expansive_be_t1923, lausanne_minority_protections__expansive_reading, base_extractiveness, 1923, 0.08).
narrative_ontology:measurement(lausanne_expansive_be_t1942, lausanne_minority_protections__expansive_reading, base_extractiveness, 1942, 0.25).
narrative_ontology:measurement(lausanne_expansive_be_t1955, lausanne_minority_protections__expansive_reading, base_extractiveness, 1955, 0.35).
narrative_ontology:measurement(lausanne_expansive_be_t1971, lausanne_minority_protections__expansive_reading, base_extractiveness, 1971, 0.42).
narrative_ontology:measurement(lausanne_expansive_be_t2003, lausanne_minority_protections__expansive_reading, base_extractiveness, 2003, 0.22).
narrative_ontology:measurement(lausanne_expansive_be_t2013, lausanne_minority_protections__expansive_reading, base_extractiveness, 2013, 0.18).
narrative_ontology:measurement(lausanne_expansive_be_t2023, lausanne_minority_protections__expansive_reading, base_extractiveness, 2023, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(lausanne_expansive_su_t1923, lausanne_minority_protections__expansive_reading, suppression_requirement, 1923, 0.1).
narrative_ontology:measurement(lausanne_expansive_su_t1942, lausanne_minority_protections__expansive_reading, suppression_requirement, 1942, 0.6).
narrative_ontology:measurement(lausanne_expansive_su_t1955, lausanne_minority_protections__expansive_reading, suppression_requirement, 1955, 0.75).
narrative_ontology:measurement(lausanne_expansive_su_t1971, lausanne_minority_protections__expansive_reading, suppression_requirement, 1971, 0.8).
narrative_ontology:measurement(lausanne_expansive_su_t2003, lausanne_minority_protections__expansive_reading, suppression_requirement, 2003, 0.4).
narrative_ontology:measurement(lausanne_expansive_su_t2013, lausanne_minority_protections__expansive_reading, suppression_requirement, 2013, 0.25).
narrative_ontology:measurement(lausanne_expansive_su_t2023, lausanne_minority_protections__expansive_reading, suppression_requirement, 2023, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__expansive_reading, 0.08).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, turkish_foundations_law).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, echr_article9_jurisprudence).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, eu_accession_criteria_religious_freedom).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% Kernel family: lausanne_minority_protections. Three readings decompose the treaty label: expansive (institutional continuity), restrictive (individual worship only), guarantor (international enforcement). ε differs: expansive=low (protective coordination), restrictive=high (state extracts institutional autonomy), guarantor=moderate (international supervision cost). Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__expansive_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
