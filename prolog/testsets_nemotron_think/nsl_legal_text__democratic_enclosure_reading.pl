% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__democratic_enclosure_reading, []).

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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: NSL as Permanent Democratic Enclosure and Dissent Criminalization
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   The Hong Kong National Security Law (NSL), imposed by Beijing on June 30,
 *   2020, bypassing HK's legislature, criminalizes secession, subversion,
 *   terrorism, and collusion with foreign forces with extraterritorial reach.
 *   This reading — democratic_enclosure_reading — interprets the NSL not as a
 *   legitimate security measure but as a mechanism for the permanent closure
 *   of Hong Kong's democratic space and the criminalization of dissent. The
 *   law's vague definitions, mandatory mainland jurisdiction for 'complex'
 *   cases, and exclusion of jury trials create a structure where any
 *   political opposition can be prosecuted as 'subversion' or 'collusion.'
 *   Over 2020-2024, the constraint has extracted civic space, press freedom,
 *   opposition politics, judicial independence, and academic freedom from
 *   Hong Kong society, transferring political control to Beijing and its
 *   local allies. The claimed_type is 'snare' from this reading's seat: the
 *   coordination story (restoring order) is cover for extraction (eliminating
 *   democratic contestation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.85).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.92).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "NSL as Permanent Democratic Enclosure and Dissent Criminalization").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '66b25f7f-1a76-4f0d-9a60-0d1878746479').
narrative_ontology:cs_kernel_codification('66b25f7f-1a76-4f0d-9a60-0d1878746479', formalized).
narrative_ontology:cs_authority_grounding('66b25f7f-1a76-4f0d-9a60-0d1878746479', extraction).
narrative_ontology:cs_interpretation_layer_present('66b25f7f-1a76-4f0d-9a60-0d1878746479').
narrative_ontology:cs_reading_relation('66b25f7f-1a76-4f0d-9a60-0d1878746479', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('66b25f7f-1a76-4f0d-9a60-0d1878746479', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('66b25f7f-1a76-4f0d-9a60-0d1878746479', foundational, permanent_democratic_closure_intent).
narrative_ontology:cs_axiom_status(permanent_democratic_closure_intent, holdable).
narrative_ontology:cs_axiom_grounding('66b25f7f-1a76-4f0d-9a60-0d1878746479', permanent_democratic_closure_intent, empirically_contingent).
narrative_ontology:cs_axiom('66b25f7f-1a76-4f0d-9a60-0d1878746479', secondary, security_pretext_for_political_extraction).
narrative_ontology:cs_axiom_status(security_pretext_for_political_extraction, holdable).
narrative_ontology:cs_axiom_grounding('66b25f7f-1a76-4f0d-9a60-0d1878746479', security_pretext_for_political_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('66b25f7f-1a76-4f0d-9a60-0d1878746479', post_1997_autonomy_framework).
narrative_ontology:cs_drift_state('66b25f7f-1a76-4f0d-9a60-0d1878746479', post_nsl_implementation_2020_2024, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('66b25f7f-1a76-4f0d-9a60-0d1878746479', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hk_establishment_executive).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_press).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, political_opposition).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, legal_profession_dissenters).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, academic_freedom_advocates).
narrative_ontology:constraint_vindicates(nsl_legal_text__democratic_enclosure_reading, state_security_primacy_over_rights).
narrative_ontology:constraint_vindicates(nsl_legal_text__democratic_enclosure_reading, extraterritorial_application_of_national_security_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted, promulgated, and retains interpretive authority over the NSL through NPCSC. Uses the law to secure political control over Hong Kong, prevent independence movements, and integrate HK into mainland governance frameworks. Collects strategic political compliance; bears no domestic enforcement cost.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Implements NSL locally through police, prosecution, and courts. Gains enhanced executive power, freedom from legislative oversight, and career security within Beijing's patronage network. Enforces the law's provisions against civil society while insulating itself from accountability.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_establishment_executive, beneficiary,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, hk_establishment_executive, agenda_setter).

% NGOs, unions, and advocacy groups face asset freezes, leadership arrests, and forced disbandment under NSL Articles 20, 22, 29. Operations criminalized; foreign funding treated as collusion. Exit means dissolution or exile; remaining means self-censorship or prosecution.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_civil_society, payer,
    organized, biographical, constrained, local).

% Outlets (Apple Daily, Stand News, Citizen News) shut down via asset freezes and sedition charges. Journalists arrested for 'collusion with foreign forces' over routine reporting. Exit means closure or offshore relocation with audience loss; remaining means editorial capture.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_press, payer,
    organized, biographical, constrained, local).

% Pro-democracy politicians disqualified, arrested (47+ in primary case), or exiled. Electoral system restructured to exclude opposition. No viable institutional path to power; exile severs constituency ties; remaining means imprisonment.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, political_opposition, payer,
    organized, biographical, trapped, local).

% Barristers and solicitors defending NSL clients face professional discipline, doxxing, and visa denial. Judiciary pressured through 'patriotic' appointment criteria. Exit means leaving practice or jurisdiction; remaining means complicity or marginalization.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, legal_profession_dissenters, payer,
    moderate, biographical, constrained, local).

% University governance restructured; courses on Hong Kong politics cancelled; scholars denied contracts or visas. Research on sensitive topics self-censored. Exit means academic exile; remaining means intellectual constraint.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, academic_freedom_advocates, payer,
    organized, biographical, constrained, local).

% UN human rights bodies, foreign governments, and NGOs document NSL's impact through reports, sanctions, and diplomatic pressure. No enforcement power; observations shape international perception but do not alter domestic enforcement.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_observers, observer,
    institutional, generational, analytical, global).

% General population subject to chilling effects: self-censorship, withdrawal from public discourse, emigration surge (200k+ 2020-2023). Would object to democratic loss but lack organized voice; exit means emigration with high personal cost; remaining means adapted silence.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_residents_general, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NSL coordinates Beijing's political control over Hong Kong by replacing the post-1997 negotiated autonomy with a unitary security framework that subordinates local institutions to central directives. It solves Beijing's coordination problem of ensuring HK's political alignment without direct military administration.
% TRANSFER_FUNCTION: Moves political agency, civic space, and rule-of-law protections from Hong Kong civil society, press, opposition, and legal profession to Beijing central authorities and the HK establishment executive. Transfers the cost of enforcement (policing, prosecution, incarceration) to HK public resources while political benefits accrue to Beijing.
% ABSENT_VOICES: Hong Kong residents at large (200k+ emigrants, silent majority) are structurally excluded from institutional representation — no elected legislature, no functional district councils, no referendum mechanism. The 2019 protesters who triggered the NSL are imprisoned or exiled. Mainland Chinese citizens who might dissent from the HK policy are suppressed domestically.
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, the 47+ opposition figures would demand release, independent media would attempt restart, civil society organizations would re-form, and the electoral system would face immediate legitimacy crisis. Beijing would lose its primary legal instrument for HK control, forcing either new coercive measures or negotiated political settlement — the HK political order would fundamentally reorganize.
% FOUNDING_PROBLEM: Beijing perceived the 2019 anti-extradition protests as an existential threat to 'one country, two systems' and national sovereignty, claiming foreign interference had hijacked HK's autonomy to create a 'color revolution' bridgehead. The NSL was presented as the necessary instrument to restore order and close the legal vacuum exploited by protesters.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and HK establishment attest the founding problem remains live, citing ongoing 'external interference' risks. International legal scholars (e.g., ICJ, HRW), UN special rapporteurs, and exiled opposition figures attest the 2019 protests were indigenous, the 'vacuum' claim is pretextual, and the NSL's provisions far exceed any legitimate security need — corroboration from outside the beneficiary set supports the shifted-function reading.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__democratic_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__democratic_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85) is high because the NSL extracts the entire infrastructure of democratic participation — elections, press, assembly, legal defense — not merely discrete resources. Suppression (0.92) is near-maximal because the law's persistence depends on active coercion: arrests, asset freezes, license revocations, and extraterritorial warrants. Theater_ratio (0.25) reflects that some performative legal process exists (trials, appeals) but outcomes are predetermined by NPCSC interpretation power and 'patriotic' judicial appointments. Accessibility_collapse (0.88) captures the near-total foreclosure of institutional alternatives: no legislative path, no judicial remedy, no electoral route. Resistance (0.75) remains substantial despite repression — evident in exile advocacy, silent non-cooperation, and persistent international documentation — but is structurally contained by the constraint's enforcement machinery.
 *
 * PERSPECTIVAL GAP:
 *   From Beijing's agenda-setter seat, the NSL appears as a necessary coordination mechanism (rope-like) that solves the sovereignty problem. From the payer seats (civil society, press, opposition), the same structure operates as a snare — pure extraction of political agency enforced through criminal law. The engine computes this divergence from the structural data: the agenda_setter has arbitrage exit and collects strategic benefits; the payers have trapped/constrained exit and bear all costs. The claimed_type 'snare' reflects the payer seats' structural reality; the sovereignty_restoration_reading's claimed 'rope' reflects the agenda_setter's self-presentation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beijing central authorities are the primary beneficiary (d ≈ 0.05): they gain strategic control at zero domestic cost, with arbitrage-grade exit (they can modify or repeal the law at will). HK establishment executive are secondary beneficiaries (d ≈ 0.15): they gain enhanced power but face some reputational and career risk if Beijing's line shifts. Civil society, press, opposition, legal dissenters, and academics are payers (d ≈ 0.85-0.95): they bear the full cost of criminalization with trapped or constrained exit. General residents are excluded (d ≈ 0.6): they bear diffuse chilling effects but lack organized voice. International observers are analytical (d = 0.5): they see the full structure but hold no structural position within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The NSL's founding mandate (restoring order after 2019) is contested: Beijing claims the threat persists; independent observers argue the law's scope far exceeds any residual protest risk. The mandate has not been resolved — the law expands rather than sunsets (Article 14 national security education, Article 43 extraterritorial application). This is not a piton (atrophied function) but an active snare: the extraction machinery is intensifying (rising suppression_requirement), not decaying. The mandatrophy risk is low because the constraint's function (political enclosure) remains live for its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pretext_vs_genuine_security,
    'Is the NSL''s national security rationale a genuine response to existential threat, or a pretext for political enclosure?',
    'Comparative analysis of NSL charging patterns vs. actual security threats: if >90% of prosecutions target non-violent political expression/association rather than violence/terrorism, pretext hypothesis gains weight. Declassified internal communications (if ever released) would be decisive.',
    'If pretext, the constraint is a pure snare (claimed_type confirmed). If genuine security rationale coexists with overbroad application, the constraint may be tangled_rope at the analytical level — coordination function real but extraction excessive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pretext_vs_genuine_security, empirical, 'Whether the security justification is causally prior to or constructed for the democratic enclosure function.').

omega_variable(
    irreversibility_of_enclosure,
    'Is the democratic closure structurally permanent, or does it contain reversible institutional pathways?',
    'Track whether any NSL provision has been narrowed by court interpretation, whether any charged activist has won acquittal on substantive grounds, whether HK establishment has ever publicly dissented from Beijing''s NSL interpretation. Absence of all three over 5+ years supports irreversibility.',
    'If irreversible, the constraint is a terminal snare with no scaffold pathway. If reversible, a scaffold dynamic may exist beneath the snare surface — the enclosure could be a transitional (though prolonged) phase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreversibility_of_enclosure, conceptual, 'Whether the constraint''s extraction trajectory admits of institutional reversal or only regime change.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the NSL legal text instantiate a single commitment system kernel, or multiple distinct kernels (security law, constitutional amendment, colonial instrument)?',
    'Compare how each sibling reading''s authority_grounding and kernel_codification map to the same Articles. If sovereignty_restoration_reading treats NSL as formalized/extraction while democratic_enclosure_reading treats it as fixed_text/extraction, the kernel itself is framingly underdetermined.',
    'If the kernel is framingly underdetermined, the three readings are not readings of ONE kernel but of THREE differently-codified kernels that happen to share a text. This would require decomposing nsl_legal_text into multiple kernel_ids.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'CS-framing ambiguity: whether the contested readings share a single kernel or inhabit different kernel framings of the same text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_dem_enc_tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(nsl_dem_enc_tr_t0, observed).
narrative_ontology:measurement(nsl_dem_enc_tr_t1, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 1, 0.18).
narrative_ontology:measurement_basis(nsl_dem_enc_tr_t1, observed).
narrative_ontology:measurement(nsl_dem_enc_tr_t2, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement_basis(nsl_dem_enc_tr_t2, observed).
narrative_ontology:measurement(nsl_dem_enc_tr_t3, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement_basis(nsl_dem_enc_tr_t3, observed).
narrative_ontology:measurement(nsl_dem_enc_tr_t4, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement_basis(nsl_dem_enc_tr_t4, observed).

% Extraction over time
narrative_ontology:measurement(nsl_dem_enc_be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement_basis(nsl_dem_enc_be_t0, observed).
narrative_ontology:measurement(nsl_dem_enc_be_t1, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 1, 0.8).
narrative_ontology:measurement_basis(nsl_dem_enc_be_t1, observed).
narrative_ontology:measurement(nsl_dem_enc_be_t2, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2, 0.83).
narrative_ontology:measurement_basis(nsl_dem_enc_be_t2, observed).
narrative_ontology:measurement(nsl_dem_enc_be_t3, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 3, 0.85).
narrative_ontology:measurement_basis(nsl_dem_enc_be_t3, observed).
narrative_ontology:measurement(nsl_dem_enc_be_t4, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 4, 0.85).
narrative_ontology:measurement_basis(nsl_dem_enc_be_t4, observed).

% Suppression requirement over time
narrative_ontology:measurement(nsl_dem_enc_su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(nsl_dem_enc_su_t0, observed).
narrative_ontology:measurement(nsl_dem_enc_su_t1, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 1, 0.88).
narrative_ontology:measurement_basis(nsl_dem_enc_su_t1, observed).
narrative_ontology:measurement(nsl_dem_enc_su_t2, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2, 0.9).
narrative_ontology:measurement_basis(nsl_dem_enc_su_t2, observed).
narrative_ontology:measurement(nsl_dem_enc_su_t3, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 3, 0.92).
narrative_ontology:measurement_basis(nsl_dem_enc_su_t3, observed).
narrative_ontology:measurement(nsl_dem_enc_su_t4, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 4, 0.92).
narrative_ontology:measurement_basis(nsl_dem_enc_su_t4, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, hong_kong_electoral_reform_2021).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, article_23_legislation_2024).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, mainland_judicial_assistance_arrangements).

% DUAL FORMULATION NOTE:
% This reading (democratic_enclosure_reading) and its siblings (sovereignty_restoration_reading, jurisdictional_capture_reading) form a constraint family decomposing the colloquial label 'the NSL.' Each reading has distinct ε, beneficiaries, victims, and claimed_type. The ε-invariance principle requires separate stories: sovereignty_restoration_reading claims low ε (rope); jurisdictional_capture_reading claims moderate ε (tangled_rope); this reading claims high ε (snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
