% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: National Security Law as Democratic Enclosure Mechanism
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   The Hong Kong National Security Law (NSL), imposed by Beijing on 30 June
 *   2020, is read here as a mechanism for the permanent enclosure of
 *   democratic space. Unlike a normal security law targeting specific violent
 *   threats, the NSL's four offences — secession, subversion, terrorism,
 *   collusion — are defined so broadly that any organised political
 *   opposition, critical journalism, or civil society advocacy can be
 *   criminalised. The law operates extraterritorially, applies retroactively
 *   in practice, and removes procedural protections (jury trial, open court,
 *   judicial review). Over 290 arrests and 100+ charges by 2025 have
 *   dismantled the entire pro-democracy ecosystem: parties, unions, press,
 *   student groups, and elected representatives. The constraint's extraction
 *   is the political agency of Hong Kong society; its beneficiaries are the
 *   central authorities and the local establishment that no longer face
 *   democratic contestation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.88).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.92).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "National Security Law as Democratic Enclosure Mechanism").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '81d2e2fc-d6c0-4f02-a8c1-ed29079c8089').
narrative_ontology:cs_kernel_codification('81d2e2fc-d6c0-4f02-a8c1-ed29079c8089', formalized).
narrative_ontology:cs_authority_grounding('81d2e2fc-d6c0-4f02-a8c1-ed29079c8089', extraction).
narrative_ontology:cs_interpretation_layer_present('81d2e2fc-d6c0-4f02-a8c1-ed29079c8089').
narrative_ontology:cs_reading_relation('81d2e2fc-d6c0-4f02-a8c1-ed29079c8089', nsl_legal_text__sovereignty_restoration_reading, forecloses).
narrative_ontology:cs_reading_relation('81d2e2fc-d6c0-4f02-a8c1-ed29079c8089', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('81d2e2fc-d6c0-4f02-a8c1-ed29079c8089', foundational, political_opposition_criminalisation_is_legitimate_security).
narrative_ontology:cs_axiom_status(political_opposition_criminalisation_is_legitimate_security, overridden).
narrative_ontology:cs_axiom_grounding('81d2e2fc-d6c0-4f02-a8c1-ed29079c8089', political_opposition_criminalisation_is_legitimate_security, conventional).
narrative_ontology:cs_axiom('81d2e2fc-d6c0-4f02-a8c1-ed29079c8089', foundational, democratic_participation_equals_subversion).
narrative_ontology:cs_axiom_status(democratic_participation_equals_subversion, holdable).
narrative_ontology:cs_axiom_grounding('81d2e2fc-d6c0-4f02-a8c1-ed29079c8089', democratic_participation_equals_subversion, deontological).
narrative_ontology:cs_reference_frame('81d2e2fc-d6c0-4f02-a8c1-ed29079c8089', pre_2020_hk_autonomy_framework).
narrative_ontology:cs_drift_state('81d2e2fc-d6c0-4f02-a8c1-ed29079c8089', post_nsl_2025, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('81d2e2fc-d6c0-4f02-a8c1-ed29079c8089', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hk_sar_establishment).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, national_security_apparatus).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hk_pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_press_outlets).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_organisations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, opposition_politicians).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, academic_freedom_advocates).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, labour_unions).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, student_organisations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, legal_profession_dissidents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted, promulgated, and interpretively control the NSL through the NPCSC; the law extends mainland sovereign power directly into HK's legal order. The central authorities define the four criminal offences (secession, subversion, terrorism, collusion) and retain final interpretive authority via Article 65. They extract political control and regime stability from the constraint's operation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% The Chief Executive, Executive Council, and pro-Beijing legislative majority gain a permanent legal shield against democratic accountability. The NSL removes the need to contest elections or policy debates — opposition is criminalised ex ante. Their continued tenure and patronage networks depend on the constraint's enforcement.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_sar_establishment, beneficiary,
    institutional, generational, identity_locked, national).

% The Committee for Safeguarding National Security, the dedicated NSL police unit, and the designated judges receive expanded powers: secret trials, no jury, extraterritorial jurisdiction, asset freezing, and immunity from judicial review on 'national security' grounds. Their budgets, headcounts, and institutional prestige have grown sharply since 2020.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, national_security_apparatus, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, national_security_apparatus, agenda_setter).

% Face life imprisonment for organising primaries, publishing slogans, or attending vigils. Over 290 arrested, 100+ charged, most denied bail under NSL's reversed burden of proof. Exit means exile, silencing, or imprisonment; identity as a democrat makes exit structurally unavailable. The constraint targets their political existence.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_pro_democracy_activists, payer,
    moderate, biographical, trapped, national).

% Apple Daily, Stand News, Citizen News forced to close after asset freezes and senior staff arrests under 'collusion with foreign forces' provisions. Remaining outlets self-censor or relocate. The constraint extracts their operational viability and the public's access to independent information.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_press_outlets, payer,
    moderate, biographical, trapped, national).

% Unions (HKCTU), professional associations, student unions, and NGOs disbanded or driven underground after police raids and 'foreign collusion' investigations. The constraint removes the organisational infrastructure of dissent — no legal space to operate, no funding channels, no public assembly rights.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, civil_society_organisations, payer,
    powerless, biographical, trapped, national).

% Disqualified from LegCo en masse (2020), arrested for primary elections (2021), or forced into exile. The NSL's 'subversion' offence criminalises winning a legislative majority with intent to block government — making electoral victory itself a crime. Political participation is the extraction target.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, opposition_politicians, payer,
    moderate, biographical, trapped, national).

% University governance restructured to ensure 'patriotism'; scholars dismissed for research or speech; libraries purge books; curricula rewritten. The constraint extracts intellectual autonomy and the university's role as a critical space.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, academic_freedom_advocates, payer,
    powerless, biographical, constrained, national).

% Designated judges hand-picked by the Chief Executive; barristers denied admission to NSL cases; legal professional bodies pressured to endorse the law. The constraint extracts the common law profession's independence and its capacity to resist executive overreach.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, legal_profession_dissidents, payer,
    moderate, biographical, constrained, national).

% UN human rights mechanisms, foreign governments, NGOs, and legal scholars document the constraint's operation. They possess no enforcement power but shape the diplomatic and reputational costs. Their assessments feed sanctions, travel advisories, and treaty-body reviews.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine. The NSL presents itself as restoring order after 2019 unrest, but its structural operation is the pre-emptive criminalisation of any political action that could challenge Beijing's authority — not coordination of a shared problem but elimination of the political space where problems are negotiated.
% TRANSFER_FUNCTION: Moves political agency, organisational survival, and civic freedom from Hong Kong civil society (activists, press, unions, opposition, academics, lawyers) to the Beijing/HK establishment and the national security apparatus. The transfer is total: the victims lose the capacity to act politically; the beneficiaries gain permanent insulation from democratic accountability.
% ABSENT_VOICES: The 2019 protest movement's core participants — frontliners, district councillors, ordinary citizens who voted in record numbers — are structurally excluded from the NSL's authorship and interpretation. They would object to the criminalisation of their demands but are precisely the target population the constraint silences. The diaspora communities (UK, Canada, Taiwan, US) are also absent from the formal process but organise resistance from outside.
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, the 2019 protest demands (universal suffrage, independent inquiry, amnesty) would immediately resurface. The 100+ charged defendants would face immediate legal reprieve. Independent media would attempt relaunch. Unions and student organisations would reorganise. The Chief Executive and security apparatus would lose their legal shield. The entire post-2020 political settlement would collapse into open contestation.
% FOUNDING_PROBLEM: Beijing framed the NSL as addressing the 'chaos' of the 2019 anti-extradition protests — violence, paralysed governance, and challenges to central authority. The law was imposed via NPCSC decision, bypassing HK's legislature, to 'restore stability' and 'prevent foreign interference.'
% FOUNDING_PROBLEM_CORROBORATION: The 2019 unrest ended before the NSL took effect — pandemic restrictions, police attrition, and mass arrests had already suppressed street protest. Independent observers (Hong Kong Bar Association, International Commission of Jurists, UN OHCHR) attest the NSL's scope far exceeds any residual security threat: it targets peaceful opposition, journalism, and civil society, not violence. The 'founding problem' is dead; the constraint persists as regime consolidation.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.88) is near-maximal because the constraint removes the very possibility of organised dissent — not merely taxing it but extinguishing its legal existence. Suppression (0.92) reflects the reversed burden of proof, secret trials, designated judges, and extraterritorial reach: the machinery of enforcement is total. Theater ratio (0.25) is low because the law's performative 'security' framing is thin; the security apparatus does not pretend to serve the public — it serves regime preservation. Accessibility collapse (0.93) captures that alternatives (elections, protests, litigation, press, unions) have been legally and physically eliminated. Resistance (0.78) remains high because victims continue to resist through exile advocacy, legal challenges, and cultural preservation despite overwhelming force.
 *
 * PERSPECTIVAL GAP:
 *   From the Beijing/HK establishment seat, the NSL is a sovereign security instrument restoring order — a 'rope' coordinating stability. From every victim seat, it is a 'snare' eliminating their political existence. The engine computes this divergence from the declared beneficiaries/victims and exit options. The claimed_type 'snare' reflects the authoring seat's structural assessment: the coordination story is cover; the constraint's persistence depends on coercion and suppression of alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Beijing central authorities and the HK SAR establishment are structural beneficiaries (d near 0.0): they gain permanent political control without accountability. The national security apparatus is a beneficiary-agenda setter hybrid (d ~0.15): it administers the extraction and expands its institutional power. All victim groups — activists, press, civil society, opposition, academics, lawyers — are structural targets (d near 1.0): they bear the full extraction with trapped or constrained exit. Identity-locked exit for the HK establishment reflects their fused institutional identity with Beijing's sovereignty claim — they cannot exit the arrangement without losing their position. International observers sit at analytical (d=0.5): they experience neither extraction nor benefit directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The NSL's founding problem (2019 unrest) is dead — the unrest ended before the law took effect. Yet the constraint not only persists but intensifies (Article 23 domestic legislation in 2024 expanded it further). This is mandatrophy in its purest form: a mandate that has outlived its function but expands its extraction. The arrangement is not a degraded piton — it is actively, vigorously enforced. The coordination function was always pretextual; the extraction function is the point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nsl_coordination_vs_extraction_boundary,
    'Does the NSL contain any genuine coordination function (e.g., counter-terrorism intelligence sharing, cyber-security standards) separable from its democratic enclosure function, or is the entire structure extractive?',
    'Comparative analysis of NSL enforcement cases: proportion of charges involving actual violence/terrorism vs. peaceful political speech/organisation. If >90% of enforcement targets non-violent dissent, the coordination claim is falsified.',
    'If no separable coordination function exists, the constraint is a pure snare with zero rope component. If a genuine but minor coordination core exists, it becomes a tangled_rope with an overwhelmingly extractive character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nsl_coordination_vs_extraction_boundary, empirical, 'Whether the NSL''s security provisions have any genuine coordination content separable from political suppression.').

omega_variable(
    extraterritorial_enforcement_effectiveness,
    'How effectively does the NSL''s extraterritorial reach (Article 38) suppress diaspora advocacy and foreign institutional engagement with Hong Kong democracy movements?',
    'Track foreign government, NGO, and academic engagement with HK diaspora organisations over time; measure chilling effects on research, funding, and advocacy.',
    'If extraterritorial enforcement is highly effective, the constraint''s spatial scope functionally becomes global, amplifying extraction beyond HK''s borders. If ineffective, the constraint''s extraction is territorially bounded despite its formal claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraterritorial_enforcement_effectiveness, empirical, 'Whether the NSL''s extraterritorial provisions function as actual suppression or symbolic deterrence.').

omega_variable(
    common_law_residual_autonomy,
    'To what extent does Hong Kong''s common law system retain autonomous interpretive capacity within NSL cases, versus total capture by designated judges and NPCSC interpretations?',
    'Analyse NSL judgments for citations of common law precedents, proportionality reasoning, and procedural protections vs. pure statutory construction favouring the prosecution.',
    'If common law autonomy is fully extinguished, the jurisdictional_capture_reading''s structural claim is vindicated and this reading''s extraction is total. If residual autonomy persists, the constraint''s enclosure is incomplete — a structural vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_law_residual_autonomy, conceptual, 'Whether the NSL''s operation has fully displaced common law reasoning or left interpretive space.').

omega_variable(
    committer_frame_disagreement_location,
    'The kernel nsl_legal_text admits three readings. Which structural element do the readings fundamentally disagree on: the beneficiary/victim sets, the constraint''s primary function (coordination vs extraction), the legitimacy of the founding problem, or the spatial/temporal scope of application?',
    'Structural comparison of the three constraint stories'' base_properties, stakeholders, and six_questions. The disagreement locus is the element where the three stories'' authored values diverge most sharply.',
    'Identifies the precise structural fault line in the kernel. If readings disagree on beneficiary/victim sets, the kernel''s extraction structure is contested. If they disagree on function, the kernel''s coordination claim is contested. This routes committer-frame under-determination through the existing omega apparatus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_disagreement_location, conceptual, 'Structural locus of disagreement among the three declared readings of nsl_legal_text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 2020, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t2020, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(nsl__tr_t2021, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2021, 0.2).
narrative_ontology:measurement(nsl__tr_t2022, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2022, 0.22).
narrative_ontology:measurement(nsl__tr_t2023, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2023, 0.24).
narrative_ontology:measurement(nsl__tr_t2024, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2024, 0.25).
narrative_ontology:measurement(nsl__tr_t2025, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(nsl__be_t2021, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2021, 0.78).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2022, 0.84).
narrative_ontology:measurement(nsl__be_t2023, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2023, 0.87).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2024, 0.88).
narrative_ontology:measurement(nsl__be_t2025, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2025, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(nsl__su_t2021, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2021, 0.85).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2022, 0.9).
narrative_ontology:measurement(nsl__su_t2023, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2023, 0.91).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2024, 0.92).
narrative_ontology:measurement(nsl__su_t2025, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2025, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, article_23_domestic_legislation).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, electoral_system_overhaul_2021).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, national_education_law_hk).

% DUAL FORMULATION NOTE:
% This story is one of three in the nsl_legal_text constraint family. The democratic_enclosure_reading centres victim extraction (civil society, press, opposition) and regime consolidation. The sovereignty_restoration_reading claims a genuine coordination function (restoring order) with negligible extraction. The jurisdictional_capture_reading centres legal-system transplantation (common law displacement) with the legal profession as primary victim. All three share the same legal text but author different ε, beneficiaries, victims, and claimed types — per ε-invariance, they are distinct constraints linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_legal_text__democratic_enclosure_reading, institutional, 0.05).
constraint_indexing:directionality_override(nsl_legal_text__democratic_enclosure_reading, organized, 0.15).
constraint_indexing:directionality_override(nsl_legal_text__democratic_enclosure_reading, moderate, 0.9).
constraint_indexing:directionality_override(nsl_legal_text__democratic_enclosure_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
