% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim Corpus as Substitution Archive (Memorial Reading of the Sacrificial Inheritance)
 *   domain: religious/rabbinic/commitment_system
 *
 * SUMMARY:
 *   After the destruction of the Second Temple (70 CE), the rabbinic movement
 *   reconstituted covenantal practice around two substitutes for the
 *   sacrificial service: prayer ('service of the heart,' keyed to the
 *   offering schedule) and Torah study (the academies' dictum that engagement
 *   with the laws of offerings stands in their place). The Kodashim order —
 *   the largest tractate-order of the Mishnah and Talmud — was preserved,
 *   redacted, and continuously studied under this settlement. This story
 *   instantiates the substitution_archive reading: the corpus is a memorial
 *   archive documenting what was superseded, and its administration claims
 *   continuity with the system it replaced — a continuity that channels the
 *   sacrificial inheritance's authority to the text-study institutions while
 *   denying living practice to those who seek it. KEY AGENTS (by structural
 *   relationship): - rabbinic_text_study_institutions: agenda-setter and
 *   primary beneficiary (institutional/arbitrage) — administers the archive,
 *   collects the continuity dividend - synagogal_prayer_establishment:
 *   secondary beneficiary (organized/constrained) — liturgy gains
 *   sacrificial-grade standing from the substitution -
 *   temple_restoration_advocates: primary payer (moderate/trapped) — seek
 *   living practice, told the system is superseded -
 *   priestly_lineage_families: payer with residual beneficiary honors
 *   (moderate/identity_locked) - sacrifice_oriented_lay_pietists: diffuse
 *   payers (powerless/constrained) — devotion redirected into study -
 *   samaritan_and_karaite_practitioners: excluded (powerless/trapped) —
 *   non-rabbinic paths outside the conversation -
 *   academic_historians_of_rabbinic_judaism: observer (analytical) ε
 *   referent: the standing arrangement under contest is the archive as
 *   administered — the corpus, its curriculum, and its liturgical embedding
 *   under the continuity claim — assessed by this reading's own lights; the
 *   reading's endorsed alternative is not the referent. Claim and metrics are
 *   independent: the claimed type is what this reading holds structurally
 *   true; the metrics are what this reading measures descriptively.
 *   Assumptions: the interval is anchored to historical years (70 CE
 *   destruction to 2020); measurement values are this reading's authored
 *   judgments over one shared grid, not instrument readings.
 *
 * KEY AGENTS:
 *   - rabbinic_text_study_institutions: agenda-setter and primary beneficiary (institutional/arbitrage) — administers the archive, collects the continuity dividend
 *   - synagogal_prayer_establishment: secondary beneficiary (organized/constrained) — liturgy gains sacrificial-grade standing from the substitution
 *   - temple_restoration_advocates: primary payer (moderate/trapped) — seek living practice, told the system is superseded
 *   - priestly_lineage_families: payer with residual beneficiary honors (moderate/identity_locked)
 *   - sacrifice_oriented_lay_pietists: diffuse payers (powerless/constrained) — devotion redirected into study
 *   - samaritan_and_karaite_practitioners: excluded (powerless/trapped) — non-rabbinic paths outside the conversation
 *   - academic_historians_of_rabbinic_judaism: observer (analytical) — sees the full structure of the settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.58).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.48).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim Corpus as Substitution Archive (Memorial Reading of the Sacrificial Inheritance)").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious/rabbinic/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, 'b8e59430-92f6-4e9f-9da9-7a700c5d2087').
narrative_ontology:cs_kernel_codification('b8e59430-92f6-4e9f-9da9-7a700c5d2087', fixed_text).
narrative_ontology:cs_authority_grounding('b8e59430-92f6-4e9f-9da9-7a700c5d2087', lineage).
narrative_ontology:cs_interpretation_layer_present('b8e59430-92f6-4e9f-9da9-7a700c5d2087').
narrative_ontology:cs_reading_relation('b8e59430-92f6-4e9f-9da9-7a700c5d2087', kodashim_corpus__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('b8e59430-92f6-4e9f-9da9-7a700c5d2087', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_axiom('b8e59430-92f6-4e9f-9da9-7a700c5d2087', foundational, prayer_and_study_fully_replace_sacrifice).
narrative_ontology:cs_axiom_status(prayer_and_study_fully_replace_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('b8e59430-92f6-4e9f-9da9-7a700c5d2087', prayer_and_study_fully_replace_sacrifice, conventional).
narrative_ontology:cs_axiom('b8e59430-92f6-4e9f-9da9-7a700c5d2087', foundational, kodashim_memorial_not_occupied_kernel).
narrative_ontology:cs_axiom_status(kodashim_memorial_not_occupied_kernel, holdable).
narrative_ontology:cs_axiom_grounding('b8e59430-92f6-4e9f-9da9-7a700c5d2087', kodashim_memorial_not_occupied_kernel, empirically_contingent).
narrative_ontology:cs_axiom('b8e59430-92f6-4e9f-9da9-7a700c5d2087', secondary, continuity_claim_obscures_replacement).
narrative_ontology:cs_axiom_status(continuity_claim_obscures_replacement, holdable).
narrative_ontology:cs_axiom_grounding('b8e59430-92f6-4e9f-9da9-7a700c5d2087', continuity_claim_obscures_replacement, empirically_contingent).
narrative_ontology:cs_reference_frame('b8e59430-92f6-4e9f-9da9-7a700c5d2087', post_destruction_substitution_settlement).
narrative_ontology:cs_drift_state('b8e59430-92f6-4e9f-9da9-7a700c5d2087', contemporary_restorationist_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b8e59430-92f6-4e9f-9da9-7a700c5d2087', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, synagogal_prayer_establishment).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, temple_restoration_advocates).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, priestly_lineage_families).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, sacrifice_oriented_lay_pietists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, priestly_lineage_families).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, sacrifice_oriented_lay_pietists).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, avodah_shebalev_substitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Redact, transmit, and adjudicate the Kodashim corpus: set the yeshiva curriculum, authorize commentaries, and rule on how the sacrificial laws relate to present practice. They teach that prayer and study stand in the place of the offerings while the corpus remains the largest continuous object of advanced study; authority, enrollment, and funding accrue to them through this continuity claim, and they control which interpretations of the corpus gain standing. Their flexibility within the system is high — they can reframe the corpus's meaning without leaving it.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, beneficiary).

% Maintains the fixed liturgy whose structure mirrors the Temple service: morning, afternoon, and evening prayers keyed to the daily offerings, festival Mussaf recitations, the Yom Kippur Avodah. The substitution doctrine gives prayer sacrificial-grade standing as 'service of the heart,' and the liturgy embeds the corpus's content in its ordinary text. Dropping the sacrificial referents would rupture the liturgy's own self-understanding, so the establishment holds its benefit on terms it cannot renegotiate.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, synagogal_prayer_establishment, beneficiary,
    organized, generational, constrained, global).

% Organizations and pietists — vessel-fabrication institutes, Temple Mount activist circles, messianically oriented communities — who seek the actual restoration of sacrificial practice. They prepare implements, train claimant priests, and petition for prayer rights on the Mount. Within the academies' frame they are told the system is superseded and that study suffices; on the Mount itself they face legal prohibition. Their aspiration has no legitimate outlet anywhere in the arrangement.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, temple_restoration_advocates, payer,
    moderate, generational, trapped, national).

% Descendants of the priestly line whose hereditary identity is constituted by a service they cannot perform. They retain residual honors from the continuity claim — the priestly blessing, precedence in the Torah reading, genealogical chapters preserved in the archive — while the core function of their lineage remains unavailable. Leaving the identity would mean dissolving a family-defining inheritance; staying means perpetual deferral.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, priestly_lineage_families, payer,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, priestly_lineage_families, beneficiary).

% Ordinary worshippers whose liturgy and longing orient toward the sacrificial service: they recite the offering descriptions daily and fund the academies that study them. The substitution tells them their prayer and study are the service; their devotional energy is redirected into maintaining the archive. In exchange they receive a complete, practiceable religious life that does not depend on the Temple.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, sacrifice_oriented_lay_pietists, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, sacrifice_oriented_lay_pietists, beneficiary).

% Communities that never accepted the rabbinic substitution: the Samaritans, who continued sacrifice on Gerizim without interruption, and the medieval Karaites, who rejected the Oral Torah's authority outright. They stand outside the academies' conversation entirely; their continuing or remembered practice is classed as schism rather than as a rival claim on the sacrificial inheritance.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, samaritan_and_karaite_practitioners, excluded,
    powerless, generational, trapped, regional).

% Scholars of Second Temple and rabbinic history who document how the post-destruction settlement was constructed at Yavneh and after, how the corpus was redacted and canonized, and how the continuity claim functions institutionally. They take no part in the practice and hold no standing in its adjudication.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, academic_historians_of_rabbinic_judaism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__substitution_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a covenantal community that lost its central institution: the corpus and the substitution doctrine key the prayer calendar to the offering schedule, preserve the legal memory of a system whose operating conditions could in principle return, and give dispersed communities a shared textual object that anchors curriculum, calendar, and rabbinic identity across the diaspora.
% TRANSFER_FUNCTION: Moves religious authority and devotional energy from the sacrificial inheritance to the text-study institutions: the aspiration for sacrificial service is redirected into liturgy and study, and the Temple cult's prestige transfers to the academies administering its textual remains. Deference, enrollment, and funding flow from sacrifice-oriented laity, priestly families, and restoration-oriented movements to the rabbinic academies; residual honors flow back to the priestly lineages.
% ABSENT_VOICES: The Samaritan community (whose Gerizim sacrifice never ceased) and the medieval Karaites (who rejected the Oral Torah's authority and with it the substitution) are structurally outside the conversation — their continuing or remembered practice is classed as schism, not as a rival claim. The displaced priestly officiant families were absorbed into honor roles rather than consulted. Restoration advocates are present today but hold no seat in the academies that adjudicate the corpus's meaning; they object from outside the curriculum.
% DISAPPEARANCE_RATIONALE: If the substitution settlement and its archive vanished overnight, the liturgy's structure (services keyed to the offerings, festival Mussaf, the Yom Kippur Avodah), the yeshiva curriculum's largest tractate-order, and the community's standing account of why it does not sacrifice would collapse together; restorationist and non-rabbinic claimants would face no textual counterweight, and post-Temple Judaism would have to be reconstituted from other materials.
% FOUNDING_PROBLEM: After 70 CE the covenant's central institution — the sacrificial service — was physically gone. The community needed either a replacement practice or a theory of suspension: how does a sacrificial covenant continue without sacrifice?
% FOUNDING_PROBLEM_CORROBORATION: The beneficiary institutions attest the problem as solved (the substitution is complete and permanent). Restoration advocates and priestly lineages attest it as live (the service is absent and its absence is felt). Corroboration from outside the beneficiary set: academic historians of the rabbinic period document the constructed character of the post-destruction settlement; the Samaritan community's continuous Gerizim practice demonstrates a non-substitution path was actually lived; the medieval Karaite rejection attests contemporaries who denied the substitution's legitimacy outright. No party outside the dispute attests the settlement as self-evident.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end): the continuity claim converts the sacrificial inheritance into institutional authority — students, funding, and deference flow to the academies that administer the corpus — but the same archive genuinely serves a dispersed community that would otherwise have no common practice or legal memory, which bounds extraction below snare levels. Suppression is moderate (0.48): the closure of alternatives is hermeneutic and institutional (marginalization of restorationist readings, historical herem against non-rabbinic Judaisms) rather than violent. Theater is moderate (0.42): memorial recitation of the offerings, festival Mussaf, and the Yom Kippur Avodah reenactment are commemorative performance layered over a real legal-preservation and liturgical-structuring function. Accessibility_collapse (0.50) reflects alternatives that persist but are marginalized: Samaritan, Karaite, and restorationist paths survive at the system's edges. Resistance (0.50) is real: Karaism was a mass rejection of the substitution, and modern restoration movements actively contest the closure. All three series share one time grid (8 points) so every metric is authored at every examined time point. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: buildup through the Geonic and medieval herem era, relaxation through modernity (the 1800 dip coincides with historicization and the Reform challenge loosening the monopoly), and re-hardening after 1967 when the Temple Mount legal regime and renewed restorationism raised the enforcement burden. The 1800 extraction dip is the same modernization episode, not noise.
 *
 * PERSPECTIVAL GAP:
 *   From the academies' seat the arrangement is faithful transmission: the substitution is the tradition's own self-understanding, and administering the archive is continuity, not capture. From the restoration-advocate and priestly seats the same structure is a locked door: the practice they seek is declared superseded by the very institutions whose standing the declaration sustains, while the corpus's prestige accrues to its custodians. The laity sit between — they receive a complete practiceable religion and pay with redirected devotion. The engine computes these per-seat classifications from the structural data; the divergence between seats is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The academies are the structural beneficiary: they collect the continuity dividend and control the interpretive apparatus, placing them near the beneficiary end with damped effective extraction. The prayer establishment benefits derivatively — sacrificial-grade standing for prayer — but its constrained exit (the liturgy's own restoration petitions bind it) keeps it near, not at, the beneficiary end. Restoration advocates are targets: their aspiration is denied and their exit is trapped (no legitimate outlet anywhere in the arrangement), placing them near the full-target end. Priestly families are dual: residual honors damp their position below the restorationists', but identity-lock removes arbitrage. The laity are near symmetric: real coordination benefit, diffuse devotional cost. The excluded communities sit outside the benefit side entirely — their exclusion is what the enforcement machinery maintains. Beneficiary/victim declarations map directly onto these relationships; no directionality override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two symmetric mislabels. Reading the archive as pure coordination (rope) would erase the extraction: the continuity claim does real work for identifiable beneficiaries while denying restoration to identifiable claimants. Reading it as pure extraction (snare) would erase the coordination: a post-Temple community genuinely needed a common practice, a liturgical calendar, and preserved legal memory, and the archive supplied them at real cost. Mandatrophy is not declared resolved: the founding problem — how a sacrificial covenant continues without sacrifice — is contested, live for the restorationist and priestly seats, declared solved by the beneficiary institutions. If the dispute were ever settled, the archive's character would change decisively: toward rope if the substitution became uncontested, toward piton if the practice were restored and the archive were left maintaining the memory of a live system's shadow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_status_reading_contest,
    'This constraint is one reading of the kodashim_corpus kernel — the substitution_archive reading, which holds the corpus as a memorial archive of a superseded system rather than an occupied kernel. Which characterization of the kernel''s status is correct: superseded-archive (this reading), occupied-through-study (the study_as_exercise sibling), or dormant-blueprint-awaiting-restoration (the performance_only sibling)?',
    'The disagreement is located on two predicates: kernel status (occupied vs archived) and restoration (denied vs awaited). Resolution would come from normative adjudication of the substitution doctrine''s own sources — whether the rabbinic texts present the substitution as completion or as suspension — not from any measurement of the corpus itself.',
    'If the occupied-through-study sibling were adopted, the corpus''s measured extraction drops toward coordination cost (the institutions would be performing the mitzvah, not collecting its inheritance); if the dormant-blueprint sibling were adopted, the restoration advocates become the legitimate claimants and the archive''s denial of restoration becomes the central extraction. This reading''s tangled_rope classification holds only on the memorial-archive premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_status_reading_contest, conceptual, 'Which reading of the kodashim kernel correctly characterizes its status; sibling readings would restructure the victim and beneficiary sets.').

omega_variable(
    restoration_denial_extraction,
    'Is the archive''s denial of living sacrificial practice a good-faith halakhic settlement (sacrifice requires a Temple, purities, and conditions that do not obtain) or an interest-laden monopoly (the conditions are treated as unmeetable because their meeting would end the institutions'' inheritance)?',
    'Examine the halakhic literature''s treatment of resumption conditions (red heifer, site purity, prophetic reauthorization) against institutional behavior: whether the academies actively pursue or quietly defer the conditions whose satisfaction would restore the practice they currently mediate.',
    'If good-faith, the measured extraction is largely the price of a real settlement and the constraint moves toward rope; if interest-laden, the denial is the extraction mechanism itself and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_denial_extraction, empirical, 'Whether the denial of restoration tracks halakhic impossibility or institutional interest.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of restorationist and non-rabbinic sacrificial practice structural (legal prohibition on the Temple Mount, halakhic impossibility, institutional gatekeeping of legitimacy) or internalized (believers experience the substitution as self-evident and do not experience the denial as denial)?',
    'Post-opening trajectory: if restorationist practice proliferates wherever legal or institutional barriers lift (as Temple Mount access politics shift), the suppression was structural; if aspiration fails to convert into practice even where permitted, the substitution has been internalized and the closure travels with the believers.',
    'If internalized, the effective suppression exceeds the structural measure and would persist after institutional barriers fall; if structural, lifting the barriers would rapidly convert suppressed demand into living practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of living sacrificial practice.').

omega_variable(
    victim_seat_ambiguity,
    'Who is the operative victim seat: restorationist advocates within the rabbinic frame, priestly lineages whose identity is deferred, the diffuse laity whose devotion is redirected, or the historically excluded Samaritan and Karaite practitioners?',
    'Trace whose religious aspiration the continuity claim actually forecloses and who bears measurable cost (legal, economic, identity): the victim set is the seat whose alternative practice the enforcement machinery specifically maintains against.',
    'The victim set drives directionality: a restorationist-centered victim set concentrates high extraction on a moderate-power trapped seat; an excluded-community-centered set spreads extraction across marginalized outsiders and lowers the measured concentration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_seat_ambiguity, conceptual, 'Which seat bears the archive''s extraction — determines the directionality derivation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 70, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_corpus__substitution_archive, theater_ratio, 70, 0.12).
narrative_ontology:measurement(koda_tr_t200, kodashim_corpus__substitution_archive, theater_ratio, 200, 0.18).
narrative_ontology:measurement(koda_tr_t600, kodashim_corpus__substitution_archive, theater_ratio, 600, 0.28).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__substitution_archive, theater_ratio, 1000, 0.32).
narrative_ontology:measurement(koda_tr_t1550, kodashim_corpus__substitution_archive, theater_ratio, 1550, 0.36).
narrative_ontology:measurement(koda_tr_t1800, kodashim_corpus__substitution_archive, theater_ratio, 1800, 0.38).
narrative_ontology:measurement(koda_tr_t1967, kodashim_corpus__substitution_archive, theater_ratio, 1967, 0.4).
narrative_ontology:measurement(koda_tr_t2020, kodashim_corpus__substitution_archive, theater_ratio, 2020, 0.42).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_corpus__substitution_archive, base_extractiveness, 70, 0.32).
narrative_ontology:measurement(koda_be_t200, kodashim_corpus__substitution_archive, base_extractiveness, 200, 0.42).
narrative_ontology:measurement(koda_be_t600, kodashim_corpus__substitution_archive, base_extractiveness, 600, 0.5).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__substitution_archive, base_extractiveness, 1000, 0.53).
narrative_ontology:measurement(koda_be_t1550, kodashim_corpus__substitution_archive, base_extractiveness, 1550, 0.55).
narrative_ontology:measurement(koda_be_t1800, kodashim_corpus__substitution_archive, base_extractiveness, 1800, 0.5).
narrative_ontology:measurement(koda_be_t1967, kodashim_corpus__substitution_archive, base_extractiveness, 1967, 0.57).
narrative_ontology:measurement(koda_be_t2020, kodashim_corpus__substitution_archive, base_extractiveness, 2020, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_corpus__substitution_archive, suppression_requirement, 70, 0.2).
narrative_ontology:measurement(koda_su_t200, kodashim_corpus__substitution_archive, suppression_requirement, 200, 0.3).
narrative_ontology:measurement(koda_su_t600, kodashim_corpus__substitution_archive, suppression_requirement, 600, 0.4).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__substitution_archive, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(koda_su_t1550, kodashim_corpus__substitution_archive, suppression_requirement, 1550, 0.46).
narrative_ontology:measurement(koda_su_t1800, kodashim_corpus__substitution_archive, suppression_requirement, 1800, 0.34).
narrative_ontology:measurement(koda_su_t1967, kodashim_corpus__substitution_archive, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(koda_su_t2020, kodashim_corpus__substitution_archive, suppression_requirement, 2020, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial object 'the Kodashim corpus' decomposes into three structurally distinct readings of one kernel, each with its own ε, beneficiary/victim structure, and classification. This (substitution_archive) story authors ε for the standing archive-as-administered arrangement by the memorial reading's lights (moderate: continuity-claim extraction over a real coordination function). The study_as_exercise sibling authors the same corpus as an occupied kernel — its extraction profile collapses toward coordination cost because study IS the practice. The performance_only sibling authors it as a dormant blueprint — its victim set inverts, since restoration advocates become the legitimate claimants. The substitution settlement is the historical mainstream the other two readings contest; family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
