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
 *   human_readable: Substitution Archive Reading of the Kodashim Corpus
 *   domain: religious/commitment_system
 *
 * SUMMARY:
 *   When Rome destroyed the Second Temple in 70 CE, the sacrificial cult —
 *   the organizing center of Israelite worship, atonement, and pilgrimage —
 *   became impossible to operate. Over the following centuries the rabbinic
 *   movement established that prayer ('the service of the heart') and Torah
 *   study discharge the sacrificial obligation, and it kept the Kodashim
 *   orders (the Mishnaic and Talmudic corpora of sacrifice law) in the
 *   curriculum and the liturgy. This story instantiates ONE reading of that
 *   arrangement — the substitution_archive reading: prayer and study did not
 *   merely occupy the old kernel, they replaced it, and the Kodashim corpus
 *   survives as a memorial archive documenting what was superseded, not as an
 *   occupied kernel. On this reading the archive's continuity claim ('this is
 *   the same service, relocated') is the load-bearing ambiguity: it
 *   legitimates the replacement by denying that a replacement occurred, while
 *   the enforcement machinery (bans on extramural altars, excommunication of
 *   rejectionists) forecloses living sacrificial practice. The epsilon
 *   referent is the standing substitution arrangement as this reading
 *   assesses it — not the restoration the performance_only sibling awaits nor
 *   the performative identification the study_as_exercise sibling asserts;
 *   those are separate constraint files linked in
 *   network.affects_constraints. The claimed type and the metric values are
 *   authored independently: tangled_rope is asserted from structure (genuine
 *   coordination function, identifiable payers, active enforcement), while
 *   the metrics describe the arrangement's operation as this reading observes
 *   it.
 *
 * KEY AGENTS:
 *   - rabbinic_text_study_institutions: Primary agenda-setter and beneficiary (institutional/identity_locked) — administers the archive, collects the study economy, cannot exit without self-dissolution
 *   - rabbinic_courts: Enforcement beneficiary (institutional/constrained) — bans extramural sacrifice, excommunicates rejectionists, gains jurisdiction
 *   - living_sacrifice_seekers: Primary target (moderate/constrained) — told the practice they seek is obsolete by the institutions that collect from the telling
 *   - karaite_samaritan_practitioners: Secondary target (organized/trapped) — rejected substitution, bore exclusion for generations
 *   - lay_worshippers: Dual-positioned (organized/constrained) — receives portable low-barrier service, pays in mediated ritual autonomy
 *   - messianic_restoration_advocates: Excluded voice (moderate/trapped) — would restore practice now, never seated in the codification
 *   - historical_liturgy_scholars: Analytical observer (analytical/analytical) — documents the constructed continuity from outside the authority structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.58).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.55).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Substitution Archive Reading of the Kodashim Corpus").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, '3697e7c9-1bba-4301-88bf-03ec2dae1beb').
narrative_ontology:cs_kernel_codification('3697e7c9-1bba-4301-88bf-03ec2dae1beb', formalized).
narrative_ontology:cs_authority_grounding('3697e7c9-1bba-4301-88bf-03ec2dae1beb', lineage).
narrative_ontology:cs_interpretation_layer_present('3697e7c9-1bba-4301-88bf-03ec2dae1beb').
narrative_ontology:cs_reading_relation('3697e7c9-1bba-4301-88bf-03ec2dae1beb', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('3697e7c9-1bba-4301-88bf-03ec2dae1beb', kodashim_corpus__performance_only, influences).
narrative_ontology:cs_axiom('3697e7c9-1bba-4301-88bf-03ec2dae1beb', foundational, prayer_and_study_discharge_sacrificial_obligation).
narrative_ontology:cs_axiom_status(prayer_and_study_discharge_sacrificial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3697e7c9-1bba-4301-88bf-03ec2dae1beb', prayer_and_study_discharge_sacrificial_obligation, conventional).
narrative_ontology:cs_axiom('3697e7c9-1bba-4301-88bf-03ec2dae1beb', secondary, kodashim_memorial_status_supersedes_operative_reading).
narrative_ontology:cs_axiom_status(kodashim_memorial_status_supersedes_operative_reading, holdable).
narrative_ontology:cs_axiom_grounding('3697e7c9-1bba-4301-88bf-03ec2dae1beb', kodashim_memorial_status_supersedes_operative_reading, conventional).
narrative_ontology:cs_reference_frame('3697e7c9-1bba-4301-88bf-03ec2dae1beb', superseded_memorial_archive).
narrative_ontology:cs_drift_state('3697e7c9-1bba-4301-88bf-03ec2dae1beb', contemporary_temple_activism_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('3697e7c9-1bba-4301-88bf-03ec2dae1beb', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_courts).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, living_sacrifice_seekers).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, karaite_samaritan_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, lay_worshippers).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, lay_worshippers).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, prayer_substitutes_for_sacrifice).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, torah_study_surpasses_sacrifice).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, rabbinic_continuity_with_temple_cult).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Received the sacrificial curriculum into the academy after the Temple fell; authored and transmitted the doctrine that prayer and Torah study discharge the sacrificial obligation; set the canon (the Kodashim orders of Mishnah and Talmud) and the daily schedule that keeps it recited. Collect students, endowments, and the authority that flows from being sole custodian of how service is rendered now. Cannot abandon the archive without dissolving the institution itself — the academy has become the archive's keeper, and the archive justifies the academy.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, beneficiary).

% Administer the enforcement side of the arrangement: halakhic bars on altars outside the Temple site, excommunication (herem and niddui) of those who reject the substitution or attempt living sacrifice, jurisdiction over the prayer and study obligations that replaced pilgrimage and offering. Gain expanded jurisdiction — a service system administered through courts and ordinance rather than a cult administered through priests. Their exit is constrained because their authority is derivative of the same continuity claim they enforce.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_courts, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, rabbinic_courts, agenda_setter).

% Individuals and movements who want to perform the actual practice — bring an offering, build an altar, slaughter the paschal lamb. Are told the practice is obsolete, fulfilled by other means, or forbidden pending restoration. Their options: accept the substitution and abandon the desired act, violate communal norms and face sanction, or channel the impulse into the approved study and prayer that the institutions provide. Some organize (Temple-preparation societies) but remain marginal to the mainstream.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, living_sacrifice_seekers, payer,
    moderate, biographical, constrained, regional).

% Communities that rejected the substitution doctrine outright — Samaritans continuing sacrifice at Gerizim, Karaites insisting on literal observance wherever possible. Bore excommunication, polemical exclusion, and centuries of marginalization for denying that text replaced altar. Trapped: their communal identity is constituted by the rejection; entering the rabbinic system would erase them, and remaining outside costs them standing in every century since.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, karaite_samaritan_practitioners, payer,
    organized, generational, trapped, regional).

% Receive a portable, low-barrier service: three daily prayers timed to the former offerings, no animals required, no priesthood required, no pilgrimage required — covenantal participation available anywhere on earth. Pay by having their service defined, scheduled, and mediated by the institutions, and by inheriting the doctrine that whatever sacrificial impulse they feel is already fulfilled by proxy. Leaving (secularization, conversion) carries heavy identity cost.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, lay_worshippers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, lay_worshippers, payer).

% Would restart sacrifice now or prepare its imminent resumption; are treated as premature, dangerous, or beyond the pale by the institutional mainstream. Were never seated in the codification or adjudication of the substitution doctrine; their objection — that the archive is a blueprint rather than a memorial — is answered with rulings and social sanction rather than argument.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, messianic_restoration_advocates, excluded,
    moderate, generational, trapped, regional).

% Document the actual sequence: sacrificial practice ceasing under Roman repression, substitution doctrines crystallizing over generations rather than arriving whole, and competing early positions — some communities holding fast to longing and resumption, others pragmatizing replacement. See the continuity claim as a constructed narrative layered over a contested transition, and publish from outside the authority structure.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, historical_liturgy_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__substitution_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: After the altar was destroyed, the covenantal service system needed a portable center: fixed prayer times mapped onto the former daily offerings, Torah study constituted as service, calendar and communal identity maintained without Temple or priesthood. The substitution solved the collective-action problem of sustaining national-religious identity and daily practice across a diaspora with no territorial cult — a problem no faction, including the rejectionists, denied existed.
% TRANSFER_FUNCTION: Moves the locus of obligatory service from the altar to the academy and synagogue: time formerly given to pilgrimage and offering is given to prayer and study; resources formerly flowing to priests and Temple flow as tuition, endowment, and allegiance to text-study institutions; interpretive authority over what counts as service moves from priestly lineage to rabbinic ordination.
% ABSENT_VOICES: Those who would continue or restore sacrifice were not seated when the substitution was codified: Samaritan communities (whose Gerizim practice the doctrine implicitly demotes), Karaite reformers (excommunicated for rejecting it), and later restoration advocates (ruled premature or dangerous). Their objection — that substitution is innovation dressed as continuation — was answered with ban and polemic rather than inclusion; the historical_liturgy_scholars seat records the transition they were written out of.
% DISAPPEARANCE_RATIONALE: If the substitution arrangement vanished overnight, rabbinic Judaism loses its service structure: the daily liturgy is timed to offerings that no longer stand in for anything, the yeshiva curriculum loses the Kodashim orders that anchor it, rabbinic authority over worship loses its warrant, and every community faces an unanswered question — how do we serve, tonight, with no altar and no doctrine? Prayer and study would not stop, but their obligating frame, schedule, and institutional custody would rearrange.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) terminated the sacrificial system around which observance, atonement, festival, and national identity were organized. The arrangement was built to answer: how does the covenant continue — how is sin addressed, how is daily service rendered — without an altar?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the benefiting parties: Josephus documents the cessation of sacrifice after 70 CE; patristic sources (Justin Martyr's Dialogue with Trypho) attest Jewish discontinuation of offering; Samaritan and Karaite communities — opponents of the rabbinic solution — independently acknowledge the interruption while disputing the answer. What no outsider attests is that substitution is the adequate answer; that claim rests on the beneficiary institutions' own authority, and corroboration for the problem's liveness comes precisely from parties who deny the solution.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.58): the substitution moved the service economy — time, tuition, allegiance, interpretive authority — from an altar open to lay bringers into institutions that alone define what counts as service, and the continuity claim obscures that transfer. Suppression (0.55) is a mixture: structural instruments (herem and niddui against rejectionists, halakhic bars on bamot and on sacrifice outside the Temple site) plus doctrinal training that renders the sacrificial impulse foreign; the temporal series shows enforcement capacity rising through the Geonic-era campaigns against dissent and then decaying into internalized conformity. Theater ratio (0.42): within this reading the archive's documentary and memorial function is real, but a large share of its maintenance is performative continuity — korbanot recited as if offered, laws studied as if operative — sustaining the appearance that nothing was replaced. Accessibility collapse (0.52): alternatives persist (Samaritan practice, Karaite literalism, restorationism) but sit outside respectability. Resistance (0.48): eight centuries of schism and a modern revival movement keep the arrangement contested. All three tracked series run on one shared seven-point grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the academy seat the arrangement is the thing that saved the covenant: portable service, no altar needed, identity intact across exile — a coordination achievement its custodians quite reasonably experience as gift rather than extraction. From the seeker and rejectionist seats the same structure operates as a locked door: the practice they want is declared obsolete by the very institutions that profit from the declaration, and the continuity claim makes protest look like heresy rather than disagreement. Lay worshippers straddle: they received a genuinely lower-barrier service and lost direct ritual agency in the same transaction. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: rabbinic_text_study_institutions and rabbinic_courts — both derive low directionality (the constraint subsidizes them: students, jurisdiction, authority). Victims declared: living_sacrifice_seekers and karaite_samaritan_practitioners — high directionality, amplified by constrained and trapped exits respectively; the identity-locked rejectionist communities sit nearest the full-target end. Lay worshippers carry dual beneficiary/payer declarations and derive near-symmetric treatment: genuine portable-service benefit, diffuse mediated cost. Messianic restoration advocates are excluded rather than coordinated — their exclusion is partly the enforcement object itself. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope (the global scope of the institutional seats modestly amplifies effective extraction on the trapped regional targets).
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure rope would erase the payers — the people told their sought practice is obsolete by institutions that collect from the telling. Reading it as pure snare would erase the real coordination achievement — a portable, low-barrier service that carried a diaspora nation for two millennia. Tangled rope holds both halves: a genuine identity-coordination function (membership and boundary maintenance without a territorial cult) plus asymmetric extraction running through the same structure. On mandatrophy: the founding problem (how to serve without an altar) is still live — the Temple has not been rebuilt — so no dead-mandate zombie flag fires; the mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds them consistent. The residual question is whether the arrangement is transitional rather than settled — routed to the messianic_contingency omega rather than asserted here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kodashim_corpus kernel (substitution_archive); would instantiating the study_as_exercise or performance_only reading instead yield a different constraint with different epsilon, different victim sets, and a different classification?',
    'Author and compile the sibling files; compare computed per-seat classifications and epsilon across the three readings of the same kernel.',
    'study_as_exercise would likely lower measured extraction (study itself is the mitzvah, no replacement concealed); performance_only would reframe the archive as transitional scaffolding awaiting restoration, moving the victim set toward those denied restoration and shifting classification toward the scaffold/snare boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story is one of three readings of the Kodashim kernel; the disagreement is located in kernel occupancy.').

omega_variable(
    continuity_claim_veracity,
    'Is the continuity claim — that prayer and study continue the Temple service rather than replacing it — a genuine theological truth within the tradition''s own warrants, a rabbinic legal fiction, or a legitimating narrative constructed after the fact?',
    'Comparative liturgical-historical analysis of Second Temple practice versus emergent rabbinic prayer; reading the earliest substitution texts (Mishnah Taanit, Bavli Megillah and Berakhot) against the timeline of enforced consolidation.',
    'If the continuity claim is constructed, the concealment component of extraction is confirmed and epsilon rises; if theologically genuine, part of the measured extraction is misattributed cover rather than cover-up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_claim_veracity, empirical, 'Whether the archive''s continuity claim tracks reality or legitimates a replacement.').

omega_variable(
    victim_scope_ambiguity,
    'Who exactly bears the cost of being told the practice is obsolete — only would-be sacrificers, or the whole laity whose pre-rabbinic ritual agency (private offering, pilgrimage choice, priest-independent atonement) was transferred to institutional mediation?',
    'Historical analysis of lay ritual participation before and after 70 CE; comparison of atonement pathways available to a commoner under the cult versus under the substitution regime.',
    'A wider victim set raises effective extraction on the lay seat and pushes the computed classification toward the snare boundary; a narrow set supports the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_scope_ambiguity, empirical, 'Scope of the payer class behind ''told it''s obsolete.''').

omega_variable(
    suppression_internalization,
    'Is the measured suppression structural (herem, bans on extramural altars, legal bars on sacrifice) or internalized (generations trained to experience the sacrificial impulse as foreign, primitive, or already fulfilled)?',
    'Post-exit suppression trajectory: communities that left the rabbinic orbit (Karaites, Samaritans) retained or resumed sacrificial orientation quickly, indicating a strong structural component; the near-absence of sacrificial longing among modern assimilated populations indicates deep internalization. Weigh both trajectories together.',
    'If largely internalized, effective suppression exceeds the structural measure and persists even where enforcement has decayed — matching the falling tail of the suppression_requirement series; if largely structural, removing enforcement would rapidly revive alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized suppression mechanism in the substitution regime.').

omega_variable(
    messianic_contingency,
    'If the Temple were restored, would the archive revert to an operative kernel — making this reading''s memorial claim provisional and the arrangement transitional rather than settled?',
    'Doctrinal analysis: mainstream positions (e.g., Maimonides) hold that sacrifice resumes in the messianic era while prayer suffices meanwhile — implying official provisionality; performance-oriented movements treat that provisionality as the main point.',
    'If supersession is officially provisional, the arrangement carries scaffold characteristics (transition support with a deferred sunset) and the memorial-archive framing understates its transitional nature; if supersession is treated as permanent, the tangled_rope reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_contingency, conceptual, 'Whether the substitution is provisional (scaffold-flavored) or permanent, contingent on restoration doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.15).
narrative_ontology:measurement(koda_tr_t10, kodashim_corpus__substitution_archive, theater_ratio, 10, 0.2).
narrative_ontology:measurement(koda_tr_t20, kodashim_corpus__substitution_archive, theater_ratio, 20, 0.26).
narrative_ontology:measurement(koda_tr_t30, kodashim_corpus__substitution_archive, theater_ratio, 30, 0.32).
narrative_ontology:measurement(koda_tr_t40, kodashim_corpus__substitution_archive, theater_ratio, 40, 0.38).
narrative_ontology:measurement(koda_tr_t50, kodashim_corpus__substitution_archive, theater_ratio, 50, 0.41).
narrative_ontology:measurement(koda_tr_t60, kodashim_corpus__substitution_archive, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(koda_be_t10, kodashim_corpus__substitution_archive, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(koda_be_t20, kodashim_corpus__substitution_archive, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(koda_be_t30, kodashim_corpus__substitution_archive, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(koda_be_t40, kodashim_corpus__substitution_archive, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(koda_be_t50, kodashim_corpus__substitution_archive, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(koda_be_t60, kodashim_corpus__substitution_archive, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(koda_su_t10, kodashim_corpus__substitution_archive, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(koda_su_t20, kodashim_corpus__substitution_archive, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(koda_su_t30, kodashim_corpus__substitution_archive, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(koda_su_t40, kodashim_corpus__substitution_archive, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(koda_su_t50, kodashim_corpus__substitution_archive, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(koda_su_t60, kodashim_corpus__substitution_archive, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Kodashim after the Temple' conflates three structurally distinct claims — the corpus as occupied-through-study (study_as_exercise), as memorial archive of a completed replacement (this file), and as dormant blueprint awaiting restoration (performance_only). Each carries its own epsilon, victim set, and claimed type; the substitution reading, being the most institutionally entrenched, influences the legitimacy conditions of the other two without foreclosing either. Sibling files link back through their own network.affects_constraints arrays.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
