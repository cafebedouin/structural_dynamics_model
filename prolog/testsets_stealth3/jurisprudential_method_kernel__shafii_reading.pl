% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__shafii_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: Four-Tier Source Hierarchy with Hadith Transmission as Arbiter (al-Shafi'i Reading)
 *   domain: legal/institutional/intellectual_history
 *
 * SUMMARY:
 *   Al-Shafi'i's methodological standardization orders legal derivation into
 *   a strict hierarchy — Qur'an, then transmitted Sunna, then consensus, then
 *   analogy — and installs authenticated hadith transmission as the arbiter
 *   that decides which texts bind. Presented as restoring unity to divine
 *   law, the arrangement also demoted entire classes of legal authority: the
 *   living practice of Medina lost source status, juristic preference was
 *   condemned outright, analogy was confined to a last resort, and local
 *   custom became voidable wherever a graded report contradicted it.
 *   Interpretive authority, appointments, and prestige migrated toward the
 *   transmission specialists whose sciences alone could certify texts. KEY
 *   AGENTS (by structural relationship): hadith_transmission_scholars —
 *   primary beneficiary (organized/constrained), authentication monopoly
 *   converts craft into arbiter status; al_shafii_madhhab_institutions —
 *   agenda setter (institutional/identity_locked), administers curricula,
 *   credentialing, and judicial staffing; medinan_practice_jurists — target
 *   seat one (powerful/constrained), practice-as-source demoted to evidence;
 *   analogical_school_jurists — target seat two (powerful/constrained),
 *   preference banned, analogy subordinated; regional_customary_communities —
 *   diffuse targets (powerless/trapped), local forms voidable against graded
 *   reports; rural_oral_tradition_keepers — excluded voice
 *   (powerless/trapped), custom kept without scholarly chains, never seated
 *   in the debate; comparative_usul_historians — analytical observer
 *   (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.62).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.6).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Four-Tier Source Hierarchy with Hadith Transmission as Arbiter (al-Shafi'i Reading)").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "legal/institutional/intellectual_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, '0dac31a2-63df-4241-bc13-cb2d637d6867').
narrative_ontology:cs_kernel_codification('0dac31a2-63df-4241-bc13-cb2d637d6867', formalized).
narrative_ontology:cs_authority_grounding('0dac31a2-63df-4241-bc13-cb2d637d6867', lineage).
narrative_ontology:cs_interpretation_layer_present('0dac31a2-63df-4241-bc13-cb2d637d6867').
narrative_ontology:cs_reading_relation('0dac31a2-63df-4241-bc13-cb2d637d6867', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('0dac31a2-63df-4241-bc13-cb2d637d6867', jurisprudential_method_kernel__maliki_reading, forecloses).
narrative_ontology:cs_reading_relation('0dac31a2-63df-4241-bc13-cb2d637d6867', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('0dac31a2-63df-4241-bc13-cb2d637d6867', foundational, closed_four_tier_source_enumeration).
narrative_ontology:cs_axiom_status(closed_four_tier_source_enumeration, holdable).
narrative_ontology:cs_axiom_grounding('0dac31a2-63df-4241-bc13-cb2d637d6867', closed_four_tier_source_enumeration, deontological).
narrative_ontology:cs_axiom('0dac31a2-63df-4241-bc13-cb2d637d6867', foundational, sunna_transmission_arbitrates_derivation).
narrative_ontology:cs_axiom_status(sunna_transmission_arbitrates_derivation, holdable).
narrative_ontology:cs_axiom_grounding('0dac31a2-63df-4241-bc13-cb2d637d6867', sunna_transmission_arbitrates_derivation, empirically_contingent).
narrative_ontology:cs_axiom('0dac31a2-63df-4241-bc13-cb2d637d6867', secondary, juristic_preference_void_as_source).
narrative_ontology:cs_axiom_status(juristic_preference_void_as_source, holdable).
narrative_ontology:cs_axiom_grounding('0dac31a2-63df-4241-bc13-cb2d637d6867', juristic_preference_void_as_source, conventional).
narrative_ontology:cs_reference_frame('0dac31a2-63df-4241-bc13-cb2d637d6867', strict_revelation_hierarchy_transmission_arbiter).
narrative_ontology:cs_drift_state('0dac31a2-63df-4241-bc13-cb2d637d6867', post_classical_codification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0dac31a2-63df-4241-bc13-cb2d637d6867', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, medinan_practice_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, analogical_school_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, regional_customary_communities).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, isnad_authentication_doctrine).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, prophetic_sunna_binding_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collect, verify, grade, and transmit reports of prophetic speech and action, developing the biographical-evaluation sciences (jarh wa ta'dil) that determine which reports courts and jurists may rely on. Teaching posts, judicial appointments, and preaching income flow through demonstrated competence in these sciences. Decades of training in transmission craft constitute their careers; pivoting to law-derivation by other means would forfeit the standing the sciences confer.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars, beneficiary,
    organized, generational, constrained, global).

% The network of madrasas, charitable endowments, and judicial appointments that teaches the four-tier derivation method, certifies jurists, staffs qadi benches across its heartlands, and controls curricula. The school's self-understanding is fused with the method it transmits: adopting a different source hierarchy would dissolve its distinctive claim against the rival schools, so administration and doctrine reinforce each other.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, al_shafii_madhhab_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Jurists of the Medinan orientation whose authority rested on the city's continuous communal practice as a faithful window onto prophetic usage. The hierarchy demotes that practice from source status to at-best corroborating evidence, obliging them to defend inherited rulings report by report. They retain deep popular followings and, at various periods, state patronage in the Maghreb, Andalusia, and parts of Egypt, but their school's reason for being is precisely the source status the hierarchy withdraws.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, medinan_practice_jurists, payer,
    powerful, generational, constrained, continental).

% Kufan- and Baghdad-oriented jurists who extend sparse texts through systematic analogy and reasoned preference. The hierarchy admits analogy only as a final resort after text, consensus, and transmitted report are exhausted, and condemns juristic preference outright as unauthorized legislation. They held imperial appointments under the Abbasids and later the Ottomans and retain serious institutional weight, but their methodological identity rests on tools the hierarchy demotes or bans.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, analogical_school_jurists, payer,
    powerful, generational, constrained, continental).

% Muslim communities from Yemen to Khurasan whose marriage forms, commercial customs, and ritual practices predate or run parallel to the scholarly corpus. Where an authenticated report conflicts with a local form, the form is void for worship and contract validity regardless of the community's age or continuity. Most comply in formal legal dealings while continuing many practices socially; shifting to another school changes little, and leaving the faith's jurisdiction entirely is unavailable or ruinous.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, regional_customary_communities, payer,
    powerless, generational, trapped, regional).

% Village-level custodians of custom — reciters, elders, families attached to local shrines — who preserve practice without scholarly chains of transmission. The debate over what counts as a source of law was conducted among urban scholarly networks they never entered, yet its outcome relabels their inherited forms as error without their testimony being taken.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, rural_oral_tradition_keepers, excluded,
    powerless, generational, trapped, local).

% Modern academic historians and philologists reconstructing how the derivation method crystallized, testing authentication claims against manuscripts and comparing formative-era polemics across schools. They hold appointments in universities rather than madrasas and take no side in any school's standing; their analyses nonetheless supply the external evidence on which assessments of the hierarchy's history depend.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, comparative_usul_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single epistemic ordering for deriving divine law from revelation: rulings become derivable, checkable, and portable across the caliphate, resolving the formative crisis in which different regional centers produced contradictory rulings attributed to the same texts.
% TRANSFER_FUNCTION: Moves interpretive authority — and the income, appointments, and prestige attached to adjudication — toward certified transmission specialists, and moves the validation of religious practice away from local custom toward authenticated written reports.
% ABSENT_VOICES: Rural oral tradition keepers and ordinary practitioners whose practices stood to be voided had no seat in the methodological debate, which was conducted among urban scholar networks; women's participation in formal transmission chains narrowed over the formative period; the communities affected were objects of rulings rather than parties to the argument.
% DISAPPEARANCE_RATIONALE: If the four-tier hierarchy vanished overnight, legal derivation would reorganize around the competing regional methodologies it displaced: Medinan practice-jurisprudence, Kufan analogism, and literalist report-centrism would resume as full rivals, madrasa curricula would lose their spine, hadith scholarship would lose its arbiter premium, and previously voided local practices would regain contested validity — substantially the pre-al-Shafi'i landscape.
% FOUNDING_PROBLEM: The formative inconsistency crisis: divergent regional legal traditions producing mutually contradictory rulings while each claimed fidelity to the same revelation, threatening law's claim to divine unity. Al-Shafi'i built the ordered hierarchy to make derivation uniform and text-bound.
% FOUNDING_PROBLEM_CORROBORATION: Rival-school polemic literature corroborates the founding crisis while disputing the solution: Hanafi and Maliki writers conceded inter-regional inconsistency was real even as they rejected hadith exclusivity as arbiter, and Ibn Rushd's cross-school survey documents the disagreement from a comparative seat. Modern academic history (Schacht, Calder, Melchert lineage) independently corroborates both the founding crisis and the contention that the arrangement's center of gravity has shifted from resolving inconsistency toward maintaining transmission-scholarly standing. No attestation comes from inside the beneficiary class alone.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__shafii_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.62 because the hierarchy's costs concentrate on identifiable seats — two jurist classes lost source status and a vast population of customary practice became voidable — while its unification benefit is real but diffusely distributed. Suppression is 0.60 as an unscaled structural property: the framework forecloses rival sources doctrinally (preference condemned as unauthorized legislation, Medinan practice denied source standing) and historically required appointment politics and curricular control to hold, though rival schools survived outside it. Theater is low (0.22): the hierarchy performs real derivational work daily; the slow rise across the series reflects growing formal compliance signaling as later fiqh quietly re-absorbed custom through recognized devices while the source theory stayed austere — substance re-localizing behind an unchanged facade. Accessibility_collapse is 0.48: inside the framework the alternatives collapse almost completely (once the closed enumeration is accepted, preference and practice-as-source are simply unavailable), but exit to a rival madhhab remained genuinely available, so collapse stops well short of natural-law completeness. Resistance is 0.58: sustained multi-century polemic, rival-school survival under intermittent state patronage, and eventual institutional pluralism. The three metric series share one time grid (every tracked metric authored at every point 0–600) so no end-state value leaks backward into earlier rows. Claim and metrics are authored independently: the tangled_rope claim states what I believe is structurally true; the metrics state what I believe is descriptively true of the operation; the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the transmission scholars' seat the hierarchy is the only honest route to revelation — everything else is conjecture dressed as law — and the same structure reads, from the Medinan and analogist seats, as the confiscation of their schools' founding assets. The madhhab institutions experience it as constitutional self-definition rather than either benefit or burden. Lay customary communities mostly never encounter the hierarchy as theory at all; they meet it as a ruling that a wedding form or a market practice is suddenly void. The analytical observers see all of this at once, which is why the computed per-seat classifications from these data should diverge sharply between payer seats, beneficiary seat, and agenda-setter seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission scholars are declared beneficiaries: the arbiter function subsidizes them directly, placing them near the beneficiary end of directionality. The madhhab institutions set and enforce the arrangement and collect administrative stability from it, but they also bear the burden of defending it against rival schools — nearer symmetric, tilted toward benefit. Medinan practice jurists and analogist jurists are declared victims with powerful-but-constrained positions: their exit is blocked because their schools' identities rest on the very sources the hierarchy demotes, so their effective extraction runs near the full-target end despite their power. Regional customary communities are victims with no exit at all — trapped between schools that differ little on source closure and an apostasy barrier — putting them nearest the full-target end of anyone. Rural tradition keepers are excluded rather than coordinated: the enforcement object includes silencing exactly the claim they would press. The historian seat is analytical and direction-neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — uniform, checkable derivation from revelation — is still performed daily wherever this usul governs, so the arrangement is not mandatrophy-resolved and the flag stays unset. The tangled_rope classification guards both mislabeling directions: reading it as pure rope erases the identifiable losers (two jurist classes and the customary-practice populations whose forms became voidable); reading it as pure snare erases the genuine coordination achievement that survives — rulings derivable identically in Fez and Samarqand, a property no rival arrangement of the era delivered. Piton is implausible: theater is low and the derivational function is exercised, not merely performed. On the R5 mismatch consumer, founding_problem_status=contested paired with disappearance_verdict=world_rearranges signals a live mandate with disputed completion, not a zombie: no dead-problem-plus-dependence capture flag fires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading (shafii_reading) of the jurisprudential_method_kernel; how much of the measured structure is indexical to this reading rather than to ''Islamic legal methodology'' generally?',
    'Cross-file comparison with the sibling readings (hanafi_reading, maliki_reading, hanbali_reading), each carrying its own epsilon and victim set over the same historical terrain; no within-file resolution exists.',
    'Epsilon here measures the transmission-arbiter hierarchy specifically. A maliki reading would shift the victim set toward hadith-exclusivity costs and validate customary practice; a hanafi reading would raise extraction on report-exclusivity instead. Averaging across readings would fabricate a constraint no party''s commitments contain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of epsilon within a contested kernel').

omega_variable(
    hadith_corpus_authentication_stability,
    'How much of the hierarchy''s arbiter function survives modern scrutiny of hadith authenticity (chain analysis, common-link criticism, matn criticism, manuscript dating), given that medium-high epsilon rides on authentication doing what it claims?',
    'Systematic cross-corpus manuscript and textual analysis of early hadith collections against the classical authentication gradings.',
    'Broad authentication failure collapses the arbiter premium and redistributes epsilon toward whichever seat holds replacement authority; narrowly contained failures leave the structure and its classification essentially intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_corpus_authentication_stability, empirical, 'Whether the authentication layer underlying arbiter status holds under modern scrutiny').

omega_variable(
    unification_extraction_separability,
    'Is the hierarchy''s unification benefit separable from transmission-arbitration — could a unified usul coordinate derivation while admitting Medinan practice and juristic preference as subordinate sources?',
    'Counterfactual comparison with pluralist legal arrangements that maintained cross-regional consistency without source closure (Ottoman kanun-and-fiqh practice, maqasid-based codification projects).',
    'If separable, the extraction is rent on an authentication monopoly and the tangled_rope reading stands firmly; if inseparable, part of the measured cost is the price of coherence itself and the profile shifts toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unification_extraction_separability, conceptual, 'Separability of coordination benefit from extraction mechanism').

omega_variable(
    suppression_mechanism_structure,
    'Is the suppression of rival methodologies structural (appointment politics, curricular control, post-exclusion) or internalized (professional socialization in juristic etiquette that produces deference without enforcement)?',
    'Compare deference patterns across regimes and periods where enforcement machinery varied widely — Isma''ili Fatimid rule, Mongol-era Iran, Ottoman millet pluralism — holding doctrine constant.',
    'If deference tracks enforcement presence, suppression is structural and falls with enforcement decay; if constant, the internalized component raises effective suppression above the structural measure and persists through institutional turnover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structure, conceptual, 'Structural versus internalized suppression of rival juristic methods').

omega_variable(
    absorption_vs_displacement_direction,
    'Did the hierarchy durably displace customary and analogical sources, or absorb them back through recognized devices (restricted application of solitary reports, acknowledged custom in transactions) while the source theory stayed austere?',
    'Trace the treatment of custom and restricted reports across classical fiqh manual generations against the parallel usul literature.',
    'If absorption dominates, the rising theater series reflects substance re-localizing behind formal compliance — softening the durable victimhood of the customary seats; if displacement dominates, the victim declarations describe lasting confiscation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_vs_displacement_direction, empirical, 'Whether later practice absorbed back what the source theory displaced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(juri_tr_t0, observed).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__shafii_reading, theater_ratio, 100, 0.09).
narrative_ontology:measurement_basis(juri_tr_t100, observed).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__shafii_reading, theater_ratio, 200, 0.13).
narrative_ontology:measurement_basis(juri_tr_t200, observed).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__shafii_reading, theater_ratio, 300, 0.16).
narrative_ontology:measurement_basis(juri_tr_t300, observed).
narrative_ontology:measurement(juri_tr_t400, jurisprudential_method_kernel__shafii_reading, theater_ratio, 400, 0.19).
narrative_ontology:measurement_basis(juri_tr_t400, observed).
narrative_ontology:measurement(juri_tr_t500, jurisprudential_method_kernel__shafii_reading, theater_ratio, 500, 0.21).
narrative_ontology:measurement_basis(juri_tr_t500, observed).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__shafii_reading, theater_ratio, 600, 0.22).
narrative_ontology:measurement_basis(juri_tr_t600, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(juri_be_t0, observed).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 100, 0.44).
narrative_ontology:measurement_basis(juri_be_t100, observed).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 200, 0.53).
narrative_ontology:measurement_basis(juri_be_t200, observed).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 300, 0.59).
narrative_ontology:measurement_basis(juri_be_t300, observed).
narrative_ontology:measurement(juri_be_t400, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 400, 0.62).
narrative_ontology:measurement_basis(juri_be_t400, observed).
narrative_ontology:measurement(juri_be_t500, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 500, 0.62).
narrative_ontology:measurement_basis(juri_be_t500, observed).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 600, 0.62).
narrative_ontology:measurement_basis(juri_be_t600, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(juri_su_t0, observed).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 100, 0.63).
narrative_ontology:measurement_basis(juri_su_t100, observed).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 200, 0.67).
narrative_ontology:measurement_basis(juri_su_t200, observed).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 300, 0.7).
narrative_ontology:measurement_basis(juri_su_t300, observed).
narrative_ontology:measurement(juri_su_t400, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 400, 0.69).
narrative_ontology:measurement_basis(juri_su_t400, observed).
narrative_ontology:measurement(juri_su_t500, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 500, 0.64).
narrative_ontology:measurement_basis(juri_su_t500, observed).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 600, 0.6).
narrative_ontology:measurement_basis(juri_su_t600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, information_standard).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Islamic legal methodology' covers four structurally distinct source-hierarchy claims and is decomposed into four files sharing the kernel_id, linked pairwise through affects_constraints. The Shafi'i reading is the formalizing node: its closed enumeration and transmission-arbiter axiom are the moves the other three readings define themselves against, so this story sits causally upstream of the siblings' defensive formations even though each sibling remains an independently epsilon-invariant constraint with its own beneficiaries and victims. Epsilon differs across the family because the victim sets differ: this reading's losses concentrate on practice-as-source and juristic preference; the siblings' concentrate elsewhere. No single story can carry the family without violating epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
