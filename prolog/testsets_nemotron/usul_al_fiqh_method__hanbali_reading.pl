% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Usul al-Fiqh: Maximal Textual Restrictiveness
 *   domain: Islamic Jurisprudence / Legal Theory / Comparative Law
 *
 * SUMMARY:
 *   The Hanbali reading of usul al-fiqh presents itself as the maximalist
 *   defense of textual authority: Quran and authenticated hadith are the only
 *   legislative sources; qiyas (analogical reasoning) is confined to cases of
 *   clear textual silence; even weak hadith is preferred over independent
 *   reasoning; and sadd al-dhara'i (blocking the means to prohibited
 *   outcomes) operates as a preventive barrier against innovation (bid'a).
 *   This reading claims to coordinate the umma around a fixed textual anchor,
 *   preventing the fragmentation that unchecked juristic discretion would
 *   produce. The authored metrics tell a different story: extraction has
 *   risen steadily (0.12 → 0.22) as the method was weaponized by state-backed
 *   institutions (Saudi religious establishment, Gulf fatwa councils) to
 *   monopolize interpretive authority and block reformist, customary, and
 *   rationalist alternatives. Suppression has hardened (0.35 → 0.48) through
 *   judicial codification, curriculum control, and the bid'a accusation
 *   apparatus. Theater has crept up (0.08 → 0.18) as the coordination
 *   function (textual continuity) becomes a thinner cover for the extraction
 *   function (institutional gatekeeping). The claim (tangled_rope) and
 *   metrics are independent — the engine computes the seat-wise types.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.22).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.48).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Usul al-Fiqh: Maximal Textual Restrictiveness").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "Islamic Jurisprudence / Legal Theory / Comparative Law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, 'e2b283fe-1575-43dc-a011-389aa6199204').
narrative_ontology:cs_kernel_codification('e2b283fe-1575-43dc-a011-389aa6199204', formalized).
narrative_ontology:cs_authority_grounding('e2b283fe-1575-43dc-a011-389aa6199204', lineage).
narrative_ontology:cs_interpretation_layer_present('e2b283fe-1575-43dc-a011-389aa6199204').
narrative_ontology:cs_reading_relation('e2b283fe-1575-43dc-a011-389aa6199204', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2b283fe-1575-43dc-a011-389aa6199204', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2b283fe-1575-43dc-a011-389aa6199204', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_axiom('e2b283fe-1575-43dc-a011-389aa6199204', foundational, textual_sources_maximally_restrictive).
narrative_ontology:cs_axiom_status(textual_sources_maximally_restrictive, holdable).
narrative_ontology:cs_axiom_grounding('e2b283fe-1575-43dc-a011-389aa6199204', textual_sources_maximally_restrictive, deontological).
narrative_ontology:cs_axiom('e2b283fe-1575-43dc-a011-389aa6199204', foundational, qiyas_minimized_to_textual_silence).
narrative_ontology:cs_axiom_status(qiyas_minimized_to_textual_silence, holdable).
narrative_ontology:cs_axiom_grounding('e2b283fe-1575-43dc-a011-389aa6199204', qiyas_minimized_to_textual_silence, deontological).
narrative_ontology:cs_axiom('e2b283fe-1575-43dc-a011-389aa6199204', secondary, weak_hadith_preferred_over_qiyas).
narrative_ontology:cs_axiom_status(weak_hadith_preferred_over_qiyas, holdable).
narrative_ontology:cs_axiom_grounding('e2b283fe-1575-43dc-a011-389aa6199204', weak_hadith_preferred_over_qiyas, conventional).
narrative_ontology:cs_axiom('e2b283fe-1575-43dc-a011-389aa6199204', secondary, sadd_al_dhara_i_preserves_textual_fidelity).
narrative_ontology:cs_axiom_status(sadd_al_dhara_i_preserves_textual_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('e2b283fe-1575-43dc-a011-389aa6199204', sadd_al_dhara_i_preserves_textual_fidelity, instrumental).
narrative_ontology:cs_reference_frame('e2b283fe-1575-43dc-a011-389aa6199204', classical_hanbali_usul_synthesis).
narrative_ontology:cs_drift_state('e2b283fe-1575-43dc-a011-389aa6199204', modern_institutional_codification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2b283fe-1575-43dc-a011-389aa6199204', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, conservative_fiqh_institutions).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, anti_bid_a_advocates).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_law_practitioners).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, legal_reformists).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, textual_primacy_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, bid_a_restriction_principle).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, sadd_al_dhara_i_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars and institutions (e.g., traditional Hanbali madrasas, Salafi academies, Saudi religious establishment) whose authority and interpretive monopoly rest on the claim that legal derivation must not exceed explicit textual warrant. They administer the constraint by controlling fatwa issuance, judicial appointments, and curricular standards. Their professional identity is fused to the textualist method; exit means abandoning the epistemic framework that constitutes their legitimacy.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, textualist_scholars, beneficiary,
    institutional, generational, identity_locked, global).

% Formal bodies (Council of Senior Scholars in Saudi Arabia, Islamic Fiqh Academy branches aligned with Hanbali methodology, fatwa councils in Gulf states) that codify and enforce the methodological boundary. They set the agenda for what counts as valid ijtihad, vet judicial reasoning, and determine which innovations are blocked. They collect institutional rent from being the gatekeepers of 'authentic' methodology.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, conservative_fiqh_institutions, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, conservative_fiqh_institutions, beneficiary).

% Activist networks, preaching movements, and online communities that mobilize the textualist method as a weapon against religious innovation. They benefit from the constraint's rhetorical power — it supplies a ready-made schema for declaring any disliked practice as bid'a. Their exit is constrained by reputational investment in the anti-innovation brand.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, anti_bid_a_advocates, beneficiary,
    organized, biographical, constrained, global).

% Scholars trained in or sympathetic to rationalist usul (qiyas expansion, maslaha reasoning, maqasid al-shari'a) who find their methodological tools delegitimized by the textualist boundary. They pay in excluded arguments, denied appointments, and the need to frame all reasoning as textual exegesis. Exit means moving to academic institutions outside the traditional authority structure or adopting the textualist vocabulary instrumentally.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, global).

% Local judges, muftis, and community elders in regions where customary practice ('urf, 'adah) historically governed family, property, and commercial disputes. The textualist constraint treats their living tradition as presumptively invalid unless a specific text authorizes it. They are trapped because state courts increasingly impose the textualist standard, and their communities lack the resources to sustain parallel dispute resolution.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_law_practitioners, payer,
    powerless, biographical, trapped, regional).

% Reform-oriented jurists, women's rights advocates, and modernist thinkers who need methodological space for gender-equal inheritance, financial innovation, or political participation rulings. The textualist constraint blocks the analogical and maslaha pathways they would use. They are payers (their projects are blocked) and excluded (their voices are treated as bid'a-adjacent). Exit means migrating to secular legal frameworks or minority fiqh enclaves.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, legal_reformists, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, legal_reformists, excluded).

% Academic scholars of Islamic law (Western university departments, IIUM, al-Azhar researchers, independent institutes) who map the methodological landscape without being bound by any single school's authority. They see the full structure: the coordination function (preserving textual continuity), the extraction (monopolizing interpretive authority), and the historical contingency of the Hanbali synthesis.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, comparative_fiqh_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the Quran and Sunna as the sole unmediated legislative source, preventing unlimited juristic discretion from rewriting the law. By minimizing qiyas, preferring weak hadith over opinion, and blocking avenues to innovation (sadd al-dhara'i), the method coordinates Muslim communities across time and space around a fixed textual anchor — the same texts, the same core rulings, the same boundary against bid'a.
% TRANSFER_FUNCTION: Moves interpretive authority and methodological legitimacy from rationalist, customary, and reformist actors to textualist institutions and scholars. The constraint transfers the power to say 'this is Islamic law' from those who reason by analogy, custom, or public interest to those who claim exclusive fidelity to text. It transfers the burden of justification: any departure from textual literalism must overcome a presumptive blockade.
% ABSENT_VOICES: Pre-modern Hanbali jurists who used qiyas more freely than the later synthesized method admits (e.g., Ibn Aqil, Ibn al-Jawzi). Early Maliki and Hanafi jurists whose practice-integrated methods are retrospectively erased by the textualist narrative. Contemporary Muslim communities in West Africa, South Asia, and Southeast Asia whose living fiqh blends textual, customary, and rationalist strands — they are not in the room when the 'authentic method' is defined.
% DISAPPEARANCE_RATIONALE: If the textualist restrictiveness constraint vanished overnight, the methodological field would reopen: qiyas would expand into areas of textual silence, weak hadith would lose their privileged status over reasoning, sadd al-dhara'i would cease to be a default blockade, and customary/maslaha-based rulings would regain legitimacy. Fatwa councils would lose their gatekeeping monopoly. The map of 'what counts as Islamic law' would be redrawn within a generation.
% FOUNDING_PROBLEM: Post-formative period proliferation of juristic opinions (ikhtilaf) untethered from textual control, leading to perceived fragmentation of the law, subjective ra'y, and innovations justified by loose analogy. The Hanbali synthesis (culminating in Ibn Taymiyya and Ibn Qayyim) responded by re-anchoring derivation in the Quran and authenticated Sunna, demoting qiyas, and erecting sadd al-dhara'i as a preventive barrier.
% FOUNDING_PROBLEM_CORROBORATION: The textualist tradition (Ibn Taymiyya, Ibn Qayyim, Ibn Rajab, modern Salafi scholarship) attests the problem remains live: textual drift and bid'a are permanent threats. Rationalist and comparative scholars (Wael Hallaq, Bernard Weiss, Muhammad Khalid Masud, Abdullahi An-Na'im) attest the founding problem was substantially solved by the classical synthesis itself — the madhhabs already stabilized the law — and the Hanbali re-restriction is a later ideological overlay. No neutral third party corroborates either side; the dispute is structural.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).
:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.22) reflects the measurable transfer: rationalist jurists lose methodological tools, customary practitioners lose legal recognition, reformists lose pathways — all redirected to textualist institutions. Suppression (0.48) is the active enforcement: fatwa vetoes, judicial review, academic gatekeeping, and the social cost of the bid'a label. Theater (0.18) is the growing gap between the coordination claim ('we only follow the text') and the operational reality (institutional control of what the text means). Accessibility collapse (0.78) is high because once the textualist frame is accepted, alternative methodologies appear not just wrong but religiously impermissible — the alternatives collapse conceptually. Resistance (0.35) is moderate: rationalist and reformist pushback exists but is fragmented, academically marginalized, and often forced into the textualist vocabulary to be heard at all.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (conservative institutions), the constraint is genuine coordination: it preserves the divine text's legislative monopoly against human encroachment. From the payer seats (rationalist jurists, customary practitioners, reformists), the same structure operates as enforced extraction: their labor, tradition, and reasoning are expropriated by a boundary they did not consent to and cannot move. The engine computes this divergence from the declared roles, power, and exit options — the claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist scholars and conservative institutions are structural beneficiaries (d ~ 0.15): they collect authority, appointments, and definitional power. Anti-bid'a advocates are secondary beneficiaries (d ~ 0.3): they gain rhetorical weaponry but less institutional rent. Rationalist jurists are payers (d ~ 0.75): their methodological capital is devalued, their arguments excluded. Customary practitioners are near-full targets (d ~ 0.9): their living tradition is structurally invalidated with no exit. Reformists are payers (d ~ 0.7) and excluded (d ~ 0.85): blocked and silenced. Comparative scholars are analytical observers (d = 0.5): they bear no extraction and collect no rent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (juristic fragmentation and textual drift) was real in the 7th–10th centuries. The Hanbali synthesis solved it by stabilizing a textualist methodology. But the solution has outlived the problem: the madhhab system already coordinated the law; the later re-restriction (Ibn Taymiyya → modern Salafi institutionalization) added extraction (institutional monopoly, bid'a weaponization) without new coordination value. The mandate (textual fidelity) has atrophied into a tool for blocking legal development that the founding problem never required blocking. The constraint persists because the beneficiaries (textualist institutions) are identity-locked to it and the payers (rationalists, customary practitioners, reformists) are trapped or constrained — classic mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_restrictiveness_vs_coordination_necessity,
    'Is the maximal textual restrictiveness (minimal qiyas, weak hadith over qiyas, default sadd al-dhara''i) structurally necessary for the coordination function (preserving textual continuity), or does it exceed what coordination requires and serve primarily to concentrate interpretive authority?',
    'Counterfactual comparison: examine historical periods and regions where Hanbali judges applied qiyas more freely (e.g., Mamluk Damascus, Ottoman Najd) — did textual continuity collapse? If not, the restrictiveness is not coordination-necessary.',
    'If restrictiveness exceeds coordination necessity, the constraint''s extraction is not the price of coordination but a separable rent — strengthening the tangled_rope classification. If restrictiveness is necessary, the extraction is the irreducible cost of the coordination function — weakening the extraction claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_restrictiveness_vs_coordination_necessity, empirical, 'Whether the method''s restrictiveness is functionally necessary for its claimed coordination purpose').

omega_variable(
    kernel_reading_identity,
    'This constraint is the hanbali_reading of the usul_al_fiqh_method kernel. The sibling readings (hanafi_reading, maliki_reading, shafii_reading) expand methodological sources (qiyas, ra''y, istihsan, ''amal, maslaha, ''urf). Does this reading foreclose the siblings, coexist with them, or influence them structurally?',
    'Analyze whether a single legal framework could simultaneously hold the Hanbali premise (textual sources are maximally restrictive, qiyas minimized) and the Hanafi premise (qiyas expansively applicable) or the Maliki premise (Medinan practice carries independent weight). If mutual holding is logically impossible within one framework, the relation is forecloses. If different parties hold each reading simultaneously across the umma, coexists_with. If the Hanbali reading''s institutional dominance (Saudi state backing) creates downstream pressure on other schools'' legitimacy without logical foreclosure, influences.',
    'forecloses would mean the kernel admits no pluralism — one reading structurally eliminates the others. coexists_with means the kernel sustains stable methodological pluralism. influences means the Hanbali reading''s institutional power reshapes the operating environment for other readings without resolving the dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural relationship between this reading and its sibling readings of the usul_al_fiqh_method kernel').

omega_variable(
    sadd_al_dhara_i_as_extraction_mechanism,
    'Is sadd al-dhara''i (blocking the means to bid''a) a genuine coordination tool preventing harm, or has it become an extraction mechanism that lets textualist institutions veto any legal development by declaring its pathway a ''means to innovation''?',
    'Case study analysis: track sadd al-dhara''i invocations in Saudi and Gulf fatwa councils (1970–present). Classify each invocation as (a) preventing a clear textual violation, (b) blocking a rationalist/maslaha argument with no textual violation, (c) blocking a customary practice with no textual violation. If (b) and (c) predominate, sadd al-dhara''i functions as extraction.',
    'If sadd al-dhara''i is primarily an extraction tool, the constraint''s suppression and extraction scores are understated — the mechanism is the main enforcement engine. If primarily protective, the scores reflect the genuine cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sadd_al_dhara_i_as_extraction_mechanism, empirical, 'Whether the preventive doctrine sadd al-dhara''i operates as coordination or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(usul_tr_t0, observed).
narrative_ontology:measurement(usul_tr_t30, usul_al_fiqh_method__hanbali_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement_basis(usul_tr_t30, observed).
narrative_ontology:measurement(usul_tr_t60, usul_al_fiqh_method__hanbali_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement_basis(usul_tr_t60, observed).
narrative_ontology:measurement(usul_tr_t90, usul_al_fiqh_method__hanbali_reading, theater_ratio, 90, 0.16).
narrative_ontology:measurement_basis(usul_tr_t90, observed).
narrative_ontology:measurement(usul_tr_t120, usul_al_fiqh_method__hanbali_reading, theater_ratio, 120, 0.18).
narrative_ontology:measurement_basis(usul_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(usul_be_t0, observed).
narrative_ontology:measurement(usul_be_t30, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement_basis(usul_be_t30, observed).
narrative_ontology:measurement(usul_be_t60, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 60, 0.18).
narrative_ontology:measurement_basis(usul_be_t60, observed).
narrative_ontology:measurement(usul_be_t90, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 90, 0.2).
narrative_ontology:measurement_basis(usul_be_t90, observed).
narrative_ontology:measurement(usul_be_t120, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 120, 0.22).
narrative_ontology:measurement_basis(usul_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(usul_su_t0, observed).
narrative_ontology:measurement(usul_su_t30, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(usul_su_t30, observed).
narrative_ontology:measurement(usul_su_t60, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(usul_su_t60, observed).
narrative_ontology:measurement(usul_su_t90, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 90, 0.45).
narrative_ontology:measurement_basis(usul_su_t90, observed).
narrative_ontology:measurement(usul_su_t120, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 120, 0.48).
narrative_ontology:measurement_basis(usul_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanbali_reading, 0.08).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, fiqh_codification__saudi_judicial_system).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, bid_a_enforcement__gulf_fatwa_councils).

% DUAL FORMULATION NOTE:
% This constraint is the hanbali_reading of the usul_al_fiqh_method kernel. The kernel decomposes into four constraint stories (one per reading) because each reading instantiates a different methodological boundary with different extractiveness, different beneficiary/victim structures, and different coordination functions. The ε-invariance principle requires separate stories: the Hanbali reading's ε (0.22) differs from the Hanafi reading's (expansive qiyas → lower extraction from rationalists, higher from textualists) and the Maliki reading's (custom-integrated → lower extraction from customary practitioners). The stories are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, institutional, 0.15).
constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, organized, 0.3).
constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, moderate, 0.72).
constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, powerless, 0.9).
constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
