% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Jurisprudential Method: Literal Text Only
 *   domain: religious/legal/philosophical
 *
 * SUMMARY:
 *   The Hanbali reading of the jurisprudential method kernel presents itself
 *   as the pure, unmediated application of divine text — Qur'an, authentic
 *   Hadith, and Companion opinions — rejecting all human rationalist tools
 *   (qiyas, istihsan, istislah, 'urf) as bid'ah (blameworthy innovation).
 *   Only unanimous consensus (ijma') of the Companions or the entire Ummah is
 *   accepted as a supplementary source. This reading claims Mountain status:
 *   it presents its methodology as the natural law of Islamic jurisprudence,
 *   the only faithful transmission of revelation. However, the constraint
 *   extracts heavily from rationalist jurists (branding their work as
 *   heresy), customary practitioners (invalidating non-textual norms), and
 *   lay Muslims (denying them equitable flexibility). The Hanbali school
 *   historically enforced this through judicial control (Saudi Arabia,
 *   Taliban Afghanistan), madrasa curricula, and fatwa authority. The
 *   claim/metric gap is structural: the reading claims zero extraction (pure
 *   textual fidelity) while the authored metrics reflect high extraction
 *   (methodological exclusion as rent-seeking by textualist gatekeepers). The
 *   engine will measure this divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.85).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.9).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, mountain).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Jurisprudential Method: Literal Text Only").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "religious/legal/philosophical").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).
domain_priors:emerges_naturally(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, 'e6326fcc-9405-486a-83e4-7d8b669bc78b').
narrative_ontology:cs_kernel_codification('e6326fcc-9405-486a-83e4-7d8b669bc78b', fixed_text).
narrative_ontology:cs_authority_grounding('e6326fcc-9405-486a-83e4-7d8b669bc78b', lineage).
narrative_ontology:cs_interpretation_layer_present('e6326fcc-9405-486a-83e4-7d8b669bc78b').
narrative_ontology:cs_reading_relation('e6326fcc-9405-486a-83e4-7d8b669bc78b', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('e6326fcc-9405-486a-83e4-7d8b669bc78b', jurisprudential_method_kernel__maliki_reading, forecloses).
narrative_ontology:cs_reading_relation('e6326fcc-9405-486a-83e4-7d8b669bc78b', jurisprudential_method_kernel__shafii_reading, forecloses).
narrative_ontology:cs_axiom('e6326fcc-9405-486a-83e4-7d8b669bc78b', foundational, literal_text_and_companions_only_source).
narrative_ontology:cs_axiom_status(literal_text_and_companions_only_source, holdable).
narrative_ontology:cs_axiom_grounding('e6326fcc-9405-486a-83e4-7d8b669bc78b', literal_text_and_companions_only_source, deontological).
narrative_ontology:cs_axiom('e6326fcc-9405-486a-83e4-7d8b669bc78b', foundational, analogical_reasoning_is_bidah).
narrative_ontology:cs_axiom_status(analogical_reasoning_is_bidah, holdable).
narrative_ontology:cs_axiom_grounding('e6326fcc-9405-486a-83e4-7d8b669bc78b', analogical_reasoning_is_bidah, deontological).
narrative_ontology:cs_reference_frame('e6326fcc-9405-486a-83e4-7d8b669bc78b', companion_textual_authority).
narrative_ontology:cs_drift_state('e6326fcc-9405-486a-83e4-7d8b669bc78b', post_classical_codification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e6326fcc-9405-486a-83e4-7d8b669bc78b', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hanbali_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practitioners).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, qiyas_practitioners).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, istihsan_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, lay_muslims).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, lay_muslims).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, divine_text_sufficiency).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, bidah_rejection).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, unanimous_consensus_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and enforce the Hanbali usul al-fiqh through madrasa curricula, judicial appointments, and fatwa authority. Their institutional identity is fused with the claim of pure textual fidelity; departure from the methodology would dissolve the school's distinctiveness. They control the interpretation of what counts as 'literal text' and 'Companion opinion'.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanbali_jurists, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, hanbali_jurists, beneficiary).

% Scholars across schools who adopt strict textualist methodology gain legitimacy and authority by aligning with the Hanbali claim to purity. They benefit from the gatekeeping function that excludes rationalist methodologies. Their careers and intellectual capital are invested in the textualist frame; exit means losing their distinctive epistemic standing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_scholars, beneficiary,
    organized, biographical, constrained, global).

% Hanafi, Shafi'i, and other jurists who employ qiyas, istihsan, maslaha, and other rationalist tools are structurally excluded from the Hanbali frame. Their methodologies are branded as bid'ah. They bear the cost of having their legal reasoning delegitimized in Hanbali-dominated spaces (historically: parts of Arabia, Central Asia; currently: Saudi judicial system, Taliban courts).
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    organized, biographical, constrained, global).

% Communities relying on 'urf (custom), 'amal (local practice), and non-textual normative orders find their practices invalidated unless they can be traced to textual evidence. This particularly affects rural, tribal, and women's customary law. Exit means abandoning communal normative systems or facing Hanbali judicial override.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practitioners, payer,
    moderate, biographical, constrained, regional).

% Jurists whose primary methodological tool is analogical reasoning (qiyas) — primarily Hanafis and Shafi'is — are directly targeted by the Hanbali bid'ah accusation. They can migrate to jurisdictions where their school is dominant (historically: Ottoman lands, South Asia, Southeast Asia), giving them mobile exit options unlike customary practitioners.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, qiyas_practitioners, payer,
    organized, biographical, mobile, global).

% Primarily Hanafi jurists who use juristic preference (istihsan) to depart from strict analogy for equitable outcomes. The Hanbali reading treats this as the archetypal bid'ah. Like qiyas practitioners, they have mobile exit to Hanafi-dominated regions.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, istihsan_practitioners, payer,
    organized, biographical, mobile, global).

% Receive the certainty and simplicity of a fixed textual code (benefit) but lose the flexibility of contextual judgment, equity, and customary accommodation (cost). In Hanbali-dominated jurisdictions (Saudi Arabia, Taliban Afghanistan), they have no meaningful exit — emigration is the only escape, which is economically and socially prohibitive for most.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, lay_muslims, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, lay_muslims, payer).

% Contemporary scholars advocating for maqasid-based reasoning, feminist tafsir, or human rights frameworks within Islamic law. They are structurally excluded from the Hanbali frame because their methods require the very rationalist tools (qiyas, istihsan, maslaha) the reading rejects as bid'ah. Their exclusion is the enforcement object itself.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, modern_reformers, excluded,
    moderate, generational, constrained, global).

% Orientalists, historians of Islamic law, comparative legal scholars, and philosophers of law who analyze the Hanbali method as a historical and structural phenomenon. They neither collect nor pay; they map the constraint's operation across time and space.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, academic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides epistemic certainty and communal unity by binding legal derivation to a fixed, closed textual corpus (Qur'an, Hadith, Companion opinions), eliminating the infinite regress of human reasoning and preventing subjective innovation (bid'ah) from corrupting divine law.
% TRANSFER_FUNCTION: Moves interpretive authority and legal flexibility from rationalist jurists (who use qiyas, istihsan, maslaha, 'urf) to textualist gatekeepers who control the definition of 'literal text' and 'authentic Companion opinion'. Moves the cost of rigidity onto customary practitioners, women, and marginalized communities whose practices lack textual pedigree.
% ABSENT_VOICES: Rationalist jurists (Hanafi, Shafi'i), Maliki Medinan practitioners, women scholars historically barred from madrasa authority, modern reformers (maqasid, feminist, human rights advocates), and non-Muslim subjects of Islamic law (dhimmis) whose customary laws were overridden by Hanbali textualism in Hanbali-dominated polities.
% DISAPPEARANCE_RATIONALE: If the Hanbali textualist constraint vanished overnight, the Saudi judicial system, Taliban courts, and Hanbali madrasa networks would lose their methodological foundation. Legal authority would shift to rationalist schools (Hanafi, Shafi'i), customary practice ('urf), and modern codified statutes. The entire edifice of 'text-only' legitimacy in these spaces would collapse, triggering a restructuring of Islamic legal authority comparable to the 19th-20th century codification movements.
% FOUNDING_PROBLEM: Early Islamic legal chaos (2nd-3rd/8th-9th century): multiple competing methodologies, uncontrolled qiyas proliferation, istihsan used to justify ruler preferences, hadith fabrication, and the perception that human reason was corrupting divine revelation. The Hanbali reading emerged as a reaction to the rationalist 'People of Opinion' (Ahl al-Ra'y) and the hadith-centric but methodologically flexible 'People of Hadith' (Ahl al-Hadith) before them.
% FOUNDING_PROBLEM_CORROBORATION: Non-Hanbali early sources (al-Shafi'i's Risala, Tabari's Ikhtilaf al-Fuqaha, Shahrastani's Milal wa Nihal) document the methodological diversity and chaos the Hanbali reading reacted against. Hanbali sources (Ibn Taymiyya, Ibn al-Qayyim) claim the threat of bid'ah is perpetual. Modern historians (Hallaq, Melchert, Lucas) corroborate that early Islamic law was methodologically fluid, but dispute whether 'chaos' is the right frame vs. 'healthy diversity'.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, ExtMetricName, E),
    domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jurisprudential_method_kernel__hanbali_reading),
    narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the constraint rejects entire methodological universes (qiyas, istihsan, 'urf, maslaha) that constitute the working toolkit of other schools, transferring interpretive monopoly to textualist gatekeepers. Suppression is extreme (0.90) because the constraint's persistence depends on actively delegitimizing and excluding alternative methodologies — not merely disagreeing but branding them as religious innovation (bid'ah), which carries apostasy-adjacent stigma. Theater ratio is moderate (0.40): the textualist performance is real (scholars genuinely master texts), but a significant portion of enforcement activity defends the school's institutional boundaries rather than textual fidelity per se (e.g., rejecting valid qiyas that reaches uncomfortable conclusions). Accessibility collapse is near-total (0.92) from the reading's internal view — alternatives are not just difficult but religiously impermissible. Resistance is substantial (0.65) from rival schools, modern reformers, and historical practice showing Hanbali jurists themselves using istislah and maslaha when pressed.
 *
 * PERSPECTIVAL GAP:
 *   From the Hanbali jurist seat, the constraint is a Mountain: divine text is fixed, human reason is fallible, the methodology is the only faithful transmission. From the rationalist jurist seat, it is a Snare: a power grab disguised as piety, extracting interpretive monopoly by branding competition as heresy. From the customary practitioner seat, it is a Snare with no coordination function — their practices are destroyed without replacement. From the lay Muslim seat, it is a Tangled Rope: genuine coordination (certainty, uniformity) bundled with extraction (rigidity, gendered inequity). The engine computes these per-seat types from the structural data; the authored claim (mountain) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanbali jurists (agenda_setter/beneficiary) sit at the beneficiary pole: they control the definition of 'literal text', collect institutional rents (judicial posts, teaching authority, fatwa monopoly), and have identity-locked exit (their school IS this methodology). Textualist scholars across schools (beneficiary) gain epistemic capital by aligning with the purity claim. Rationalist jurists, qiyas/istihsan practitioners (payer) bear the transfer: their methodologies are excluded, their authority delegitimized in Hanbali spaces. Customary practitioners (payer, powerless, trapped) bear the highest effective extraction — their entire normative world is invalidated with no exit. Lay Muslims (beneficiary/payer, powerless, trapped) receive certainty but lose equity; they cannot exit the jurisdiction. Modern reformers (excluded) are the enforcement object — their exclusion maintains the boundary. Academic observers (analytical) see the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early methodological chaos) is contested: Hanbalis say bid'ah is an eternal threat; historians say the chaos resolved into stable schools. The Hanbali reading's mandate (exclude all rationalist tools) has outlived its founding conditions — the 'People of Opinion' are gone, replaced by sophisticated usul traditions. Yet the constraint persists and intensifies (Wahhabi revival, Taliban). This is mandatrophy: the original coordination function (preventing uncontrolled innovation) has atrophied, but the extraction function (gatekeeping authority) has hypertrophied. The theater ratio rise (0.20→0.40→0.50) tracks this: more performance of textual fidelity, less actual textual derivation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the Hanbali reading a distinct constraint from the jurisprudential method kernel, or merely a stringent application of the same kernel?',
    'Compare ε values across readings: if hanbali_reading ε on analogical reasoning rejection (0.85) differs structurally from hanafi_reading ε on qiyas acceptance (near 0), they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own classification; if same kernel, the framework must model observable-dependent classification (which it rejects).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to ε-invariance: one kernel, multiple readings = multiple constraints.').

omega_variable(
    structural_delta_siblings,
    'What specific structural elements change across the four readings of the jurisprudential method kernel?',
    'Map each reading''s beneficiary/victim sets, extractiveness profile, and suppression targets. Hanafi: beneficiaries=qiyas practitioners, victims=textualists. Maliki: beneficiaries=Medinan custom holders, victims=non-Medinan schools. Shafi''i: beneficiaries=hadith transmitters, victims=opinion-based jurists.',
    'Clarifies whether the kernel is a single constraint with parameter variation (forbidden) or a family of distinct constraints (required).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_delta_siblings, conceptual, 'Structural delta mapping across the four madhhab readings.').

omega_variable(
    disagreement_location,
    'Where exactly is the disagreement located among the four readings — on the kernel text, the authority structure, the interpretation layer, or the drift acknowledgment?',
    'Analyze cs_structure fields across readings: all share kernel_codification=''fixed_text'' but differ on authority_grounding (lineage vs practice vs extraction vs expertise) and interpretation_layer_present. The disagreement is on authority_grounding and which interpretation_layer is legitimate.',
    'If disagreement is on authority_grounding, forecloses relations are justified; if on interpretation_layer only, coexists_with may apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location, conceptual, 'Locating the committer-axis disagreement within cs_structure parameters.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the Hanbali constraint a genuine natural law (Mountain) — the literal text of revelation — or a constructed constraint benefiting textualist scholars (False Summit Mountain)?',
    'Post-hoc historical analysis: did Hanbali jurists consistently derive law ONLY from literal text, or did they develop de facto rationalist tools (istislah, maslaha, urf as ''custom of the Companions'') while denying it? If the latter, the Mountain claim is a constructed cover.',
    'If constructed, FSM triggers reclassification to tangled_rope; the beneficiaries (textualist scholars) are the rent-collectors; the victims (rationalists, customary practitioners) are the payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, empirical, 'FSM candidate: Mountain with declared beneficiaries requires omega documenting natural-law vs constructed ambiguity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the Hanbali constraint''s suppression structural (state enforcement, judicial monopoly) or internalized (scholars genuinely believe alternatives are heretical)?',
    'Post-exit suppression trajectory: do rationalist jurists who leave Hanbali spaces (e.g., move to Hanafi domains) continue to self-censor, or do they freely practice qiyas? If internalized, effective suppression exceeds structural measure.',
    'If internalized, the constraint''s effective suppression is higher than 0.90 — the target carries the suppression with them. This affects χ computation for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in a religious-legal constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanbali_method_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hanbali_method_tr_t200, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 200, 0.3).
narrative_ontology:measurement(hanbali_method_tr_t400, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 400, 0.35).
narrative_ontology:measurement(hanbali_method_tr_t600, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 600, 0.45).
narrative_ontology:measurement(hanbali_method_tr_t800, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 800, 0.4).
narrative_ontology:measurement(hanbali_method_tr_t1000, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1000, 0.5).
narrative_ontology:measurement(hanbali_method_tr_t1200, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1200, 0.4).

% Extraction over time
narrative_ontology:measurement(hanbali_method_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(hanbali_method_be_t200, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 200, 0.7).
narrative_ontology:measurement(hanbali_method_be_t400, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 400, 0.75).
narrative_ontology:measurement(hanbali_method_be_t600, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 600, 0.8).
narrative_ontology:measurement(hanbali_method_be_t800, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 800, 0.82).
narrative_ontology:measurement(hanbali_method_be_t1000, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1000, 0.88).
narrative_ontology:measurement(hanbali_method_be_t1200, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1200, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(hanbali_method_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(hanbali_method_su_t200, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 200, 0.75).
narrative_ontology:measurement(hanbali_method_su_t400, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 400, 0.8).
narrative_ontology:measurement(hanbali_method_su_t600, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 600, 0.85).
narrative_ontology:measurement(hanbali_method_su_t800, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 800, 0.9).
narrative_ontology:measurement(hanbali_method_su_t1000, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1000, 0.92).
narrative_ontology:measurement(hanbali_method_su_t1200, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1200, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanbali_reading, 0.08).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hanbali_reading of the jurisprudential_method_kernel family. It forecloses the sibling readings by logically ruling out qiyas, istihsan, and non-unanimous ijma as valid sources. The kernel decomposes into four distinct constraints because each reading has a different ε profile on analogical reasoning: hanbali (ε=0.85 rejection), hanafi (ε≈0 acceptance), maliki (ε≈0.3 partial acceptance via 'amal), shafii (ε≈0.2 regulated acceptance via 4th-tier qiyas). The ε-invariance principle requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__hanbali_reading, institutional, 0.1).
constraint_indexing:directionality_override(jurisprudential_method_kernel__hanbali_reading, organized, 0.85).
constraint_indexing:directionality_override(jurisprudential_method_kernel__hanbali_reading, moderate, 0.9).
constraint_indexing:directionality_override(jurisprudential_method_kernel__hanbali_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
