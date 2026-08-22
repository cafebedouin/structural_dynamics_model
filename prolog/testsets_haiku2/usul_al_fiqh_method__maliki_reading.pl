% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Integration of Practice, Public Interest, and Custom
 *   domain: jurisprudence/legal_theory/religious_authority
 *
 * SUMMARY:
 *   The Maliki school of Islamic jurisprudence claims that Medinan practice
 *   ('amal ahl al-Madina), public interest (maslaha mursala), and custom
 *   ('urf) are legitimate independent sources of law alongside Quran and
 *   hadith. This represents a contestable reading of how Islamic law should
 *   be derived and what sources carry authority. Universalist textualists
 *   argue that all law must derive from authenticated textual sources;
 *   literalists insist that analogy should be minimized. The Maliki reading
 *   elevates situated practice and welfare reasoning to co-equal status with
 *   text, permitting law to be responsive to regional variation and local
 *   need while remaining anchored in precedent. The constraint operates
 *   through enforcement of the source hierarchy: Maliki jurists administer
 *   recognition of what counts as legitimate 'amal and maslaha, defending the
 *   method against challenges from competing schools. The beneficiaries are
 *   regional customary practitioners whose practices are validated as law,
 *   and local maslaha advocates empowered to make welfare-based claims. The
 *   payers are universalist textualists and literalists who lose the
 *   exclusive authority claim that law derives from text alone.
 *
 * KEY AGENTS:
 *   - Medinan jurists: institutional power, administering the source hierarchy and defending 'amal and maslaha against textualist challenge
 *   - Regional customary practitioners: organized power, benefiting from recognition of custom but constrained by textual non-contradiction
 *   - Local maslaha advocates: moderate power, identity-locked to the Maliki tradition, deploying welfare reasoning to derive situated law
 *   - Universalist textualists: powerful institutional seats, paying a cost in reduced explanatory power when practice is widespread
 *   - Literalist hadith prioritizers: powerful institutional seats, experiencing displacement of textual priority by practice-based and welfare-based authority
 *   - Comparative legal analysts: analytical observer seats examining internal consistency and methodological stakes
 *   - Textual community guardians: institutionally excluded, objecting to non-textual sources from outside the Maliki framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.62).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.48).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Jurisprudential Method: Integration of Practice, Public Interest, and Custom").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "jurisprudence/legal_theory/religious_authority").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, 'dc07b276-e0c5-4876-82bd-81729f811527').
narrative_ontology:cs_kernel_codification('dc07b276-e0c5-4876-82bd-81729f811527', formalized).
narrative_ontology:cs_authority_grounding('dc07b276-e0c5-4876-82bd-81729f811527', lineage).
narrative_ontology:cs_interpretation_layer_present('dc07b276-e0c5-4876-82bd-81729f811527').
narrative_ontology:cs_reading_relation('dc07b276-e0c5-4876-82bd-81729f811527', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc07b276-e0c5-4876-82bd-81729f811527', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc07b276-e0c5-4876-82bd-81729f811527', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('dc07b276-e0c5-4876-82bd-81729f811527', foundational, practice_authority_doctrine).
narrative_ontology:cs_axiom_status(practice_authority_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('dc07b276-e0c5-4876-82bd-81729f811527', practice_authority_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('dc07b276-e0c5-4876-82bd-81729f811527', foundational, welfare_reasoning_doctrine).
narrative_ontology:cs_axiom_status(welfare_reasoning_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('dc07b276-e0c5-4876-82bd-81729f811527', welfare_reasoning_doctrine, instrumental).
narrative_ontology:cs_reference_frame('dc07b276-e0c5-4876-82bd-81729f811527', medinan_jurisprudential_authority).
narrative_ontology:cs_drift_state('dc07b276-e0c5-4876-82bd-81729f811527', contemporary_islamic_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dc07b276-e0c5-4876-82bd-81729f811527', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, medinan_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, regional_customary_practitioners).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, local_maslaha_advocates).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, universalist_textualists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, literalist_hadith_prioritizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, regional_customary_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jurists rooted in Medinan scholarly tradition who assert 'amal ahl al-Madina (the established practice of Medina's early community) as an independent source of law. They claim continuity with the Medina where the Prophet lived and the first generations interpreted Islamic law, grounding their methodology in place-based legitimate practice. They administer the integration of Medinan custom into legal derivation and defend its weight against challenges from competing methodologies.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, medinan_jurists, agenda_setter,
    institutional, civilizational, mobile, regional).

% Communities whose established customs ('urf), trade practices, and local conventions are validated and preserved as legitimate legal sources under the Maliki reading. They benefit from having their settled practices recognized as juridically authoritative rather than requiring case-by-case textualist justification. They simultaneously bear the cost of integration discipline: custom is admitted only where it does not contradict explicit textual injunctions, which constrains drift away from foundational principles.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, regional_customary_practitioners, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__maliki_reading, regional_customary_practitioners, payer).

% Scholars and jurists who employ maslaha mursala (consideration of public interest/welfare unrestricted by specific textual precedent) to derive law responsive to local need. They argue that the Maliki method's admission of welfare-based reasoning permits situated judgment: a practice may be novel textually but legitimate if it serves communal benefit without violating foundational principles. They are identity-locked to the interpretive tradition—their legitimacy and professional authority rest on maintaining continuity with recognized Maliki sources even when extending them.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, local_maslaha_advocates, beneficiary,
    moderate, biographical, identity_locked, regional).

% Scholars—within Islam and in comparative law study—who insist that law derives exclusively (or nearly so) from authenticated textual sources: Quran and rigorously-verified hadith. They argue that elevation of Medinan practice and unrestricted maslaha introduces subjective variation that fragments legal authority and permits unjustified innovation. They pay a cost in reduced explanatory power when situated custom is widespread: the Maliki reading can account for observed practice while universalist textualism must either deny the practice is legitimate or declare it textually-grounded (reinterpreting text to fit, which strains credulity).
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, universalist_textualists, payer,
    powerful, civilizational, constrained, global).

% Methodologically-literalist jurists (particularly associated with Hanbali and ultra-conservative readings) who minimize qiyas and maximize direct textual authority, treating authenticated hadith as the ceiling of legitimate derivation. Under the Maliki reading, their insistence on textual priority is displaced by a framework that treats Medinan practice as co-equal and maslaha as a parallel source. They are not trapped (alternative methodologies persist globally), but they experience the Maliki reading as expanding the legitimate space for innovation beyond what textual literalism permits.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, literalist_hadith_prioritizers, payer,
    powerful, civilizational, mobile, global).

% Scholars studying Islamic jurisprudence and comparative law who analyze the methodological stakes: Does the Maliki elevation of practice and public interest constitute a genuine second-order rule governing source hierarchy, or is it a cover for contextual decision-making dressed in methodological language? They do not participate in the interpretive tradition but examine whether the constraints it creates are internally consistent and genuinely binding on adherents.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_legal_analysts, observer,
    analytical, biographical, analytical, global).

% Custodians of Quranic and hadith authentication systems (rijal al-hadith, isnad verification) who would object to the Maliki reading's claim that practice and welfare-reasoning can override or supplement textual grounding. They are excluded from the Maliki interpretive space because the reading pre-commits to admitting non-textual sources; their fundamental objection—that law must rest on authenticated text—is ruled out by the framework itself.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, textual_community_guardians, excluded,
    institutional, civilizational, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of deriving law in a geographically distributed Islamic community where Medinan precedent is revered but local practice has legitimacy: the Maliki method permits recognized customs and place-based interpretations to count as legal sources without requiring either complete uniformity (universalist textualism) or abandonment of textual grounding. It coordinates between textual fidelity and situated judgment.
% TRANSFER_FUNCTION: Moves interpretive authority from exclusively-textual sources (Quran, hadith) toward a wider evidential base including Medinan practice and welfare-based reasoning. The beneficiary is regional custom and local jurists empowered to recognize situated practice as law; the payer is universalist textualism, which loses the exclusive claim that all law derives from authenticated text.
% ABSENT_VOICES: Textual community guardians (rijal al-hadith, isnad specialists) who would insist that any non-textual source (practice, welfare-reasoning) is a departure from Islamic law's true foundation. They are structurally excluded because the Maliki framework pre-commits to admitting them. Also absent: later jurists from non-Maliki schools who would argue the framework privileges Medina over other early communities (Kufa, Basra) and thus serves a particular sectional interest.
% DISAPPEARANCE_RATIONALE: If the Maliki reading disappeared (replaced entirely by universalist textualism), Medinan practice would revert to evidentiary weight only where it could be textually grounded; local custom would require hadith or Quranic support rather than standing as an independent source; maslaha claims would need textual anchoring. Jurists who relied on the method to validate situated practice would face pressure to deny the practice is legitimate or to reinterpret texts. Regional legal pluralism would collapse into a more uniform, text-centered framework. The reorganization would be extensive and institutional.
% FOUNDING_PROBLEM: Early Islamic jurisprudence faced the problem of how to govern an expanding, geographically diverse community while maintaining connection to Medina as the site of the Prophet's life and the Companions' settlement. Textual sources alone (Quran and early hadith) were insufficient to address all local needs; pure analogy risked drift from practice-based legitimacy. The Maliki reading was developed to admit the established practice of Medina and local customs as legal sources, permitting law to be responsive to regional variation while remaining anchored in precedent.
% FOUNDING_PROBLEM_CORROBORATION: Maliki scholars attest the founding problem remains live: geographic and temporal distance from Medina means local practice carries authority to preserve Islamic law's connection to lived tradition. Universalist textualists and literalist hadith prioritizers attest the founding problem is largely solved by improved hadith authentication and that continued reliance on Medinan practice and welfare-reasoning is now a cover story for innovation. Comparative legal historians (e.g., Hallaq, Wael) corroborate that the founding problem was historically genuine and that the Maliki reading genuinely addresses it, while also noting that the method's expansion of maslaha created drift that later jurists (even within the Maliki school) had to constrain.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.62 because the Maliki reading extracts methodological authority from universalist textualists by displacing the exclusive claim that law derives from text. The extraction rises from 0.45 at the origin (early Maliki consolidation, when the method was less systematized and less directly challenging to alternatives) to 0.63 at T=600 (height of Maliki institutional influence), then moderates to 0.62 at T=1400 (contemporary), reflecting that while the method remains authoritative within its tradition, competing schools have consolidated their own methodological defenses and no longer see the Maliki reading as simply displacing their claims. Suppression is 0.48 because the method depends on active enforcement—defending Medinan practice against textualist challenges requires ongoing assertion that practice carries authority alongside text. This suppression is not coercive (there is no external enforcement machinery) but rather doctrinal: Maliki jurists must continuously argue for 'amal and maslaha weight, and non-Maliki jurists must suppress their objection when operating within the framework. Theater is 0.22, low-to-moderate, because while maslaha reasoning can be performative (deployed to justify predetermined outcomes), the Maliki method also contains genuine constraint: maslaha claims must be articulated as welfare reasoning and must not contradict explicit textual injunctions. The measurements track the trajectory: suppression increases early (T=0 to T=600) as the method becomes institutionalized and requires active defense, then stabilizes (T=600 to T=1400) as institutional equilibrium is reached. Theater increases modestly over the same period, reflecting some drift toward performative invocation of maslaha, then stabilizes, suggesting the drift reaches a saturation point beyond which the method's constraints reassert.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of Medinan jurists and regional practitioners, the Maliki reading is a legitimate, constraint-respecting method that permits law to be responsive to situated need while remaining anchored in precedent and avoiding pure discretion. From the seat of universalist textualists, the same structure appears extractive: it displaces the exclusive authority of authenticated text and permits innovation under the guise of welfare reasoning. From the analytical seat, the key question is whether the method genuinely constrains judgment (tangled_rope) or provides a cover for interest-driven decision-making (snare). The engine computes each seat's classification from the structural data: Medinan jurists (beneficiary, institutional power, mobile exit) will compute a different effective extraction than universalist textualists (payer, powerful but constrained by the framework's legitimacy). The perspectival gap is thus structural and expected; the authored metrics describe the constraint's operation across all seats, not from any single seat's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Medinan jurists, regional customary practitioners, maslaha advocates) are positioned near the beneficiary end of the directionality spectrum (d approaching 0.0) because they collect interpretive authority and see their practices validated as law without bearing the enforcement cost—the burden of defending the method falls on the institutional structure, not on each practitioner. Victims (universalist textualists, literalist hadith prioritizers) are positioned near the target end (d approaching 1.0) because they bear the cost of a methodology that displaces textual priority and permits innovation without bearing the benefit of that displaced authority—they must either accept the Maliki reading or exit the framework. The structural asymmetry is what makes this tangled_rope rather than rope: both genuine coordination (situating law in practice and welfare) and asymmetric extraction (losing exclusive authority) are present. A directionality override is not needed because the power atoms and exit options already differentiate the seats: Medinan jurists (institutional, mobile) have more exit and less cost than universalist textualists (powerful but constrained by institutional legitimacy of the method).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: early Islamic jurisprudence faced the coordination challenge of governing a geographically dispersed community while maintaining connection to Medina. The Maliki reading genuinely solved this by admitting practice as a source. However, the divergence between founding_problem_status (contested) and disappearance_verdict (world_rearranges) is real and diagnostic: universalist textualists and some contemporary Maliki jurists argue the founding problem is largely solved by improved hadith authentication and that continued reliance on maslaha now serves innovation rather than coordination. If the founding problem is dead (the coordination challenge of geographically-dispersed law is solved by text+hadith+qiyas), but the Maliki method persists (the world would rearrange if it disappeared because regional practice is embedded in law), then the constraint carries mandatrophy: the founding justification has atrophied while the structure remains, redistributing authority and extracting from textualists. The measured theater_ratio (0.22, low-to-moderate) and the rising extraction over T=0 to T=600 (then stabilizing) support this: early consolidation of the method (high theater, rising extraction) as it became institutionalized, then equilibrium once competing schools solidified defenses. This is a constraint that genuinely coordinates situated judgment but increasingly operates as an authority-extraction mechanism by which Maliki jurists retain methodological flexibility that other schools have constrained. The mandatrophy is partial and contested (reflected in the omega on maslaha drift), not complete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_mediation_under_determination,
    'Is the Maliki reading a coherent jurisprudential method grounded in genuine second-order principles (source hierarchy), or is it a cover story for contextual, interest-driven decision-making that selectively invokes Medinan practice and maslaha to justify preferred outcomes?',
    'Systematic analysis of a large corpus of Maliki legal judgments: if methodological consistency holds (practice and maslaha are invoked on stable grounds), the reading is coherent; if invocation is selective and outcome-driven, the method is post-hoc rationalization.',
    'If incoherent, the constraint reclassifies from tangled_rope (genuine coordination of situated judgment within textual constraints) to snare (a methodological mask for discretionary extraction of authority by Maliki jurists). If coherent, the classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_mediation_under_determination, empirical, 'Whether the Maliki method is a genuine jurisprudential principle or outcome-driven cover.').

omega_variable(
    regional_vs_universal_reading_contest,
    'Is the Maliki elevation of Medinan practice a reading of ONE kernel (usul al-fiqh_method, shared with other schools), or does it constitute an alternative kernel grounded in place-specific legitimacy (regional jurisprudence as a distinct commitment system)?',
    'Historical textual analysis: if Maliki jurists cite the same foundational authorities (Quran, Prophet''s precedent) as other schools and differ only on source hierarchy, it is a reading of one kernel. If they appeal to distinct authorities (Medina''s unique sacred status) not cited by rivals, it is a separate kernel.',
    'If one kernel, the constraint is a kernel reading and sibling relationships (coexists_with vs. forecloses) hold as authored. If separate kernel, the Maliki system is an independent commitment system and should be decomposed into multiple constraint stories per the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_vs_universal_reading_contest, conceptual, 'Whether Maliki jurisprudence reads a shared usul al-fiqh kernel or grounds itself in an alternative kernel.').

omega_variable(
    maslaha_drift_mechanism,
    'To what extent is the measured theater_ratio (0.22) driven by jurists invoking maslaha to justify predetermined outcomes (performance masking interest), versus genuine constraint-respecting deliberation about public welfare within textual limits?',
    'Close reading of jurisprudential justifications: where maslaha is invoked, does the jurist explicitly acknowledge conflicting textual evidence and explain why welfare overrides text? Or is maslaha deployed without acknowledgment of constraint, suggesting post-hoc rationalization?',
    'High theatrical component (maslaha as mask for preference) would support reclassification toward snare; low theatrical component (genuine deliberation despite discretion) supports tangled_rope as claimed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maslaha_drift_mechanism, empirical, 'Whether maslaha reasoning is constraint-respecting deliberation or performance masking preference.').

omega_variable(
    suppression_internalization_in_scholarly_identity,
    'Is the measured suppression (0.48) of universalist textualists and literalist hadith prioritizers primarily STRUCTURAL (textual community guardians are institutionally excluded from Maliki discourse) or INTERNALIZED (non-Maliki scholars have adopted the framework''s legitimacy assumptions, limiting their own critical objection)?',
    'Study of non-Maliki scholarship: do Hanbali and ultra-literalist scholars actively argue for the exclusion of practice/maslaha, or have they accepted that such sources are legitimate even while disagreeing on weight? Post-framework identity shifts (where do scholars educated in Maliki schools go intellectually when they encounter literalist criticism) also test internalization.',
    'If structural, the suppression is enforced by institutional boundaries and sustained by active defense. If internalized, non-Maliki jurists carry suppression internalized even after leaving the Maliki space, suggesting deeper identity-fusion. Internalized suppression implies higher effective constraint power than the scalar metric alone indicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_scholarly_identity, empirical, 'Whether suppression of alternative readings is structural or internalized in scholarly identity.').

omega_variable(
    medina_sacred_geography_vs_methodological_claim,
    'Does the Maliki elevation of Medinan practice rest on methodological grounds (practice as a more reliable evidential base than pure analogy) or on theological/sacred geography grounds (Medina''s unique status as the Prophet''s city grants its practice special authority)?',
    'Textual analysis of foundational Maliki authorities (e.g., Malik''s Muwatta, early Maliki usul works): do they justify ''amal on methodological reliability grounds, or on Medina''s sacred geography? If the former, the constraint is a jurisprudential method; if the latter, it is a reading of a sacred-geography kernel.',
    'If methodological, other geographies might claim equivalent practice-based authority, universalizing the principle. If sacred-geography grounded, the Maliki reading is irreducibly particular to Medina and related readings cannot be written for other sacred cities without changing the kernel. This affects whether competing readings coexist or whether geography itself is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medina_sacred_geography_vs_methodological_claim, conceptual, 'Whether Medinan practice authority rests on methodology or sacred geography.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method__maliki_reading, theater_ratio, 200, 0.11).
narrative_ontology:measurement(usul_tr_t400, usul_al_fiqh_method__maliki_reading, theater_ratio, 400, 0.15).
narrative_ontology:measurement(usul_tr_t600, usul_al_fiqh_method__maliki_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement(usul_tr_t800, usul_al_fiqh_method__maliki_reading, theater_ratio, 800, 0.22).
narrative_ontology:measurement(usul_tr_t1000, usul_al_fiqh_method__maliki_reading, theater_ratio, 1000, 0.24).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__maliki_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement(usul_tr_t1400, usul_al_fiqh_method__maliki_reading, theater_ratio, 1400, 0.22).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method__maliki_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(usul_be_t400, usul_al_fiqh_method__maliki_reading, base_extractiveness, 400, 0.58).
narrative_ontology:measurement(usul_be_t600, usul_al_fiqh_method__maliki_reading, base_extractiveness, 600, 0.63).
narrative_ontology:measurement(usul_be_t800, usul_al_fiqh_method__maliki_reading, base_extractiveness, 800, 0.61).
narrative_ontology:measurement(usul_be_t1000, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1000, 0.59).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement(usul_be_t1400, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1400, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method__maliki_reading, suppression_requirement, 200, 0.38).
narrative_ontology:measurement(usul_su_t400, usul_al_fiqh_method__maliki_reading, suppression_requirement, 400, 0.42).
narrative_ontology:measurement(usul_su_t600, usul_al_fiqh_method__maliki_reading, suppression_requirement, 600, 0.46).
narrative_ontology:measurement(usul_su_t800, usul_al_fiqh_method__maliki_reading, suppression_requirement, 800, 0.48).
narrative_ontology:measurement(usul_su_t1000, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1000, 0.49).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1200, 0.48).
narrative_ontology:measurement(usul_su_t1400, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1400, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__maliki_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% The usul al-fiqh kernel admits four distinct readings (constraint stories), each representing a coherent methodological position on Islamic jurisprudential sources. The Maliki reading is distinguished by elevation of Medinan practice and maslaha mursala to independent source status. The four stories form a constraint family: they read the same kernel (what sources count as law) but produce different classifications due to distinct source hierarchies and beneficiary/victim structures. Each story links to the others via affects_constraints; the network models how methodological choices in one reading (e.g., Maliki admission of maslaha) create structural pressure on siblings (e.g., Hanbali restriction of analogy as a defensive response).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
