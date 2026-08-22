% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Textualist-Literalist Reading of the Jurisprudential Method Kernel
 *   domain: Islamic Jurisprudence / Legal Philosophy / Institutional History
 *
 * SUMMARY:
 *   This story instantiates the Hanbali reading of the jurisprudential method
 *   kernel: law derives strictly from the literal text of Qur'an and Hadith
 *   and directly attested Companion opinion, with analogical reasoning
 *   (qiyas) and juristic preference (istihsan) rejected as bid'ah —
 *   innovation that corrupts the kernel — and only unanimous consensus (ijma'
 *   in its strictest sense) admitted as a third source. This is one of four
 *   sibling readings of the same kernel (Hanafi, Maliki, Shafi'i are the
 *   others); each is authored as its own constraint with its own ε,
 *   beneficiary/victim structure, and classification, per the ε-invariance
 *   principle. This story does not describe the contest among readings — it
 *   authors only the Hanbali reading's own structural operation.
 *
 * KEY AGENTS:
 *   - hanbali_textualist_scholars: agenda-setting beneficiary — administers the text-only standard and gains authority from it
 *   - hadith_transmission_specialists: beneficiary — professional standing concentrated by the reading's exclusive reliance on transmission expertise
 *   - rationalist_jurists: primary target — their entire methodological toolkit is delegitimized
 *   - customary_practice_communities: target — local custom loses recognized legal standing absent unanimous consensus
 *   - novel_case_litigants: target — cases without direct textual coverage go unresolved or are strained into forced literal readings
 *   - hanafi_and_shafii_jurists: excluded — hold the competing methodology this reading defines itself against but have no seat within Hanbali jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.62).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Textualist-Literalist Reading of the Jurisprudential Method Kernel").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "Islamic Jurisprudence / Legal Philosophy / Institutional History").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, '0f6a9016-639e-4d5e-8855-e5d35342e3b2').
narrative_ontology:cs_kernel_codification('0f6a9016-639e-4d5e-8855-e5d35342e3b2', fixed_text).
narrative_ontology:cs_authority_grounding('0f6a9016-639e-4d5e-8855-e5d35342e3b2', lineage).
narrative_ontology:cs_interpretation_layer_present('0f6a9016-639e-4d5e-8855-e5d35342e3b2').
narrative_ontology:cs_reading_relation('0f6a9016-639e-4d5e-8855-e5d35342e3b2', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('0f6a9016-639e-4d5e-8855-e5d35342e3b2', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f6a9016-639e-4d5e-8855-e5d35342e3b2', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_axiom('0f6a9016-639e-4d5e-8855-e5d35342e3b2', foundational, analogical_reasoning_is_corrupting_innovation).
narrative_ontology:cs_axiom_status(analogical_reasoning_is_corrupting_innovation, holdable).
narrative_ontology:cs_axiom_grounding('0f6a9016-639e-4d5e-8855-e5d35342e3b2', analogical_reasoning_is_corrupting_innovation, deontological).
narrative_ontology:cs_axiom('0f6a9016-639e-4d5e-8855-e5d35342e3b2', foundational, only_unanimous_consensus_is_valid_source).
narrative_ontology:cs_axiom_status(only_unanimous_consensus_is_valid_source, holdable).
narrative_ontology:cs_axiom_grounding('0f6a9016-639e-4d5e-8855-e5d35342e3b2', only_unanimous_consensus_is_valid_source, conventional).
narrative_ontology:cs_reference_frame('0f6a9016-639e-4d5e-8855-e5d35342e3b2', companion_era_textual_purity).
narrative_ontology:cs_drift_state('0f6a9016-639e-4d5e-8855-e5d35342e3b2', post_classical_juristic_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f6a9016-639e-4d5e-8855-e5d35342e3b2', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hanbali_textualist_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hadith_transmission_specialists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, novel_case_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer legal rulings by insisting every ruling trace to explicit Qur'anic text, sound Hadith, or a directly attested Companion opinion. They author and police the boundary of what counts as bid'ah, and their scholarly authority rests entirely on mastery of transmitted text rather than juristic reasoning — their standing would collapse if reasoning-based method were legitimized alongside theirs.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanbali_textualist_scholars, agenda_setter,
    institutional, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, hanbali_textualist_scholars, beneficiary).

% Their expertise — chains of transmission, narrator biography, authentication grading — becomes the sole currency of legal legitimacy under this reading, since no other interpretive tool is admitted. They gain professional and social standing precisely because the reading forecloses competing methodologies that would not need their skill in the same way.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hadith_transmission_specialists, beneficiary,
    organized, generational, identity_locked, regional).

% Practice analogical reasoning (qiyas) and juristic preference (istihsan) to resolve cases the explicit texts do not address. Under this reading their entire method is branded innovation that corrupts the kernel; they can migrate to Hanafi or Shafi'i communities, but within Hanbali-governed jurisdictions their rulings are delegitimized and their livelihoods as jurists foreclosed.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, regional).

% Rely on locally evolved custom ('urf) to handle everyday disputes — commercial practice, land arrangements, family custom — where no explicit text speaks directly. This reading treats such accommodation as illegitimate unless it can be traced to unanimous consensus, which rarely exists for local custom, leaving their practices without recognized legal standing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities, payer,
    powerless, generational, trapped, local).

% Bring disputes — new commercial instruments, unprecedented family situations, technological questions — for which no directly applicable text or Companion opinion exists. Under strict textualism their cases either go unresolved or are forced into ill-fitting literal analogies presented as text rather than acknowledged reasoning, since acknowledged reasoning is barred.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, novel_case_litigants, payer,
    powerless, immediate, trapped, local).

% Operate competing schools that treat qiyas and structured hierarchy as legitimate. They are excluded from Hanbali courts' internal deliberation entirely; their entire methodological apparatus is what the Hanbali reading defines itself against, but they hold no seat within Hanbali-administered jurisdictions to contest the characterization.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanafi_and_shafii_jurists, excluded,
    organized, civilizational, mobile, continental).

% The doctrinal standard (ijma' as unanimity, not majority or scholarly consensus) that this reading holds as the only valid third source beyond Qur'an and Hadith. Because unanimity across the entire scholarly community is almost never achievable in practice, this standard functions less as an active source of law than as a rhetorical bar that forecloses reasoning-based alternatives without itself generating many actual rulings.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, unanimous_consensus_requirement, observer,
    institutional, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(jurisprudential_method_kernel__hanbali_reading, unanimous_consensus_requirement).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanbali_reading, hanbali_textualist_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, low-interpretive-latitude standard for deriving law directly from revealed text and earliest transmitted practice, reducing the risk that legal rulings drift from the Prophet's example under the guise of reasoned extension.
% TRANSFER_FUNCTION: Moves interpretive authority and the social/professional status that follows it away from jurists skilled in analogical and customary reasoning, concentrating it in scholars whose expertise is textual transmission and hadith authentication; moves the cost of unresolved novel cases onto litigants and communities whose situations the explicit texts do not cover.
% ABSENT_VOICES: Rationalist jurists and communities governed by long-standing custom would object that the reading forecloses legitimate legal reasoning that earlier generations of Companions themselves practiced informally; within Hanbali-administered jurisdictions they have no recognized seat to raise this, since the school's own method excludes their tools from the conversation by definition.
% DISAPPEARANCE_RATIONALE: If the strict textualist standard vanished, Hanbali courts would need to admit qiyas or istihsan (or some functional equivalent) to resolve the large volume of cases the explicit texts do not directly address; the professional standing currently concentrated in hadith-transmission expertise would diffuse toward jurists skilled in reasoned extension, and rulings on novel commercial and technological questions would proceed rather than stall or be forced into strained literal readings.
% FOUNDING_PROBLEM: Early controversies over rationalist theology (kalam) and speculative jurisprudence were seen by figures like Ahmad ibn Hanbal as introducing human speculation into divine law, risking the corruption of revealed guidance by fallible reasoning — the reading was built to wall off legal method from that perceived contamination.
% FOUNDING_PROBLEM_CORROBORATION: Hanbali scholars themselves attest the problem remains live — that reasoned innovation continues to threaten textual fidelity. Historians of Islamic law and jurists from the Hanafi and Shafi'i traditions, writing from outside the Hanbali school, attest that qiyas was already an accepted tool among the Companions and early generations, and that the strict-textualist framing was itself a later methodological innovation responding to contemporaneous theological disputes rather than a preservation of an unbroken original consensus.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the reading does perform a real coordination function — reducing interpretive drift from revealed sources — but does so by foreclosing an entire class of legitimate legal reasoning (qiyas, istihsan, 'urf) that other schools treat as valid tools, redirecting authority and case-resolution capacity toward a narrower scholarly class. Suppression (0.62) is substantial but not absolute: exit to competing schools exists at the continental scope, though it is foreclosed within Hanbali-administered jurisdictions themselves, which is where the constraint's coercive force actually operates. Theater ratio is comparatively low (0.28) because the textual-transmission function is largely genuine scholarly labor, not performance — the extraction operates through genuine, effortful exclusion of a rival method, not through hollow ritual. Accessibility collapse (0.58) reflects that alternatives (Hanafi/Shafi'i method) are conceptually available and practiced elsewhere but are excluded by definition from Hanbali courts. Resistance (0.71) is high because rationalist jurists and their intellectual lineage have persistently contested the textualist foreclosure across centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting textualist scholars' seat, the reading is a faithful preservation of revealed method against corrupting innovation — a mountain-like fidelity claim. From the rationalist jurists' and novel-case litigants' seats, the same standard operates as an enforced foreclosure of a functioning legal tool, leaving real disputes without recourse. The engine computes these divergent seat classifications from the structural power/exit data; the claimed_type (tangled_rope) is authored as the analytically true middle position, not as an average of the two seat experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanbali textualist scholars and hadith specialists sit near the beneficiary end: the reading concentrates interpretive authority and professional standing on exactly their skill set (institutional power, identity-locked because their scholarly identity is constituted by textual mastery). Rationalist jurists, customary communities, and novel-case litigants sit near the target end: their tools are branded illegitimate (rationalist jurists), their practices lose standing (customary communities), or their disputes go unresolved (litigants), and their exit options range from constrained (jurists, who can relocate to other schools but at high cost) to trapped (powerless local litigants and communities with no meaningful jurisdictional choice).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — guarding against theological speculation corrupting revealed law — was live in early Islamic history amid real controversies over kalam and unregulated juristic invention. Whether it remains live today is contested: Hanbali scholars maintain the threat persists; outside historians and rival schools attest that qiyas itself was an early, broadly accepted tool, meaning the strict-textualist boundary was itself a later methodological innovation responding to a particular historical moment, not an unbroken preservation of original consensus. Classifying this as tangled_rope (rather than pure snare) honors that the coordination function — textual fidelity, resistance to unmoored speculation — is real and valued by many outside the beneficiary class, while the asymmetric extraction (foreclosing rationalist method, leaving novel cases and custom unresolved) is also real and falls on identifiable victims. Collapsing this into either pure coordination or pure extraction would erase one half of the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textualism_vs_constructed_boundary,
    'Is the Hanbali reading''s rejection of qiyas a recovery of the earliest, purest legal method, or a later constructed boundary that retroactively excludes tools the earliest generations themselves used informally?',
    'Historical analysis of early Companion and Successor legal reasoning for informal analogical patterns predating the formal qiyas/istihsan vocabulary, cross-checked against independent historiography of early Islamic legal development outside all four schools'' own self-narratives.',
    'If early practice already contained reasoning-by-analogy in substance, the Hanbali boundary is a constructed later innovation rather than a preservation, strengthening the case that beneficiary capture (textualist scholars gaining exclusive authority) rather than genuine kernel-fidelity is driving the reading''s persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualism_vs_constructed_boundary, conceptual, 'Whether Hanbali textualism recovers or retroactively constructs the kernel''s original boundary.').

omega_variable(
    unanimous_consensus_functional_status,
    'Does the strict unanimity requirement for ijma'' function as an active, operative third source of law, or as a rhetorical bar that in practice almost never certifies new rulings and therefore mainly serves to foreclose competing methods rather than generate positive law?',
    'Survey of documented Hanbali legal rulings across history to determine what proportion rest on genuinely certified unanimous consensus versus text/hadith alone versus disguised reasoning presented as literal textual application.',
    'If unanimity rarely certifies rulings in practice, the third source is largely inert and the reading''s real operative content is narrower than claimed — increasing the case that novel disputes are systematically underserved rather than resolved by the doctrine as stated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimous_consensus_functional_status, empirical, 'Whether unanimous consensus is a functioning legal source or a mostly inert rhetorical constraint.').

omega_variable(
    committer_framing_alternative,
    'Could this reading instead be framed as centered on the doctrine of unanimous ijma'' (rather than on textual literalism) as its distinguishing kernel commitment, given that other schools accept majority or scholarly-community consensus?',
    'Compare classification outcomes if the primary axiom were framed around consensus-strictness rather than text-exclusivity; check whether beneficiary/victim sets and ε shift under that alternative framing.',
    'If the consensus-strictness framing produces a materially different beneficiary structure (e.g., emphasizing exclusion of majority-consensus schools rather than exclusion of rationalist method), it would suggest the kernel decomposition itself is under-determined and a further story split may be warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Whether textual-literalism or unanimity-strictness is the more structurally fundamental distinguishing axiom of this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanbali_reading, 0.1).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings decomposing the natural-language concept 'the jurisprudential method kernel' (Qur'an/Hadith derivation methodology across Sunni schools). Each reading — hanafi (qiyas/istihsan admitted), hanbali (this story: strict textualism, unanimous ijma' only), maliki (living Medinan practice as source), shafii (four-tier hierarchy with hadith transmission as arbiter) — is authored as an independent constraint with its own ε, beneficiary/victim structure, and claimed type, per the ε-invariance principle. They are linked here rather than merged because measuring 'the kernel' under each school's own lights yields structurally distinct extraction profiles and victim sets; treating them as one constraint would average away exactly the distinctions the framework exists to preserve.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
