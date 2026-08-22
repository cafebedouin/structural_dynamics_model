% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: 'Amal Ahl al-Madina as Valid Source
 *   domain: Islamic jurisprudence / legal philosophy / institutional history
 *
 * SUMMARY:
 *   The Maliki school of Islamic jurisprudence claims that law derives from
 *   Qur'an and Hadith as practiced in the Medinan community. The living
 *   tradition of Medina ('amal ahl al-Madina) is positioned as a valid and
 *   uniquely reliable source because Medina preserved the Prophet's practice
 *   most faithfully—the Prophet lived in Medina, his companions made legal
 *   judgments there, and those judgments were transmitted through scholarly
 *   lineage. This is ONE READING of a contested kernel about jurisprudential
 *   sources and methods. The Maliki reading competes with Hanafi (reason and
 *   analogical extension), Shafi'i (rigorous four-tier hierarchy with hadith
 *   transmission as arbiter), and Hanbali (literal text and consensus only)
 *   readings of the same kernel. The constraint operates as tangled rope:
 *   genuine coordination benefit (a clear epistemological standard for judges
 *   and practitioners) coupled with asymmetric extraction (non-Medinan
 *   schools are positioned as less authentically grounded). Enforcement is
 *   active: Maliki scholarship continuously vindicates Medinan precedents,
 *   transmits them through teaching networks, and contests other schools'
 *   epistemologies.
 *
 * KEY AGENTS:
 *   - Medinan scholarly lineage: transmits and enforces the Maliki method; collects institutional authority
 *   - Non-Medinan jurisprudential schools (Hanafi, Shafi'i, Hanbali): bear the cost of being positioned as epistemically less grounded; constrained by the Maliki frame's pre-determination of authenticity
 *   - Contemporary Islamic legal historians: analyze whether Medina's practice actually survives intact and whether it reliably reflects Prophetic intent
 *   - Muslim legal practitioners (judges, muftis): benefit from a clear epistemic framework for judgment; receive coordination benefit from Maliki method's clarity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.48).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.32).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Jurisprudential Method: 'Amal Ahl al-Madina as Valid Source").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "Islamic jurisprudence / legal philosophy / institutional history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, '980e63df-9353-460f-b9bc-a39c8e9b77b5').
narrative_ontology:cs_kernel_codification('980e63df-9353-460f-b9bc-a39c8e9b77b5', fixed_text).
narrative_ontology:cs_authority_grounding('980e63df-9353-460f-b9bc-a39c8e9b77b5', lineage).
narrative_ontology:cs_interpretation_layer_present('980e63df-9353-460f-b9bc-a39c8e9b77b5').
narrative_ontology:cs_reading_relation('980e63df-9353-460f-b9bc-a39c8e9b77b5', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('980e63df-9353-460f-b9bc-a39c8e9b77b5', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('980e63df-9353-460f-b9bc-a39c8e9b77b5', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('980e63df-9353-460f-b9bc-a39c8e9b77b5', foundational, medinan_practice_preserves_prophetic_intent).
narrative_ontology:cs_axiom_status(medinan_practice_preserves_prophetic_intent, holdable).
narrative_ontology:cs_axiom_grounding('980e63df-9353-460f-b9bc-a39c8e9b77b5', medinan_practice_preserves_prophetic_intent, empirically_contingent).
narrative_ontology:cs_axiom('980e63df-9353-460f-b9bc-a39c8e9b77b5', foundational, living_tradition_epistemologically_superior_to_reason_alone).
narrative_ontology:cs_axiom_status(living_tradition_epistemologically_superior_to_reason_alone, holdable).
narrative_ontology:cs_axiom_grounding('980e63df-9353-460f-b9bc-a39c8e9b77b5', living_tradition_epistemologically_superior_to_reason_alone, deontological).
narrative_ontology:cs_reference_frame('980e63df-9353-460f-b9bc-a39c8e9b77b5', medinan_practice_authenticity_doctrine).
narrative_ontology:cs_drift_state('980e63df-9353-460f-b9bc-a39c8e9b77b5', contemporary_historical_scrutiny_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('980e63df-9353-460f-b9bc-a39c8e9b77b5', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, muslim_legal_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmits and interprets the Maliki method, claiming direct continuity with Medina's preserved practice ('amal). Collects institutional authority and legitimacy from the claim that Medina preserved the Prophet's practice most faithfully. Enforces this through teaching networks, fatwah issuance, and canonical text selection. Gains competitive advantage in the marketplace of jurisprudential methods by declaring other schools' sources (extensive qiyas, istihsan, raw hadith transmission without community vetting) as less authentic.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, beneficiary).

% Other schools (Hanafi, Shafi'i, Hanbali) operate under different epistemological constraints: their sources (qiyas, istihsan, strict hadith hierarchy, literal text without communal vetting) are treated as less grounded in authentic Prophetic practice under the Maliki framing. They pay the cost of being positioned as derivative or corrupted by innovation (bid'ah) or excessive reason. Their exit is constrained because Islamic jurisprudence operates as a single legitimacy field — positioning one's school as less authentically Prophetic-grounded carries institutional and social costs.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims, payer,
    institutional, generational, constrained, global).

% The body of documented legal judgments and practices from Medina in the early Islamic period. The Maliki method vindicates these precedents as reliable evidence of Prophetic intent. They are treated as non-agent because they are historical facts, not actors in the legitimacy dispute, though the doctrine makes them beneficiaries of the constraint's validation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_legal_precedents, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jurisprudential_method_kernel__maliki_reading, medinan_legal_precedents).

% Operates a jurisprudential method centered on qiyas (analogical reasoning) and istihsan (juristic preference) as legitimate extensions of divine law. Would argue that reason is a gift from God and must be deployed to solve novel cases; Medina's practice is one input among many, not the arbiter of authenticity. Their exclusion is structural: the Maliki frame pre-determines that their reliance on reason-based derivation marks them as less grounded.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, hanafi_scholarly_lineage, excluded,
    institutional, generational, constrained, global).

% Operates a method centered on literal text and Companion opinion, treating analogical reasoning and juristic preference as bid'ah (innovation). Would argue that Medina's practice, if it diverges from literal text, is itself a corruption; only text and consensus are reliable. Their exclusion is structural: the Maliki frame treats their literalism as severing practice from the Prophetic precedent Medina preserved.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, hanbali_scholarly_lineage, excluded,
    institutional, generational, constrained, global).

% Operates al-Shafi'i's systematic four-tier hierarchy: Qur'an, Hadith, Ijma, Qiyas. Would argue that this hierarchy resolves earlier schools' inconsistencies by making hadith transmission (with rigorous chains, isnad) the arbiter, not communal practice. Their exclusion is structural: the Maliki frame treats Medina's vetting of hadith as superior to the Shafi'i insistence on transmitted documentation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, shafii_scholarly_lineage, excluded,
    institutional, generational, constrained, global).

% Analyze the epistemological status of 'amal ahl al-Madina and test whether Medinan precedents actually align with Qur'an and hadith, or whether the Maliki claim to authenticity rests on selection bias or historical contingency. Their analysis can undermine or support the Maliki frame's legitimacy.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, contemporary_islamic_legal_scholars, observer,
    institutional, biographical, analytical, global).

% Judges, muftis, and legal advisors who adopt the Maliki method benefit from a clear epistemic framework: appeals to Medinan practice provide a unified standard for judgment, reducing uncertainty in novel cases. The framework gives them authority to say 'this is how the Prophet's community resolved this' rather than requiring them to reason from first principles. They receive coordination benefit and institutional legitimacy from the method.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, muslim_legal_practitioners, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified epistemological standard for Islamic jurisprudence: a clear rule for which sources are authoritative (Qur'an, Hadith, then Medinan precedent) and which are derivative or corrupted (excessive qiyas, istihsan without grounding in practice). Solves the problem of what to do when sources conflict or are silent: appeal to how Medina resolved it. Creates continuity between Prophetic intent and contemporary judgment.
% TRANSFER_FUNCTION: Transfers epistemic authority and institutional legitimacy from non-Medinan jurisprudential traditions (Hanafi emphasis on reason, Hanbali emphasis on literal text, Shafi'i emphasis on transmitted hadith chains) to the Maliki lineage and its Medinan rooting. Non-Medinan schools bear the cost of being positioned as less authentically grounded in Prophetic practice. Practitioners who adopt the Maliki method gain a clearer epistemic framework; those who reject it defend their alternative against the charge of inauthenticity.
% ABSENT_VOICES: Scholars from non-Medinan traditions who would argue (and do argue in their own jurisprudential corpus) that reason, textual precision, or rigorous hadith transmission are equally or more reliable than communal practice. Voices that would say Medina's practice was historically contingent, influenced by local custom rather than universal Prophetic intent. Voices that would question whether Medina's recorded practice actually survives intact or whether it has been selectively remembered by Maliki jurists. These dissenting seats exist (in Hanafi, Hanbali, and Shafi'i scholarship) but are excluded from the Maliki frame by definition.
% DISAPPEARANCE_RATIONALE: If the Maliki method and its claim that 'amal ahl al-Madina is a valid authoritative source vanished, other jurisprudential schools would expand their claim to represent the authentic Islamic legal tradition without that constraining competitor. Practitioners would reorganize around Shafi'i, Hanafi, or Hanbali methods. The landscape of Islamic jurisprudence would lose a major methodological pole and would realign around the remaining schools' epistemologies. The institutional authority currently held by Maliki lineages would disperse.
% FOUNDING_PROBLEM: Early Islamic jurisprudence lacked a unified, agreed-upon hierarchy of sources. When Qur'an and Hadith were silent or conflicting, schools diverged: some extended the divine law through reason (qiyas), some refused innovation and adhered to text alone, some required rigorous chains of transmission. The Maliki response was to ground authenticity in Medina's preserved practice—the Prophet was in Medina, Medina's legal judgments were made in his presence and immediately after, so Medina's practice embodies his intent even when not explicitly stated in texts.
% FOUNDING_PROBLEM_CORROBORATION: The Maliki scholarly lineage attests that Medina's practice is a reliable source of law because of its proximity to the Prophet. Contemporary Islamic legal historians, drawing on sources outside the benefiting Maliki tradition, dispute whether Medina's recorded practice actually survives intact (evidence is fragmentary, curated by later jurists), whether 'amal reflects the Prophet's intent or local Medinan custom, and whether the Maliki interpretation of Medina's practice is accurate or selective. Some contemporary scholars (e.g., from Shafi'i and Hanbali traditions) argue that written hadith, with rigorous chains, provides more reliable grounding than oral tradition (even if preserved in Medina). The founding problem is live for those who accept Maliki epistemology but contested for those who ask whether Medina's practice is the best epistemic source.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the Maliki method positions rival schools as less authentic without explicitly denying their sources are Islamic. The extraction is epistemological: a positioning claim ('Medina preserved the Prophet's practice most faithfully') that advantages one lineage's interpretation over others. Suppression is moderate (0.32) because competing schools remain active in Islamic jurisprudence—they are not silenced, but they operate under the structural constraint of defending themselves against the charge of inauthenticity. Theater is low (0.18) because the Maliki method's functional purpose (providing a clear epistemological standard) is real and central to its operation; performativity is not the mechanism. Accessibility collapse is moderate (0.62): once the Maliki frame is understood as privileging Medinan practice, alternatives (pure reason, pure text, rigorous transmission) become less available as 'authentic' sources, though schools continue to defend them. Resistance is moderate-high (0.58) because competing schools actively resist the Maliki claim that Medina preserved practice most faithfully; contemporary historians question whether Medina's practice survives intact. The measurement series shows extractiveness rising over the interval (0.35 to 0.48), suggesting that as Maliki jurisprudence became more institutionalized, the positioning cost for rival schools increased—though it plateaus by the end, suggesting maturation of the institutional hierarchy.
 *
 * PERSPECTIVAL GAP:
 *   From the Medinan scholarly lineage's seat, this is genuine coordination: they offer judges a reliable standard rooted in Prophetic proximity. From the non-Medinan schools' seats, this is enforced extraction: they are required to justify themselves against a pre-determined standard that favors Medina. The engine computes this divergence from power (institutional for both), exit options (arbitrage for Medinan, constrained for non-Medinan), and beneficiary/victim declaration. The payer seat (non-Medinan schools) has constrained exit because Islamic jurisprudence is a single legitimacy field; leaving means institutional isolation. The beneficiary seat (Medinan lineage) has arbitrage-grade exit: they could adopt Shafi'i methods if needed, but they benefit from the status quo.
 *
 * DIRECTIONALITY LOGIC:
 *   Medinan scholarly lineage: directionality = ~0.2 (beneficiary end). They set the agenda (enforce the method through teaching and canonical texts), collect institutional authority, and benefit from the positioning claim. Exit is arbitrage because they could adopt other methods if the Maliki claim lost credibility, but that is not their constraint. Non-Medinan schools: directionality = ~0.75 (target end). They bear the cost of being positioned as less authentically grounded, their exit is constrained (leaving Islamic jurisprudence means institutional isolation), and they have no choice but to defend themselves within a field where Medina is pre-determined as most authentic. Contemporary scholars: directionality = ~0.5 (symmetric or observer). They benefit from clarity about the epistemic standard but also bear the cost of the constraint's enforcement (they must engage with Maliki epistemology even to critique it).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (lack of unified source hierarchy) is contested, not resolved. Maliki method offered one solution; other schools offered others. The constraint persists because it has institutional support (Maliki lineages remain powerful in Islamic scholarship) and because the founding problem itself remains live—Islamic jurisprudence still requires a source hierarchy, and each school claims theirs is most reliable. Mandatrophy would arise if Medina's practice were shown to NOT preserve the Prophet's intent, or if a superior epistemological method were universally accepted—but neither has occurred, and the contest remains active. The constraint prevents mislabeling by maintaining the asymmetry: it is not pure extraction (genuine coordination benefit exists), but the coordination is coupled with extraction (privileging one lineage's interpretation). This is tangled rope, not rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medinan_practice_historical_survival,
    'Does Medina''s legal practice actually survive as a coherent, identifiable tradition, or is it reconstructed selectively by later Maliki jurists based on fragmentary evidence?',
    'Systematic historical analysis of earliest Islamic sources (Qur''an, Hadith, early legal documents) comparing what scholars claim was Medinan practice against what the sources actually attest. Textual and archaeological analysis of Medina''s actual legal institutions in the Prophet''s lifetime and immediately after.',
    'If ''amal ahl al-Madina is substantially reconstructed rather than directly transmitted, the epistemic grounding of the Maliki method is weakened—Medina''s practice would be a reading of texts, not an independent source. This would reduce extractiveness (Maliki schools could not claim special access to preserved practice) and might reclassify the constraint as snare (positioning based on historical fiction rather than coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_practice_historical_survival, empirical, 'Whether Medina''s practice survives intact as transmitted or is selectively reconstructed.').

omega_variable(
    authenticity_criterion_circularity,
    'Is the claim that Medina preserved the Prophet''s practice most faithfully itself grounded in sources, or is it a foundational assertion that pre-determines which sources are authentic?',
    'Textual analysis of Qur''an and earliest Hadith to determine whether they explicitly endorse Medina''s practice as authoritative, or whether Maliki scholars infer this from Medina''s geographic proximity to the Prophet.',
    'If the claim is inferred rather than textually grounded, the Maliki method rests on a foundational axiom (Medina = most faithful) that other schools reject, not on a shared source. This would change the classification from tangled rope (coordination + extraction) to conceptual omega territory: the constraint''s legitimacy depends on whether one accepts the foundational Maliki axiom. Different readings of the same kernel become truly incommensurable, not just different.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authenticity_criterion_circularity, conceptual, 'Whether Medina''s authenticity is derived from sources or is a foundational Maliki assertion.').

omega_variable(
    cultural_specificity_vs_universality,
    'Is Medina''s legal practice authentically Prophetic, or is it Medina-specific custom that the Prophet accommodated to local conditions?',
    'Analysis of Qur''anic and Hadith evidence for whether specific Medinan legal practices (e.g., particular property or marriage rules) are presented as universal law or as context-specific judgment. Comparison with Prophetic rulings in other contexts (pre-Medina in Mecca, etc.).',
    'If Medina''s practice is locally adapted rather than universally prescriptive, then treating it as the highest source for all Islamic law across cultures and time periods would be overextension. The Maliki method would need to distinguish between Medina''s universal principles and its local accommodations. This could reduce extractiveness (less grounds for privileging Medina) or sharpen the constraint into a clearer separation of coordination (universal principles) from extraction (privileging Medina''s interpretation of them).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_specificity_vs_universality, empirical, 'Whether Medina''s practice reflects universal Prophetic law or local accommodation.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the Maliki and Hanbali readings logically foreclose each other, or can both be held within a single jurisprudential framework?',
    'Analysis of whether a scholar can accept Medina''s practice as authoritative (Maliki) while also requiring text-only derivation with no innovation (Hanbali). Can both principles coexist, or does one demand the denial of the other?',
    'If they foreclose each other, the kernel reading relations are foreclosure-type (rare, logically incompatible). If they coexist (different scholars in different traditions can both be correct within their frameworks), the relation is coexistence. This affects the structural classification of the constraint family: foreclosure indicates true incompatibility requiring institutional resolution; coexistence indicates pluralism without logical resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether Maliki and Hanbali readings are logically incompatible or can coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(juri_tr_t8, jurisprudential_method_kernel__maliki_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(juri_tr_t16, jurisprudential_method_kernel__maliki_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(juri_tr_t24, jurisprudential_method_kernel__maliki_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(juri_tr_t32, jurisprudential_method_kernel__maliki_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__maliki_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(juri_be_t8, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(juri_be_t16, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(juri_be_t24, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(juri_be_t32, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 32, 0.48).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(juri_su_t8, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(juri_su_t16, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 16, 0.28).
narrative_ontology:measurement(juri_su_t24, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 24, 0.31).
narrative_ontology:measurement(juri_su_t32, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 32, 0.32).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 40, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__maliki_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the contested kernel 'jurisprudential_method_kernel'. Each reading instantiates a different constraint because each reading produces a different ε (extractiveness of positioning non-adopted schools as less authentically grounded), different beneficiary/victim structure, and different source hierarchy. The Maliki reading treats 'amal ahl al-Madina as uniquely authoritative; the Hanafi reading treats qiyas and istihsan as legitimate extensions; the Shafi'i reading treats rigorous hadith transmission as the arbiter; the Hanbali reading treats literal text and consensus as constraints on innovation. These are not the same constraint viewed from different angles—they are structurally distinct claims about what grounds Islamic law. They are linked via network.affects_constraints because each reading's institutional strength affects the others' legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__maliki_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
