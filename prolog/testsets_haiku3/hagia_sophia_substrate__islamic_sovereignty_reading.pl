% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__islamic_sovereignty_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia Islamic Sovereignty and Waqf Authority
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   The Hagia Sophia sits at the intersection of three legitimacy claims:
 *   Islamic sovereignty based on Ottoman conquest and waqf endowment (the
 *   reading instantiated here), Orthodox restitution based on Byzantine
 *   founding, and universal heritage transcending all three. This story
 *   instantiates THE ISLAMIC SOVEREIGNTY READING — which dates legitimacy to
 *   1453 Ottoman conquest and waqf establishment, reads the 1934
 *   secularization as a deviation from continuous Islamic authority, and
 *   justifies the 2020 decree as restoration. The constraint is CLAIMED as
 *   tangled_rope (coordination of Islamic worship with Ottoman heritage
 *   doctrine) and authored with MODERATE-HIGH extraction (0.68): the reading
 *   vindicates a political coalition and Islamic identity against secularist
 *   and international regimes, suppressing dissenting voices. The
 *   claim/metric gap is deliberate and structural: from the agenda-setter's
 *   frame this is genuine coordination (reassembling Islamic and Ottoman
 *   identity); from the payer seats (non-Muslim visitors, secularist Turks,
 *   international regimes) this is enforced extraction riding atop a
 *   contested legitimacy claim. The engine measures this divergence from the
 *   per-seat structural data.
 *
 * KEY AGENTS:
 *   - AKP political coalition: agenda-setter, institutional power, collects symbolic and electoral capital
 *   - Turkish Islamic constituency: beneficiary, organized power, gains religious and national identity validation
 *   - Non-Muslim visitors: payer, powerless, face contingent access and behavioral restrictions
 *   - Secularist Turkish public: payer, moderate power, identity-locked, experience ideological defeat
 *   - International heritage regime (UNESCO): excluded, institutional power, jurisdiction denied
 *   - Orthodox ecclesiastical interests: excluded, institutional power, restitution claim foreclosed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.61).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia Islamic Sovereignty and Waqf Authority").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, '7f1f2d60-77a0-4229-8c3c-b94bc3ca7254').
narrative_ontology:cs_kernel_codification('7f1f2d60-77a0-4229-8c3c-b94bc3ca7254', formalized).
narrative_ontology:cs_authority_grounding('7f1f2d60-77a0-4229-8c3c-b94bc3ca7254', extraction).
narrative_ontology:cs_interpretation_layer_present('7f1f2d60-77a0-4229-8c3c-b94bc3ca7254').
narrative_ontology:cs_reading_relation('7f1f2d60-77a0-4229-8c3c-b94bc3ca7254', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_reading_relation('7f1f2d60-77a0-4229-8c3c-b94bc3ca7254', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('7f1f2d60-77a0-4229-8c3c-b94bc3ca7254', foundational, ottoman_conquest_establishes_perpetual_islamic_sovereignty).
narrative_ontology:cs_axiom_status(ottoman_conquest_establishes_perpetual_islamic_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('7f1f2d60-77a0-4229-8c3c-b94bc3ca7254', ottoman_conquest_establishes_perpetual_islamic_sovereignty, conventional).
narrative_ontology:cs_axiom('7f1f2d60-77a0-4229-8c3c-b94bc3ca7254', foundational, waqf_endowment_doctrine_supersedes_secular_governance).
narrative_ontology:cs_axiom_status(waqf_endowment_doctrine_supersedes_secular_governance, holdable).
narrative_ontology:cs_axiom_grounding('7f1f2d60-77a0-4229-8c3c-b94bc3ca7254', waqf_endowment_doctrine_supersedes_secular_governance, deontological).
narrative_ontology:cs_reference_frame('7f1f2d60-77a0-4229-8c3c-b94bc3ca7254', ottoman_waqf_sovereignty_framework).
narrative_ontology:cs_drift_state('7f1f2d60-77a0-4229-8c3c-b94bc3ca7254', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7f1f2d60-77a0-4229-8c3c-b94bc3ca7254', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolically).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turkish_public).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, international_heritage_regime).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ruling party retains and defends the 2020 decree converting the Hagia Sophia from a museum to a mosque under waqf authority. Controls the administrative apparatus and justifies the ruling through Islamic sovereignty doctrine and response to popular religious sentiment. Gains symbolic capital and electoral coalition consolidation from the policy. Absorbs diplomatic costs and UNESCO sanction machinery.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% Views the return to Islamic use as righting a historical injustice and vindicating Turkish national and religious identity. Gains symbolic presence in a globally iconic space and institutional validation of Islamic claim to Ottoman heritage. The constraint legitimates a reading of Turkish history and Islam's place in it. Does not bear direct administrative costs; benefits are primarily identity and political representation.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, generational, mobile, national).

% The Hagia Sophia conversion signals global recognition of Islamic claims to contested heritage sites and rejects secular postcolonial arrangements that marginalized Muslim agency in cultural institutions. The symbolic benefit is diffuse across Muslim-majority states and communities; no single organizational actor collects rents, but the precedent reshapes legitimacy claims globally. The beneficiary is partly a doctrinal position, not a unified agent, but coheres as a geopolitical interest in heritage-sovereignty arguments.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolically, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolically).

% Face restrictions on entry during prayer times, required removal of shoes, head coverings, and behavioral restrictions consistent with mosque conduct. Access is permitted but contingent on acceptance of Islamic liturgical norms. Non-Muslim tourism is not prohibited but is subordinated to worship priority. The constraint forces a choice: visit under conditions they do not control, or forgo the site. International visitors experience the site differently than Muslim worshippers — their access is mediated by the rules that define it as Muslim space.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    powerless, biographical, constrained, global).

% Experienced the decree as a reversal of the 1934 secularization ruling by Atatürk that made the site a museum and symbol of secular Turkish modernity. They argue the site should remain neutral or return to Christian stewardship to honor its founding. Their objections to the policy are often treated as ideological opposition to Islam rather than as legitimate heritage claims, which suppresses their voice in the legitimacy conversation. They cannot exit Turkey to escape the constraint's symbolic force — identity is tied to the national public good the site represents.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turkish_public, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turkish_public, excluded).

% UNESCO and international cultural heritage frameworks classify the site as universal heritage transcending national sovereignty claims. The 2020 decree subordinates that regime's authority to Turkish state Islamic sovereignty doctrine, denying UNESCO substantive say in the site's disposition. The regime can protest and delist the site but cannot override executive authority. Its exclusion is structural: the constraint's entire logic is to assert state/Islamic authority OVER international governance norms.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, international_heritage_regime, excluded,
    institutional, generational, constrained, global).

% The Eastern Orthodox tradition claims the site as founding church and seat of Patriarch-level significance. The Islamic sovereignty reading forecloses Orthodox restitution by declaring Ottoman conquest as the legitimating event (1453) rather than the original Byzantine dedication (537). Orthodox voices are not integrated into the Turkish administrative or legitimacy frameworks; their claims are treated as foreign interference or historical nostalgia rather than as valid heritage rights. They are structurally excluded from the decision apparatus.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, orthodox_ecclesiastical_interests, excluded,
    institutional, generational, trapped, global).

% Validated the 2020 decree through the 2020 court ruling that reversed the 1934 Atatürk-era ruling declaring the site a museum. Courts are formally independent but operate within a political environment where the ruling coalition's preferences are clear. The court's reversal was technically a procedural correction but functionally an enabling act for the executive decree. They observe the constraint's operation and provide legal cover for its enforcement.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__islamic_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes singular authoritative governance of a globally contested site: coordinates Muslim worship access, Ottoman historical narrative, and Turkish state control, preventing fragmented competing claims and ambiguity about the site's religious status.
% TRANSFER_FUNCTION: Moves symbolic authority, religious legitimacy, and political capital from secular modernity and international governance to Ottoman Islamic sovereignty and Turkish Islamic constituencies. Non-Muslim tourists absorb access constraints; secularist Turks and international heritage regimes absorb ideological defeat and jurisdictional denial.
% ABSENT_VOICES: Orthodox ecclesiastical authorities and international heritage advocates are structurally excluded from administrative decision-making. Secularist Turkish scholars and secular civil society are marginalized as ideologically hostile. These voices would argue for universal heritage governance, Christian restitution, or secular preservation, but they are not seated in the institutional apparatus that determines legitimacy — their exclusion is the enforcement mechanism itself.
% DISAPPEARANCE_RATIONALE: If the Islamic sovereignty constraint disappeared, the site would revert to museum status (per 1934 ruling) or enter international heritage governance, or possibly be returned to Orthodox ecclesiastical administration. Turkish national identity narratives would shift from Ottoman-Islamic sovereignty to secular modernity or Christian restoration. Global precedent for Islamic sovereignty claims on contested heritage would weaken.
% FOUNDING_PROBLEM: After Ottoman conquest (1453), the site was incorporated into Islamic waqf legal structure, establishing continuous religious endowment and Islamic worship. The 1934 secularization violated this continuity. The constraint restores waqf authority, treating the restoration as correcting the secular deviation from Ottoman-Islamic legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: The AKP coalition and Turkish Islamic jurisprudence scholars attest that Ottoman waqf legitimacy is historically valid and presently live. However, NO corroborating source outside the benefiting parties endorses this framing. International heritage scholars dispute that secularization was a deviation rather than a legitimate reframing. Orthodox ecclesiastical authorities dispute that Ottoman conquest supersedes Byzantine founding. Secularist Turkish historians dispute that waqf authority persists across the 1934 rupture. The founding problem is asserted only by the benefiting parties; no independent corroboration exists.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The reading's ε is high (0.68) because it redistributes symbolic authority and access rights: Islamic constituencies gain institutional presence in a globally iconic site; secular Turks and non-Muslim visitors lose freedom of religious interpretation and unrestricted access. Suppression is substantial (0.61) because the constraint's persistence depends on actively excluding Orthodox claims and secularist objections, not on participant preference — the 2020 court reversal that validated the decree overrode the 1934 secular ruling despite no new facts, signaling political rather than epistemic authority. Theater is moderate (0.42): genuine Islamic worship occurs (real coordination function), but a substantial portion of enforcement activity defends the exclusion of alternative readings rather than supporting the worship function itself. The measurement series traces 90 years of institutional history: the site under 1934 secular framing (near-zero extractiveness, high theater — performative secularism with suppressed Islamic claims); the transition period 1970–2010 as Islamist political movements grew (extractiveness rising, suppression rising, theater declining as the secular framing weakened); the 2020 decree and aftermath (extractiveness stabilizing at 0.68, theater at 0.42 as the constraint solidifies into enforced practice rather than contested reversal). All metrics share one time grid (auditing discipline: every metric authored at every shared time point).
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute radically different types from the same constraint. From the AKP-coalition and Turkish-Islamic-constituency seats: this is coordination (reassembling Muslim identity with the heritage site, solving the problem of secular alienation from Ottoman history). From the non-Muslim-visitor and international-heritage seats: this is extraction (access subordinated to Islamic norms, international voice denied). From the secularist-Turkish seat: this is both — identity-locked identity-payer (cannot exit Turkey to escape the symbolic constraint), experiencing simultaneous suppression of their historical narrative and extraction of their freedom to interpret the site secularly. The structural asymmetry is the core story: all seats see the same decree, the same access rules, the same waqf authority. But the cost/benefit redistribution is radically skewed. The engine derives d separately for each seat from the power atom, exit options, and beneficiary/victim declarations; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The AKP coalition is the structural beneficiary: it collects electoral capital, religious-constituency consolidation, and Ottoman-heritage narrative control. d approaches 0.1 (full beneficiary). Turkish Islamic constituency is primarily beneficiary (gains identity validation, presence in global icon) with modest side-effect costs (constrained tourism, diplomatic friction affects Turkish soft power diffusely). d ≈ 0.25. Non-Muslim visitors and secularist Turks are payer seats. Non-Muslim visitors pay in constrained access; their exit options (visit under rules they don't control, or forgo the site) are constrained. d ≈ 0.75. Secularist Turkish public is identity-locked payer — they cannot exit Turkey to escape the symbolic constraint's force on their national identity. For identity-locked targets, d is maximized: d ≈ 0.88. International heritage regime and Orthodox interests are excluded, not paid, but excluded from something they could theoretically access. Effective extraction differs: heritage regime has escape routes (delisting, alternative funding), Orthodox interests do not (cannot undo Ottoman conquest or rewrite history). Heritage regime d ≈ 0.60 (moderate target), Orthodox interests d ≈ 0.82 (near-full target). No overrides are necessary; the standard derivation chain (beneficiary/victim + exit → d) captures the structure accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Ottoman waqf legitimacy and continuous Islamic authority) was functionally dead for 90 years under the 1934 secular regime. The 2020 decree invoked restoration of waqf authority to justify reconverting the site — framing the 1934 secularization as a deviation from Islamic law rather than a legitimate modernization. The mandate is live again only for the benefiting parties (AKP, Turkish Islamic constituency); international heritage and Orthodox interests dispute that restoration of a superseded mandate is justified. The 1934 ruling treated secularization as the correction, making Islamic waqf authority the historical deviation. The 2020 ruling reversed that, treating secularization as the deviation and Ottoman waqf as the foundational legitimacy. This is a classic mandatrophy flip: the founding problem's definition was reversed, and the legitimacy frame inverted. The constraint does not dissolve because the benefiting parties' power is sufficient to enforce the new frame; but the instability is real (the dispute would reignite if political power shifted). Commentary declares the status as contested because the founding problem's legitimacy is not accepted by non-benefiting seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ottoman_conquest_legitimacy_contested,
    'Is Ottoman conquest in 1453 a legitimate basis for perpetual Islamic authority over a site originally built as Christian Byzantine cathedral, or does founding-as-cathedral establish prior claim?',
    'Philosophical/legal framework determination: no empirical resolution exists because the question is about which historical event has legitimacy priority in inherited sovereignty. Different legal traditions (Islamic waqf law, Christian ecclesiastical canon law, international heritage law) give different answers.',
    'If conquest is accepted as legitimating: Islamic sovereignty reading stands, Orthodox restitution is foreclosed. If founding-as-cathedral is prioritized: Orthodox claims revive, universal heritage reading gains ground. If neither is privileged: the site defaults to shared governance or international trusteeship.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ottoman_conquest_legitimacy_contested, conceptual, 'Which historical event (Byzantine founding vs. Ottoman conquest) constitutes the binding legitimacy claim.').

omega_variable(
    waqf_perpetuity_vs_secular_rupture,
    'Does the 1934 secularization constitute a rupture in waqf authority that can be reversed by decree, or is it a permanent legal dissolution of the waqf endowment with its own legitimacy?',
    'Historical-legal analysis of Ottoman and Turkish legal traditions: was the 1934 ruling a mere administrative transfer (reversible) or a fundamental dissolution requiring new waqf establishment? Ottoman law scholars and Turkish courts disagree on this.',
    'If reversible: the 2020 decree is restoration, and the constraint''s legitimacy rests on recovered waqf authority. If permanent: the 2020 conversion is novel Islamic sovereignty, not restoration, and the extraction appears as new political decision rather than historical correction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_perpetuity_vs_secular_rupture, empirical, 'Whether the 1934 secularization broke or merely suspended waqf authority.').

omega_variable(
    reading_kernel_incommensurability,
    'Are the three readings (Islamic sovereignty, Orthodox restitution, universal heritage) incommensurable framings of the same kernel, or do they admit of a single integrated framework?',
    'Institutional experiment: can any governance arrangement be structured that honors all three readings simultaneously (e.g., shared sacred space with rotating access and authority structures)? Or does each reading require institutional dominance incompatible with the others?',
    'If incommensurable: the constraint is not stabilizable via compromise; it will remain politically contestable as long as legitimacy is at stake. If integrable: a framework accepting all three readings could be constructed, reframing the constraint from zero-sum to multi-valued.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_incommensurability, conceptual, 'Whether the contested kernel admits of a unified legitimacy framework or remains irreducibly triadic.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of secularist and Orthodox voices structural (legal rules excluding them from decision-making) or internalized (they have internalized the illegitimacy of dissent)?',
    'Post-constraint relaxation: if suppression were removed (court returned authority to international regime, or secularization were reversed), would dissenting voices re-mobilize with the same vigor, or would they remain inhibited by internalized defeat?',
    'If structural: removing the constraint''s enforcement machinery could relatively quickly re-activate suppressed voices. If internalized: the constraint carries through ideologically even if formal authority structures change, making reversal harder than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression is enforced externally or carried internally by the targets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 1934, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1934, 0.92).
narrative_ontology:measurement(hagi_tr_t1970, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1970, 0.88).
narrative_ontology:measurement(hagi_tr_t2000, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2000, 0.78).
narrative_ontology:measurement(hagi_tr_t2010, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2010, 0.61).
narrative_ontology:measurement(hagi_tr_t2019, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2019, 0.48).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1934, 0.05).
narrative_ontology:measurement(hagi_be_t1970, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1970, 0.08).
narrative_ontology:measurement(hagi_be_t2000, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(hagi_be_t2010, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(hagi_be_t2019, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2019, 0.62).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1934, 0.02).
narrative_ontology:measurement(hagi_su_t1970, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1970, 0.05).
narrative_ontology:measurement(hagi_su_t2000, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(hagi_su_t2010, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(hagi_su_t2019, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2019, 0.54).
narrative_ontology:measurement(hagi_su_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__islamic_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% The hagia_sophia_substrate kernel decomposes into three structurally distinct readings with different beneficiary/victim structures, ε values, and types. The Islamic Sovereignty Reading (this constraint) vindicates Ottoman waqf doctrine with ε=0.68 (moderate-high extraction, political consolidation). The Orthodox Restitution Reading (sibling) vindicates Byzantine founding and ecclesiastical restitution with different victim set and different ε (expected higher, less contestable within Orthodox institutional frame). The Universal Heritage Reading (sibling) vindicates transcultural human heritage with yet another ε (expected lower, emphasizes coordination over extraction). Each reading is a separate constraint because the referent is the same (the standing arrangement) but the ε-evaluation differs by reading (per OQ-26: ε is reading-indexed). These are not perspectives on one constraint; they are incommensurable constraints on the same physical object. Links establish the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__islamic_sovereignty_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
