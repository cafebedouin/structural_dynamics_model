% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: Al-Shafi'i's Four-Tier Hierarchy with Hadith Transmission as Arbiter
 *   domain: Islamic Jurisprudence / Legal Philosophy / Institutional History
 *
 * SUMMARY:
 *   Al-Shafi'i's Risala (c. 820 CE) codified a strict four-source hierarchy
 *   for Islamic law: Qur'an, then authenticated Hadith, then consensus
 *   (Ijma), then analogical reasoning (Qiyas). This resolved the
 *   methodological chaos of early Islamic jurisprudence where regional
 *   schools (Kufan, Medinan, Basran) used hadith selectively, privileged
 *   local practice ('amal), and employed juristic preference (istihsan)
 *   freely. The Shafi'i innovation was making *hadith transmission* — isnad
 *   chains, transmitter reliability, grading — the arbiter of legal
 *   authority. This created a new epistemic gatekeeping class: hadith
 *   scholars (muhaddithun) who controlled which transmissions counted as
 *   binding. The constraint operates as a tangled rope: it genuinely
 *   coordinates a fragmented legal field (Rope) but simultaneously extracts
 *   authority from customary practice and analogical independence (Snare).
 *   Beneficiaries are the hadith scholar class; victims are regional
 *   tradition bearers and analogical reasoning practitioners. The hierarchy
 *   persists through active enforcement: madrasa curricula, judicial
 *   appointment criteria, and the institutionalization of hadith criticism as
 *   a prerequisite for legal authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.72).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Al-Shafi'i's Four-Tier Hierarchy with Hadith Transmission as Arbiter").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "Islamic Jurisprudence / Legal Philosophy / Institutional History").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, '46ff4d21-1e6d-434f-8d99-0394f68550c3').
narrative_ontology:cs_kernel_codification('46ff4d21-1e6d-434f-8d99-0394f68550c3', formalized).
narrative_ontology:cs_authority_grounding('46ff4d21-1e6d-434f-8d99-0394f68550c3', lineage).
narrative_ontology:cs_interpretation_layer_present('46ff4d21-1e6d-434f-8d99-0394f68550c3').
narrative_ontology:cs_reading_relation('46ff4d21-1e6d-434f-8d99-0394f68550c3', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('46ff4d21-1e6d-434f-8d99-0394f68550c3', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('46ff4d21-1e6d-434f-8d99-0394f68550c3', jurisprudential_method_kernel__hanbali_reading, influences).
narrative_ontology:cs_axiom('46ff4d21-1e6d-434f-8d99-0394f68550c3', foundational, hadith_transmission_as_sole_sunna_arbiter).
narrative_ontology:cs_axiom_status(hadith_transmission_as_sole_sunna_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('46ff4d21-1e6d-434f-8d99-0394f68550c3', hadith_transmission_as_sole_sunna_arbiter, conventional).
narrative_ontology:cs_axiom('46ff4d21-1e6d-434f-8d99-0394f68550c3', foundational, four_tier_hierarchy_exhaustive_and_exclusive).
narrative_ontology:cs_axiom_status(four_tier_hierarchy_exhaustive_and_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('46ff4d21-1e6d-434f-8d99-0394f68550c3', four_tier_hierarchy_exhaustive_and_exclusive, conventional).
narrative_ontology:cs_axiom('46ff4d21-1e6d-434f-8d99-0394f68550c3', secondary, istihsan_and_unrestricted_qiyas_invalid).
narrative_ontology:cs_axiom_status(istihsan_and_unrestricted_qiyas_invalid, holdable).
narrative_ontology:cs_axiom_grounding('46ff4d21-1e6d-434f-8d99-0394f68550c3', istihsan_and_unrestricted_qiyas_invalid, conventional).
narrative_ontology:cs_reference_frame('46ff4d21-1e6d-434f-8d99-0394f68550c3', risala_methodology_204_ah).
narrative_ontology:cs_drift_state('46ff4d21-1e6d-434f-8d99-0394f68550c3', classical_madhhab_consolidation_400_ah, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('46ff4d21-1e6d-434f-8d99-0394f68550c3', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, transmission_specialists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, customary_practice_adherents).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, analogical_reasoning_practitioners).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, regional_tradition_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, consensus_participants).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, consensus_participants).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, hadith_authority_supremacy).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, methodological_standardization_necessity).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, transmission_chain_epistemology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the authentication of hadith transmissions (isnad criticism, transmitter grading, hadith classification). Their epistemic authority determines which hadiths enter the legal hierarchy at tier 2. They collect rents through judicial appointments, madrasa positions, patronage, and the social capital of being gatekeepers of the Prophet's sunna. Exit is arbitrage-grade: they can move between patron dynasties, regions, and schools while retaining their specialized capital.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_scholars, beneficiary,
    organized, generational, arbitrage, continental).

% Specialize in hadith transmission, compilation, and criticism (e.g., Bukhari, Muslim, Tirmidhi, later critics like Ibn Hajar, Dhahabi). They set the agenda for which hadiths are 'sahih' and thus legally binding. Their work product (hadith collections, critical apparatus) becomes the infrastructure the legal system runs on. Exit is mobile: their skills transfer across the Islamic world, but they are invested in the hadith-centric epistemology.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, transmission_specialists, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, transmission_specialists, agenda_setter).

% Bearers of regional 'amal (practice) and 'urf (custom) — especially Maliki 'amal ahl al-Madina and local traditions in Kufa, Basra, Syria, Egypt. Their epistemic authority derives from continuous communal practice traced to the Companions. The four-tier hierarchy subordinates this to authenticated hadith (tier 2), requiring them to validate practice through isnad — a foreign epistemology. Exit is identity_locked: abandoning their tradition means abandoning their scholarly identity and communal legitimacy.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, customary_practice_adherents, payer,
    powerless, biographical, identity_locked, regional).

% Hanafi and other jurists who use qiyas (analogical reasoning) and istihsan (juristic preference) as independent sources. The hierarchy restricts qiyas to tier 4 (only when no text exists) and effectively eliminates istihsan. They must now ground reasoning in authenticated hadiths, surrendering interpretive autonomy. Exit is constrained: they can adapt within the hierarchy (become hadith-savvy Hanafis) but cannot reject the hierarchy without losing mainstream legitimacy.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, analogical_reasoning_practitioners, payer,
    moderate, biographical, constrained, continental).

% Local legal communities (e.g., Yemeni, North African, Central Asian) whose practice integrates tribal custom, pre-Islamic law converted to Islamic forms, and saintly lineages. The standardized hierarchy has no tier for these; they are invisible to the isnad-centric epistemology. They are structurally excluded from the conversation — their objections are not heard because they do not speak the language of hadith authentication. Exit is trapped: no pathway into the centralized system without total epistemic conversion.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, regional_tradition_bearers, excluded,
    powerless, generational, trapped, regional).

% The ulema who constitute Ijma (consensus) at tier 3. Formally empowered as a source, but practically constrained: consensus can only form on questions where authenticated hadiths are silent or ambiguous. The hadith scholars (tier 2 controllers) effectively set the agenda for what reaches consensus. They benefit from the hierarchy's stability but pay through subordinated agenda-setting. Exit is constrained: the consensus mechanism is their institutional voice, but it operates within the hadith-defined boundaries.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, consensus_participants, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, consensus_participants, beneficiary).

% Political authorities who endow madrasas, appoint judges, and patronize scholars. They backed the Shafi'i standardization because it produced a portable, verifiable legal technology useful for imperial administration (Abbasids, Seljuks, Mamluks, Ottomans). They set the institutional agenda by funding the hadith-centric curriculum. Exit is arbitrage: they can shift patronage to other schools or secular law, but the hadith-centric system became the default for Islamic legitimacy.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, state_patrons_caliphs_sultans, agenda_setter,
    institutional, biographical, arbitrage, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolved the methodological chaos of early Islamic jurisprudence (c. 150-250 AH) where regional schools used hadith selectively, privileged local practice, and employed unrestricted analogical reasoning. The four-tier hierarchy created a shared epistemic framework enabling cross-school dialogue, predictable judicial outcomes, and a portable legal technology for expanding Islamic polities.
% TRANSFER_FUNCTION: Moves epistemic authority and institutional resources (judicial appointments, madrasa positions, patronage, social capital) from regional tradition bearers and analogical reasoning practitioners to hadith scholars and transmission specialists. The transfer operates through the authentication bottleneck: control over which hadiths count as 'sahih' determines legal outcomes, school dominance, and scholarly careers.
% ABSENT_VOICES: Regional tradition bearers (Yemeni, North African, Central Asian, Southeast Asian) whose practice integrates custom, converted pre-Islamic law, and saintly lineages — they are structurally excluded because their epistemology has no tier in the hierarchy. Also absent: early Kufan and Basran jurists whose living tradition was displaced before the hierarchy solidified; their voices survive only as filtered through the hadith-centric canon.
% DISAPPEARANCE_RATIONALE: If the four-tier hierarchy vanished overnight, the hadith scholar class would lose its gatekeeping monopoly; regional practices (Maliki, Hanafi, local customs) would regain independent epistemic authority; analogical reasoning would expand beyond tier-4 restrictions; state patrons would need new legitimacy mechanisms. The Islamic legal field would fragment into the pre-Shafi'i pluralism — but with 1000+ years of hadith scholarship as a shared resource, not a binding hierarchy.
% FOUNDING_PROBLEM: Early Islamic jurisprudence (c. 100-200 AH) was methodologically inconsistent: Kufans used hadith selectively and favored reasoning (ra'y); Medinans privileged 'amal ahl al-Madina; Basrans developed early hadith criticism but no unified methodology. Judges in different regions reached different rulings on identical cases. No shared standard for verifying Prophetic authority existed. The chaos undermined legal predictability, cross-regional commerce, and imperial administration.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (methodological chaos) is attested as substantially resolved by 400 AH by scholars outside the Shafi'i school: Ibn Hazm (Zahiri, critical of all four schools) notes the hadith corpus is stabilized and methodology codified; Al-Ghazali (Shafi'i but critical of taqlid) acknowledges the four-school equilibrium is established; Ottoman kanunname secular codes (15th-16th c.) demonstrate the hierarchy's coordination function was absorbed into state law. No major jurist after 400 AH argues the *methodological* chaos persists — the dispute shifts to *which* hadiths authenticate and *who* controls authentication.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__shafii_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the hadith scholar class's gatekeeping rents: control over which hadiths authenticate determines legal outcomes. Suppression (0.72) captures the active marginalization of 'amal, istihsan, and unrestricted qiyas — not merely epistemic persuasion but institutional enforcement via curriculum and judiciary. Theater ratio (0.28) is moderate: the coordination function (unified methodology) is real but increasingly performative as the hadith corpus stabilizes and the gatekeeping becomes rent-seeking. Accessibility collapse (0.61) and resistance (0.58) reflect that alternatives (Maliki practice, Hanafi reasoning) persist but are structurally disadvantaged — they survive as minority positions, not erased. The claimed_type tangled_rope captures the hybrid: genuine methodological coordination AND asymmetric extraction via authentication monopoly.
 *
 * PERSPECTIVAL GAP:
 *   From the hadith scholar seat, the hierarchy is a Mountain: it appears as the natural order of legal epistemology, divinely ordained. From the customary practice seat, it is a Snare: an imposed epistemology that delegates their living tradition to a new gatekeeping class. From the analogical reasoning seat, it is a Tangled Rope: coordination is real (shared methodology enables cross-school dialogue) but extraction is real (their independent reasoning is subordinated). The engine computes this divergence from the structural data — the declared beneficiaries/victims and exit options drive the per-seat classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith scholars (beneficiaries) sit at d≈0.15: they collect epistemic rents from controlling the authentication bottleneck, with arbitrage-grade exit (they can move between patrons, regions). Customary practice adherents and analogical practitioners (victims) sit at d≈0.85: their epistemic authority is structurally suppressed, exit is identity_locked (abandoning their methodological tradition means abandoning their scholarly identity). Ijma participants sit near symmetric (d≈0.5): consensus is formally recognized but practically constrained by hadith authentication — you can only consensus on what authenticated hadiths permit. The four-tier hierarchy is not a neutral coordination device; it allocates epistemic authority upward to transmission specialists.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (methodological inconsistency across early schools) was live in 150 AH / 767 CE. By 400 AH / 1009 CE, the hadith corpus was largely stabilized, authentication methodology codified, and the four-school equilibrium established. The constraint's coordination function (unified methodology) was substantially achieved; its extraction function (hadith scholar gatekeeping rents) persisted and intensified. The mandatrophy is unresolved: the arrangement continues to extract via authentication monopoly long after the coordination problem it was built to solve has been largely settled. The theater_ratio rise (0.12→0.28) tracks this: coordination becomes increasingly performative cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the shafii_reading of the jurisprudential_method_kernel. How do sibling readings (hanafi_reading, maliki_reading, hanbali_reading) structurally differ on the beneficiary/victim distribution and extraction locus?',
    'Comparative constraint stories for each sibling reading with their own ε, beneficiaries, and victims; cross-reading analysis via network.affects_constraints.',
    'If sibling readings show substantially different ε and beneficiary structures, the kernel is confirmed as a genuine committer-frame contest; if they converge, the kernel may be a false partition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commiter-frame identification: this is one reading of a contested kernel, not a standalone constraint').

omega_variable(
    hadith_authentication_extraction_locus,
    'Does the extraction fall on the *authentication infrastructure* (isnad science, transmitter criticism, hadith grading) or on the *substantive rulings* that authenticated hadiths produce? The four-tier hierarchy claims Qur''an then Hadith, but the operational bottleneck is hadith authentication.',
    'Historical analysis of whether hadith scholars'' gatekeeping rents derived from authentication services or from the legal rulings those authenticated hadiths grounded; comparison with maliki_reading''s ''amal ahl al-Madina which bypasses isnad for communal practice.',
    'If extraction is on authentication infrastructure, the constraint is a coordination mechanism (hadith verification) with extractive gatekeeping; if on substantive rulings, it is a Snare-like control over legal outcomes. Determines whether claimed_type tangled_rope is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_authentication_extraction_locus, empirical, 'Whether extraction targets the authentication process or the legal output of authenticated hadiths').

omega_variable(
    transmission_standardization_vs_content_control,
    'Did al-Shafi''i''s standardization primarily *coordinate* a fragmented hadith transmission landscape (genuine Rope function) or primarily *concentrate interpretive authority* in a new scholarly class (extraction function)?',
    'Pre-Shafi''i legal practice analysis: measure variance in hadith usage across early schools; post-Shafi''i convergence analysis. If variance was chaotic and standardization reduced transaction costs for legal coordination, Rope component is real. If variance was functional pluralism and standardization eliminated rival epistemologies, extraction dominates.',
    'If primarily coordination, claimed_type tangled_rope is correct (hybrid). If primarily authority concentration, the constraint may be a Snare with coordination cover. Affects mandatrophy analysis: was the founding problem (inconsistent methodology) live or constructed?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transmission_standardization_vs_content_control, conceptual, 'Whether methodological standardization solved a genuine coordination problem or created an extractive monopoly').

omega_variable(
    customary_practice_displacement_mechanism,
    'Was the displacement of customary practice (''urf, ''amal) and independent analogical extension (qiyas/istihsan) a necessary consequence of hadith supremacy, or an active suppression requiring enforcement?',
    'Historical tracking of regional schools'' adaptation: did Maliki and Hanafi traditions voluntarily restrict qiyas/istihsan when confronted with authenticated hadiths, or was institutional pressure (judicial appointments, madrasa curricula, state patronage) required? Compare with hanbali_reading which also suppresses qiyas but via different mechanism (textual literalism).',
    'If displacement was voluntary convergence, suppression metric (0.72) is overstated — the constraint coordinates. If active suppression was required, tangled_rope is confirmed. Affects network edges: does this constraint structurally suppress maliki_reading and hanafi_reading?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_practice_displacement_mechanism, empirical, 'Whether the hierarchy''s lower tiers were displaced by epistemic force or institutional coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 150, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_tr_t150, jurisprudential_method_kernel__shafii_reading, theater_ratio, 150, 0.12).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_tr_t200, jurisprudential_method_kernel__shafii_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_tr_t250, jurisprudential_method_kernel__shafii_reading, theater_ratio, 250, 0.19).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_tr_t300, jurisprudential_method_kernel__shafii_reading, theater_ratio, 300, 0.22).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_tr_t350, jurisprudential_method_kernel__shafii_reading, theater_ratio, 350, 0.25).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_tr_t400, jurisprudential_method_kernel__shafii_reading, theater_ratio, 400, 0.28).

% Extraction over time
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_be_t150, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 150, 0.45).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_be_t200, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_be_t250, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 250, 0.58).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_be_t300, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 300, 0.62).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_be_t350, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 350, 0.65).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_be_t400, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 400, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_su_t150, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 150, 0.55).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_su_t200, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 200, 0.61).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_su_t250, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 250, 0.65).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_su_t300, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 300, 0.68).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_su_t350, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 350, 0.7).
narrative_ontology:measurement(jurisprudential_method_kernel__shafii_reading_su_t400, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 400, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, information_standard).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__shafii_reading, 0.04).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanbali_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, hadith_authentication_infrastructure).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, madrasa_curriculum_standardization).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, judicial_appointment_criteria_classical).

% DUAL FORMULATION NOTE:
% This constraint family (four readings of jurisprudential_method_kernel) decomposes the natural-language concept 'Islamic legal method' into structurally distinct constraints. The shafii_reading centers hadith transmission as arbiter (ε=0.68). The hanafi_reading centers analogical reasoning (expected ε lower on authentication, higher on reasoning flexibility). The maliki_reading centers communal practice (expected ε on transmission, but different beneficiary: Medinan tradition bearers). The hanbali_reading centers textual literalism (expected ε on authentication but different suppression target: all non-textual reasoning). Each reading has its own ε, beneficiaries, victims, and type. They are linked via network.affects_constraints because they compete for the same institutional seats (judgeships, madrasa chairs, state patronage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__shafii_reading, organized, 0.15).
constraint_indexing:directionality_override(jurisprudential_method_kernel__shafii_reading, powerless, 0.85).
constraint_indexing:directionality_override(jurisprudential_method_kernel__shafii_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
