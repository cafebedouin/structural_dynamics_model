% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Orthodox Literal Reading of Bhagavad Gita: Caste Duty and Righteous War
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The orthodox literal reading of the Bhagavad Gita treats the text as
 *   Krishna's direct revelation to Arjuna on the battlefield of Kurukshetra,
 *   mandating varna-dharma (caste duty) as immutable cosmic law and
 *   legitimizing violence performed as ksatriya-dharma (warrior duty) when
 *   sanctioned by proper authority. This reading became dominant through the
 *   commentarial tradition (Shankara, Ramanuja, Madhva) and was
 *   institutionalized in medieval temple networks, Brahminical law
 *   (Dharmashastra), and modern Hindu nationalist movements. It functions as
 *   a tangled rope: it genuinely coordinates social order, martial
 *   mobilization, and identity for millions (coordination function), while
 *   simultaneously extracting labor, life, and liberty from lower castes,
 *   women, and war victims through the same structure (asymmetric
 *   extraction), maintained by active enforcement (social exclusion,
 *   scriptural authority, state power).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.78).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.82).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Orthodox Literal Reading of Bhagavad Gita: Caste Duty and Righteous War").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '04cf6302-f752-425f-9727-a758460b31c3').
narrative_ontology:cs_kernel_codification('04cf6302-f752-425f-9727-a758460b31c3', fixed_text).
narrative_ontology:cs_authority_grounding('04cf6302-f752-425f-9727-a758460b31c3', lineage).
narrative_ontology:cs_interpretation_layer_present('04cf6302-f752-425f-9727-a758460b31c3').
narrative_ontology:cs_reading_relation('04cf6302-f752-425f-9727-a758460b31c3', gita_kurukshetra_discourse__gandhian_allegorical_reading, forecloses).
narrative_ontology:cs_reading_relation('04cf6302-f752-425f-9727-a758460b31c3', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('04cf6302-f752-425f-9727-a758460b31c3', foundational, varna_dharma_divinely_ordained_immutable).
narrative_ontology:cs_axiom_status(varna_dharma_divinely_ordained_immutable, holdable).
narrative_ontology:cs_axiom_grounding('04cf6302-f752-425f-9727-a758460b31c3', varna_dharma_divinely_ordained_immutable, deontological).
narrative_ontology:cs_axiom('04cf6302-f752-425f-9727-a758460b31c3', foundational, ksatriya_dharma_requires_righteous_violence).
narrative_ontology:cs_axiom_status(ksatriya_dharma_requires_righteous_violence, holdable).
narrative_ontology:cs_axiom_grounding('04cf6302-f752-425f-9727-a758460b31c3', ksatriya_dharma_requires_righteous_violence, deontological).
narrative_ontology:cs_axiom('04cf6302-f752-425f-9727-a758460b31c3', secondary, brahmin_interpretive_monopoly_authoritative).
narrative_ontology:cs_axiom_status(brahmin_interpretive_monopoly_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('04cf6302-f752-425f-9727-a758460b31c3', brahmin_interpretive_monopoly_authoritative, conventional).
narrative_ontology:cs_reference_frame('04cf6302-f752-425f-9727-a758460b31c3', krsna_direct_revelation_kurukshetra).
narrative_ontology:cs_drift_state('04cf6302-f752-425f-9727-a758460b31c3', postcolonial_hindutva_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('04cf6302-f752-425f-9727-a758460b31c3', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_authority).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, orthodox_hindu_institutions).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_dalit_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, war_dead_civilians_combatants).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, women_in_patriarchal_dharma).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, varna_dharma_immutability).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, ksatriya_dharma_righteous_violence).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_epistemic_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains exclusive hermeneutic authority over the Gita's meaning through guru-parampara lineages, temple institutions, and textual commentaries. Their interpretive monopoly legitimates the caste order and sanctifies state power. Exit would require renouncing the identity-structuring claim to represent divine revelation — professional, social, and spiritual suicide within the tradition.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_authority, agenda_setter,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_authority, beneficiary).

% Receives theological justification for martial violence and political rule as sacred duty (ksatriya_dharma). The text sanctifies their monopoly on legitimate force. They pay with their lives in wars framed as dharmic and with the psychological burden of violence legitimated as duty. Exit is constrained by caste identity and the material privileges of the warrior-aristocratic role.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, payer).

% Temple trusts, mathas, educational networks, and political organizations (e.g., RSS-affiliated) that derive legitimacy, funding, and mobilizational capacity from the orthodox reading. They administer the constraint's social enforcement through ritual, education, and cultural policing. Exit would mean losing the theological warrant for their institutional existence.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, orthodox_hindu_institutions, beneficiary,
    organized, generational, constrained, global).

% Structurally locked into the bottom of the varna hierarchy the reading declares divinely ordained. Their labor, exclusion from sacred knowledge, and vulnerability to violence are religiously mandated. Identity-locked because caste is ascribed at birth, reinforced by endogamy, pollution-purity ideology, and the karmic framework that makes their position appear as earned desert. Conversion or exit movements face severe social, economic, and sometimes physical retaliation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_dalit_castes, payer,
    powerless, generational, identity_locked, continental).

% Those killed or wounded in wars justified by the dharmic-war reading — soldiers conscripted by ksatriya_dharma, civilians in conflict zones. They have no exit from the violence the constraint legitimates; their bodies are the extraction's terminal point. The reading provides no mechanism for their consent or dissent.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, war_dead_civilians_combatants, payer,
    powerless, immediate, trapped, local).

% The orthodox reading genders dharma: women's duty is pativrata (husband-service) and reproductive service to the varna order. They bear the constraint's extraction through enforced domesticity, son-preference, widow stigmatization, and exclusion from ritual authority. Exit is constrained by family structure, economic dependence, and the religious framing of their subordination as spiritual merit.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, women_in_patriarchal_dharma, payer,
    moderate, biographical, constrained, continental).

% Read Kurukshetra as internal spiritual struggle; reject literal violence and caste hierarchy. Historically marginalized within orthodox institutions; their reading is treated as distortion by the agenda-setters. They retain mobility because their reading does not require institutional recognition — it operates in ashrams, civil society, and global discourse.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, gandhian_allegorical_interpreters, excluded,
    organized, generational, mobile, global).

% Bhakti traditions (historical: Ramanuja, Caitanya, Kabir; contemporary: ISKCON, Swaminarayan, neo-Vedanta) that teach caste-transcending devotion. Excluded from orthodox authority structures but maintain parallel institutional forms. Mobile because their reading's legitimacy derives from devotional experience, not hereditary office.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, universalist_devotional_practitioners, excluded,
    organized, biographical, mobile, global).

% Legal scholars, political theorists, and human rights observers who evaluate the constraint's compatibility with constitutional equality, secularism, and international humanitarian law. They neither collect nor pay within the dharmic frame but their analyses shape the external pressure on the constraint's enforcement.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, secular_constitutional_analysts, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified cosmological and social order: explains human difference as divinely ordained, coordinates inter-caste obligations, legitimates state violence as cosmic maintenance, and anchors identity in a karmic framework that makes hierarchy appear as justice rather than oppression.
% TRANSFER_FUNCTION: Moves ritual purity, epistemic authority, political legitimacy, and material surplus upward from shudra/dalit castes and women to brahmins and kshatriyas; moves the burden of violence and death onto soldiers and civilians in dharmic wars; moves interpretive control over the text itself to the brahmin commentarial tradition.
% ABSENT_VOICES: The dead of Kurukshetra and subsequent dharmic wars — they cannot testify. Dalit oral traditions and counter-readings (e.g., Phule, Ambedkar, Periyar) were historically excluded from Sanskrit academies and temple institutions. Women's own articulations of dharma (e.g., in bhakti poetry) were filtered through male commentarial lenses. Contemporary queer and trans Hindus who find no place in varna-ashrama-dharma.
% DISAPPEARANCE_RATIONALE: If the orthodox literal reading vanished overnight, the theological warrant for caste hierarchy would collapse, the sacred legitimization of war would dissolve, and the brahmin interpretive monopoly would lose its foundation. Indian constitutional law (Articles 14, 15, 17, 21) would no longer contend with a rival sacred hierarchy. The social order would rearrange — not painlessly, but the constraint's disappearance removes the divine mandate that makes the hierarchy structurally resistant to change.
% FOUNDING_PROBLEM: The Gita was composed/compiled (c. 2nd century BCE–2nd century CE) in a period of Brahmanical reconsolidation after Buddhist and Jain challenges. It needed to: (1) absorb kshatriya warrior ethos into Brahmanical framework, (2) re-sacralize varna hierarchy against renunciatory and heterodox critiques, (3) provide a theodicy for violence that protects the social order, (4) anchor Brahmin authority in Krishna's revelation rather than mere ritual monopoly.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical scholarship (Olivelle, Brockington, van Buitenen, Fitzgerald) corroborates the Brahmanical reconsolidation thesis from textual and epigraphic evidence — outside the beneficiary set. Orthodox tradition asserts the Gita is eternal (sanatana) and the varna order is Krishna's direct creation — the founding problem in their frame is 'how to preserve eternal dharma in kali yuga,' which is a different problem. Ambedkar's 'Annihilation of Caste' and Phule's 'Gulamgiri' provide subaltern corroboration that the constraint was built to enthrall, not liberate.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78) because the constraint moves massive material and symbolic value upward while externalizing violence and death downward. Suppression is very high (0.82) because the constraint's persistence depends on active policing of caste boundaries, censorship of counter-readings, legal enforcement of religious personal laws, and the karmic ideology that makes resistance appear as spiritual self-harm. Theater ratio is moderate (0.45): the text's devotional and philosophical beauty is real, but a growing share of institutional energy goes to defending caste hierarchy and militarized nationalism rather than spiritual liberation. The measurement series shows rising extractiveness and suppression over two millennia as the reading was weaponized by successive polities (Gupta, Mughal collaboration, British codification, postcolonial Hindu nationalism).
 *
 * PERSPECTIVAL GAP:
 *   From the brahmin/kshatriya/institutional seats, the constraint appears as sacred coordination — the only framework that makes cosmic and social order intelligible. From the shudra/dalit, war dead, and women's seats, it appears as divine-sanctioned extraction enforced by identity-lock and violence. The engine will compute this divergence from the structural data; the claimed_type (tangled_rope) acknowledges both functions are real and inseparable in this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmins are structural beneficiaries (d ~ 0.15): they collect epistemic rents, ritual control, and social supremacy. Kshatriyas are dual-positioned (d ~ 0.45): they receive political legitimacy but pay with their lives in dharmic wars. Orthodox institutions are beneficiaries (d ~ 0.2): they capture mobilizational capacity and state patronage. Shudra/Dalit castes are full targets (d ~ 0.95): identity-locked, they bear the full extraction with no exit. War dead are trapped (d = 1.0): zero agency, terminal extraction. Women are constrained payers (d ~ 0.7): caste and gender intersect to constrain exit. Excluded readers (Gandhian, universalist) have mobile exit — their exclusion is the enforcement mechanism that protects the beneficiary structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Brahmanical reconsolidation against heterodoxy) is historically dead — Buddhism/Jainism are no longer existential threats to the varna order, and the kshatriya warrior ethos has been absorbed into modern state militaries. Yet the constraint persists and intensifies because its beneficiaries (brahmin authority, kshatriya political legitimacy, institutional networks) have captured the arrangement. The mandate has atrophied into rent extraction; the coordination function now serves primarily to maintain the extraction structure. This is mandatrophy unresolved: the constraint's original justification is gone but its enforcement has hardened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (caste endogamy, legal disabilities, economic lock-in) or internalized (karmic self-blame, identity-fusion with varna-dharma, devotional surrender as acceptance)?',
    'Post-exit suppression trajectory study: track Dalit converts to Buddhism/Christianity/Islam — does the karmic framework''s psychological suppression persist after structural exit? Compare with Ambedkarite communities that reject the karmic frame entirely.',
    'If internalized suppression is substantial, the constraint''s effective suppression exceeds the structural measure — the target carries the suppression internally after formal exit. This would increase effective extraction for identity-locked seats beyond what structural metrics capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in caste-religious constraints').

omega_variable(
    coordination_extraction_separability,
    'Can the Gita''s genuine coordination function (existential orientation, devotional community, ethical framework) be separated from its extraction function (caste hierarchy, war legitimization, brahmin monopoly) within the orthodox reading itself?',
    'Historical analysis of reform movements within orthodoxy (e.g., Ramanuja''s inclusion of shudras in prapatti, Vivekananda''s reinterpretation, contemporary Dalit Vaishnavism): do any succeed in retaining coordination while shedding extraction, or does the orthodox reading''s internal logic bind them inseparably?',
    'If inseparable, the tangled_rope classification is structurally necessary — the coordination IS the extraction mechanism. If separable, a scaffold transition might be possible within the tradition, and the current tangled_rope state reflects beneficiary capture rather than structural necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the orthodox reading''s coordination and extraction are structurally separable').

omega_variable(
    kernel_reading_foreclosure,
    'Does the orthodox literal reading''s core premise (Gita as Krishna''s literal mandate of varna-dharma and ksatriya-dharma) logically foreclose the gandhian allegorical reading within a single commitment framework, or do they coexist as live options for different parties?',
    'Analyze whether a single hermeneutic community can simultaneously hold: (1) Krishna commands Arjuna to fight a physical war as caste duty, and (2) the Gita''s true teaching is nonviolent internal struggle. If no community sustains both without compartmentalization, the forecloses relation holds.',
    'If forecloses, the two readings cannot coexist in one sampradaya — the kernel''s dispute is zero-sum at the commitment level. If coexists_with, the kernel sustains plural readings across communities, and the dispute is about institutional dominance, not logical incompatibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical foreclosure between orthodox literal and gandhian allegorical readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_orthodox_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gita_orthodox_tr_t500, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 500, 0.3).
narrative_ontology:measurement(gita_orthodox_tr_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1000, 0.38).
narrative_ontology:measurement(gita_orthodox_tr_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1500, 0.42).
narrative_ontology:measurement(gita_orthodox_tr_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 2000, 0.45).

% Extraction over time
narrative_ontology:measurement(gita_orthodox_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(gita_orthodox_be_t500, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 500, 0.7).
narrative_ontology:measurement(gita_orthodox_be_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1000, 0.73).
narrative_ontology:measurement(gita_orthodox_be_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1500, 0.76).
narrative_ontology:measurement(gita_orthodox_be_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 2000, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gita_orthodox_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(gita_orthodox_su_t500, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 500, 0.68).
narrative_ontology:measurement(gita_orthodox_su_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1000, 0.73).
narrative_ontology:measurement(gita_orthodox_su_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1500, 0.78).
narrative_ontology:measurement(gita_orthodox_su_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 2000, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__orthodox_literal_reading, 0.08).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__universalist_devotional_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, manusmriti_varna_dharma_enforcement).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, hindutva_nationalist_mobilization).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, indian_constitutional_secularism).

% DUAL FORMULATION NOTE:
% Part of the gita_kurukshetra_discourse constraint family. This reading (orthodox_literal) has high extractiveness (0.78) because it treats caste hierarchy and righteous violence as divine mandates. The gandhian_allegorical reading has near-zero extractiveness (violence is metaphor, caste is transcended). The universalist_devotional reading has moderate extractiveness (devotional coordination with residual caste habits). The ε-invariance principle requires separate stories: each reading instantiates a different constraint with different beneficiaries, victims, and metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, institutional, 0.15).
constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, powerful, 0.45).
constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, powerless, 0.95).
constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
