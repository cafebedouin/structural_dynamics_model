% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__hereditary_monopoly_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Hereditary Brahminical Ritual Monopoly (Vedic/Dharmic Corpus Reading)
 *   domain: religious/social/interpretive
 *
 * SUMMARY:
 *   This constraint story instantiates the hereditary_monopoly_reading of the
 *   vedic_dharmic_corpus kernel: the claim that ritual authority, textual
 *   interpretation, and varna hierarchy derive from birth into Brahmin
 *   lineage as divinely ordained. The constraint operates through
 *   institutional control of temples, ritual economy (dakshina, priestly
 *   fees, endowment management), and the textual monopoly on shruti/smriti
 *   exegesis. Beneficiaries are the Brahmin priestly class, temple
 *   institutions, and the orthodox scholastic establishment; victims are
 *   Shudras, Dalits (avarnas), women across varnas (barred from Vedic study
 *   and ritual office), and non-Brahmin scholars. The claimed_type is
 *   tangled_rope: genuine coordination (ritual continuity, textual
 *   transmission, cosmic order maintenance) coexists with asymmetric
 *   extraction (material tribute, status monopoly, exclusion from spiritual
 *   authority). Active enforcement is required (temple gatekeeping, legal
 *   recognition of hereditary priesthood, social ostracism of transgressors).
 *   The reading denies the legitimacy of sibling readings — bhakti devotional
 *   and reformist egalitarian — but structurally coexists with them across
 *   different institutional loci.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: Primary beneficiary (institutional/identity_locked) — receives ritual tribute, controls textual interpretation, holds hereditary office
 *   - temple_institutions: Agenda setter (institutional/generational) — administers ritual economy, controls access, enforces hereditary succession
 *   - orthodox_scholastic_establishment: Secondary beneficiary (organized/identity_locked) — produces interpretive authority, legitimizes hierarchy through commentary tradition
 *   - shudra_varna: Primary victim (powerless/identity_locked) — excluded from Vedic ritual, barred from textual study, provides labor and tribute
 *   - dalit_avarna: Primary victim (powerless/trapped) — excluded entirely from varna system, subjected to ritual pollution doctrine, performs stigmatized labor
 *   - women_across_varnas: Victim (powerless to moderate/identity_locked) — barred from Vedic initiation and ritual office even within Brahmin families, subject to patriarchal control legitimized by same texts
 *   - non_brahmin_scholars: Victim (moderate/constrained) — excluded from authoritative interpretation despite textual competence, limited to secondary commentary roles
 *   - state_secular_authority: Observer (institutional/analytical) — constitutional equality framework creates external pressure but recognizes religious autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.78).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Brahminical Ritual Monopoly (Vedic/Dharmic Corpus Reading)").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious/social/interpretive").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, 'fd4cd6a1-01ee-453e-b02a-2c51c447f677').
narrative_ontology:cs_kernel_codification('fd4cd6a1-01ee-453e-b02a-2c51c447f677', fixed_text).
narrative_ontology:cs_authority_grounding('fd4cd6a1-01ee-453e-b02a-2c51c447f677', lineage).
narrative_ontology:cs_interpretation_layer_present('fd4cd6a1-01ee-453e-b02a-2c51c447f677').
narrative_ontology:cs_reading_relation('fd4cd6a1-01ee-453e-b02a-2c51c447f677', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd4cd6a1-01ee-453e-b02a-2c51c447f677', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('fd4cd6a1-01ee-453e-b02a-2c51c447f677', foundational, brahmin_birth_ritual_authority).
narrative_ontology:cs_axiom_status(brahmin_birth_ritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('fd4cd6a1-01ee-453e-b02a-2c51c447f677', brahmin_birth_ritual_authority, deontological).
narrative_ontology:cs_axiom('fd4cd6a1-01ee-453e-b02a-2c51c447f677', foundational, varna_hierarchy_divine_ordination).
narrative_ontology:cs_axiom_status(varna_hierarchy_divine_ordination, holdable).
narrative_ontology:cs_axiom_grounding('fd4cd6a1-01ee-453e-b02a-2c51c447f677', varna_hierarchy_divine_ordination, deontological).
narrative_ontology:cs_axiom('fd4cd6a1-01ee-453e-b02a-2c51c447f677', foundational, textual_interpretation_brahmin_monopoly).
narrative_ontology:cs_axiom_status(textual_interpretation_brahmin_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('fd4cd6a1-01ee-453e-b02a-2c51c447f677', textual_interpretation_brahmin_monopoly, conventional).
narrative_ontology:cs_reference_frame('fd4cd6a1-01ee-453e-b02a-2c51c447f677', brahminical_orthodoxy_shruti_smriti).
narrative_ontology:cs_drift_state('fd4cd6a1-01ee-453e-b02a-2c51c447f677', post_constitutional_equality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fd4cd6a1-01ee-453e-b02a-2c51c447f677', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_institutions).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, orthodox_scholastic_establishment).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_varna).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, dalit_avarna).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women_across_varnas).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_scholars).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, varna_dharma_do_not_ontology).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, ritual_purity_heredity).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, textual_authority_brahmin_monopoly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary claimants to ritual office and textual authority. Receive dakshina, temple honoraria, and endowment revenue. Control access to Vedic education and ritual initiation. Exit requires renouncing caste identity and social world — structurally impossible for most. Their authority is reproduced through birth, upanayana samskara, and institutional recognition.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, beneficiary,
    institutional, generational, identity_locked, continental).

% Administer the ritual economy: manage endowments (hundi, land grants, state funding), appoint hereditary archakas, control temple entry and ritual participation. Enforce hereditary succession through trust deeds and custom. Could reform (some temples have), but endowment structures and devotee expectations create prohibitive fixing cost. State regulation (HRCE acts) creates partial accountability but recognizes hereditary principle.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_institutions, agenda_setter,
    institutional, generational, arbitrage, continental).

% Produces the interpretive tradition (bhashyas, tikas, nibandhas) that legitimizes hereditary authority. Controls Sanskrit pathashalas, Vedic recension transmission, and smarta court adjudication. Gains prestige, patronage, and institutional position. Exit means abandoning the tradition that constitutes their epistemic identity — identity_locked. Some individuals defect to reformist or academic positions, but the establishment as a structure persists.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, orthodox_scholastic_establishment, beneficiary,
    organized, generational, identity_locked, continental).

% Provide labor, agricultural produce, and ritual tribute to Brahmin recipients. Barred from Vedic study, upanayana, and ritual officiation. Participate in temple worship as lay devotees but excluded from inner sanctum and priestly roles. Identity is fused with varna designation — exit requires spatial migration, religious conversion, or political mobilization, all of which carry severe social costs. The constraint extracts labor and tribute while denying spiritual authority.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_varna, payer,
    powerless, biographical, identity_locked, continental).

% Exist outside the varna system entirely. Subjected to ritual pollution doctrine: temple exclusion, water segregation, occupational stigma (manual scavenging, leather work). Extraction is total: labor without remuneration, dignity without recognition, spiritual access without mediation. Exit is structurally blocked: conversion offers partial relief but carries social rupture; political assertion (Ambedkarite movement) faces violent suppression. The constraint's enforcement machinery (social boycott, violence, legal disability historically) targets this group most intensely.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, dalit_avarna, payer,
    powerless, biographical, trapped, continental).

% Barred from Vedic initiation (upanayana) and ritual office across all varnas. Brahmin women participate in domestic ritual but hold no public authority; their status derives from male relatives. Non-Brahmin women face compounded extraction: varna subordination plus patriarchal control legitimized by the same textual tradition. Exit through education, employment, or religious reform is possible but contested — identity_locked because gender and varna are co-constituted in the textual framework.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women_across_varnas, payer,
    moderate, biographical, identity_locked, continental).

% Possess textual competence (Sanskrit, Vedic recension, mimamsa) but excluded from authoritative interpretation (pramana status). Limited to secondary roles: teaching in non-traditional institutions, academic positions, reformist commentary. The constraint gatekeeps the title 'shrotriya' and the right to perform Vedic ritual. Exit into secular academia or reformist movements is possible but carries loss of traditional legitimacy — constrained rather than trapped.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_scholars, payer,
    moderate, biographical, constrained, continental).

% Constitutional framework (Articles 14, 15, 17, 25-28) mandates equality and prohibits untouchability but recognizes religious denomination autonomy. Regulates temple administration through HRCE acts, endowment boards, and judicial review of hereditary priesthood. Creates external pressure but cannot dismantle the constraint without violating religious freedom jurisprudence. Analytical seat: sees full structure but cannot directly change it.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, state_secular_authority, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains ritual continuity, textual transmission, and cosmic order (rta) through a dedicated hereditary priesthood that preserves Vedic recension, ritual precision, and interpretive tradition across generations without state dependence.
% TRANSFER_FUNCTION: Moves material tribute (dakshina, temple offerings, endowment revenue), status capital (ritual purity, spiritual authority), and epistemic monopoly (textual interpretation, ritual officiation) from lower varnas, women, and non-Brahmin scholars to the Brahmin priestly class and temple institutions.
% ABSENT_VOICES: Dalit and Adivasi communities who reject the varna framework entirely; women who would claim ritual authority but are barred by textual prescription; anti-caste movements (Phule, Ambedkar, Periyar lineages) that were historically excluded from the conversation and remain marginalized in traditional institutional fora.
% DISAPPEARANCE_RATIONALE: If hereditary priesthood and varna-based ritual monopoly vanished overnight, temple economies would collapse (endowments tied to hereditary office), ritual transmission would fragment without institutional scaffolding, millions of hereditary priests would lose livelihood and identity, and the textual tradition would lose its primary custodial structure. The religious landscape would reorganize around alternative authorities (bhakti gurus, reformist institutions, state-appointed priests) — contested and chaotic.
% FOUNDING_PROBLEM: Preserve Vedic ritual efficacy and textual fidelity across generations in a pre-literate/early-literate society where ritual error was believed to cause cosmic disorder; ensure a dedicated class maintains the complex oral transmission of shruti without dilution.
% FOUNDING_PROBLEM_CORROBORATION: Beneficiaries (orthodox establishments) attest the problem is live: ritual precision requires hereditary training, cosmic order depends on qualified officiants. Victims and reformists (Ambedkar, Phule, contemporary Dalit scholars, feminist critics) attest the problem is dead or manufactured: ritual efficacy is a claim without evidence; cosmic order is a metaphysical cover for material hierarchy; textual fidelity was always contested (multiple recensions, regional variations). Colonial ethnography (Risley, Hutton) and Indological scholarship (Olivelle, Doniger) corroborate that varna hierarchy solidified centuries after Vedic composition — the hereditary monopoly is historical accretion, not scriptural essence.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65 reflects high but not total extraction: the constraint coordinates genuine ritual continuity and textual preservation (rope function) while extracting material tribute, status rents, and epistemic monopoly (snare function). Suppression 0.78 is high because alternatives (bhakti movements, reformist reinterpretations, Dalit assertion) are actively contained through temple control, social ostracism, and legal recognition of hereditary priesthood. Theater ratio 0.42 indicates substantial performative maintenance: ritual elaboration and textual commentary increasingly serve to legitimize the hierarchy rather than the coordination function. Accessibility collapse 0.72 is high but not total: bhakti and reformist alternatives persist as live alternatives for those who can access them, but identity-locking prevents exit for most victims. Resistance 0.35 is moderate: resistance exists (anti-caste movements, bhakti traditions, constitutional challenges) but is fragmented and largely contained within the constraint's own logic.
 *
 * PERSPECTIVAL GAP:
 *   The Brahmin priestly seat experiences this as mountain/rope (cosmic order, necessary coordination); the Dalit seat experiences it as snare (total exclusion, internalized pollution); the Shudra seat experiences it as tangled rope (some ritual inclusion but structural subordination); the reformist observer seat experiences it as extractive institution. The engine computes this divergence from the structural data — the claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priestly class and temple institutions are structural beneficiaries (d ~ 0.1-0.2): they collect dakshina, control endowments, hold hereditary office with minimal accountability. Orthodox scholastic establishment is secondary beneficiary (d ~ 0.25): gains interpretive authority and institutional position but depends on priestly class for ritual validation. Shudras and Dalits are primary targets (d ~ 0.85-0.95): birth-ascribed, identity-locked, no exit without total social rupture. Women across varnas are targets (d ~ 0.7-0.85): Brahmin women have ritual inclusion but no authority; lower-caste women face compounded extraction. Non-Brahmin scholars are constrained (d ~ 0.6): textual competence exists but institutional recognition is gatekept. State authority is analytical (d ~ 0.5): constitutional mandate creates pressure but religious autonomy doctrine limits intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ritual continuity, cosmic order maintenance through qualified personnel) is contested: beneficiaries claim it is live; victims and reformists claim it is dead (cosmic order does not require hereditary monopoly) or never existed as claimed. The constraint persists because the cost of fixing (dismantling temple institutions, redistributing endowments, confronting identity-fused belief) is prohibitive for the state and reformers, while beneficiaries lack incentive to change. This is not pure extraction (coordination function is real) nor pure coordination (extraction is asymmetric and enforced) — the tangled rope classification captures the hybrid structure. The mandatrophy_resolved flag is false: the mandate has outlived its function but the arrangement persists through identity-locked enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_hierarchy,
    'Is the varna hierarchy a genuine natural law of cosmic order (rta/dharma) or a constructed social hierarchy that benefits identifiable agents?',
    'Comparative analysis of textual strata (Shruti vs. Smriti), archaeological evidence of social organization, and the historical emergence of endogamous jatis. If hierarchy tracks textual interpolation and material power rather than invariant cosmic order, the natural-law claim fails.',
    'If constructed, the constraint is a false summit mountain (FSM candidate) or tangled rope with concentrated extraction; if natural law, it remains mountain from the internal reading''s seat but snare from excluded seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_hierarchy, conceptual, 'Whether varna hierarchy is ontological or constructed').

omega_variable(
    reading_commitment_location,
    'Where is the structural disagreement between this reading and its siblings located — in the kernel text itself, the interpretive tradition, the authority structure, or the material enforcement?',
    'Trace each sibling''s divergence point: bhakti_reading locates it in the authority structure (guru/lineage vs. direct access); reformist_reading locates it in the interpretive tradition (historical-critical vs. traditional). The hereditary_monopoly_reading locates it nowhere — it denies divergence exists.',
    'If disagreement is in authority structure, the kernel is stable and readings are institutional; if in interpretive tradition, the kernel is under-specified; if in text, the kernel is fragmented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_commitment_location, conceptual, 'Location of structural disagreement among kernel readings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (temple control, legal disability, economic exclusion) or internalized (self-concept fused with varna identity, belief in karmic desert)?',
    'Post-exit suppression trajectory: if suppression persists after legal barriers are removed (e.g., post-1950 constitutional equality), reclassify as partially internalized. Measure via longitudinal studies of caste identity persistence.',
    'If internalized, effective suppression is higher than structural measure suggests — the target carries the suppression after formal exit. Affects directionality derivation for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in caste constraint').

omega_variable(
    bhakti_devotional_coexistence,
    'Does the bhakti devotional reading genuinely foreclose this reading within a single framework, or do they coexist as live positions held by different parties?',
    'Examine historical practice: do bhakti traditions that gain institutional power maintain hereditary priesthood for ritual functions (e.g., temple archakas)? If yes, coexistence; if they structurally replace hereditary authority with devotional qualification, foreclosure.',
    'Foreclosure would make the readings mutually exclusive in any single institutional framework; coexistence means both remain live across different institutional loci.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bhakti_devotional_coexistence, conceptual, 'Structural relation between hereditary monopoly and bhakti devotional readings').

omega_variable(
    reformist_constitutional_pressure,
    'Does the reformist egalitarian reading create structural downstream pressure on this reading''s legitimacy conditions without foreclosing it?',
    'Track temple entry legislation, anti-discrimination law, and state regulation of religious institutions. If reformist pressure changes resource flows (state funding, legal recognition) without eliminating hereditary priesthood, the relation is influences.',
    'Influences relation means this reading''s operating environment shifts (legitimacy erosion, resource pressure) but its core premise remains holdable within its own framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformist_constitutional_pressure, empirical, 'Reformist reading''s structural pressure on hereditary monopoly reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1800, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1800, 0.25).
narrative_ontology:measurement(vedi_tr_t1850, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1850, 0.3).
narrative_ontology:measurement(vedi_tr_t1900, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1900, 0.38).
narrative_ontology:measurement(vedi_tr_t1950, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1950, 0.45).
narrative_ontology:measurement(vedi_tr_t1975, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 1975, 0.48).
narrative_ontology:measurement(vedi_tr_t2000, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 2000, 0.44).
narrative_ontology:measurement(vedi_tr_t2025, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1800, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1800, 0.58).
narrative_ontology:measurement(vedi_be_t1850, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1850, 0.62).
narrative_ontology:measurement(vedi_be_t1900, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1900, 0.68).
narrative_ontology:measurement(vedi_be_t1950, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1950, 0.72).
narrative_ontology:measurement(vedi_be_t1975, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 1975, 0.68).
narrative_ontology:measurement(vedi_be_t2000, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(vedi_be_t2025, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1800, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(vedi_su_t1850, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1850, 0.75).
narrative_ontology:measurement(vedi_su_t1900, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(vedi_su_t1950, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1950, 0.78).
narrative_ontology:measurement(vedi_su_t1975, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 1975, 0.72).
narrative_ontology:measurement(vedi_su_t2000, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(vedi_su_t2025, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.08).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_endowment_economy).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, personal_law_caste_provisions).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, reservation_policy_constitutional).

% DUAL FORMULATION NOTE:
% The vedic_dharmic_corpus kernel decomposes into three constraint stories corresponding to its three live readings. This story (hereditary_monopoly_reading) claims the kernel's authority is unitary and birth-derived. The bhakti_devotional_reading claims authority is devotional and accessible. The reformist_egalitarian_reading claims authority must conform to equality principles. Their epsilon values differ substantially: this reading ~0.65 (high extraction), bhakti_reading ~0.25 (low extraction, high coordination), reformist_reading ~0.15 (minimal extraction). They are linked via network.affects_constraints because the hereditary monopoly reading's institutional control shapes the operating environment for the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__hereditary_monopoly_reading, institutional, 0.15).
constraint_indexing:directionality_override(vedic_dharmic_corpus__hereditary_monopoly_reading, organized, 0.25).
constraint_indexing:directionality_override(vedic_dharmic_corpus__hereditary_monopoly_reading, powerless, 0.9).
constraint_indexing:directionality_override(vedic_dharmic_corpus__hereditary_monopoly_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
