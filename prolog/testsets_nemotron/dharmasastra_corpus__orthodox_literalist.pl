% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Orthodox Literalist Reading of Dharmasastra Varna/Jati Hierarchy
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   The orthodox literalist reading of the Dharmasastra corpus treats
 *   varna/jati hierarchy as eternal, revealed truth requiring literal
 *   observance. This reading has been the dominant interpretive frame for
 *   roughly two millennia, institutionalized through brahmin priesthood,
 *   royal patronage, and pandit establishments. The constraint extracts
 *   ritual privilege, educational access, occupational monopoly, and material
 *   tribute from lower varnas and all women, concentrating benefits in
 *   brahmin and kshatriya institutions. Active enforcement historically
 *   combined scriptural authority, state power, social ostracism, and
 *   violence. The claimed mountain status (eternal natural law) is belied by
 *   the measurable extraction and suppression; the engine will compute
 *   per-seat classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.88).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.92).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.88).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, snare).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Orthodox Literalist Reading of Dharmasastra Varna/Jati Hierarchy").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '50ff4cae-8735-4cf5-95f2-1b6f5187cd79').
narrative_ontology:cs_kernel_codification('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', fixed_text).
narrative_ontology:cs_authority_grounding('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', lineage).
narrative_ontology:cs_interpretation_layer_present('50ff4cae-8735-4cf5-95f2-1b6f5187cd79').
narrative_ontology:cs_reading_relation('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_reading_relation('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', dharmasastra_corpus__reformist_contextual, influences).
narrative_ontology:cs_axiom('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', foundational, varna_hierarchy_eternal_revealed).
narrative_ontology:cs_axiom_status(varna_hierarchy_eternal_revealed, holdable).
narrative_ontology:cs_axiom_grounding('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', varna_hierarchy_eternal_revealed, theological).
narrative_ontology:cs_axiom('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', foundational, brahmin_interpretive_monopoly).
narrative_ontology:cs_axiom_status(brahmin_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', brahmin_interpretive_monopoly, theological).
narrative_ontology:cs_axiom('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', secondary, caste_as_cosmic_debt).
narrative_ontology:cs_axiom_status(caste_as_cosmic_debt, holdable).
narrative_ontology:cs_axiom_grounding('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', caste_as_cosmic_debt, theological).
narrative_ontology:cs_reference_frame('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', vedic_sacrificial_orthopraxy).
narrative_ontology:cs_drift_state('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', contemporary_constitutional_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('50ff4cae-8735-4cf5-95f2-1b6f5187cd79', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, kshatriya_aristocracy).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, orthodox_pandit_establishment).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalit_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudra_laboring_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_all_varnas).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, adivasi_tribal_groups).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, non_twice_born_males).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, varna_dharma_eternal).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, brahmin_supremacy_revealed).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, caste_hierarchy_divine_ordinance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control ritual performance, textual interpretation, and sacramental gatekeeping across the Hindu ecumene. Collect dakshina, temple revenues, and educational gatekeeping rents. Their authority derives from the claim that only twice-born males can perform Vedic rites and interpret shruti/smriti. Exit is arbitrage-grade: they can migrate to reformist or secular institutions while retaining cultural capital, but the reading's validity is their professional existence.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood, agenda_setter).

% Administer the textual tradition through pathashalas, mathas, and judicial pandit courts (historically). Their institutional survival depends on the literalist reading's authority — without it, their interpretive monopoly and state patronage collapse. Exit is identity-locked: the pandit identity is constituted by mastery of the very texts whose eternal validity they certify; abandoning the reading dissolves the professional self.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, orthodox_pandit_establishment, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, orthodox_pandit_establishment, beneficiary).

% Gain legitimating ideology (kshatriya dharma as protector of varna order) and administrative cooperation from brahmin establishments. Historically enforced the hierarchy through state power. Bear costs of maintaining enforcement machinery and managing resistance. Exit is constrained: can adopt reformist or secular legitimations but loses the specific divine-ordinance mandate that distinguishes traditional kingship.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, kshatriya_aristocracy, beneficiary,
    powerful, biographical, constrained, regional).

% Bear the full extraction: ritual pollution stigma, occupational hereditary bondage, denial of education/temple entry/water access, violence for norm violation. Caste is ascribed at birth; religious identity fuses with social existence. Conversion (Buddhism, Christianity, Islam, Sikhism) is possible but carries immense cultural severance and often fails to erase caste stigma. Legal protections exist but enforcement is weak; the internalized frame makes exit existentially costly.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalit_communities, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, dalit_communities, excluded).

% Bear extraction through occupational hereditary assignment, ritual exclusion (no upanayana, no Vedic study), and service obligations to twice-born varnas. Some mobility via sanskritization (adopting upper-caste customs) but this validates the hierarchy rather than escaping it. Identity-locked: jati identity is the primary social fact; exit requires total cultural rupture.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudra_laboring_castes, payer,
    powerless, biographical, identity_locked, local).

% Bear gendered extraction across all varnas: exclusion from Vedic education and rites, dependence on male relatives for ritual status, widowhood penalties, patrilineal property exclusion. Even brahmin women are subordinate to brahmin men within the hierarchy. Exit is identity-locked: gender and varna are co-constituted in the textual framework; the reading offers no path to ritual autonomy for women.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_all_varnas, payer,
    powerless, biographical, identity_locked, local).

% Positioned outside the varna system entirely (avarna), subject to assimilationist pressure (sanskritization) or exclusion. Bear extraction through land alienation, forest rights denial, and cultural erasure when incorporated as low-status jatis. Exit is constrained: can resist assimilation (maintaining tribal identity) but face state marginalization; can accept incorporation but enter at the bottom of the hierarchy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, adivasi_tribal_groups, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, adivasi_tribal_groups, excluded).

% Shudra and avarna men who bear the hierarchy's labor extraction but may hold local power (landowning dominant castes). Some negotiate status through sanskritization or political mobilization. Exit is constrained: can achieve political/economic power but ritual status remains capped without twice-born recognition.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, non_twice_born_males, payer,
    moderate, biographical, constrained, local).

% Organizations (Arya Samaj, Brahmo Samaj, Ramakrishna Mission, etc.) that contest the literalist reading while remaining within the Hindu tradition. They are excluded from the orthodox establishment's institutional succession, temple control, and state patronage. Their exit is mobile: they build parallel institutions but remain contested as 'inauthentic' by the orthodox seat.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_hindu_organizations, excluded,
    organized, generational, mobile, continental).

% Post-1950 Indian state that constitutionally abolishes untouchability (Art. 17) and guarantees equality (Art. 14-15) while managing personal law and temple administration. It observes the constraint's operation from outside the kernel's commitment frame, intervening selectively (Hindu Code Bills, temple entry judgments) but lacking authority to adjudicate the text's internal legitimacy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, constitutional_state, observer,
    institutional, generational, analytical, national).

% Political and intellectual movements (Ambedkarite, Periyarist, Dalit Panthers, etc.) that reject the kernel's authority entirely. They are excluded from the textual conversation by design — the orthodox reading denies them standing to interpret. Their exit is mobile: they build counter-institutions (Buddhist conversion, political parties, universities) but face structural suppression from the state and orthodox establishments.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalit_bahujan_movements, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a ritual-social order for Iron Age and early medieval polities: sacrificial coordination, kingship legitimation, occupational specialization, and conflict containment through hierarchical complementarity. Solved the problem of integrating diverse tribes and occupations into a single ritual polity.
% TRANSFER_FUNCTION: Moves ritual status, educational access, occupational monopoly, land rights, bodily autonomy, and material tribute from Dalits, Shudras, women, and avarnas to brahmin priesthood, pandit establishments, and kshatriya aristocracy. The transfer is justified as cosmic debt (rna) and ritual obligation (dharma).
% ABSENT_VOICES: Dalits, Shudras, women, and avarnas were structurally excluded from the textual conversation — the very texts that consign them to subordination deny them adhikara (qualification) to study or interpret them. Their objection would be that the hierarchy is not cosmic order but human cruelty sanctified by power. They are absent from the tradition's own hermeneutic; their voices enter only through resistance traditions (bhakti, anti-caste movements, colonial testimony).
% DISAPPEARANCE_RATIONALE: If the literalist reading vanished overnight, the ritual economies (temple priesthood, Vedic education, sanskritic patronage) would collapse; the interpretive authority of pandit establishments would dissolve; caste-endogamy and occupational sorting would lose their theological warrant; the personal law system grounded in Dharmasastra would lose its textual anchor. The social world would reorganize — but the internalized identity frames would persist as cultural ghosts.
% FOUNDING_PROBLEM: How to integrate diverse tribal, occupational, and kinship groups into a stable ritual polity with a legitimate sovereign, a functioning sacrificial order, and a reproducible social division of labor — in Iron Age North India where no prior universalistic framework existed.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as dead by: (1) modern constitutional states that provide alternative coordination (citizenship, legal equality, secular law); (2) the historical record showing the varna system never functioned as described in the texts (empirical studies by Romila Thapar, Suvira Jaiswal, Nicholas Dirks); (3) the abolitionist and reformist readings themselves, which demonstrate the problem is solvable without the hierarchy. The orthodox establishment alone attests the problem is live — a self-interested claim.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is extremely high (0.88) because the hierarchy commandeers labor, ritual status, education, property rights, and bodily autonomy across the victim set. Suppression is near-maximal (0.92) because the constraint's persistence has required continuous active enforcement: legal codes (Manusmriti as law), state violence, excommunication, and the internalized identity frame that makes exit existentially costly. Theater ratio is low (0.22) because the ritual and coordination functions (sacrificial order, social stability) are real but dwarfed by the extractive machinery. Accessibility collapse is near-total (0.95) — alternatives (Buddhism, Jainism, bhakti movements, colonial legal equality, constitutional abolition) have been understood for centuries but remain structurally inaccessible to most victims. Resistance is moderate (0.35) — significant but fragmented across time, suppressed by the constraint's multi-layered enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the brahmin/pandit seat, the constraint appears as genuine coordination of cosmic order (rope/mountain). From the Dalit/woman seat, it is pure extraction with no coordination benefit (snare). The kshatriya seat sees a tangled rope: legitimate kingship ideology mixed with priestly dominance. The engine computes this divergence from the structural declarations; the authored claim (snare) reflects the analytical observer seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priesthood and orthodox pandit establishment are full beneficiaries (d ~ 0.05): they collect ritual fees, educational gatekeeping rents, interpretive authority, and state patronage. Kshatriya aristocracy are partial beneficiaries (d ~ 0.25): they gain legitimating ideology and administrative cooperation but bear some costs of enforcement. Dalit communities, shudra castes, and women are full targets (d ~ 0.95): they bear the extraction with identity-locked exit (caste is ascribed, not chosen; religious identity fuses with social existence). Adivasi groups and non-twice-born males are constrained targets (d ~ 0.85): some mobility through sanskritization or conversion, but at immense cultural cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ritual order and social stability in Iron Age polities) is dead — modern constitutional states provide alternative coordination. Yet the arrangement persists through identity-locked internalization and institutional inertia. The beneficiary set (brahmin priesthood, pandit establishments) has narrowed but still captures ritual economies and interpretive authority. The constraint is a snare whose mandate atrophied centuries ago but whose enforcement machinery (now partly internalized) remains operational.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_orthodox_literalist,
    'Is the orthodox literalist reading a genuine commitment to eternal revealed truth, or a constructed reading that concentrates beneficiary status in upper-caste institutions?',
    'Comparative textual history showing when ''eternal literal observance'' became the dominant hermeneutic versus when it was one reading among others; institutional genealogy of pandit establishments that benefit from the reading''s authority.',
    'If the reading is a historically contingent construction that serves identifiable beneficiaries, the constraint''s claimed mountain status (eternal natural law) is a false summit; the engine would reclassify via FSM to tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_orthodox_literalist, conceptual, 'Whether the orthodox literalist reading is a natural law claim or a beneficiary-serving construction').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression maintaining varna/jati hierarchy primarily structural (legal, economic, violent enforcement) or internalized (identity-fused acceptance of hierarchy as cosmic order)?',
    'Post-exit trajectory analysis: when structural barriers are removed (legal equality, urbanization, migration), does the hierarchy persist through internalized identity frames? Measure caste-endogamy rates, occupational sorting, and self-reported identity salience across generations after legal emancipation.',
    'If substantially internalized, effective suppression exceeds the structural measure — targets carry the constraint with them after formal exit, making the constraint more snare-like and less reformable by legal means alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in caste hierarchy maintenance').

omega_variable(
    reformist_contextual_viability,
    'Can the reformist contextual reading (ethical core separable from caste prescriptions) sustain a coherent authority structure without the orthodox literalist reading''s beneficiary base?',
    'Track institutional survival of reformist movements (Arya Samaj, Brahmo Samaj, neo-Vedanta organizations) over generations: do they retain ritual authority, transmit lineage, and command material resources without the hierarchical beneficiary structure?',
    'If reformist readings cannot sustain authority without upper-caste beneficiary capture, the orthodox literalist reading''s beneficiary structure may be structurally necessary for *any* Dharmasastra-based authority — making the kernel itself extractive at root.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformist_contextual_viability, conceptual, 'Whether a non-extractive reading of the kernel can maintain institutional coherence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dhar_tr_t200, dharmasastra_corpus__orthodox_literalist, theater_ratio, 200, 0.18).
narrative_ontology:measurement(dhar_tr_t500, dharmasastra_corpus__orthodox_literalist, theater_ratio, 500, 0.22).
narrative_ontology:measurement(dhar_tr_t800, dharmasastra_corpus__orthodox_literalist, theater_ratio, 800, 0.25).
narrative_ontology:measurement(dhar_tr_t1200, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1200, 0.28).
narrative_ontology:measurement(dhar_tr_t1500, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1500, 0.24).
narrative_ontology:measurement(dhar_tr_t1800, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1800, 0.21).
narrative_ontology:measurement(dhar_tr_t2000, dharmasastra_corpus__orthodox_literalist, theater_ratio, 2000, 0.22).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(dhar_be_t200, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 200, 0.82).
narrative_ontology:measurement(dhar_be_t500, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 500, 0.86).
narrative_ontology:measurement(dhar_be_t800, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 800, 0.89).
narrative_ontology:measurement(dhar_be_t1200, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1200, 0.91).
narrative_ontology:measurement(dhar_be_t1500, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1500, 0.88).
narrative_ontology:measurement(dhar_be_t1800, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1800, 0.85).
narrative_ontology:measurement(dhar_be_t2000, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 2000, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(dhar_su_t200, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 200, 0.88).
narrative_ontology:measurement(dhar_su_t500, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 500, 0.91).
narrative_ontology:measurement(dhar_su_t800, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 800, 0.93).
narrative_ontology:measurement(dhar_su_t1200, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1200, 0.94).
narrative_ontology:measurement(dhar_su_t1500, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1500, 0.92).
narrative_ontology:measurement(dhar_su_t1800, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1800, 0.88).
narrative_ontology:measurement(dhar_su_t2000, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 2000, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__orthodox_literalist, 0.08).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, colonial_personal_law_system).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, constitutional_caste_abolition).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, hindu_code_bills).

% DUAL FORMULATION NOTE:
% The dharmasastra_corpus kernel decomposes into three constraint stories: this orthodox_literalist reading (snare, high extraction, identity-locked victims), the reformist_contextual reading (tangled rope, moderate extraction, contested coordination), and the abolitionist_rejection reading (mountain/rope from the abolitionist seat — negligible extraction for those who have exited the kernel). The orthodox reading's beneficiary structure and enforcement machinery structurally influence the viability of the reformist reading and the political conditions the abolitionist reading contests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, institutional, 0.05).
constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, powerful, 0.25).
constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, powerless, 0.95).
constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
