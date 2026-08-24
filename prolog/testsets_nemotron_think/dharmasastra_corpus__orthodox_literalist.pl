% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Dharmasastra Varna/Jati Hierarchy as Eternal Revealed Truth
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   The orthodox_literalist reading of the Dharmasastra corpus holds that
 *   varna/jati hierarchy — especially the fourfold varna order with Brahmins
 *   at the apex and Dalits outside it — is eternal, authorless (apaurusheya)
 *   revelation (shruti/smriti) requiring literal observance. This reading is
 *   advanced by traditional priesthoods, monastic orders, and conservative
 *   Hindu nationalist institutions. It claims the hierarchy is not a human
 *   construction but the structural grammar of dharma itself. The metrics
 *   describe a constraint that extracts massively from Dalits, Shudras, and
 *   women via ritual exclusion, occupational fixity, and patriarchal
 *   guardianship, while concentrating sacramental authority, land, and labor
 *   control in upper-caste institutions. The claimed_type is 'mountain'
 *   (eternal natural law); the authored metrics show high extraction,
 *   suppression, and accessibility collapse — the engine will detect the
 *   divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.82).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.88).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.82).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, mountain).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra Varna/Jati Hierarchy as Eternal Revealed Truth").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).
domain_priors:emerges_naturally(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, 'a491b945-f6bc-45e7-8159-2a2b543feaf6').
narrative_ontology:cs_kernel_codification('a491b945-f6bc-45e7-8159-2a2b543feaf6', fixed_text).
narrative_ontology:cs_authority_grounding('a491b945-f6bc-45e7-8159-2a2b543feaf6', lineage).
narrative_ontology:cs_interpretation_layer_present('a491b945-f6bc-45e7-8159-2a2b543feaf6').
narrative_ontology:cs_reading_relation('a491b945-f6bc-45e7-8159-2a2b543feaf6', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_reading_relation('a491b945-f6bc-45e7-8159-2a2b543feaf6', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('a491b945-f6bc-45e7-8159-2a2b543feaf6', foundational, varna_hierarchy_eternally_revealed).
narrative_ontology:cs_axiom_status(varna_hierarchy_eternally_revealed, holdable).
narrative_ontology:cs_axiom_grounding('a491b945-f6bc-45e7-8159-2a2b543feaf6', varna_hierarchy_eternally_revealed, theological).
narrative_ontology:cs_axiom('a491b945-f6bc-45e7-8159-2a2b543feaf6', foundational, literal_observance_mandatory).
narrative_ontology:cs_axiom_status(literal_observance_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('a491b945-f6bc-45e7-8159-2a2b543feaf6', literal_observance_mandatory, theological).
narrative_ontology:cs_reference_frame('a491b945-f6bc-45e7-8159-2a2b543feaf6', shruti_based_eternal_dharma).
narrative_ontology:cs_drift_state('a491b945-f6bc-45e7-8159-2a2b543feaf6', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a491b945-f6bc-45e7-8159-2a2b543feaf6', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, kshatriya_aristocracy).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, vaishya_merchant_elites).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalit_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudra_laborers).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_all_castes).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, varna_dharma_eternal_order).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, ritual_purity_hereditary_transmission).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, caste_based_occupational_duty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret, teach, and enforce Dharmasastra prescriptions; control ritual access, Vedic education, and sacramental authority; derive material support (dakshina, land grants, temple revenues) from the hierarchy they administer; can exit into modern professional roles but lose ritual supremacy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood, agenda_setter,
    institutional, generational, arbitrage, continental).

% Hold political-military authority legitimated by Dharmasastra's kingly dharma; collect taxes and labor from lower varnas; their status depends on the hierarchy's ritual validation; exit into democratic politics dilutes hereditary privilege.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, kshatriya_aristocracy, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, kshatriya_aristocracy, agenda_setter).

% Control trade, finance, and landholding sanctioned by varna dharma; benefit from labor discipline of shudra/dalit workforces; can diversify into modern capital but lose caste-network advantages.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, vaishya_merchant_elites, beneficiary,
    organized, biographical, mobile, national).

% Subjected to untouchability, forced menial labor, denial of temple entry, education, water access, and land ownership; extraction enforced by violence, social boycott, and religious sanction; identity fused to caste status — exit requires total social rupture (conversion, migration, anonymity) and remains incomplete.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalit_communities, payer,
    powerless, generational, identity_locked, local).

% Obliged to service the three higher varnas; excluded from Vedic study, ritual participation, and property rights in classical formulation; extraction takes form of labor tribute, ritual subordination, and occupational fixity; some mobility via sanskritization or urbanization but structural position persists.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudra_laborers, payer,
    moderate, biographical, constrained, regional).

% Subject to patriarchal guardianship (father, husband, son); denied Vedic education, sacramental independence, property rights, and ritual authority across all varnas; upper-caste women gain status proxy through male relatives but remain subordinated; Dalit women face compounded extraction; exit requires rejecting kinship-religious identity.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_all_castes, payer,
    powerless, generational, identity_locked, local).

% Advocate ethical reinterpretation (dharma as righteous conduct) separating it from caste prescriptions; marginalized by orthodox institutions, denied platforms in traditional academies; their voices would challenge the literalist reading's authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_hindu_intellectuals, excluded,
    moderate, biographical, mobile, national).

% Organize political rejection of caste system; demand constitutional rights, reservations, and annihilation of caste; structurally excluded from Dharmasastra's interpretive authority; their absence from the textual tradition is the condition of its literalist persistence.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, ambedkarite_bahujan_movements, excluded,
    organized, generational, constrained, national).

% Adjudicate conflicts between religious freedom (Article 25-26) and equality/non-discrimination (Articles 14-17); their rulings (e.g., temple entry, personal law reform) reshape the constraint's enforcement without engaging its theological claims.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a comprehensive cosmic-social order linking ritual purity, occupational duty, and kingship into a single hierarchical framework that regulates marriage, food, worship, and governance across the subcontinent.
% TRANSFER_FUNCTION: Moves ritual status, educational access, land control, labor power, and sacramental authority from Dalits, Shudras, and women to Brahmin priesthood, Kshatriya aristocracy, and Vaishya elites — enforced by religious sanction and social violence.
% ABSENT_VOICES: Dalit and Bahujan intellectual traditions (from Chokhamela to Ambedkar), feminist reinterpretations of dharma, and anti-caste movements are excluded from the textual authority structure; they would deny the hierarchy's revealed status and demand its dismantling.
% DISAPPEARANCE_RATIONALE: If the literalist reading vanished, the ritual-occupational hierarchy would lose its theological warrant; caste would persist as social practice but lose its 'eternal truth' defense; constitutional equality, anti-discrimination law, and democratic politics would become the unchallenged normative framework.
% FOUNDING_PROBLEM: How to maintain cosmic order (rita) and social cohesion in a heterogeneous society by assigning each group a fixed, ritually sanctioned place in a hierarchy mirroring the cosmic sacrifice (purusha sukta).
% FOUNDING_PROBLEM_CORROBORATION: Modern historians (Romila Thapar, D.D. Kosambi), sociologists (M.N. Srinivas, Andre Beteille), and Ambedkarite scholars attest the varna system was always a theoretical ideal never fully realized, and that colonial enumeration hardened fluid jatis into rigid castes; no corroborating source outside the beneficiary priesthood affirms the founding problem as live.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, ExtMetricName, E),
    domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dharmasastra_corpus__orthodox_literalist),
    narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) reflects the scale of resource, status, and autonomy transfer from subordinated groups to upper castes over millennia. Suppression (0.88) captures the combination of religious sanction (pollution doctrine), violence (atrocity), and structural exclusion (education, temple, water, land) that maintains the hierarchy. Theater ratio (0.42) acknowledges genuine coordination functions (ritual calendar, pilgrimage networks, charitable endowments) alongside the performative maintenance of hierarchy. Accessibility collapse (0.87) is high because the reading declares alternatives religiously impossible — to reject varna dharma is to reject dharma itself. Resistance (0.62) reflects continuous anti-caste struggle (Bhakti, Sikh, Ambedkarite, Dalit Panther, contemporary movements) that the constraint must actively suppress.
 *
 * PERSPECTIVAL GAP:
 *   From the priesthood's seat, the constraint is genuine coordination (cosmic order maintained); from Dalit and women's seats, it is enforced extraction with no exit. The reformist and abolitionist readings are excluded from the authority structure — their absence is structural, not accidental. Constitutional courts observe but do not participate in the theological claim; their interventions reshape enforcement without touching the reading's internal logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priesthood is the primary agenda_setter (controls interpretation, ritual access, education) with arbitrage-grade exit (can enter modern professions) but deep identity investment — d near beneficiary end. Kshatriya and Vaishya elites are beneficiaries with constrained/mobile exit — they collect material gains but can partially diversify. Dalits, Shudras, and women are payers with identity_locked or constrained exit — the hierarchy constitutes their social being; leaving it requires total rupture. Women across castes face compounded extraction: upper-caste women gain status proxy but remain ritually subordinate; Dalit women face intersectional extraction. The engine will compute per-seat directionality from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cosmic-social order via fixed hierarchy) is dead — modern state, market, and democratic citizenship have dissolved the material conditions it organized. Yet the arrangement persists and intensifies extraction (rising metrics over 3000 years). This is mandatrophy: a coordination scaffold that became a mountain claim to prevent its own sunset. The false summit mechanism (FSM) should trigger: mountain claim + identifiable beneficiaries + high extraction = reclassification toward tangled_rope/snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (orthodox_literalist) of the contested dharmasastra_corpus kernel. How does the kernel''s multi-reading structure affect the classification of this specific reading?',
    'Compare the structural metrics and stakeholder configurations across all three readings (orthodox_literalist, reformist_contextual, abolitionist_rejection) to identify which elements are kernel-invariant vs. reading-dependent.',
    'If extraction/suppression metrics are kernel-invariant (high across all readings), the hierarchy itself is extractive regardless of interpretation. If metrics vary radically by reading, the ''eternal truth'' claim may be a reading-specific overlay on a more malleable textual core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committee-frame committer structure: this reading''s ε, beneficiaries, victims relative to sibling readings').

omega_variable(
    false_summit_natural_law_ambiguity,
    'Is the varna/jati hierarchy a genuine natural/cosmic law (mountain) or a constructed social hierarchy that claims natural-law status to benefit identifiable upper-caste groups (false summit)?',
    'Historical-philological analysis of Dharmasastra composition layers; comparative study of varna ideology vs. jati practice; genealogical tracing of beneficiary institutions (priesthood, monarchy, merchant guilds).',
    'If false summit, FSM signature triggers reclassification to tangled_rope (coordination + extraction) or snare (pure extraction). The mountain claim itself becomes an extraction mechanism — naturalizing hierarchy prevents challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_ambiguity, empirical, 'Natural-law vs. constructed hierarchy ambiguity — core FSM candidate').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.88) primarily structural (legal, violent, economic barriers) or substantially internalized (caste habitus, purity psychology, spiritual legitimacy of subordination)?',
    'Post-exit trajectory studies: track Dalit converts to Buddhism/Christianity/Islam — does suppression persist after formal exit? Ethnography of caste habitus in diaspora where structural barriers are reduced.',
    'If substantially internalized, effective suppression exceeds structural measure — the constraint travels with the agent. This would increase χ for identity_locked payers beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism for identity-locked victims').

omega_variable(
    gender_caste_intersectional_extraction,
    'Does the extraction from women_all_castes operate as a single coherent mechanism, or are upper-caste women''s subordination (patriarchal guardianship, ritual exclusion) and Dalit women''s subordination (sexual violence, labor extraction, untouchability) structurally distinct extraction logics?',
    'Disaggregate women_all_castes into varna-specific victim groups; measure extraction vectors separately; test whether upper-caste women function as secondary beneficiaries (status proxy) alongside being payers.',
    'If distinct, the single ''women_all_castes'' stakeholder masks a cross-cutting extraction structure where upper-caste women''s complicity in caste hierarchy is itself an extraction mechanism from Dalit women.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_caste_intersectional_extraction, conceptual, 'Intersectional extraction structure within the gender victim category').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 3000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dharmasastra_orthodox_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dharmasastra_orthodox_tr_t500, dharmasastra_corpus__orthodox_literalist, theater_ratio, 500, 0.3).
narrative_ontology:measurement(dharmasastra_orthodox_tr_t1000, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(dharmasastra_orthodox_tr_t1500, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1500, 0.38).
narrative_ontology:measurement(dharmasastra_orthodox_tr_t2000, dharmasastra_corpus__orthodox_literalist, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(dharmasastra_orthodox_tr_t2500, dharmasastra_corpus__orthodox_literalist, theater_ratio, 2500, 0.41).
narrative_ontology:measurement(dharmasastra_orthodox_tr_t3000, dharmasastra_corpus__orthodox_literalist, theater_ratio, 3000, 0.42).

% Extraction over time
narrative_ontology:measurement(dharmasastra_orthodox_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(dharmasastra_orthodox_be_t500, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 500, 0.72).
narrative_ontology:measurement(dharmasastra_orthodox_be_t1000, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1000, 0.78).
narrative_ontology:measurement(dharmasastra_orthodox_be_t1500, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1500, 0.81).
narrative_ontology:measurement(dharmasastra_orthodox_be_t2000, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 2000, 0.79).
narrative_ontology:measurement(dharmasastra_orthodox_be_t2500, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 2500, 0.8).
narrative_ontology:measurement(dharmasastra_orthodox_be_t3000, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 3000, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(dharmasastra_orthodox_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(dharmasastra_orthodox_su_t500, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 500, 0.8).
narrative_ontology:measurement(dharmasastra_orthodox_su_t1000, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1000, 0.83).
narrative_ontology:measurement(dharmasastra_orthodox_su_t1500, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1500, 0.86).
narrative_ontology:measurement(dharmasastra_orthodox_su_t2000, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 2000, 0.84).
narrative_ontology:measurement(dharmasastra_orthodox_su_t2500, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 2500, 0.87).
narrative_ontology:measurement(dharmasastra_orthodox_su_t3000, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 3000, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__orthodox_literalist, 0.1).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, indian_constitution__equality_nondiscrimination).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, hindu_personal_law__marriage_inheritance).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, reservation_policy__caste_based_affirmative_action).

% DUAL FORMULATION NOTE:
% This story is one member of the dharmasastra_corpus constraint family. The kernel decomposes into three readings with radically different ε values: orthodox_literalist (ε≈0.82, high extraction), reformist_contextual (ε≈0.35, moderate coordination with residual extraction), abolitionist_rejection (ε≈0.05, near-zero extraction — the constraint is the rejection itself). They are linked by network.affects_constraints. The orthodox reading's high extraction is sustained by citing the kernel's authority; the reformist reading's lower extraction depends on the kernel's ethical core; the abolitionist reading's near-zero extraction treats the kernel as void.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, institutional, 0.1).
constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, powerful, 0.25).
constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, organized, 0.3).
constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, powerless, 0.95).
constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
