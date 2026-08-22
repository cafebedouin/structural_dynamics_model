% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Marriage as Civil Contract Under Secular State Law
 *   domain: legal/political/social
 *
 * SUMMARY:
 *   This constraint story models the secular contractual reading of the
 *   family_law_authority kernel: marriage as a civil contract between
 *   autonomous individuals, validated solely by state registration, with no
 *   religious requirement, gender-symmetric rights, and interfaith marriage
 *   permitted. The constraint coordinates recognition of intimate
 *   partnerships across a religiously plural society by providing a single,
 *   state-backed legal status that is portable across jurisdictions and
 *   independent of community or clerical approval. Its extraction is low but
 *   non-zero: the state collects administrative fees, imposes documentary
 *   burdens, and reserves the power to define who may marry (age, capacity,
 *   prohibited degrees). The constraint requires active enforcement to
 *   maintain the registration monopoly against competing religious and
 *   customary validity systems. Over the interval (0-100, representing
 *   roughly the late 19th century to present in most jurisdictions),
 *   extractiveness has risen modestly as the state has expanded the
 *   regulatory surround of marriage (licensing, waiting periods, blood tests
 *   historically, now same-sex inclusion), theater has increased as
 *   ceremonial aspects of civil registration have grown, and suppression has
 *   remained stable as the state tolerates parallel religious marriages but
 *   refuses them legal recognition. The claimed type is rope: a genuine
 *   coordination mechanism with minimal coercive overhead, net beneficiaries,
 *   and no suppressed alternatives (religious marriages remain valid within
 *   their communities, just not in civil law).
 *
 * KEY AGENTS:
 *   - secular_couples: Primary beneficiaries (moderate/organized/constrained) — gain portable legal recognition without religious gatekeeping
 *   - interfaith_couples: Primary beneficiaries (moderate/constrained) — only this reading permits their marriage without conversion
 *   - same_sex_couples: Primary beneficiaries (organized/constrained) — gained access through expansion of this reading
 *   - state_registration_authorities: Agenda setters (institutional/generational/arbitrage) — administer the registry, collect fees, define eligibility
 *   - religious_communities: Excluded (organized/generational/trapped) — their validity criteria are not recognized by the state; they would object to state monopoly on legal recognition
 *   - undocumented_and_stateless_persons: Payers (powerless/immediate/trapped) — bear documentary burdens they cannot meet, excluded from recognition
 *   - legal_scholars_and_courts: Observers (analytical/civilizational/analytical) — interpret and contest the boundary between civil and religious authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.22).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.38).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Marriage as Civil Contract Under Secular State Law").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "legal/political/social").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3').
narrative_ontology:cs_kernel_codification('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', formalized).
narrative_ontology:cs_authority_grounding('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', lineage).
narrative_ontology:cs_interpretation_layer_present('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3').
narrative_ontology:cs_reading_relation('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', foundational, state_registration_sole_validity_criterion).
narrative_ontology:cs_axiom_status(state_registration_sole_validity_criterion, holdable).
narrative_ontology:cs_axiom_grounding('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', state_registration_sole_validity_criterion, conventional).
narrative_ontology:cs_axiom('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', foundational, gender_symmetric_contractual_capacity).
narrative_ontology:cs_axiom_status(gender_symmetric_contractual_capacity, holdable).
narrative_ontology:cs_axiom_grounding('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', gender_symmetric_contractual_capacity, deontological).
narrative_ontology:cs_axiom('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', secondary, interfaith_marriage_permitted_without_conversion).
narrative_ontology:cs_axiom_status(interfaith_marriage_permitted_without_conversion, holdable).
narrative_ontology:cs_axiom_grounding('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', interfaith_marriage_permitted_without_conversion, deontological).
narrative_ontology:cs_reference_frame('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', secular_contractual_family_law).
narrative_ontology:cs_drift_state('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6d41adf5-ccf5-4ab3-bcc6-f2a2ae5332f3', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, secular_couples).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, interfaith_couples).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, same_sex_couples).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, state_registration_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, undocumented_and_stateless_persons).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, marriage_equality_principle).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, state_neutrality_in_family_law).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, autonomy_of_contracting_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Couples who marry under civil law without religious ceremony. They gain a portable legal status recognized across jurisdictions, with rights to inheritance, medical decision-making, tax benefits, and immigration sponsorship. Their exit option is forgoing civil marriage, but this means losing all legal protections. They are net beneficiaries: the coordination benefit (legal recognition) far exceeds the administrative cost.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, secular_couples, beneficiary,
    moderate, biographical, constrained, national).

% Couples from different religious backgrounds who cannot marry under any single religious law without conversion. The secular reading is the only path to legal recognition without one partner abandoning their faith. They are beneficiaries in the strongest sense: the constraint enables a marriage that would otherwise be legally impossible. Exit is constrained — they could marry religiously (if one converts) or not at all.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, interfaith_couples, beneficiary,
    moderate, biographical, constrained, national).

% Couples who gained access to civil marriage through judicial or legislative expansion of the secular reading. They are beneficiaries because the constraint's gender-neutral contractual form made their inclusion structurally possible — religious readings did not and do not provide this path. Exit is constrained: they could enter domestic partnerships or civil unions where available, but these lack full portability and federal recognition.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, same_sex_couples, beneficiary,
    organized, biographical, constrained, national).

% The civil registries, vital statistics offices, and family courts that administer marriage licenses, record marriages, and adjudicate dissolutions. They set the procedural rules (waiting periods, documentation, fees, eligibility criteria) and collect the revenue. They could reform the system (and have, e.g., eliminating blood tests, adding same-sex marriage). Their exit is arbitrage-grade: they are the state, they write the rules.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_registration_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Religious bodies (churches, mosques, temples, community councils) that maintain their own marriage validity criteria. They are excluded from the civil recognition monopoly: their marriages have no civil effects unless also registered. They would object to the state's claim to be the sole source of legal validity, but they cannot exit the state's legal order. Their exit is trapped — they must operate within the civil framework for their members' legal rights. They are not victims in the extractive sense (they do not pay the state), but they are structurally displaced from authority.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_communities, excluded,
    organized, generational, trapped, national).

% Persons who cannot meet the documentary requirements for civil marriage registration (birth certificates, identity documents, proof of prior marital status). They bear the full administrative burden of the constraint but receive no recognition. They are payers: the constraint extracts documentary compliance they cannot render, and in return they are excluded from legal marriage and its protections. Exit is trapped — they cannot leave the jurisdiction to marry elsewhere, and they cannot regularize their status through marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, undocumented_and_stateless_persons, payer,
    powerless, immediate, trapped, national).

% Academics, judges, and law reform bodies who interpret the boundary between civil and religious marriage authority, adjudicate conflicts between the readings, and propose reforms. They neither collect nor pay; they analyze. Their seat is the analytical vantage from which the constraint's structure is visible.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, legal_scholars_and_courts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__secular_contractual_reading, state_registration_authorities).
narrative_ontology:fixing_cost_class(family_law_authority__secular_contractual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, state-backed legal status for intimate partnerships that is portable across jurisdictions, independent of religious or community approval, and interoperable with the full apparatus of civil law (inheritance, tax, immigration, custody, medical decision-making). Solves the problem of recognition in a religiously plural society where no single community's marriage law can serve as a universal coordinate.
% TRANSFER_FUNCTION: Moves administrative fees and documentary compliance from marrying couples to state registration authorities, in exchange for the legal status of marriage and its attendant rights. Moves definitional authority over who may marry from religious communities to the state. The transfer is modest in monetary terms but significant in authority terms.
% ABSENT_VOICES: Polygamous families (excluded by the two-person contractual form), child marriage prohibition advocates (who see the secular reading's age floor as still too low in some jurisdictions), asexual and aromantic persons (for whom marriage is an irrelevant form), and radical feminists and queer theorists who critique marriage as an institution — these voices are not in the room when the civil marriage contract is designed, though some enter through litigation.
% DISAPPEARANCE_RATIONALE: If civil marriage registration vanished overnight, the legal infrastructure of spousal rights (inheritance, medical proxy, tax filing, immigration, custody presumptions) would collapse. Religious marriages would have no civil effects. Couples would lose portable legal recognition. The state would lose a primary mechanism for tracking family units for legal and administrative purposes. The world would rearrange: new mechanisms would be needed (contractual bundles, domestic partnership registries, legislative defaults), but the transition would be chaotic and rights would be lost in the interim.
% FOUNDING_PROBLEM: In religiously plural societies, no single community's marriage law can serve as a universal legal coordinate. The state needs a single, portable legal status for intimate partnerships that works across community boundaries, enables interoperable rights, and does not require religious assent. The secular civil contract was built to solve this coordination problem.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record of civil marriage laws (e.g., French Code Civil 1804, German ZGB 1900, Indian Special Marriage Act 1954) — these statutes explicitly cite religious pluralism as the motivation. Contemporary corroboration comes from international human rights bodies (UN Human Rights Committee, European Court of Human Rights) which treat civil marriage access as a requirement for religious freedom and non-discrimination, not merely a state preference. No religious authority corroborates the founding problem; they regard it as a state imposition.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22) reflects the state's monopoly on legal recognition: fees, documentary requirements, and definitional power. This is low because the constraint's primary function is coordination (solving the problem of interoperable recognition in a plural society) and the costs are broadly distributed and modest relative to benefits. Suppression (0.38) is moderate: the constraint does not forbid religious marriage, but it denies religious marriages civil effects unless also registered, and it historically suppressed certain forms (polygamy, child marriage) through criminal law. The constraint is not a mountain (it is a human institution, not a natural law), not a snare (no identifiable victim class that is net-extracted from), not a tangled rope (the coordination function is genuine and not a cover for extraction), not a scaffold (no sunset clause), not a piton (the constraint is actively maintained and expanded, not atrophied). The rope classification fits: a coordination mechanism that solves a genuine collective-action problem (portable, interoperable legal status across pluralism) with minimal coercive overhead, where participants are net beneficiaries and alternatives (religious marriage without civil effects) are not suppressed.
 *
 * PERSPECTIVAL GAP:
 *   From the secular couple's seat: the constraint is a pure coordination good — it delivers recognition, rights, and portability at low cost. From the state's seat: it is an administrative regime that generates revenue and regulatory control. From the religious community's seat: it is an imposition of secular definitions on sacred institutions, extracting their authority over marriage. From the undocumented person's seat: it is a barrier that extracts documentary compliance they cannot render. The engine will compute different effective extraction for each seat from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular, interfaith, and same-sex couples are beneficiaries (d ~ 0.15-0.25): they receive legal recognition they could not otherwise obtain, with constrained but real exit options (they could forgo civil marriage, but lose legal protections). State registration authorities are agenda_setters with arbitrage-grade exit (d ~ 0.05): they administer the system and could reform it. Religious communities are excluded (d ~ 0.6-0.7): they bear the cost of non-recognition without consent, but their exit is trapped — they cannot leave the state's legal order. Undocumented and stateless persons are payers with trapped exit (d ~ 0.8-0.9): they bear documentary burdens they cannot meet and receive no recognition. The engine derives d from these declarations plus power and exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — portable legal recognition across religious pluralism — remains live. The constraint has not outlived its function; rather, its function has expanded (same-sex inclusion, gender symmetry). No mandatrophy resolution is declared. The constraint is not a degraded remnant; it is actively maintained and litigated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this reading of family_law_authority kernel structurally distinct from the religious readings, or does it share a coordination substrate that makes it a mode of the same constraint?',
    'Compare the coordination function of this reading (state registration as sole validity criterion) against religious readings (community/clerical validation as validity criterion). If the coordination problem solved is ''interoperable recognition across plural societies'' vs ''community boundary maintenance'', they are distinct constraints with different ε.',
    'If distinct, this reading''s ε = 0.22 is accurate. If shared substrate, ε must be recomputed over the kernel family and the secular reading''s extraction recalculated as a seat within the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the secular contractual reading is a separate constraint or a seat within a kernel-family constraint').

omega_variable(
    state_capacity_to_enforce_registration,
    'Does the state''s capacity to enforce registration-as-sole-validity depend on coercive machinery that extracts from non-consenting populations (e.g., communities that reject civil marriage)?',
    'Measure enforcement actions against communities that operate parallel marriage systems (religious courts, customary law). If enforcement requires suppressing alternatives, suppression is structural and extractive; if alternatives are tolerated, the constraint is closer to pure coordination.',
    'High suppression with coercive enforcement would shift classification toward tangled_rope. Tolerated pluralism supports rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_capacity_to_enforce_registration, empirical, 'Whether enforcement of civil marriage registration suppresses competing validity systems').

omega_variable(
    registration_cost_as_barrier,
    'Do the administrative costs and documentary requirements of civil registration function as a barrier that extracts from marginalized populations (undocumented migrants, stateless persons, rural poor)?',
    'Compare registration completion rates and cost-burden across socioeconomic strata. If the constraint''s formal neutrality masks disparate exclusion, the extractiveness metric understates effective extraction for those populations.',
    'If registration is a barrier, the constraint has an extractive dimension not captured by the aggregate ε. The constraint family would need a ''registration_access'' sub-constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(registration_cost_as_barrier, empirical, 'Whether civil registration neutrality masks disparate exclusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flasr_tr_t0, family_law_authority__secular_contractual_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(flasr_tr_t0, observed).
narrative_ontology:measurement(flasr_tr_t25, family_law_authority__secular_contractual_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement_basis(flasr_tr_t25, observed).
narrative_ontology:measurement(flasr_tr_t50, family_law_authority__secular_contractual_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(flasr_tr_t50, observed).
narrative_ontology:measurement(flasr_tr_t75, family_law_authority__secular_contractual_reading, theater_ratio, 75, 0.14).
narrative_ontology:measurement_basis(flasr_tr_t75, observed).
narrative_ontology:measurement(flasr_tr_t100, family_law_authority__secular_contractual_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement_basis(flasr_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(flasr_be_t0, family_law_authority__secular_contractual_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(flasr_be_t0, observed).
narrative_ontology:measurement(flasr_be_t25, family_law_authority__secular_contractual_reading, base_extractiveness, 25, 0.2).
narrative_ontology:measurement_basis(flasr_be_t25, observed).
narrative_ontology:measurement(flasr_be_t50, family_law_authority__secular_contractual_reading, base_extractiveness, 50, 0.21).
narrative_ontology:measurement_basis(flasr_be_t50, observed).
narrative_ontology:measurement(flasr_be_t75, family_law_authority__secular_contractual_reading, base_extractiveness, 75, 0.21).
narrative_ontology:measurement_basis(flasr_be_t75, observed).
narrative_ontology:measurement(flasr_be_t100, family_law_authority__secular_contractual_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement_basis(flasr_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(flasr_su_t0, family_law_authority__secular_contractual_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(flasr_su_t0, observed).
narrative_ontology:measurement(flasr_su_t25, family_law_authority__secular_contractual_reading, suppression_requirement, 25, 0.36).
narrative_ontology:measurement_basis(flasr_su_t25, observed).
narrative_ontology:measurement(flasr_su_t50, family_law_authority__secular_contractual_reading, suppression_requirement, 50, 0.37).
narrative_ontology:measurement_basis(flasr_su_t50, observed).
narrative_ontology:measurement(flasr_su_t75, family_law_authority__secular_contractual_reading, suppression_requirement, 75, 0.37).
narrative_ontology:measurement_basis(flasr_su_t75, observed).
narrative_ontology:measurement(flasr_su_t100, family_law_authority__secular_contractual_reading, suppression_requirement, 100, 0.38).
narrative_ontology:measurement_basis(flasr_su_t100, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=100
narrative_ontology:measurement(flasr_grid_01, family_law_authority__secular_contractual_reading, accessibility_collapse(class), 0, 0.55).
narrative_ontology:measurement_basis(flasr_grid_01, observed).
narrative_ontology:measurement(flasr_grid_02, family_law_authority__secular_contractual_reading, accessibility_collapse(class), 100, 0.58).
narrative_ontology:measurement_basis(flasr_grid_02, observed).
narrative_ontology:measurement(flasr_grid_03, family_law_authority__secular_contractual_reading, accessibility_collapse(individual), 0, 0.25).
narrative_ontology:measurement_basis(flasr_grid_03, observed).
narrative_ontology:measurement(flasr_grid_04, family_law_authority__secular_contractual_reading, accessibility_collapse(individual), 100, 0.3).
narrative_ontology:measurement_basis(flasr_grid_04, observed).
narrative_ontology:measurement(flasr_grid_05, family_law_authority__secular_contractual_reading, accessibility_collapse(organizational), 0, 0.4).
narrative_ontology:measurement_basis(flasr_grid_05, observed).
narrative_ontology:measurement(flasr_grid_06, family_law_authority__secular_contractual_reading, accessibility_collapse(organizational), 100, 0.45).
narrative_ontology:measurement_basis(flasr_grid_06, observed).
narrative_ontology:measurement(flasr_grid_07, family_law_authority__secular_contractual_reading, accessibility_collapse(structural), 0, 0.45).
narrative_ontology:measurement_basis(flasr_grid_07, observed).
narrative_ontology:measurement(flasr_grid_08, family_law_authority__secular_contractual_reading, accessibility_collapse(structural), 100, 0.42).
narrative_ontology:measurement_basis(flasr_grid_08, observed).
narrative_ontology:measurement(flasr_grid_09, family_law_authority__secular_contractual_reading, resistance(class), 0, 0.7).
narrative_ontology:measurement_basis(flasr_grid_09, observed).
narrative_ontology:measurement(flasr_grid_10, family_law_authority__secular_contractual_reading, resistance(class), 100, 0.65).
narrative_ontology:measurement_basis(flasr_grid_10, observed).
narrative_ontology:measurement(flasr_grid_11, family_law_authority__secular_contractual_reading, resistance(individual), 0, 0.3).
narrative_ontology:measurement_basis(flasr_grid_11, observed).
narrative_ontology:measurement(flasr_grid_12, family_law_authority__secular_contractual_reading, resistance(individual), 100, 0.35).
narrative_ontology:measurement_basis(flasr_grid_12, observed).
narrative_ontology:measurement(flasr_grid_13, family_law_authority__secular_contractual_reading, resistance(organizational), 0, 0.55).
narrative_ontology:measurement_basis(flasr_grid_13, observed).
narrative_ontology:measurement(flasr_grid_14, family_law_authority__secular_contractual_reading, resistance(organizational), 100, 0.6).
narrative_ontology:measurement_basis(flasr_grid_14, observed).
narrative_ontology:measurement(flasr_grid_15, family_law_authority__secular_contractual_reading, resistance(structural), 0, 0.4).
narrative_ontology:measurement_basis(flasr_grid_15, observed).
narrative_ontology:measurement(flasr_grid_16, family_law_authority__secular_contractual_reading, resistance(structural), 100, 0.45).
narrative_ontology:measurement_basis(flasr_grid_16, observed).
narrative_ontology:measurement(flasr_grid_17, family_law_authority__secular_contractual_reading, stakes_inflation(class), 0, 0.4).
narrative_ontology:measurement_basis(flasr_grid_17, observed).
narrative_ontology:measurement(flasr_grid_18, family_law_authority__secular_contractual_reading, stakes_inflation(class), 100, 0.45).
narrative_ontology:measurement_basis(flasr_grid_18, observed).
narrative_ontology:measurement(flasr_grid_19, family_law_authority__secular_contractual_reading, stakes_inflation(individual), 0, 0.15).
narrative_ontology:measurement_basis(flasr_grid_19, observed).
narrative_ontology:measurement(flasr_grid_20, family_law_authority__secular_contractual_reading, stakes_inflation(individual), 100, 0.2).
narrative_ontology:measurement_basis(flasr_grid_20, observed).
narrative_ontology:measurement(flasr_grid_21, family_law_authority__secular_contractual_reading, stakes_inflation(organizational), 0, 0.3).
narrative_ontology:measurement_basis(flasr_grid_21, observed).
narrative_ontology:measurement(flasr_grid_22, family_law_authority__secular_contractual_reading, stakes_inflation(organizational), 100, 0.35).
narrative_ontology:measurement_basis(flasr_grid_22, observed).
narrative_ontology:measurement(flasr_grid_23, family_law_authority__secular_contractual_reading, stakes_inflation(structural), 0, 0.25).
narrative_ontology:measurement_basis(flasr_grid_23, observed).
narrative_ontology:measurement(flasr_grid_24, family_law_authority__secular_contractual_reading, stakes_inflation(structural), 100, 0.28).
narrative_ontology:measurement_basis(flasr_grid_24, observed).
narrative_ontology:measurement(flasr_grid_25, family_law_authority__secular_contractual_reading, suppression(class), 0, 0.5).
narrative_ontology:measurement_basis(flasr_grid_25, observed).
narrative_ontology:measurement(flasr_grid_26, family_law_authority__secular_contractual_reading, suppression(class), 100, 0.52).
narrative_ontology:measurement_basis(flasr_grid_26, observed).
narrative_ontology:measurement(flasr_grid_27, family_law_authority__secular_contractual_reading, suppression(individual), 0, 0.2).
narrative_ontology:measurement_basis(flasr_grid_27, observed).
narrative_ontology:measurement(flasr_grid_28, family_law_authority__secular_contractual_reading, suppression(individual), 100, 0.25).
narrative_ontology:measurement_basis(flasr_grid_28, observed).
narrative_ontology:measurement(flasr_grid_29, family_law_authority__secular_contractual_reading, suppression(organizational), 0, 0.35).
narrative_ontology:measurement_basis(flasr_grid_29, observed).
narrative_ontology:measurement(flasr_grid_30, family_law_authority__secular_contractual_reading, suppression(organizational), 100, 0.38).
narrative_ontology:measurement_basis(flasr_grid_30, observed).
narrative_ontology:measurement(flasr_grid_31, family_law_authority__secular_contractual_reading, suppression(structural), 0, 0.3).
narrative_ontology:measurement_basis(flasr_grid_31, observed).
narrative_ontology:measurement(flasr_grid_32, family_law_authority__secular_contractual_reading, suppression(structural), 100, 0.32).
narrative_ontology:measurement_basis(flasr_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(family_law_authority__secular_contractual_reading, 0.1).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, civil_registration_universal_access).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, marriage_equality_jurisprudence).

% DUAL FORMULATION NOTE:
% This reading decomposes the family_law_authority kernel by extracting the state-registration coordination function from the religious validity functions of the sibling readings. The secular reading's ε (0.22) is substantially lower than the religious readings' ε (estimated 0.45-0.65) because it does not enforce community boundary maintenance, gender asymmetry, or conversion requirements. The religious readings coordinate community membership and theological order; this reading coordinates civil legal status across pluralism. They are linked via affects_constraints because the secular reading's civil effects (inheritance, custody, tax) create structural pressure on religious readings to produce civil-compliant outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__secular_contractual_reading, institutional, 0.05).
constraint_indexing:directionality_override(family_law_authority__secular_contractual_reading, organized, 0.2).
constraint_indexing:directionality_override(family_law_authority__secular_contractual_reading, moderate, 0.25).
constraint_indexing:directionality_override(family_law_authority__secular_contractual_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
