% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Minority Protections — Expansive Reading: Functional Continuity of Pre-1923 Religious Governance
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The 1923 Treaty of Lausanne (Arts. 37-45) guarantees protections for
 *   non-Muslim minorities in Turkey. The expansive reading interprets these
 *   provisions as guaranteeing the functional continuity of pre-1923
 *   religious governance structures: institutional self-administration (e.g.,
 *   patriarchates, chief rabbinates managing internal affairs), property
 *   rights (foundations/ vakıfs owning and administering communal assets),
 *   and clergy formation through theological schools (e.g., Halki Seminary
 *   for the Ecumenical Patriarchate). This reading presents a moderate
 *   coordination rope: minority institutions are self-governing but depend on
 *   treaty compliance for their legal personality and operational security.
 *   No beneficiary extracts rents from the arrangement; the constraint
 *   coordinates recognition and non-interference. However, the institutions
 *   are vulnerable if the reading loses the interpretive contest — they
 *   become subject to unilateral domestic regulation without international
 *   recourse.
 *
 * KEY AGENTS:
 *   - minority_religious_institutions: Primary beneficiary (institutional/biographical/constrained) — exercise self-administration, hold property, form clergy under treaty guarantee
 *   - turkish_state: Agenda setter (institutional/generational/arbitrage) — holds sovereign authority, implements treaty obligations, controls domestic legal framework
 *   - theological_schools: Beneficiary (organized/biographical/constrained) — depend on treaty protection for legal recognition and operational autonomy
 *   - clergy_candidates: Beneficiary (moderate/biographical/constrained) — access formation pathways guaranteed by treaty
 *   - minority_community_members: Beneficiary (organized/generational/constrained) — depend on institutional continuity for communal life
 *   - guarantor_states: Observer (institutional/generational/analytical) — parties to the treaty with supervisory interest but limited enforcement
 *   - european_court_of_human_rights: Observer (institutional/generational/analytical) — adjudicates individual applications invoking Lausanne via ECHR Art. 1 Protocol 1 and Art. 9
 *   - domestic_courts_turkey: Agenda setter (institutional/biographical/arbitrage) — apply domestic law interpreting or displacing treaty obligations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.15).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.25).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Minority Protections — Expansive Reading: Functional Continuity of Pre-1923 Religious Governance").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, 'dd5fa450-cd20-4ea3-abba-10c7ddde2db1').
narrative_ontology:cs_kernel_codification('dd5fa450-cd20-4ea3-abba-10c7ddde2db1', formalized).
narrative_ontology:cs_authority_grounding('dd5fa450-cd20-4ea3-abba-10c7ddde2db1', lineage).
narrative_ontology:cs_interpretation_layer_present('dd5fa450-cd20-4ea3-abba-10c7ddde2db1').
narrative_ontology:cs_reading_relation('dd5fa450-cd20-4ea3-abba-10c7ddde2db1', lausanne_minority_protections__restrictive_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd5fa450-cd20-4ea3-abba-10c7ddde2db1', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('dd5fa450-cd20-4ea3-abba-10c7ddde2db1', foundational, functional_continuity_requires_institutional_self_administration).
narrative_ontology:cs_axiom_status(functional_continuity_requires_institutional_self_administration, holdable).
narrative_ontology:cs_axiom_grounding('dd5fa450-cd20-4ea3-abba-10c7ddde2db1', functional_continuity_requires_institutional_self_administration, conventional).
narrative_ontology:cs_axiom('dd5fa450-cd20-4ea3-abba-10c7ddde2db1', foundational, clergy_formation_via_theological_schools_is_protected).
narrative_ontology:cs_axiom_status(clergy_formation_via_theological_schools_is_protected, holdable).
narrative_ontology:cs_axiom_grounding('dd5fa450-cd20-4ea3-abba-10c7ddde2db1', clergy_formation_via_theological_schools_is_protected, conventional).
narrative_ontology:cs_reference_frame('dd5fa450-cd20-4ea3-abba-10c7ddde2db1', treaty_text_as_living_obligation).
narrative_ontology:cs_drift_state('dd5fa450-cd20-4ea3-abba-10c7ddde2db1', contemporary_echr_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dd5fa450-cd20-4ea3-abba-10c7ddde2db1', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_religious_institutions).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, theological_schools).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, clergy_candidates).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_community_members).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, treaty_obligations_bind_state_sovereignty).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, religious_minority_institutional_autonomy_is_preserved_by_international_law).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, functional_continuity_requires_self_administration_property_clergy_formation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise self-administration of internal affairs (elections, discipline, liturgy) under treaty guarantee. Hold legal personality recognized by Lausanne. Depend on treaty for protection against unilateral state restructuring. Exit would mean accepting domestic legal personality under Turkish foundation law (Vakıflar Kanunu) which subjects governance to state oversight.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_religious_institutions, beneficiary,
    institutional, generational, constrained, national).

% Holds sovereign authority over territory; signed Lausanne accepting minority protections. Implements treaty through domestic legislation (sometimes restrictively). Controls courts, police, bureaucracy that give treaty obligations effect or deny them. Can denounce treaty, reinterpret domestically, or comply — exit from the constraint is arbitrage-grade (sovereign choice), but carries international reputational and legal costs.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Clergy formation institutions (e.g., Halki Seminary) depend on treaty for legal recognition of degrees, property tenure, and operational autonomy. Under domestic law alone, they face closure or state control (Halki closed 1971- present under state university monopoly on higher education). Exit means relocating abroad or accepting state accreditation that subordinates theological curriculum to secular standards.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, theological_schools, beneficiary,
    organized, biographical, constrained, national).

% Access formation pathways guaranteed by treaty. Without treaty protection, candidates face barriers: no recognized domestic theological schools, state control over religious education, emigration required for formation. Exit is constrained — can study abroad but lose connection to community they would serve.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, clergy_candidates, beneficiary,
    moderate, biographical, constrained, national).

% Depend on institutional continuity for communal life (marriage, burial, education, charity). Institutions provide services the state does not. Exit means assimilation or emigration — constrained by identity, property, family ties. Treaty protects the institutional infrastructure their communal life runs on.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_community_members, beneficiary,
    organized, generational, constrained, national).

% Original parties to Lausanne (France, UK, Italy, Japan, Greece, etc.) with supervisory interest. Can raise diplomatic protests, submit to League/UN mechanisms, but have no direct enforcement. Their exit is analytical — they observe compliance but are not subject to the constraint.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, guarantor_states, observer,
    institutional, generational, analytical, regional).

% Adjudicates individual applications invoking Lausanne protections via ECHR Article 1 Protocol 1 (property) and Article 9 (religion). Jurisprudence (e.g., Bozcaada, Fener Rum Patrikhanesi) treats Lausanne as creating enforceable rights. Exit is analytical — the Court interprets but does not bear the constraint's costs or benefits.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, european_court_of_human_rights, observer,
    institutional, generational, analytical, regional).

% Apply domestic law interpreting or displacing treaty obligations. Foundation law (Vakıflar Kanunu) and higher education law (YÖK) have been used to restrict minority institutional autonomy. Courts can choose expansive or restrictive reading of Lausanne — their interpretive choice sets the operational reality. Exit is arbitrage-grade (judicial independence within domestic hierarchy).
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, domestic_courts_turkey, agenda_setter,
    institutional, biographical, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__expansive_reading, diffuse).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__expansive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates recognition and non-interference: the Turkish state refrains from restructuring minority religious institutions, and minority institutions operate with legal personality and autonomy. Solves the problem of post-imperial minority survival by converting Ottoman millet autonomy into treaty-guaranteed functional continuity.
% TRANSFER_FUNCTION: Moves compliance burden (administrative restraint, property restitution, non-interference) from minority institutions to the Turkish state. No resource transfer between private parties; the transfer is sovereign obligation for institutional autonomy.
% ABSENT_VOICES: Minority communities that disappeared (e.g., Pontic Greeks, Armenians in eastern provinces) — their institutions were destroyed before Lausanne or in the 1923 population exchange; they would object to a reading that treats Lausanne as protecting only survivors. Also absent: Turkish nationalist voices that view Lausanne as a temporary concession to be normalized away — they are present in domestic politics but excluded from the treaty's interpretive community.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished overnight, minority institutions would lose treaty-grounded legal personality and autonomy. They would become subject to domestic foundation law and higher education law without international recourse — Halki would remain closed, foundations would remain under state-appointed boards, clergy formation would remain blocked. The world rearranges: institutional continuity collapses without the treaty guarantee.
% FOUNDING_PROBLEM: Post-Ottoman Turkey needed to stabilize its eastern borders and minority populations after genocide, war, and population exchange. The founding problem was preventing further violence and displacement by guaranteeing non-Muslim minorities could maintain their religious communal life under Turkish sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Minority institutions and guarantor states attest the problem is live — minorities remain vulnerable, treaty protections remain necessary. Turkish state practice at intervals (1930s, 1970s, post-2011 foundation law reforms) treats the problem as resolved — minorities are equal citizens, special protections are anachronistic. ECHR jurisprudence corroborates the live-problem reading: recurring violations (property, education, legal personality) show the founding problem persists. No single external authority has declared the problem dead.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness (0.15): The constraint coordinates non-interference and recognition; it does not transfer resources from a payer to a beneficiary. The Turkish state bears compliance costs (administrative restraint, property restitution) but these are treaty obligations, not extraction for another's gain. Suppression (0.25) reflects historical periods where domestic law displaced treaty guarantees (1930s Wealth Tax, 1971 Halki closure, foundation law restrictions) — the constraint requires active international supervision to hold, but not domestic coercion against minorities. Theater ratio (0.15) is low: the coordination function (institutional continuity) is genuine, though compliance has been performative at intervals. Accessibility collapse (0.35) is moderate: alternatives (domestic legal protection, EU accession conditionality) exist but are incomplete substitutes for treaty-grounded autonomy. Resistance (0.55) is significant: Turkish state practice has repeatedly contested the expansive reading's scope, generating the interpretive contest this omega documents.
 *
 * PERSPECTIVAL GAP:
 *   From the minority institution seat: the constraint is a rope — genuine coordination securing their existence. From the Turkish state seat (at intervals): the constraint appears as an externally imposed scaffold or snare — a transitional arrangement they would prefer to normalize into domestic law. From the guarantor/ECHR seat: the constraint is a rope with eroding compliance — coordination that requires active supervision to maintain. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority religious institutions, theological schools, clergy candidates, and community members are beneficiaries (d near 0.0) — the constraint subsidizes their institutional autonomy. The Turkish state is the agenda setter with high power and arbitrage-grade exit (can denounce, reinterpret, or comply minimally) — d near 0.3-0.4 (symmetric-to-target, bearing compliance costs). Guarantor states and ECHR are observers with analytical exit — d = 0.5 (analytical). No victims declared: no group bears asymmetric extraction; the state's compliance costs are treaty obligations voluntarily undertaken.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting non-Muslim minorities in post-Ottoman Turkey) remains contested in status: the treaty parties and minorities attest it is live; Turkish state practice at intervals treats it as resolved. The arrangement has not atrophied into piton — it remains functionally necessary for institutional continuity, not merely performative. The coordination function (self-administration, property, clergy formation) is the treaty's operative core, not a legacy shell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_of_kernel_lausanne_minority_protections,
    'Is the expansive reading of Lausanne minority protections (functional continuity of pre-1923 religious governance) the correct instantiation of the kernel, or do the restrictive_reading or guarantor_reading better capture the treaty''s binding obligation?',
    'Interpretive contest in international tribunals, state practice, and scholarly consensus over the treaty''s object and purpose; the European Court of Human Rights'' margin of appreciation jurisprudence on Lausanne.',
    'If the expansive reading is foreclosed by authoritative interpretation, the coordination function it authorizes (self-administration, property rights, theological schools) loses its treaty-grounded legitimacy and becomes a contested domestic claim — the constraint''s type would shift from rope (treaty-grounded coordination) to snare (domestic imposition) or scaffold (transitional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_of_kernel_lausanne_minority_protections, conceptual, 'Which reading of the Lausanne kernel is structurally authoritative — expansive (this story), restrictive, or guarantor').

omega_variable(
    institutional_vulnerability_without_treaty_grounding,
    'If the expansive reading loses the interpretive contest, do minority institutions retain functional capacity under domestic Turkish law alone, or does their governance collapse without treaty protection?',
    'Empirical observation of minority institutional operations in periods of weakened treaty enforcement (e.g., 1930s-1960s Turkish nationalist policies, post-1974 Cyprus crisis property restrictions, recent foundation law reforms).',
    'If institutions collapse without treaty grounding, the expansive reading''s coordination is existentially necessary — its loss would be world_rearranging. If they persist domestically, the treaty is a reinforcement layer, not the foundation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vulnerability_without_treaty_grounding, empirical, 'Whether minority institutional survival depends on the expansive reading''s treaty guarantee or on domestic legal accommodation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 1923, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_tr_t1923, lausanne_minority_protections__expansive_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_tr_t1940, lausanne_minority_protections__expansive_reading, theater_ratio, 1940, 0.3).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_tr_t1960, lausanne_minority_protections__expansive_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_tr_t1980, lausanne_minority_protections__expansive_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_tr_t2000, lausanne_minority_protections__expansive_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_tr_t2025, lausanne_minority_protections__expansive_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_be_t1923, lausanne_minority_protections__expansive_reading, base_extractiveness, 1923, 0.1).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_be_t1940, lausanne_minority_protections__expansive_reading, base_extractiveness, 1940, 0.25).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_be_t1960, lausanne_minority_protections__expansive_reading, base_extractiveness, 1960, 0.2).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_be_t1980, lausanne_minority_protections__expansive_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_be_t2000, lausanne_minority_protections__expansive_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_be_t2025, lausanne_minority_protections__expansive_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_su_t1923, lausanne_minority_protections__expansive_reading, suppression_requirement, 1923, 0.15).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_su_t1940, lausanne_minority_protections__expansive_reading, suppression_requirement, 1940, 0.4).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_su_t1960, lausanne_minority_protections__expansive_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_su_t1980, lausanne_minority_protections__expansive_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_su_t2000, lausanne_minority_protections__expansive_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(lausanne_minority_protections__expansive_reading_su_t2025, lausanne_minority_protections__expansive_reading, suppression_requirement, 2025, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__expansive_reading, 0.08).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, turkish_foundation_law_reforms).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, ecmhr_lausanne_jurisprudence).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, halki_seminary_closure).

% DUAL FORMULATION NOTE:
% Lausanne minority protections kernel decomposes into three readings with different structural profiles: expansive_reading (this story) = moderate coordination rope grounding institutional autonomy in treaty text; restrictive_reading = snare/tangled_rope where domestic law extracts institutional capacity from minorities; guarantor_reading = rope with enforcement_mechanism coordination type where international supervision is the coordination function. The expansive reading coexists with both siblings as live interpretive positions; it influences the guarantor reading by providing the substantive content that international supervision would enforce; it is influenced by the restrictive reading's domestic legal pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__expansive_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
