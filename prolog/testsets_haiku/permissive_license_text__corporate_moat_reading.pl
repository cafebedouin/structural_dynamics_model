% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Text (Corporate Moat Reading)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   Permissive open-source licenses (MIT, Apache 2.0, BSD) grant unrestricted
 *   freedom to use, modify, and redistribute software with no obligation to
 *   contribute back or disclose improvements. This constraint story
 *   instantiates the CORPORATE MOAT READING: the permissive license text
 *   enables enterprises to extract unpaid labor and intellectual property
 *   value from open-source maintainers by building closed-source derivatives
 *   without reciprocal obligation or compensation. Individual maintainers
 *   release software under permissive licenses expecting voluntary
 *   collaboration and community benefit; corporate entities instead wholesale
 *   the software, add proprietary features, and capture all downstream value.
 *   The volunteer contributor base is locked out of the improved version they
 *   helped create. The constraint is CLAIMED as snare (this reading's
 *   verdict) because extraction persists by suppressing the visibility of who
 *   is capturing value and by relying on legal structures (permissive license
 *   text) that encode the extraction asymmetrically. This is one of three
 *   readings of the kernel 'permissive_license_text'; sibling readings
 *   (commons_coordination_reading, copyleft_counterfactual_reading)
 *   instantiate different constraints from the same legal text, with
 *   different epsilon values and different beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - Individual open-source maintainers: moderate power, constrained exit — release software expecting reciprocal collaboration; discover corporate extraction
 *   - Volunteer contributor communities: powerless, trapped exit — contribute unpaid labor; are locked out of closed derivatives
 *   - Enterprise corporations: institutional power, arbitrage exit — select permissive open-source, build closed derivatives, capture all value
 *   - License authors (FSF, OSI): analytical observers — steward the permissive license texts and interpret whether they enable extraction or universal freedom
 *   - Free and open-source advocates: organized, constrained exit — excluded from permissive-license decision-making; advocate for GPL-style reciprocity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.68).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.59).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.59).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Text (Corporate Moat Reading)").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__corporate_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, '22ea9bb9-93d7-4201-bc45-d733b31a00d3').
narrative_ontology:cs_kernel_codification('22ea9bb9-93d7-4201-bc45-d733b31a00d3', fixed_text).
narrative_ontology:cs_authority_grounding('22ea9bb9-93d7-4201-bc45-d733b31a00d3', extraction).
narrative_ontology:cs_interpretation_layer_present('22ea9bb9-93d7-4201-bc45-d733b31a00d3').
narrative_ontology:cs_reading_relation('22ea9bb9-93d7-4201-bc45-d733b31a00d3', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('22ea9bb9-93d7-4201-bc45-d733b31a00d3', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('22ea9bb9-93d7-4201-bc45-d733b31a00d3', foundational, permissive_text_enables_uncompensated_extraction).
narrative_ontology:cs_axiom_status(permissive_text_enables_uncompensated_extraction, holdable).
narrative_ontology:cs_axiom_grounding('22ea9bb9-93d7-4201-bc45-d733b31a00d3', permissive_text_enables_uncompensated_extraction, empirically_contingent).
narrative_ontology:cs_axiom('22ea9bb9-93d7-4201-bc45-d733b31a00d3', secondary, corporate_value_capture_without_reciprocal_obligation).
narrative_ontology:cs_axiom_status(corporate_value_capture_without_reciprocal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('22ea9bb9-93d7-4201-bc45-d733b31a00d3', corporate_value_capture_without_reciprocal_obligation, empirically_contingent).
narrative_ontology:cs_reference_frame('22ea9bb9-93d7-4201-bc45-d733b31a00d3', permissive_license_as_mutual_freedom_mechanism).
narrative_ontology:cs_drift_state('22ea9bb9-93d7-4201-bc45-d733b31a00d3', contemporary_corporate_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('22ea9bb9-93d7-4201-bc45-d733b31a00d3', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, commercial_derivative_builders).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_open_source_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, volunteer_contributor_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, downstream_software_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Release software under permissive license expecting collaborative community development and attribution. Discover that commercial entities extract the software wholesale, build proprietary derivatives without contribution, and capture market value the maintainer built. Cannot easily pursue legal recourse; relicense is costly and splits the user base. Community contributions often fail to materialize from downstream commercial users.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_open_source_maintainers, payer,
    moderate, biographical, constrained, global).

% Contribute unpaid labor to open-source projects under permissive licenses, believing they are building a shared commons. Their contributions are extracted by commercial entities that close the derivative product, preventing upstream benefit and locking volunteers out of the improved version they helped create. Exit requires abandoning both the project and the labor already sunk.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, volunteer_contributor_communities, payer,
    powerless, immediate, trapped, global).

% Select permissive-licensed open-source software as a foundation for proprietary products. Invest in integration, polish, and features built on top of the foundation; package as closed-source derivatives; capture all downstream value. License text imposes no reciprocity or disclosure requirement. Defend the practice as 'appropriate use of permissive licensing' and 'value-add engineering.' Maintain relationships with upstream maintainers at token level (sponsorship, attribution) while extracting asymmetrically.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_corporations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__corporate_moat_reading, enterprise_corporations, beneficiary).

% Build commercial services on top of permissive open-source software. The license grants freedom to modify and redistribute; they exercise that freedom to create closed derivatives, proprietary SaaS wrappers, or commercial forks. This practice is legally sanctioned by the permissive license text; they capture the value of the derivative while contributing no improvements upstream.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, commercial_derivative_builders, beneficiary,
    powerful, generational, mobile, global).

% Legal scholars, policy designers, and institutional bodies (FSF, OSI, standards bodies) that author and steward permissive license texts. Interpret the texts as either enablers of universal freedom (commons_coordination reading) or structural vulnerabilities to uncompensated extraction (this reading). Their framings influence which licenses get adopted and whether reciprocity clauses get included.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, license_authors, observer,
    analytical, civilizational, analytical, global).

% Advocate for stronger reciprocity clauses (GPL, AGPL) or mandatory contribution-back requirements. Excluded from the permissive-license ecosystem's decision-making because they use different licenses; they would argue that permissive licensing enables the extraction this constraint describes, but are not at the table when maintainers choose to go permissive.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, free_and_open_source_advocates, excluded,
    organized, generational, constrained, global).

% Benefit from the derivative proprietary products built on open-source foundations: they are polished, integrated, and often cost less than they would if built from scratch. They do not face the extraction directly; the extraction is borne by maintainers and volunteer contributors, invisibly subsidizing the derivatives.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, downstream_software_users, beneficiary,
    powerless, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing coordinates on a single question: legal freedom to use, modify, and redistribute software without restriction or liability. It solves the coordination problem of removing legal friction from software reuse and building on existing work.
% TRANSFER_FUNCTION: Transfers unpaid labor (initial creation, maintenance, improvements from volunteer contributors) and intellectual property value from open-source maintainers to commercial entities that build and sell closed derivatives. The maintainers retain attribution; they receive no share of downstream commercial value.
% ABSENT_VOICES: Volunteer contributors who have sunk labor into projects are structurally excluded from the commercial derivative product and have no seat at the table where the derivative's business model is decided. GPL advocates and copyleft advocates are also excluded: they would argue for stronger reciprocity, but permissive-license communities do not include them in governance.
% DISAPPEARANCE_RATIONALE: If permissive licensing with uncompensated extraction disappeared and mandatory reciprocity or contribution-back were required, commercial entities would either contribute upstream, close their development to avoid licensing obligations, invest in proprietary alternatives, or negotiate commercial licenses. Open-source maintainers would retain more agency over derivatives and would receive more direct contribution. The software economy would reorganize around either stronger reciprocity requirements or commercial licensing for companies wanting to close derivatives.
% FOUNDING_PROBLEM: Early software licensing was restrictive and patent-encumbered, making it difficult to build on existing work. Permissive licensing was created to remove legal friction and enable universal collaborative software development.
% FOUNDING_PROBLEM_CORROBORATION: License authors (FSF, OSI) attest the founding problem (restrictive licensing blocking reuse) is solved; permissive licenses achieved universal implementation freedom. Open-source maintainers and volunteer contributors attest that the founding problem has been re-cast: legal freedom to reuse is abundant, but the problem has shifted to economic extraction through closed derivatives that benefit from open contributions without reciprocity. GPL advocates cite case studies of maintainer burnout and capture of value. Independent economic analysis shows the asymmetry in value capture.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval because the permissive license text's adoption has become ubiquitous (Apache 2.0, MIT across major ecosystems), making it the default choice for maintainers who do not expect corporate extraction. As adoption spreads, the constraint's scope and effectiveness grow. Theater rises from 0.18 to 0.41 because corporate entities increasingly justify the extraction with 'value-add engineering' and 'appropriate use of permissive licensing,' masking the underlying asymmetry. Suppression is moderate and stable (0.48→0.59) because the constraint works primarily through legal text and information asymmetry, not through overt coercion — maintainers often do not realize the extraction is happening until after their contributions are absorbed. Resistance is high (0.71) because volunteer communities and GPL advocates mount sustained objections to the model; the constraint persists despite real resistance because the legal structure (permissive license text) is so entrenched. The measurement series tracks the growing scope and visibility of corporate moat-building on open-source foundations.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (enterprise corporations) perceives this as a rope or scaffold: they view permissive licensing as efficient coordination that enables their value-add engineering on stable foundations. The payer seats (individual maintainers, volunteer contributors) perceive a snare: they realize their contributed value is being extracted without reciprocity or compensation. The observer seats (license authors, free/open-source advocates) perceive a contested kernel: the permissive license text is the same object, but one reading emphasizes universal freedom (commons_coordination), another emphasizes structural vulnerability to closure (copyleft_counterfactual), and this reading emphasizes uncompensated extraction (corporate_moat). The engine computes per-seat classification from the structural data: beneficiary seats (enterprises, downstream users) will compute closer to rope; payer seats will compute closer to snare; excluded seats (GPL advocates) will see both sibling readings as superior alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Enterprise corporations and derivative builders are beneficiaries: they receive the extracted value (closed derivatives, proprietary products built on free labor). Their directionality is low (they subsidize the cost of building, they benefit from the legal immunity permissive licensing grants). Individual maintainers and volunteer contributors are victims: they bear the cost (unpaid labor, loss of agency over derivatives, locked-out improvements). Their directionality is high (the constraint extracts from them, suppresses their visibility, forecloses their participation in the derivative). Downstream software users are beneficiaries (they receive polished products at lower cost than from-scratch development) but do not perceive the extraction directly; the cost is borne invisibly by maintainers. License authors and advocates are analytical observers with no direct stake in this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was 'restrictive licensing blocking reuse.' Permissive licensing solved that problem — reuse is now ubiquitous and legally friction-free. However, the constraint has acquired a secondary function that is not mandated by the founding problem: it now serves as a vehicle for corporate value extraction. The theater_ratio rising from 0.18 to 0.41 shows this secondary function increasingly dominating: the license text's stated purpose (enabling reuse) is increasingly used to justify non-reciprocal capture of value (suppressing upstream benefit, closing derivatives). The permissive license text remains the mechanism, but the mandate has drifted from 'enable collaboration' to 'enable corporate closure without reciprocity.' This is mandatrophy in slow motion: the original problem is solved, but the solution has been captured and re-purposed for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maintainer_intent_vs_license_mechanism,
    'Does the extractive outcome reflect maintainers'' actual intent when they choose permissive licensing, or is it a divergence between stated intent and structural outcome?',
    'Survey open-source maintainers about their expectations when releasing under permissive licenses; track how many expected corporate derivatives vs. collaborative contribution. Compare intent data with observed extraction patterns.',
    'If maintainers expected and consented to corporate derivatives, the extraction is less extractive (coordinated licensing) and the constraint reclassifies. If the extraction is a surprise, it is a suppressed externality and the constraint remains snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maintainer_intent_vs_license_mechanism, empirical, 'Whether permissive licensing outcomes match maintainer expectations or reveal suppressed information asymmetry').

omega_variable(
    volunteer_contributor_coercion_mechanism,
    'Is suppression of volunteer contributors structural (they are technically locked out of closed derivatives, legal barriers) or internalized (they have normalized the extraction as ''the way open-source works'')?',
    'Post-relicense natural experiment: if a maintainer switches from permissive to copyleft, do the same volunteer contributors return to active participation? If so, suppression was structural (exit barriers); if they remain absent, it was internalized (belief that extraction is normal).',
    'Structural suppression indicates the constraint is held up by legal/technical barriers and remediable by license change. Internalized suppression indicates the constraint is self-reinforcing through cultural normalization; fixing it requires shifting narrative and community expectations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volunteer_contributor_coercion_mechanism, empirical, 'Whether volunteer suppression is structural or internalized').

omega_variable(
    kernel_reading_boundary,
    'Is the extractive outcome a necessary feature of permissive licensing, or a contingent outcome of how the license is deployed in a power-imbalanced market?',
    'Counterfactual: if permissive licensing were adopted in a market where maintainers had equal bargaining power with enterprises (e.g., through unionization or collective licensing platforms), would the extraction persist or would cooperation emerge despite permissive text? Alternatively, if reciprocity norms were culturally enforced (not legally required) around permissive licensing, would the pattern change?',
    'If extraction is necessary to permissive licensing, the constraint is a structural feature of the license choice and the copyleft_counterfactual reading is correct (permissive licensing IS the vulnerability). If extraction is contingent on power asymmetry or cultural norms, then the permissive license is neutral and the problem is the market context, not the text.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether corporate moat extraction is inherent to permissive licensing or contingent on power/market asymmetries').

omega_variable(
    reading_foreclosure_via_axiom_contradiction,
    'Does this reading''s core axiom (permissive licensing enables uncompensated extraction) logically foreclose the commons_coordination reading''s core axiom (permissive licensing maximizes universal freedom)?',
    'Logical analysis: both readings can be true if ''universal freedom'' is read narrowly as ''legal freedom to reuse'' (which permissive text grants) and ''uncompensated extraction'' is read as a side-effect of that freedom in an asymmetric market. If both can coexist in a single coherent framework, they are coexists_with. If one fundamentally denies the other''s core claim, they foreclose.',
    'If they coexist, both readings are live positions and the constraint family is a genuine three-way dispute. If they foreclose, one reading is ruled out and the dispute is binary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_via_axiom_contradiction, conceptual, 'Whether corporate-moat and commons-coordination readings logically foreclose each other or coexist as live interpretations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(perm_tr_t3, permissive_license_text__corporate_moat_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement(perm_tr_t6, permissive_license_text__corporate_moat_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement(perm_tr_t12, permissive_license_text__corporate_moat_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(perm_tr_t18, permissive_license_text__corporate_moat_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__corporate_moat_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(perm_be_t3, permissive_license_text__corporate_moat_reading, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(perm_be_t6, permissive_license_text__corporate_moat_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(perm_be_t12, permissive_license_text__corporate_moat_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(perm_be_t18, permissive_license_text__corporate_moat_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__corporate_moat_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__corporate_moat_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(perm_su_t3, permissive_license_text__corporate_moat_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(perm_su_t6, permissive_license_text__corporate_moat_reading, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(perm_su_t12, permissive_license_text__corporate_moat_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(perm_su_t18, permissive_license_text__corporate_moat_reading, suppression_requirement, 18, 0.59).
narrative_ontology:measurement(perm_su_t25, permissive_license_text__corporate_moat_reading, suppression_requirement, 25, 0.59).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__corporate_moat_reading, 0.18).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'permissive_license_text'. All three share the same legal artifact (permissive license texts: MIT, Apache 2.0, BSD) but instantiate different constraints because different parties interpret the text's implications differently. The commons_coordination_reading reads permissive licensing as enabling universal collaborative freedom (high coordination, low extraction, mountain-to-rope category). The copyleft_counterfactual_reading reads permissive licensing as structurally vulnerable to closure and argues for GPL-style reciprocity as necessary (coordination + forced reciprocity, tangled_rope). This reading (corporate_moat) reads permissive licensing as enabling corporate value extraction without reciprocal obligation (snare category, high extraction, structural suppression). All three readings are live in the policy and community discourse. The epsilon values differ substantially because the referent (the permissive license text's structural impact) is read differently by different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
