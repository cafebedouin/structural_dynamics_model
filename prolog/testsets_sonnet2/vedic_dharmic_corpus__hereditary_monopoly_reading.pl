% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Hereditary Monopoly Reading of Varna Ritual Authority
 *   domain: Religious Authority / Social Stratification / Interpretive Legitimacy
 *
 * SUMMARY:
 *   This story instantiates the hereditary-monopoly reading of the
 *   vedic_dharmic_corpus kernel: the claim that ritual and interpretive
 *   authority are properly transmitted by birth into Brahmin lineage and that
 *   varna hierarchy reflects divine ordination textually prescribed, not
 *   social convention. This is one of three structurally distinct readings of
 *   the same kernel text tradition; the bhakti_devotional_reading locates
 *   spiritual authority in sincere devotion irrespective of birth, and the
 *   reformist_egalitarian_reading treats caste hierarchy as historical
 *   accretion subject to constitutional equality critique. Each reading is
 *   authored as its own constraint with its own epsilon and stakeholder
 *   structure per the ε-invariance principle — this file authors only the
 *   hereditary-monopoly reading, on its own terms, as its proponents would
 *   describe it, not as a strawman.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_lineages: agenda_setter/beneficiary (institutional/arbitrage) — controls interpretive access and collects ritual-economy rents
 *   - shudra_and_dalit_communities: payer (powerless/trapped) — bears exclusion from study, temple access, and priestly office
 *   - women_across_varnas: payer (powerless/constrained) — excluded from Vedic recitation and independent interpretive standing regardless of caste
 *   - temple_administrative_trusts: beneficiary (organized/mobile) — administers endowments stabilized by hereditary succession
 *   - reform_minded_jurists_and_movements: excluded (organized/constrained) — contest legitimacy from outside the interpretive body
 *   - textual_scholars_comparative_religion: observer (analytical/analytical) — traces historical layering without adjudicating the contest
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
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Monopoly Reading of Varna Ritual Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "Religious Authority / Social Stratification / Interpretive Legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, '4fb4f3c3-524d-482b-b78c-1f4f7b14285a').
narrative_ontology:cs_kernel_codification('4fb4f3c3-524d-482b-b78c-1f4f7b14285a', fixed_text).
narrative_ontology:cs_authority_grounding('4fb4f3c3-524d-482b-b78c-1f4f7b14285a', lineage).
narrative_ontology:cs_interpretation_layer_present('4fb4f3c3-524d-482b-b78c-1f4f7b14285a').
narrative_ontology:cs_reading_relation('4fb4f3c3-524d-482b-b78c-1f4f7b14285a', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('4fb4f3c3-524d-482b-b78c-1f4f7b14285a', vedic_dharmic_corpus__reformist_egalitarian_reading, forecloses).
narrative_ontology:cs_axiom('4fb4f3c3-524d-482b-b78c-1f4f7b14285a', foundational, ritual_authority_transmitted_by_birth).
narrative_ontology:cs_axiom_status(ritual_authority_transmitted_by_birth, holdable).
narrative_ontology:cs_axiom_grounding('4fb4f3c3-524d-482b-b78c-1f4f7b14285a', ritual_authority_transmitted_by_birth, theological).
narrative_ontology:cs_axiom('4fb4f3c3-524d-482b-b78c-1f4f7b14285a', foundational, varna_hierarchy_textually_essential_not_accretive).
narrative_ontology:cs_axiom_status(varna_hierarchy_textually_essential_not_accretive, holdable).
narrative_ontology:cs_axiom_grounding('4fb4f3c3-524d-482b-b78c-1f4f7b14285a', varna_hierarchy_textually_essential_not_accretive, conventional).
narrative_ontology:cs_reference_frame('4fb4f3c3-524d-482b-b78c-1f4f7b14285a', vedic_smriti_prescribed_varna_order).
narrative_ontology:cs_drift_state('4fb4f3c3-524d-482b-b78c-1f4f7b14285a', post_independence_constitutional_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4fb4f3c3-524d-482b-b78c-1f4f7b14285a', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_administrative_trusts).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_and_dalit_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women_across_varnas).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_ritual_practitioners).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, varna_hierarchy_divine_ordination).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, textual_prescription_of_social_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls interpretive access to Vedic and dharmashastric texts, performs the rites that mediate ritual purity and social standing, and administers major temple institutions. Birth into the lineage is presented as the sole valid credential for these functions; the class collects fees, land grants, and social deference tied to ritual monopoly, and adjudicates who counts as qualified to interpret scripture at all.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages, beneficiary).

% Historically and often presently barred from Vedic study, temple sanctum access, and priestly office regardless of learning or devotion. Bear the social, economic, and ritual costs of a hierarchy justified as textually fixed and divinely ordained; exit requires conversion, migration, or political mobilization against an order that claims cosmic sanction, not mere custom.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_and_dalit_communities, payer,
    powerless, generational, trapped, national).

% Excluded from Vedic recitation and most priestly functions under the hereditary-monopoly reading regardless of birth varna; ritual authority is gendered as well as caste-bound. Some mobility exists through marriage into ritual households, but independent interpretive standing is foreclosed by the same textual-prescription logic that governs caste.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women_across_varnas, payer,
    powerless, generational, constrained, national).

% Individuals from other varnas who have studied texts, learned rites, or built devotional followings but are denied recognized interpretive authority because legitimacy is keyed to lineage rather than demonstrated competence. They can practice informally or regionally but are excluded from institutional temple office and formal textual adjudication.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_ritual_practitioners, payer,
    moderate, biographical, constrained, regional).

% Manage endowments, land, and ritual-economy revenue that flow through Brahmin-administered rites. Benefit from the stability and exclusivity the hereditary reading provides, since a fixed, birth-determined priesthood simplifies succession disputes and legitimizes continued control of temple assets.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_administrative_trusts, beneficiary,
    organized, generational, mobile, regional).

% Constitutional courts, anti-caste movements, and reformist religious organizations argue the hierarchy is historical accretion rather than textual essence and press for temple entry and priestly access reform. They are structurally excluded from Brahmin interpretive institutions themselves and must contest legitimacy from outside via law and social movement rather than from within the ritual-authority structure.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, reform_minded_jurists_and_movements, excluded,
    organized, generational, constrained, national).

% Study the historical layering of varna texts, the relationship between smriti prescription and later social practice, and the divergence between textual claims and lived caste dynamics across regions and eras. Their findings inform but do not resolve the contest between readings.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, textual_scholars_comparative_religion, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, low-dispute mechanism for allocating ritual office, textual authority, and temple administration by fixed hereditary criterion rather than continuous contest — avoiding constant renegotiation of who may perform which rites.
% TRANSFER_FUNCTION: Moves ritual fees, land endowments, social deference, and interpretive authority from non-Brahmin castes and women toward Brahmin lineages and the institutions they administer, justified as following from birth status rather than as a negotiated transfer.
% ABSENT_VOICES: Shudra and Dalit religious scholars, women seeking independent Vedic study, and reformist theologians who would contest the divine-ordination premise are structurally outside the interpretive body that adjudicates textual meaning — they can petition courts or build parallel institutions but cannot alter the reading from within.
% DISAPPEARANCE_RATIONALE: If hereditary ritual monopoly vanished overnight, temple administration, priestly succession, and the social deference structure tied to varna would have to reconstitute on competence, election, or devotional criteria — land and endowment control would become contestable, and social status currently anchored to birth-lineage would require new legitimating grounds.
% FOUNDING_PROBLEM: Ancient ritual and textual traditions required specialists trained from childhood in memorization, pronunciation, and performance of complex Vedic rites where error was believed to carry cosmic consequence; hereditary transmission within families provided continuity of this specialized knowledge before widespread literacy or formal educational institutions existed.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars and constitutional courts (outside the Brahmin institutional structure) attest that literacy, textual translation, printing, and formal religious education have long since made hereditary transmission unnecessary for preserving textual accuracy; the continued birth-based exclusivity is corroborated by legal challenges and reformist theological scholarship as serving social and economic function rather than knowledge-preservation function. Brahmin institutional authorities themselves largely maintain that the problem remains live in a spiritual/cosmic sense not reducible to literacy — that self-interested corroboration is noted as such.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is set at 0.65, reflecting substantial but not maximal extraction: temple economies, land endowments, and ritual fees flow disproportionately to Brahmin lineages, and social status is durably allocated by birth rather than demonstrated merit or devotion. Suppression is high (0.78) because maintaining the reading requires active enforcement — historical exclusion from temple sanctums, denial of Vedic study, social ostracism for boundary violations, and legal battles over temple entry into the present day. Theater ratio rose over the measured interval (0.20 to 0.42) as literacy spread and printed texts became widely available, undermining the original knowledge-transmission rationale while ritual exclusivity persisted anyway — an increasing share of the maintained hierarchy is performative rather than functionally necessary for textual preservation. Accessibility collapse is high (0.70): once a person accepts the divine-ordination premise, alternative readings of the same texts become very difficult to countenance from within that framework. Resistance is substantial (0.72), reflecting centuries of internal reform movements, external legal challenge, and comparative-textual scholarship contesting the reading.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin agenda-setter seat, the arrangement reads as faithful transmission of a divinely ordained order across generations — a coordination function of the highest order. From the payer seats, the identical structure operates as enforced extraction dressed in cosmological language. The engine computes both per-seat classifications from the same structural data; this story does not adjudicate which reading is correct, only that the structural asymmetry (who sets terms, who bears costs, who can exit) is real and measurable.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priestly lineages and the temple trusts they administer sit at the beneficiary end: they set the interpretive terms, collect the ritual-economy proceeds, and hold arbitrage-grade exit (able to reposition within or outside the ritual economy as conditions change) while the arrangement's costs land elsewhere. Shudra and Dalit communities and women across varnas sit at the target end: they are structurally trapped or constrained, bear the social and material costs of exclusion, and cannot exit the hierarchy without exiting the religious and social order it is embedded in. Non-Brahmin ritual practitioners occupy an intermediate position — real informal practice is possible, but institutional recognition is foreclosed by the same lineage criterion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing is deliberately double-edged: the knowledge-preservation rationale that once made hereditary transmission functionally load-bearing (oral transmission before literacy, error-sensitive ritual performance) is now largely obsolete given printing, translation, and formal education — corroborated by outside scholarship and legal challenge — yet the hierarchy persists with its original divine-ordination justification largely intact within Brahmin institutional discourse itself. This is exactly the mismatch the R5 genealogy check is built to surface: founding_problem_status=dead paired with disappearance_verdict=world_rearranges signals that what remains is not the original coordination function but a distribution of status and resource claims that has outlived its stated justification — a tangled_rope reading rather than a pure mountain of textual necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hereditary_monopoly_reading_identity,
    'Is this reading best understood as the historically dominant and textually literal interpretation of varna prescription, or as one contested reading among several live traditions (bhakti devotional access, reformist egalitarian critique) that has enjoyed disproportionate institutional power rather than superior textual warrant?',
    'Comparative textual-historical analysis of smriti layering versus srauta/smarta practice divergence across regions and periods; documentation of how much of the varna-hierarchy claim is present in earliest strata versus later accretion; cross-reference with bhakti and reformist reading''s own textual warrants.',
    'If hereditary monopoly reflects genuine textual consensus across traditions, its beneficiary/victim structure is more deeply load-bearing culturally; if it is one reading sustained disproportionately by the institutional power of those it benefits, the tangled_rope classification''s extraction component is better evidenced and the mandatrophy analysis strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hereditary_monopoly_reading_identity, conceptual, 'Whether the hereditary-monopoly reading represents textual consensus or institutionally-favored contested interpretation.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the hereditary-monopoly reading''s core premise (divine ordination of birth-determined ritual authority) logically foreclose the bhakti devotional reading''s premise (sincere devotion supersedes birth) within a single coherent theological framework, or can both premises coexist as different registers (institutional ritual authority vs. personal spiritual access) within the same broader tradition?',
    'Examine historical and contemporary religious communities that hold both premises simultaneously (e.g., bhakti movements operating alongside continued Brahmin temple administration) versus communities where adopting bhakti egalitarianism explicitly displaced hereditary ritual claims.',
    'If the premises genuinely coexist without contradiction in most lived traditions, coexists_with is the correct relation; if adopting bhakti''s core claim requires explicitly rejecting hereditary ritual exclusivity in an internally consistent framework, a forecloses relation would be more accurate for at least some sub-traditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether hereditary-monopoly and bhakti-devotional premises are logically compatible or mutually exclusive within single frameworks.').

omega_variable(
    enforcement_mechanism_present_day_scope,
    'How much of the measured suppression (0.78) reflects continuing active enforcement in the present day (temple entry restriction, social sanction) versus historically accumulated suppression whose active enforcement has substantially declined but whose effects persist through path-dependent social and economic structure?',
    'Region-by-region survey of temple entry practice, priestly office access litigation outcomes, and social sanction incidence over the past several decades, distinguishing legally mandated open access from de facto continued exclusion.',
    'If active enforcement has substantially declined while structural effects persist, the constraint may be transitioning toward a piton profile (inertial persistence without active maintenance) in some jurisdictions even while remaining a fully enforced tangled_rope in others — suggesting the classification may need regional decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_present_day_scope, empirical, 'Degree to which measured suppression reflects live enforcement versus path-dependent residue.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(vedi_tr_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(vedi_tr_t120, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 120, 0.4).
narrative_ontology:measurement(vedi_tr_t160, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 160, 0.41).
narrative_ontology:measurement(vedi_tr_t200, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(vedi_be_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement(vedi_be_t120, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 120, 0.66).
narrative_ontology:measurement(vedi_be_t160, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 160, 0.64).
narrative_ontology:measurement(vedi_be_t200, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 200, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(vedi_su_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 80, 0.74).
narrative_ontology:measurement(vedi_su_t120, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 120, 0.79).
narrative_ontology:measurement(vedi_su_t160, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 160, 0.76).
narrative_ontology:measurement(vedi_su_t200, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 200, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.08).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% Member of the vedic_dharmic_corpus kernel family (3 readings). This story (hereditary_monopoly_reading) authors high extraction (ε=0.65) with clear beneficiary/victim structure and active institutional enforcement. bhakti_devotional_reading is expected to author substantially lower extraction (devotional access bypasses the birth-criterion this reading extracts through). reformist_egalitarian_reading is expected to reframe the same textual corpus as historical accretion rather than binding prescription, with correspondingly different beneficiary structure (constitutional/legal actors rather than ritual lineage). All three share the same underlying textual kernel but instantiate structurally distinct constraints per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
