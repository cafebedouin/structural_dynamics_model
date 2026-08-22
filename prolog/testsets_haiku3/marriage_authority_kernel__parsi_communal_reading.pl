% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__parsi_communal_reading, []).

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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Marriage and Divorce Act 1936 Authority Structure
 *   domain: legal/religious/communal
 *
 * SUMMARY:
 *   The Parsi Marriage and Divorce Act 1936 codifies Zoroastrian religious
 *   law as the binding family law regime for Parsi communities in India. The
 *   Act is presented by community authorities as essential protection for a
 *   historically persecuted minority's religious identity and cultural
 *   continuity. It is experienced by women in mixed marriages and religious
 *   dissenters as an endogamy enforcement mechanism that restricts choice and
 *   constrains exit. This constraint is ONE READING of the contested kernel:
 *   'marriage/family law authority derives from [source].' The Parsi communal
 *   reading asserts authority derives from codified Zoroastrian custom and
 *   community tribunal interpretation. This reading differs structurally from
 *   the Hindu codified reading (secular civil courts interpret Hindu law),
 *   the Muslim shariat reading (qazis and boards interpret Islamic law), the
 *   Christian canonical reading (canonical law interpreted by community
 *   courts), and the secular civil reading (constitutional individual rights
 *   ground law). The constraint story is about THIS reading: how the Act
 *   operates as a Parsi-community-controlled authority structure, who it
 *   benefits and harms, and how demographic decline threatens the viability
 *   of the community it claims to preserve.
 *
 * KEY AGENTS:
 *   - parsi_community_elders: institutional authority; agenda-setters; identity-locked in Zoroastrian tradition
 *   - parsi_women_married_under_act: moderate power; constrained exit; benefit from gender equity provisions while trapped by endogamy enforcement
 *   - women_in_mixed_marriages: powerless; trapped by non-recognition; the Act's primary victims
 *   - parsi_men_in_mixed_marriages: moderate power; beneficiary; asymmetric endogamy enforcement favors them
 *   - religious_minorities_within_parsis: powerless; identity-locked; excluded from tribunal remedies
 *   - civil_courts: institutional observer; boundary position between communal and constitutional law
 *   - indian_state: institutional observer; permits pluralism but obliged to prevent discrimination
 *   - demographic decline: structural pressure; not an agent but a constraint-generated dynamic threatening community viability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.62).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.71).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Marriage and Divorce Act 1936 Authority Structure").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "legal/religious/communal").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, 'a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11').
narrative_ontology:cs_kernel_codification('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', fixed_text).
narrative_ontology:cs_authority_grounding('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', lineage).
narrative_ontology:cs_interpretation_layer_present('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11').
narrative_ontology:cs_reading_relation('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', foundational, zoroastrian_religious_autonomy_necessary).
narrative_ontology:cs_axiom_status(zoroastrian_religious_autonomy_necessary, holdable).
narrative_ontology:cs_axiom_grounding('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', zoroastrian_religious_autonomy_necessary, deontological).
narrative_ontology:cs_axiom('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', foundational, community_tribunal_authority_legitimate).
narrative_ontology:cs_axiom_status(community_tribunal_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', community_tribunal_authority_legitimate, conventional).
narrative_ontology:cs_reference_frame('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', zoroastrian_communal_self_governance).
narrative_ontology:cs_drift_state('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', contemporary_demographic_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a97e4cb6-4f2b-4e3c-aab5-a8b8067efa11', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, community_tribunal_apparatus).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, women_in_mixed_marriages).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, religious_minorities_within_parsis).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_women_married_under_act).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_men_in_mixed_marriages).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_women_married_under_act).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the Parsi Marriage and Divorce Act 1936 through community councils and tribunals. Interpret Zoroastrian custom and enforce endogamy norms that constitute Parsi religious and ethnic identity. Defend the arrangement as protecting a historically persecuted minority community's cultural continuity and the legitimacy of community-based family law.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from substantive gender equity provisions in the Act (women have near-equal divorce and succession rights, property protection during marriage). Also constrained by community enforcement of endogamy rules, tribunal governance, and the cost of challenging family law outcomes within community rather than civil courts.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_women_married_under_act, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_women_married_under_act, payer).

% Face non-recognition or degraded status under the Act when married to non-Parsi partners. The Act's endogamy requirement means women who marry outside the community may lose inheritance rights, guardianship recognition, and access to community tribunal remedies. Trapped between community law (which does not recognize their union) and civil law (which does, but at a different legal regime).
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, women_in_mixed_marriages, payer,
    powerless, biographical, trapped, national).

% Under the Act, retain inheritance and community standing even when married to non-Parsi women; the Act's endogamy enforcement is asymmetric — a man's marriage outside the community is treated differently (more permissively) than a woman's. Benefit from the Act's gender asymmetry while remaining nominally subject to it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_men_in_mixed_marriages, beneficiary,
    moderate, biographical, constrained, national).

% Parsi converts to other faiths, or those questioning Zoroastrian orthodoxy, find their family law status ambiguous under the Act. The tribunal system presumes Zoroastrian religious identity; deviation triggers exclusion from benefits or tribunal jurisdiction, with no appeal path outside the community authority structure.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, religious_minorities_within_parsis, payer,
    powerless, biographical, identity_locked, national).

% Adjudicate conflicts between Parsi communal law and civil law (especially the Special Marriage Act 1954), and review community tribunal decisions where constitutional rights are invoked. Occupy a structural boundary: recognize communal autonomy while enforcing constitutional limits on discrimination.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, civil_courts, observer,
    institutional, generational, analytical, national).

% Permits personal law pluralism (constitutional Articles 25–28) including communal family law, but is constitutionally obliged to prevent discrimination. State authority over marriage is fragmented across multiple law regimes; the Parsi Act operates under state delegation but community interpretation is substantially autonomous.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_state, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__parsi_communal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a community-controlled family law regime that preserves Zoroastrian religious identity and permits Parsi marriage, inheritance, and succession law to be adjudicated within community authority rather than imposing secular civil law on a persecuted minority. Solves the collective-action problem of minority cultural preservation in a pluralist state.
% TRANSFER_FUNCTION: Moves authority over marriage, divorce, and succession from individual choice (secular option) and civil courts to community tribunals and elders. Enforces endogamy (particularly on women), which transfers marriage partners to the in-community set and transfers inheritance outcomes to those recognized as legitimate community members.
% ABSENT_VOICES: Women in mixed marriages, religious dissenters within the community, and younger Parsis who wish to marry outside the faith are structurally excluded from tribunal processes that frame their choices as illegitimate. They would contest the endogamy requirement and demand individual choice or inclusive community interpretation; their exclusion is maintained by the tribunal system itself.
% DISAPPEARANCE_RATIONALE: If the Act and its tribunal enforcement disappeared overnight, Parsi marriages would fall entirely under the Special Marriage Act 1954 and Hindu Succession Act (applied to Parsis in some contexts). Community identity and succession arrangements would reorganize under civil law; the question is whether the community identity itself would persist (preservationist Parsi argument: the Act is necessary to identity, so loss of the Act is identity loss; pluralist argument: identity persists through cultural practice, not legal structure). The contest is over whether the constraint is constitutive or instrumental to the arrangement it claims to protect.
% FOUNDING_PROBLEM: Parsis, as a Zoroastrian minority persecuted under Islamic and later colonial rule, needed legal autonomy to preserve their religious identity and community continuity. Codification of Parsi marriage law in 1936 answered the founding problem: how to permit a persecuted minority to maintain religious identity and manage family affairs according to Zoroastrian law rather than colonial/majoritarian regimes.
% FOUNDING_PROBLEM_CORROBORATION: Parsi community authorities and historians attest the founding problem remains live: minority identity requires legal protection and community autonomy. Civil rights scholars, women's rights advocates, and younger Parsis attest the founding problem is substantially addressed (India post-1950 is a constitutional democracy that protects minority religion and permits pluralism; persecution is not the present regime); the Act now functions to enforce community endogamy and restrict exit, not primarily to protect against persecution. The contest is between preservation and coercion readings.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, contested).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__parsi_communal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the Act concentrates marriage/divorce/succession authority in tribunals controlled by community elders, restricting individual choice and forcing compliance with endogamy norms. Suppression is high (0.71) because enforcement depends on social ostracism, loss of inheritance rights, and tribunal non-recognition of out-community unions; alternatives (civil law exit) are costly because they mean loss of community identity. Theater ratio is moderate-low (0.28) because the primary function—preserving community religious identity through controlled marriage—is real and constitutive, but a growing share of enforcement activity (especially toward younger Parsis and women in mixed marriages) functions to maintain authority structures rather than substantive religious practice. The measurement series shows extractiveness rising gradually from 0.48 to 0.62 over the interval, plateauing; this reflects increasing pressure from demographic decline (smaller population, tighter enforcement of endogamy to maintain viability) and rising challenges to tribunal legitimacy (younger Parsis taking mixed marriages to civil courts). Suppression requirement rises sharply early (0.62 to 0.68 by t=12), then plateaus (0.71), indicating initial intensification of enforcement machinery and then stabilization at a higher baseline. Theater ratio rises gradually and plateaus, consistent with continued investment in the legitimacy narrative (religious continuity, identity protection) even as the functional outcome shifts toward boundary enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The parsi_community_elders and the parsi_women_married_under_act should compute very differently from the payer seats. From the elders' position (institutional, identity-locked, generational horizon), the Act is genuine coordination—it solves the collective-action problem of minority religious preservation by vesting authority in trusted community institutions. Their directionality approaches 0 (beneficiary: they set and enforce the rules, collects authority/legitimacy). From the women_in_mixed_marriages position (powerless, trapped, biographical horizon), the same structure is pure extraction: they bear the cost of enforced endogamy, have no voice in tribunal interpretation, and face non-recognition if they violate the endogamy norm. Their directionality approaches 1 (target: they are excluded from benefits and trapped by the enforcement). Parsi women married under the Act occupy a complex middle position: they benefit from real gender equity provisions (divorce rights, succession rights uncommon in historical religious family law) but also pay the cost of endogamy enforcement and tribunal dependency. This dual position (role: beneficiary + secondary_role: payer) reflects the structural asymmetry: gender equity was a genuine coordination achievement within the Act's framework, but it is bundled with endogamy enforcement that restricts who can marry and who benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Authority derives from Zoroastrian custom codified in the 1936 Act and administered by community tribunals. Community elders have institutional power (can set interpretation, enforce via social sanction) and identity-locked time horizons (Zoroastrian religious identity is not easily exit-able). They benefit from the arrangement (collect authority, validate religious identity through law). Directionality for elders: low, near beneficiary. Women in mixed marriages have powerless institutional position, trapped exit (loss of community identity if they exit tribunal jurisdiction), and bear the cost of non-recognition. Directionality: high, near target. Parsi women married under the Act have moderate power (they can invoke tribunal remedies or appeal to civil courts), constrained exit (appealing to civil courts means loss of community standing), and benefit from gender-equity provisions while paying endogamy costs. Directionality: moderate, near symmetric but tilted toward payer (constrained exit + endogamy enforcement outweigh gender-equity benefits for many). No directionality overrides are needed; the structural data (power, exit, beneficiary/victim designation) derives honest d values for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is CLAIMED as tangled_rope: genuine coordination function (minority religious preservation via autonomous community law) + asymmetric extraction (endogamy enforcement that benefits elders and men, harms women in mixed marriages and dissenters). This is not mislabeling extraction as coordination—the mandatrophy test passes because BOTH the coordination function and the extraction are structurally real and necessary to the same mechanism. The Act solves the founding problem (how to preserve minority religion in a pluralist state) AND enforces endogamy simultaneously. Removing the endogamy enforcement would not preserve the coordination—the coordination is constituted through endogamy (maintaining in-group marriage is what preserves religious identity in Parsi theology). So this is NOT a snare (extraction with cover story) or a rope (pure coordination). The tangled-rope classification is structurally correct: you cannot have one without the other, but one benefits the elders and harms the targets. The measurement series confirm this: extractiveness rises as demographic pressure intensifies endogamy enforcement (fewer Parsis, more need to police boundaries), while theater ratio rises only modestly (the legitimacy narrative remains largely stable because it IS the founding truth—the Act was built for religious preservation; the extraction is not theater but a real feature of the coordination mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minority_protection_vs_boundary_coercion,
    'Is the Parsi Marriage and Divorce Act primarily a minority-protection mechanism (necessary to preserve religious identity under majoritarian pressure) or a boundary-enforcement mechanism (using legal power to restrict exit and reproduction)?',
    'Historical and comparative analysis: (1) Did Parsi communities maintain religious identity in jurisdictions where the Act does not apply (diaspora communities, countries without personal law pluralism)? (2) What mechanisms drove the 1936 codification—was it response to persecution or to internal community concerns about assimilation? (3) Do younger Parsis who exit the Act (via civil marriage) maintain Zoroastrian identity practice, or does exit entail religious identity loss? (4) Would a reformed version of Parsi family law—maintaining inheritance and community recognition for mixed marriages—serve minority protection without endogamy enforcement?',
    'If primarily minority protection, the constraint is justified as a legitimate accommodation of minority autonomy; the extraction is a necessary feature of the coordination. If primarily boundary coercion, the constraint is an identity-preservation tool that unjustly restricts exit and denies rights to women and dissenters; the extraction should be decoupled from the identity function (e.g., via reformed law that protects inheritance rights regardless of spouse religion). The classification (tangled rope vs. snare + false legitimation) depends on this.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_protection_vs_boundary_coercion, conceptual, 'Whether the endogamy enforcement in the Act is structurally necessary to minority religious protection or instrumentally chosen for boundary maintenance.').

omega_variable(
    identity_lock_suppression_mechanism,
    'Is the suppression measured in the Act (0.71) primarily structural (tribunal non-recognition, legal barriers, economic consequences) or primarily internalized (agents believe they deserve community enforcement, have fused their identity with Parsi law, experience exit as identity annihilation)?',
    'Post-exit ethnography and psychological assessment: (1) Parsis who exit to civil law and face tribunal non-recognition—do they experience suppression as external force or internalized obligation? (2) Do they report identity loss after exit, or identity continuity with changed legal status? (3) For those who remain in community despite disagreeing with tribunal authority, what maintains compliance—fear of social sanction or internalized belief that the tribunal has legitimate authority?',
    'If suppression is primarily structural, the constraint operates through external barriers; exit becomes possible if barriers are lowered (reformed law, civil law access, tribunal reform). If suppression is primarily internalized, the constraint persists through cognitive/emotional fusion; even those with legal exit options may not take them because exit feels like self-loss. The engine''s effective suppression calculation should reflect this: internalized suppression is harder to address and more likely to persist after barrier removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Mechanism of suppression: structural barriers vs. internalized identity fusion').

omega_variable(
    demographic_tipping_point_viability,
    'Is there a population threshold below which the Parsi community cannot reproduce itself as a religiously and ethnically distinct population, even with endogamy enforcement via the Act?',
    'Demographic modeling: at what population size does outmarriage rate exceed endogamy enforcement capacity? Interview data on Parsi marriage decisions: at what point do economic constraints (need for professional mobility) outweigh family/community pressure to marry within community? Historical comparison with other religious minorities that faced similar constraints (e.g., Zoroastrians outside India, Jewish communities under restrictive marriage laws).',
    'If a tipping point exists and is approaching (some analyses place it at 20,000–30,000 population; current estimate ~35,000), the Act''s enforcement may be counterproductive: tighter endogamy enforcement accelerates exit (younger Parsis postpone or refuse marriage under tribunal rules, choosing civil law or inter-religious marriage). If the tipping point is distant or avoidable through inclusive reform (e.g., permitting mixed marriages while maintaining inheritance recognition), the Act''s current form may be unnecessary. This affects the mandatrophy diagnosis: if the constraint is approaching viability failure (demographic squeeze), it should be reclassified from tangled rope toward piton (theater-dependent, inertial maintenance despite failing function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_tipping_point_viability, empirical, 'Whether demographic decline will make endogamy enforcement counterproductive before it makes the community non-viable.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the Parsi communal reading logically foreclose the secular civil reading of the same kernel (can both coexist, or does one rule out the other in a single framework)?',
    'Constitutional analysis: (1) Can the Indian Constitution simultaneously recognize communal marriage law under Articles 25–28 (minority autonomy) and require individual choice under Articles 14, 21 (equal rights)? (2) Is there a principled way to permit both Parsi tribunal marriage and Special Marriage Act civil marriage to coexist, or does legitimating one undermine the other? (3) What do the sibling readings (Hindu codified, Christian canonical, Muslim shariat) reveal—do they coexist peacefully with secular civil reading, or is there a hierarchy?',
    'If readings foreclose each other, the kernel contest is a zero-sum distribution of authority; one reading must be chosen, others rejected. If readings coexist, the kernel permits multiple authority structures for the same domain; the question becomes how to resolve conflicts between them (hierarchy, individual choice, situational routing). This affects the cs_structure.reading_relations classification: determines whether the Parsi reading and secular civil reading are in forecloses vs. coexists_with relation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether the Parsi communal reading and secular civil reading can logically coexist in a single constitutional framework.').

omega_variable(
    axiomic_shift_in_parsi_authority_grounding,
    'Is the authority grounding of the Parsi Act still ''lineage'' (unbroken transmission of Zoroastrian law from ancient tradition through community practice), or has it shifted toward ''extraction'' (the community now uses the Act to extract compliance and boundary maintenance)?',
    'Genealogical analysis of tribunal decisions: (1) Do recent tribunal decisions cite ancient Zoroastrian sources and traditional interpretation, or do they focus on the 1936 Act as the canonical text? (2) Have tribunal interpretations become more restrictive (boundary enforcement) or more adaptive (accommodating community diversity)? (3) What do tribunal members report about their legitimacy source—fidelity to tradition or community service/protection?',
    'If lineage grounding remains primary, the Act''s authority rests on continuity of tradition; this supports the minority-protection narrative. If extraction grounding is now primary, the Act''s authority increasingly derives from the community institutions'' power to enforce and control outcomes; this supports the boundary-coercion reading. Shift in authority grounding would suggest the cs_structure.authority_grounding should be ''extraction'' rather than ''lineage,'' which would change the kernel classification dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiomic_shift_in_parsi_authority_grounding, empirical, 'Whether the Parsi Act''s authority is grounded in lineage (tradition) or extraction (institutional power).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(marr_tr_t4, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(marr_tr_t8, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(marr_tr_t12, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(marr_tr_t16, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(marr_tr_t24, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(marr_tr_t28, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 28, 0.28).
narrative_ontology:measurement(marr_tr_t32, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 32, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(marr_be_t4, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(marr_be_t8, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(marr_be_t12, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(marr_be_t16, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(marr_be_t24, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(marr_be_t28, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 28, 0.62).
narrative_ontology:measurement(marr_be_t32, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 32, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(marr_su_t4, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 4, 0.64).
narrative_ontology:measurement(marr_su_t8, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(marr_su_t12, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(marr_su_t16, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(marr_su_t24, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(marr_su_t28, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 28, 0.71).
narrative_ontology:measurement(marr_su_t32, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 32, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__parsi_communal_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family: the contested marriage/family law authority in India. Five readings instantiate different structural authority sources: Parsi communal (this file), Hindu codified, Muslim shariat, Christian canonical, and secular civil. Each reading has its own ε, beneficiary/victim structure, and measured type. They are not the same constraint from different angles—they are different constraints grounded in the same contested kernel. Sibling readings differ in: (1) authority source (community custom vs. civil code vs. constitutional rights); (2) beneficiary structures (who controls interpretation; who benefits from the regime); (3) measured extractiveness (communal readings show higher extraction from those who exit; secular reading shows lower extraction from dissenters because exit to another regime is legal). The family is linked because they compete for jurisdiction over the same domain (marriage and family law) and because changes to one reading affect the others (e.g., if civil courts strengthen enforcement of Special Marriage Act, the relative attractiveness of tribunal-based regimes changes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__parsi_communal_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
