% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority (Special Marriage Act 1954)
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   India's marriage authority is fragmented across five distinct legal
 *   frameworks, each grounded in a different authority structure. This
 *   constraint describes the secular civil reading: marriage and family law
 *   authority derives from the Special Marriage Act 1954, grounded in
 *   constitutional individual rights (Articles 14, 15, 25 of the Indian
 *   Constitution) and administered by civil courts. This reading competes
 *   with four sibling readings: Hindu codified law (Hindu Marriage Act 1955),
 *   Muslim Shariat (personal law boards and qazis), Christian canonical law
 *   (Indian Christian Marriage Act 1872), and Parsi communal custom (Parsi
 *   Marriage and Divorce Act 1936). Each reading instantiates a different
 *   constraint with different beneficiary/victim structures, different
 *   authorial authorities, and different operative extraction mechanisms. The
 *   secular reading is classified as ROPE: it genuinely coordinates across
 *   religious boundaries (a real problem solved), but it also extracts
 *   authority from religious institutions and imposes costs on those who lose
 *   community gatekeeping power. Extraction is low (0.31) because the
 *   constraint's primary function is coordination (enabling interfaith
 *   marriage, creating gender-symmetric exit), not rent collection;
 *   suppression is low (0.18) because the constraint's persistence depends
 *   primarily on constitutional legitimacy and legislative affirmation, not
 *   on coercing compliance. The measurement series show stable, low
 *   extraction and suppression over the 70-year interval — no evidence of the
 *   constraint degrading toward pure extraction or increasing theatrical
 *   maintenance.
 *
 * KEY AGENTS:
 *   - Civil courts system: administers secular marital law; derives authority from constitution
 *   - Interfaith couples: primary beneficiaries; gain legal recognition across religious lines
 *   - Women seeking exit: beneficiaries; gain gender-symmetric dissolution grounds
 *   - Religious community authorities: payers; lose adjudicatory gatekeeping role and membership leverage
 *   - Men in patriarchal systems: payers; lose asymmetric unilateral divorce under some personal laws
 *   - Constitutional rights advocates: beneficiaries; their reading of equal protection is vindicated by the Act
 *   - Lower-caste and minority groups: beneficiaries; can circumvent caste endogamy through civil marriage
 *   - Community enforcement structures: payers; lose capacity to enforce caste and religious boundaries
 *   - Legislature and constitution drafters: observers; formal sovereigns who could amend the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.31).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.18).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, 'baa804d4-f895-4a4d-82b8-0bacf1ba72ed').
narrative_ontology:cs_kernel_codification('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', formalized).
narrative_ontology:cs_authority_grounding('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', lineage).
narrative_ontology:cs_interpretation_layer_present('baa804d4-f895-4a4d-82b8-0bacf1ba72ed').
narrative_ontology:cs_reading_relation('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', foundational, constitutional_equality_overrides_religious_law).
narrative_ontology:cs_axiom_status(constitutional_equality_overrides_religious_law, holdable).
narrative_ontology:cs_axiom_grounding('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', constitutional_equality_overrides_religious_law, deontological).
narrative_ontology:cs_axiom('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', foundational, civil_courts_as_neutral_adjudicators).
narrative_ontology:cs_axiom_status(civil_courts_as_neutral_adjudicators, holdable).
narrative_ontology:cs_axiom_grounding('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', civil_courts_as_neutral_adjudicators, conventional).
narrative_ontology:cs_reference_frame('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', constitutional_individual_rights_supremacy).
narrative_ontology:cs_drift_state('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', contemporary_pluralist_challenge, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('baa804d4-f895-4a4d-82b8-0bacf1ba72ed', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, interfaith_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_exit).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, constitutional_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, lower_caste_and_minority_groups).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, religious_community_authorities).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, men_in_patriarchal_systems).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, community_enforcement_structures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Special Marriage Act 1954 as the primary gateway for marriage registration and divorce. Interprets marital rights through constitutional equality and individual autonomy principles. Adjudicates inter-religious and inter-caste marriages that community-based personal laws do not recognize. Courts enforce uniform procedural standards and grounds for dissolution that transcend religious doctrine.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_courts_system, agenda_setter,
    institutional, generational, analytical, national).

% Can marry across religious boundaries through the secular civil framework when their religions do not recognize such unions. Their marriage is legally recognized and enforceable by state courts regardless of community acceptance. Without this route, many would face community ostracism or forced entry into a single-faith personal law system.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, interfaith_couples, beneficiary,
    moderate, biographical, mobile, national).

% Can access dissolution through civil court procedures that recognize irretrievable breakdown, cruelty, and desertion as gender-neutral grounds. In some personal law systems (particularly Muslim law), women's divorce rights are constrained; the civil route provides an exit mechanism. The procedure is uniform and does not depend on religious doctrine about gender roles or marital fidelity asymmetry.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_exit, beneficiary,
    moderate, biographical, mobile, national).

% Lose adjudicatory authority and prestige when couples marry under the secular Act instead of community rites. Their authority to interpret and enforce marital norms is circumvented. They pay through reduced membership oversight, diminished communal boundary control, and loss of revenue from religious ceremonies that mark legitimate union. They are formally excluded from civil court proceedings; their doctrinal positions do not constrain judicial interpretation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_community_authorities, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, religious_community_authorities, excluded).

% Face gender-symmetric dissolution grounds and alimony obligations under civil law, where some personal law traditions (notably Muslim law) grant men unilateral divorce by talaq. They lose the asymmetric exit option they would possess under community law. Secular divorce proceedings are longer and more litigious, increasing cost and uncertainty.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, men_in_patriarchal_systems, payer,
    moderate, biographical, constrained, national).

% The secular civil framework vindicates their reading of Articles 14 (equality), 15 (non-discrimination), and 25 (freedom of religion) as requiring state-neutral marital law decoupled from religious doctrine. The existence and enforcement of the Act is evidence that individual constitutional rights constrain personal law spheres. They collect validation of their interpretive framework with each secular marriage and each civil court decision upholding equal protection.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, constitutional_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Can circumvent caste-endogamy norms enforced within Hindu personal law frameworks by marrying inter-caste through the civil Act. The constraint enables exits that would trigger severe community sanctions under caste-based law. They gain a formal legal route even though social costs (community rejection, family rupture) remain real.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, lower_caste_and_minority_groups, beneficiary,
    powerless, biographical, mobile, national).

% Caste panchayats, religious councils, and family-honor structures lose enforcement capacity when marriages registered under the civil Act are recognized by the state regardless of community blessing. The secular route de-legitimizes their gatekeeping role. They bear the cost of eroded social control and diminished relevance in marital regulation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, community_enforcement_structures, payer,
    organized, generational, constrained, regional).

% Established the constitutional framework (Part III, rights) and enacted the Special Marriage Act as a secular alternative coexisting with personal law systems. They are the formal sovereigns who could amend or repeal the Act; legislative silence or affirmation sustains the arrangement. Observers of the constraint's operation because they set its design but do not directly adjudicate individual cases.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, legislature_and_constitution_drafters, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__secular_civil_reading, civil_courts_system).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__secular_civil_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, state-administered framework for marital registration, rights, and dissolution that operates across religious and caste boundaries. Solves the coordination problem of how individuals with no shared faith or communal tradition can have a legally recognized union with enforceable mutual obligations and exit mechanisms. Centralizes recordkeeping and adjudication so couples need not navigate multiple overlapping personal law regimes.
% TRANSFER_FUNCTION: Transfers adjudicatory authority from religious and community-based institutions to civil courts. Transfers marital legitimacy from community approval to state recognition. Transfers exit power from (in some traditions) unilateral male prerogative to gender-symmetric civil procedures. Redistribution is asymmetric: civil courts gain institutional authority and prestige; religious authorities lose gatekeeping power and membership leverage.
% ABSENT_VOICES: Religious fundamentalist movements that reject the legitimacy of secular family law for their adherents are structurally excluded from civil proceedings — their objection is to the jurisdiction itself, not to terms within it. Communal-enforcement structures (panchayats, caste councils) have no seat in civil court; their authority claims are overridden by judicial precedent, not negotiated. These absent voices would argue for restoration of community law primacy and rejection of state interference in intimate relations.
% DISAPPEARANCE_RATIONALE: If the Special Marriage Act were repealed, interfaith and inter-caste marriages would lose a primary legal route; couples would either marry under one party's personal law (with the other spouse potentially stateless within that system) or forego legal marriage. Community gatekeeping would re-entrench. Women's exit routes would narrow (especially for those married under Muslim or Hindu law with asymmetric dissolution). The entire legal landscape of marital equality would shift — the Act's disappearance would reorganize family law governance toward religious and communal fragmentation.
% FOUNDING_PROBLEM: Post-independence India needed a secular marriage law decoupled from religious doctrine to enable inter-religious and inter-caste unions in a religiously plural society; to establish a single administrative apparatus for marriage registration (prior systems had fragmented record-keeping); to provide equal protection under Article 14 while respecting religious minorities' personal law rights.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and the Supreme Court have affirmed the problem is still live: religious fragmentation of marital law creates asymmetric rights for women under different personal laws, inter-religious couples have no unified legal identity, and caste endogamy enforcement persists through informal community pressure even though legally unenforceable. Independent human-rights organizations document the continued barriers to interfaith marriage and women's exit from oppressive relationships under personal law systems. Religious authorities dispute this, arguing their traditions are legitimate and not oppressive — that dispute is itself evidence the founding problem (how to govern marital relations in a pluralistic society) remains contested.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low and stable because the secular civil framework is justified primarily by coordination (solving the interfaith/inter-caste problem) and by equal protection, not by collecting rents from one party to another. The constraint's primary function—enabling marriages that would not be recognized under personal law systems—is genuine coordination that benefits all parties ex-ante (couples can marry; the state has clear marital records; civil courts have jurisdiction clarity). The extraction that occurs is asymmetric redistribution of authority: religious institutions lose adjudicatory power and community control. This extraction is sustained not by suppression (the constraint's operation does not require secret-keeping or systematic falsification) but by constitutional entrenchment—civil courts are empowered by the Constitution itself, making religious authorities' loss a feature of the constitutional architecture, not a coercive overlay. Theater is minimal (0.12) because the constraint's justification rests on equal protection and individual rights, not on performative legitimacy—the courts need not maintain elaborate theatrical justifications; the constitutional grounding is the justification. Suppression is modest (0.18) because the only sustained suppression required is preventing community authorities from interfering in civil marital proceedings—a jurisdictional boundary, not coercion of individual behavior. Most couples comply with the secular framework because it genuinely benefits them (especially interfaith and inter-caste couples), not because of coercion. The stability of all metrics across the 70-year interval indicates no drift toward higher extraction or degradation—the constraint has maintained its primary coordination function without requiring ratcheting suppression or increasing theatrical justification.
 *
 * PERSPECTIVAL GAP:
 *   The civil courts and constitutional advocates see this constraint as establishing the rule of law and equal protection, correcting the prior fragmentation and unequal treatment of women under personal law. Religious authorities see it as state overreach into intimate domains that should be governed by tradition and doctrine. Interfaith couples and women seeking exit see it as liberation; men in patriarchal systems see it as constraint; caste-enforcing communities see it as threat to social order. Each seat experiences a different constraint—not because measurement is observer-relative (it is not; ε is fixed to the standing arrangement), but because each seat's power, exit options, and interests are different. The engine computes per-seat classification from the authored structural data; the divergence is expected and informative, not a defect.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality follows from beneficiary/victim declarations and exit options. Civil courts benefit (d ≈ 0.1–0.2, near beneficiary); interfaith couples benefit (d ≈ 0.2, beneficiary with modest cost); women seeking exit benefit (d ≈ 0.25, beneficiary with some procedural cost); religious authorities pay (d ≈ 0.75–0.85, near target, constrained exit); community enforcement structures pay (d ≈ 0.80, near target, constrained exit); men losing divorce privileges pay (d ≈ 0.6, moderate target, constrained exit). The extraction is low because it is authority redistribution (from religious to civil institutions), not concentration of rents in a single beneficiary's hands. Effective extraction χ is scaled downward for beneficiaries and upward for targets, but the base ε (0.31) is modest because the constraint does not systematically extract wealth or resources from victims—it extracts legitimacy and authority, which is real but not as economically measurable as commission gates or debt traps.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (mandate outliving function) is NOT present in this constraint. The founding problem (how to govern marriage in a pluralistic society, enable interfaith marriage, protect women's equal rights) remains live. The constraint continues to serve this function: the Special Marriage Act is used by interfaith couples, women seeking equal grounds for dissolution, and lower-caste individuals escaping endogamy. The Act has not degraded into pure theater or zombie enforcement. Legislative efforts to restrict the Act or to subordinate civil law to personal law have largely failed, indicating ongoing political commitment to the secular framework. The constraint is actively contested (some religious fundamentalist groups and caste-based communities actively resist it) but not mandatropic—contestation is evidence of live function, not degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_reading_vs_institutional_choice,
    'Is the secular civil reading of marriage authority a necessary inference from constitutional text, or is it one legitimate institutional choice among several?',
    'Comparative constitutional jurisprudence: examine how other pluralistic democracies (Malaysia, Nigeria, Lebanon) handle religious vs. secular family law authority, and whether constitutional equality principles necessarily foreclose personal law systems.',
    'If necessary inference: the secular reading''s authority is grounded in constitutional logic that coexists with other readings only through political tolerance. If institutional choice: the secular reading is contingent on legislative commitment and constitutional interpretation, making it more vulnerable to reversal via constitutional amendment or judicial re-reading. Classification would shift from Rope (genuine coordination with legitimacy transfer) toward Tangled Rope (coordination plus coercive imposition of one reading over others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_reading_vs_institutional_choice, conceptual, 'Whether secular marriage authority is constitutionally mandated or one defensible choice among several.').

omega_variable(
    social_cost_externality_of_exit,
    'How much of the actual barrier to exit from personal law systems is suppression (state coercion) vs. internalized social cost (community rejection, family rupture, loss of identity)?',
    'Post-exit ethnography: interview individuals who have switched between personal law and secular civil frameworks, measuring suppression experienced within formal legal systems vs. informal social pressure from family and community. Track legal cost (court fees, time) vs. social cost (relationship dissolution, status change).',
    'If suppression-dominant: the secular civil framework''s low authored suppression (0.18) is accurate; exits are legally enabled. If social-cost-dominant: the authored suppression understates the actual barrier because individuals carry community enforcement with them; effective exit is more constrained than legal analysis suggests, making the constraint more extractive for those seeking exit from tradition-bound contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_cost_externality_of_exit, empirical, 'Whether the measured suppression captures legal barriers or understates community-enforced exit costs.').

omega_variable(
    kernel_reading_multiplicity_bias,
    'Does generating five separate constraint stories for the five readings of marriage authority risk overfactorizing a unified legal reality that should be modeled as one constraint with five interpretations?',
    'Check whether the five readings have structurally distinct ε values (base extractiveness), different beneficiary/victim sets, or different operational mechanisms. If all five share the same underlying function (coordinate marital law) with only interpretive differences, decomposition is over-analysis; if ε values and beneficiary structures genuinely differ, decomposition is correct per ε-invariance.',
    'If overfactorization: the corpus will contain five competing constraint stories that should have been integrated into one constraint with reading-relative properties (a future ontology expansion). Classification accuracy will suffer from multiplication of similar-power constraints. If correct decomposition: the corpus accurately reflects that the five readings have structurally different extraction profiles, different authorities, and different victims—each is a real constraint, not a mere reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_multiplicity_bias, conceptual, 'Whether kernel decomposition into five constraint stories is justified by structural differences or an artifact of the reading-centric authoring framework.').

omega_variable(
    legitimacy_source_ambiguity_secular_reading,
    'Does the secular civil reading''s authority derive from constitutional text, from constituent power (the people who enacted the Constitution), or from ongoing democratic affirmation?',
    'Constitutional history and amendment politics: examine whether the secular reading would survive constitutional amendment removing Article 14 equality, and whether the secular framework is sustained by active legislative reaffirmation or by doctrinal entrencement in Supreme Court jurisprudence.',
    'If constituent power: the secular reading is fixed by the founding moment and resistant to reversal. If democratic affirmation: the reading is vulnerable to legislative amendment and democratic backlash (observable in campaigns to restrict the Act). If doctrinal entrenchment: the reading persists via judicial interpretation but could be reversed by a different judicial coalition. Classification would remain Rope but the stability and resilience of the constraint would vary by source of legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity_secular_reading, conceptual, 'What authority source sustains the secular civil reading''s legitimacy over time.').

omega_variable(
    gender_equity_benefit_distribution,
    'Does the gender-equitable dissolution and marital rights regime under secular civil law benefit women as a class, or does it primarily benefit women with resources (education, income, family backing) to navigate litigation?',
    'Access-to-justice empirical study: compare women''s actual use of civil court dissolution vs. personal law informal dissolution (negotiated, unrecorded); measure correlation between economic status and ability to enforce court orders; track duration and cost of civil cases by socioeconomic status of parties.',
    'If equitable benefit: the secular civil framework is a genuine coordination mechanism that solves the women''s exit problem across income levels. If skewed benefit: the framework benefits a class and extracts from lower-income women (by making formal dissolution more expensive than informal, or by enforcing male-favorable defaults when women cannot litigate). Classification would remain Rope for the high-income beneficiary class but shift toward Tangled Rope or Snare for lower-income women if extraction is masked by formal equality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_equity_benefit_distribution, empirical, 'Whether gender-equitable law translates to equitable access and outcomes across economic classes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__secular_civil_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__secular_civil_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__secular_civil_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__secular_civil_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__secular_civil_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(marr_tr_t50, marriage_authority_kernel__secular_civil_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__secular_civil_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__secular_civil_reading, theater_ratio, 70, 0.12).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(marr_be_t50, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 50, 0.31).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 60, 0.31).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 70, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(marr_su_t40, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 40, 0.16).
narrative_ontology:measurement(marr_su_t50, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 50, 0.17).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 60, 0.18).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 70, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__secular_civil_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% The secular civil reading is one instantiation of the contested marriage authority kernel in Indian law. It coexists with four sibling readings (Hindu codified, Muslim Shariat, Christian canonical, Parsi communal), each grounded in different authority sources and producing different ε values and beneficiary/victim structures. The secular reading is authored as a constraint with independent ε, structural data, and stakeholders; sibling readings are separate constraint files linked via network.affects_constraints. The decomposition is justified by the ε-invariance principle: the five readings have structurally distinct extraction profiles (the secular reading privileges certain couples and costs religious authorities; the Hindu reading privileges Hindu community gatekeepers; the Muslim reading privileges male prerogative under classical Shariat, etc.). Each reading is ε-invariant within its own instantiation but produces a different ε across readings because the referent is the same standing arrangement (marriage authority in India) read through five different interpretive frames, each with different beneficiaries and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
