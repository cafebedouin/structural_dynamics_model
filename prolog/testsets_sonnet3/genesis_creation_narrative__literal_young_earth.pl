% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__literal_young_earth
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__literal_young_earth, []).

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
 *   constraint_id: genesis_creation_narrative__literal_young_earth
 *   human_readable: Young Earth Creationist Reading of Genesis 1-2 as Inerrant Historical-Scientific Chronicle
 *   domain: religious_studies/biblical_hermeneutics/science_religion_interface
 *
 * SUMMARY:
 *   This story models the literal-historical, young-earth reading of Genesis
 *   1-2 as it functions within institutions (denominations, seminaries,
 *   creation-science organizations, YEC-affiliated colleges) that have made
 *   affirmation of this specific reading a condition of doctrinal fidelity,
 *   employment, and ordination. The reading treats the creation narrative as
 *   an inerrant chronicle: six literal 24-hour days, a universe on the order
 *   of thousands rather than billions of years old, and 'dominion' (Genesis
 *   1:28) as license for largely unconstrained human use of the created
 *   order. The coordination function (a stable, unambiguous origins doctrine
 *   that anchors group identity against secularizing pressure) is genuine.
 *   But the same structure that coordinates belief also extracts professional
 *   and relational costs from scientifically-trained faculty, clergy holding
 *   alternative readings, and congregants who privately accept mainstream
 *   science — hence tangled_rope rather than a clean rope or a pure mountain.
 *   Per the ε-invariance rule, this file covers ONLY the literal_young_earth
 *   reading; the allegorical_ancient_near_east and theistic_evolutionary
 *   readings are separate constraints with their own ε, beneficiaries, and
 *   stakeholders, linked here via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - young_earth_institutional_leadership: agenda_setter (institutional/arbitrage) — sets and enforces the doctrinal affirmation requirement
 *   - creationist_publishing_and_museum_industry: beneficiary (organized/arbitrage) — commercial ecosystem dependent on the reading's persistence
 *   - biology_and_geology_faculty_at_affiliated_institutions: payer (moderate/constrained) — bears professional cost of the affirmation requirement
 *   - science_literate_congregants: payer (powerless/constrained) — bears social and relational cost of private disagreement
 *   - youth_raised_in_yec_communities: payer (powerless/trapped) — inherits the doctrine before capacity for independent evaluation
 *   - theistic_evolutionist_clergy_seeking_ordination: excluded (moderate/constrained) — alternative reading structurally barred from institutional consideration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__literal_young_earth, 0.58).
domain_priors:suppression_score(genesis_creation_narrative__literal_young_earth, 0.79).
domain_priors:theater_ratio(genesis_creation_narrative__literal_young_earth, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, extractiveness, 0.58).
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__literal_young_earth, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__literal_young_earth, "Young Earth Creationist Reading of Genesis 1-2 as Inerrant Historical-Scientific Chronicle").
narrative_ontology:topic_domain(genesis_creation_narrative__literal_young_earth, "religious_studies/biblical_hermeneutics/science_religion_interface").

domain_priors:requires_active_enforcement(genesis_creation_narrative__literal_young_earth).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__literal_young_earth, '89d4194a-7923-4232-843c-4fc9d703eb98').
narrative_ontology:cs_kernel_codification('89d4194a-7923-4232-843c-4fc9d703eb98', fixed_text).
narrative_ontology:cs_authority_grounding('89d4194a-7923-4232-843c-4fc9d703eb98', lineage).
narrative_ontology:cs_interpretation_layer_present('89d4194a-7923-4232-843c-4fc9d703eb98').
narrative_ontology:cs_reading_relation('89d4194a-7923-4232-843c-4fc9d703eb98', genesis_creation_narrative__theistic_evolutionary, forecloses).
narrative_ontology:cs_reading_relation('89d4194a-7923-4232-843c-4fc9d703eb98', genesis_creation_narrative__allegorical_ancient_near_east, coexists_with).
narrative_ontology:cs_axiom('89d4194a-7923-4232-843c-4fc9d703eb98', foundational, genesis_as_inerrant_historical_scientific_record).
narrative_ontology:cs_axiom_status(genesis_as_inerrant_historical_scientific_record, holdable).
narrative_ontology:cs_axiom_grounding('89d4194a-7923-4232-843c-4fc9d703eb98', genesis_as_inerrant_historical_scientific_record, deontological).
narrative_ontology:cs_axiom('89d4194a-7923-4232-843c-4fc9d703eb98', foundational, young_earth_chronology_required_by_biblical_fidelity).
narrative_ontology:cs_axiom_status(young_earth_chronology_required_by_biblical_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('89d4194a-7923-4232-843c-4fc9d703eb98', young_earth_chronology_required_by_biblical_fidelity, empirically_contingent).
narrative_ontology:cs_reference_frame('89d4194a-7923-4232-843c-4fc9d703eb98', young_earth_six_day_chronology).
narrative_ontology:cs_drift_state('89d4194a-7923-4232-843c-4fc9d703eb98', post_geological_and_evolutionary_consensus_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('89d4194a-7923-4232-843c-4fc9d703eb98', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__literal_young_earth, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, young_earth_institutional_leadership).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, creationist_publishing_and_museum_industry).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, doctrinal_gatekeepers_in_affiliated_seminaries).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, biology_and_geology_faculty_at_affiliated_institutions).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, science_literate_congregants).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, youth_raised_in_yec_communities).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, theistic_evolutionist_clergy_seeking_ordination).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, affiliated_denomination_membership_at_large).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, affiliated_denomination_membership_at_large).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__literal_young_earth, scriptural_inerrancy_doctrine).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__literal_young_earth, young_earth_chronology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Denominational leaders, seminary presidents, and creation-science organization heads set doctrinal statements requiring affirmation of a literal six-day, 24-hour creation and a young earth (typically 6,000-10,000 years). They administer statements of faith, control ordination and faculty employment contingent on affirmation, and derive institutional identity, donor base, and authority from being the guardians of biblical literalism against 'compromise' with mainstream science.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, young_earth_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Organizations that produce creation-science curricula, apologetics literature, and creationist museums and theme parks derive revenue and institutional relevance directly from the literal-historical reading's persistence. A shift toward theistic evolution or allegorical readings would collapse their market and mission rationale.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, creationist_publishing_and_museum_industry, beneficiary,
    organized, biographical, arbitrage, national).

% Faculty and administrators who hold positions specifically because they affirm and teach the young-earth reading. Their professional standing, publishing careers, and institutional roles depend on the reading remaining the required orthodoxy; they administer loyalty oaths and review committees that enforce it.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, doctrinal_gatekeepers_in_affiliated_seminaries, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__literal_young_earth, doctrinal_gatekeepers_in_affiliated_seminaries, agenda_setter).

% Scientists employed at YEC-affiliated colleges and seminaries must either publicly disavow mainstream radiometric dating, geology, and evolutionary biology or face termination, denial of tenure, or reputational exile within their faith community. Their professional exit means leaving both the institution and often the religious community that gave them their credentials and social network.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, biology_and_geology_faculty_at_affiliated_institutions, payer,
    moderate, biographical, constrained, national).

% Lay members who accept mainstream scientific consensus on the age of the universe and biological evolution are told this constitutes rejecting biblical authority and, in stricter congregations, salvation-relevant faithfulness. They face social sanction, exclusion from teaching or leadership roles, and family rupture if they voice disagreement; leaving the congregation means losing community, and in tight-knit contexts, kinship ties.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, science_literate_congregants, payer,
    powerless, biographical, constrained, local).

% Children and adolescents are taught the literal-historical reading as a precondition of both religious and community belonging before they have the resources to evaluate it. Encountering mainstream science later (in college, media, or the workplace) often produces a forced choice between the faith community's plausibility structure and their own reasoning, with high psychological and relational costs regardless of which way they resolve it.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, youth_raised_in_yec_communities, payer,
    powerless, biographical, trapped, local).

% Clergy and seminarians who hold a theistic-evolutionary or allegorical reading are excluded from ordination tracks, denied pulpits, or required to affirm statements of faith they do not sincerely hold. Their alternative reading is a live theological position among biblical scholars broadly but is structurally barred from consideration within YEC-controlled institutions.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, theistic_evolutionist_clergy_seeking_ordination, excluded,
    moderate, biographical, constrained, national).

% The broader membership gets a clear, unified, easily-transmitted doctrinal identity that simplifies catechesis and strengthens in-group cohesion against a perceived secularizing culture. Some of the same members also bear the cost when their children leave the faith entirely upon discovering the scientific record, a documented driver of the broader 'faith deconversion' phenomenon.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, affiliated_denomination_membership_at_large, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__literal_young_earth, affiliated_denomination_membership_at_large, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__literal_young_earth, young_earth_institutional_leadership).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__literal_young_earth, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, unambiguous doctrinal answer to 'what does the Bible say about origins,' allowing a community to teach a shared catechesis, resist perceived secular encroachment, and maintain a stable in-group identity without adjudicating unsettled hermeneutical or scientific debates congregation by congregation.
% TRANSFER_FUNCTION: Moves professional standing, ordination access, and institutional legitimacy toward those who affirm literal six-day young-earth creation and away from scientifically-trained faculty, clergy holding alternative readings, and congregants who accept mainstream geology and biology; also moves donor revenue toward creationist publishing and museum organizations whose product depends on the reading's persistence.
% ABSENT_VOICES: Working geologists, biologists, and biblical scholars trained in Ancient Near Eastern comparative literature who hold theistic-evolutionary or allegorical readings are largely absent from YEC institutional decision-making bodies; their professional exclusion is often a precondition of institutional employment, so the doctrinal statements are set by a self-selected population that has already passed the affirmation test.
% DISAPPEARANCE_RATIONALE: If the literal-historical reading's institutional enforcement disappeared overnight, affiliated seminaries would lose their primary faculty-screening mechanism, creationist publishing and museum organizations would lose their doctrinal rationale, ordination tracks would open to theistic-evolutionist clergy, and many science-literate congregants and faculty currently constrained by loyalty requirements would surface positions they already privately hold. Family and community rupture around origins beliefs would substantially decrease.
% FOUNDING_PROBLEM: Nineteenth and twentieth-century American Protestant communities, confronting Darwinian evolution and geological deep time, sought to defend biblical authority against perceived erosion by secular science and liberal theology, treating a literal historical-scientific reading of Genesis as the load-bearing wall protecting inerrancy as a whole.
% FOUNDING_PROBLEM_CORROBORATION: Historians of American religion (outside both YEC institutions and their theological opponents) document that the 24-hour/young-earth reading as a fundamentalist test case is a comparatively recent innovation (consolidating chiefly in the 20th century, notably following works like 'The Genesis Flood,' 1961) rather than the unbroken historic Christian consensus YEC institutions claim; mainstream evangelical scholars and theistic-evolutionist clergy corroborate that the founding anxiety (defending inerrancy) persists but argue the specific young-earth solution is neither theologically necessary nor historically required to satisfy it. YEC institutional leadership itself attests the founding problem (defending scriptural authority) remains fully live and treats the literal reading as inseparable from it.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__literal_young_earth, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__literal_young_earth, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__literal_young_earth, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(genesis_creation_narrative__literal_young_earth, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__literal_young_earth, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__literal_young_earth_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(genesis_creation_narrative__literal_young_earth, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genesis_creation_narrative__literal_young_earth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is moderate-high: real coordination benefit exists (shared catechesis, group cohesion) but a substantial share of what the reading accomplishes is filtering out scientifically-informed dissent from institutional power, which is extraction on those specific parties rather than pure coordination cost. Suppression (0.79) is high and rising over the measured interval, reflecting the hardening of statement-of-faith enforcement and loyalty-oath mechanisms across affiliated institutions in response to growing public awareness of the scientific consensus on deep time and evolution — an enforcement ratchet, not a static picture, hence the tracked suppression_requirement series. Theater ratio (0.42) is moderate: creation-science apologetics contains genuine argumentative content but an increasing share of institutional activity (museum exhibits, curriculum defense, statement-of-faith re-affirmation drives) functions to perform doctrinal loyalty rather than engage the underlying empirical questions. Accessibility collapse (0.62) is moderate-high — once inside an affiliated institution, alternative readings become practically unavailable, though the collapse is narrower than a mountain's because mainstream science and alternative theological readings remain visible and available outside the institutional boundary. Resistance (0.55) reflects real internal and external pushback: faculty resignations, congregant departures, and denominational splits over the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this is coordination: a shared, defensible doctrinal line against secular encroachment, protecting the coherence of the whole theological system. From the payer seats — faculty who know the geological record, clergy who hold a different theology, congregants raising scientifically literate children — the same structure computes as extraction: a career or belonging tax levied on holding an empirically or theologically defensible alternative view. The engine should compute these divergently from the same structural data; the claim (tangled_rope) is authored to reflect that both readings are structurally correct simultaneously, not to average them.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership and the creationist commercial ecosystem sit near the full-beneficiary end: they set the terms, collect the loyalty, and derive revenue or authority from persistence of the reading, with arbitrage-grade exit (they can always found a new organization if displaced). Faculty, clergy candidates, and congregants sit toward the target end: constrained or trapped exit, bearing real professional and relational costs for holding or revealing a different view. Youth raised in these communities are the most target-like of all — trapped exit, since exit requires severing family and community ties before they have independent means to do so.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defending scriptural authority against perceived secular erosion) is contested as either live or resolved: leadership treats it as permanently live and treats the specific young-earth solution as inseparable from it; historians and theistic-evolutionist theologians corroborate that the underlying anxiety persists but argue the specific literal-chronology solution is a comparatively recent 20th-century innovation, not an irreducible requirement of biblical fidelity. This mismatch (status: contested, verdict: world_rearranges) is exactly the signature the mandatrophy detector should flag for review rather than resolve by fiat — the classification as tangled_rope, not snare, preserves the genuine coordination function (shared identity, catechetical clarity) that a pure-extraction read would erase, while the beneficiary/victim/enforcement declarations preserve the extraction the coordination-only read would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Is ''the Genesis creation narrative'' a single constraint whose proper reading is a matter of correct interpretation, or is it a kernel that different interpretive communities read into structurally distinct constraints with different beneficiaries, victims, and extraction profiles?',
    'This question is resolved by construction at the level of the corpus, not within this story: the kernel genesis_creation_narrative is instantiated as three separate constraint files (literal_young_earth, theistic_evolutionary, allegorical_ancient_near_east), each with its own epsilon, claimed_type, and stakeholder set, linked via network.affects_constraints. This file authors ONLY the literal_young_earth reading.',
    'Treating the three readings as one constraint would force an artificial averaging of epsilon values that are genuinely different (this reading is substantially more extractive toward scientifically-trained insiders than the allegorical reading, which claims no historical-scientific content and thus has near-zero epsilon on that axis).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Whether Genesis 1-2 is one constraint or a kernel with multiple structurally distinct readings; resolved by decomposition per the epsilon-invariance principle.').

omega_variable(
    sincere_belief_vs_institutional_capture,
    'Is the persistence of the literal-historical reading driven primarily by sincere theological conviction among leadership, or by institutional capture (organizations whose funding, identity, and employment structures depend on the reading''s continuation regardless of its theological necessity)?',
    'Compare doctrinal statement revision patterns and leadership succession outcomes across YEC-affiliated institutions against those in comparable evangelical institutions that do not require the young-earth affirmation; examine whether institutional dependence (donor base tied to creation-science branding) predicts resistance to doctrinal reconsideration independent of theological argument quality.',
    'If institutional capture dominates, the tangled_rope classification understates the extraction component and the constraint trends toward snare for the faculty and clergy payer seats; if sincere conviction dominates and institutional dependence is incidental, the coordination function is more load-bearing than the extraction and the classification is closer to rope with isolated extraction at the margins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincere_belief_vs_institutional_capture, empirical, 'Whether the reading''s institutional persistence is driven by conviction or by capture.').

omega_variable(
    dominion_reading_scope,
    'Does ''dominion'' (Genesis 1:28) as read by this constraint license unconstrained environmental exploitation, or is even the literal-historical reading compatible with a stewardship interpretation of dominion that limits exploitation?',
    'Survey doctrinal statements and applied environmental policy positions across YEC-affiliated institutions to determine whether exploitation-license or stewardship-limited readings of dominion actually predominate within the literal-historical camp, independent of the creation-chronology question.',
    'If exploitation-license readings predominate, this reading''s victim set should be extended to include future generations and ecological systems bearing costs of extraction rationalized by dominion theology, materially raising epsilon; if stewardship-limited readings predominate even among literalists, the dominion-as-exploitation-license delta noted in the structural expectation is less load-bearing than assumed and should be narrowed in a future revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dominion_reading_scope, conceptual, 'Whether the literal reading''s dominion doctrine is inherently exploitative or contingently so.').

omega_variable(
    suppression_mechanism_ambiguity,
    'For science-literate congregants and YEC-raised youth, is the measured suppression primarily structural (loyalty oaths, employment contingency, formal doctrinal tests) or internalized (a formed conscience that treats doubt itself as sin, persisting after any external barrier is removed)?',
    'Post-exit trajectory analysis: track whether individuals who leave YEC-affiliated institutions or congregations continue to experience guilt, doubt-suppression, or plausibility-structure lock-in regarding mainstream science years after the structural enforcement (employment, membership, family proximity) is no longer present.',
    'If suppression is substantially internalized, the constraint''s effective suppression on former insiders is higher than the structural measure (0.79) suggests, since it travels with the person after exit; this would argue for authoring a directionality override for the youth_raised_in_yec_communities seat reflecting persistent effective extraction even post-exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism affecting exited individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__literal_young_earth, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_narrative__literal_young_earth, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gene_tr_t10, genesis_creation_narrative__literal_young_earth, theater_ratio, 10, 0.26).
narrative_ontology:measurement(gene_tr_t20, genesis_creation_narrative__literal_young_earth, theater_ratio, 20, 0.31).
narrative_ontology:measurement(gene_tr_t30, genesis_creation_narrative__literal_young_earth, theater_ratio, 30, 0.34).
narrative_ontology:measurement(gene_tr_t40, genesis_creation_narrative__literal_young_earth, theater_ratio, 40, 0.37).
narrative_ontology:measurement(gene_tr_t50, genesis_creation_narrative__literal_young_earth, theater_ratio, 50, 0.4).
narrative_ontology:measurement(gene_tr_t60, genesis_creation_narrative__literal_young_earth, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_narrative__literal_young_earth, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gene_be_t10, genesis_creation_narrative__literal_young_earth, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(gene_be_t20, genesis_creation_narrative__literal_young_earth, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(gene_be_t30, genesis_creation_narrative__literal_young_earth, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(gene_be_t40, genesis_creation_narrative__literal_young_earth, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(gene_be_t50, genesis_creation_narrative__literal_young_earth, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(gene_be_t60, genesis_creation_narrative__literal_young_earth, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_narrative__literal_young_earth, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gene_su_t10, genesis_creation_narrative__literal_young_earth, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(gene_su_t20, genesis_creation_narrative__literal_young_earth, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(gene_su_t30, genesis_creation_narrative__literal_young_earth, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(gene_su_t40, genesis_creation_narrative__literal_young_earth, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(gene_su_t50, genesis_creation_narrative__literal_young_earth, suppression_requirement, 50, 0.78).
narrative_ontology:measurement(gene_su_t60, genesis_creation_narrative__literal_young_earth, suppression_requirement, 60, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__literal_young_earth, identity_coordination).
narrative_ontology:boltzmann_floor_override(genesis_creation_narrative__literal_young_earth, 0.08).
narrative_ontology:affects_constraint(genesis_creation_narrative__literal_young_earth, theistic_evolutionary).
narrative_ontology:affects_constraint(genesis_creation_narrative__literal_young_earth, allegorical_ancient_near_east).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the Genesis creation narrative' per the epsilon-invariance principle: literal_young_earth (this file, tangled_rope, moderate-high epsilon driven by institutional enforcement against scientifically-informed dissent), theistic_evolutionary (expected rope or tangled_rope with substantially lower epsilon — coordination without the categorical evolution-denial enforcement machinery), and allegorical_ancient_near_east (expected rope or mountain-adjacent with near-zero epsilon on the historical-scientific axis, since it makes no such claims to enforce). The three share a textual kernel (Genesis 1-2) but instantiate structurally distinct constraints with different beneficiaries, victims, and enforcement profiles; they must not be merged into one epsilon value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
