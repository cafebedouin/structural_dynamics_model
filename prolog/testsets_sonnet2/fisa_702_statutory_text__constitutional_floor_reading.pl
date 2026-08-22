% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__constitutional_floor_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: Fourth Amendment Constitutional Floor Reading of Section 702 Queries
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This story instantiates the constitutional-floor reading of the Section
 *   702 kernel: the claim that a database query of incidentally collected
 *   communications for U.S.-person identifiers is itself a Fourth Amendment
 *   search requiring individualized probable cause, regardless of how the
 *   underlying collection was authorized. The reading has been pressed in
 *   litigation and by civil liberties advocates through successive
 *   reauthorization cycles (2008, 2012, 2018, 2023) but has not been adopted
 *   by a controlling federal appellate decision; it remains a minority
 *   constitutional position with growing traction in dissents and
 *   concurrences. Two sibling readings of the same statutory kernel — the
 *   incidental_collection_reading (queries are permissible incident to lawful
 *   foreign-intelligence collection) and the foreign_target_strict_reading
 *   (the statute's own foreign-target language should be read narrowly to
 *   minimize U.S.-person exposure without reaching the constitutional
 *   question) — are separate constraints, not alternate framings of this one.
 *   ε is authored here at 0.25, reflecting constitutional compliance cost to
 *   the executive (delay, disclosure, docket burden) under this reading's own
 *   lights, not the ε of the status quo query practice the
 *   incidental_collection_reading would measure, and not the ε of an
 *   idealized post-adoption warrant regime.
 *
 * KEY AGENTS:
 *   - us_person_communicants: Primary target/beneficiary (powerless/trapped) — bears warrantless query exposure under current practice; would benefit from probable cause gate
 *   - intelligence_and_law_enforcement_agencies: Primary agenda-setter (institutional/arbitrage) — operates query systems, resists judicial imposition of pre-query review
 *   - fisa_court: Secondary institutional actor (institutional/constrained) — would absorb expanded adjudicative burden if reading adopted
 *   - criminal_defense_bar: Secondary payer (moderate/constrained) — litigates a not-yet-recognized suppression theory
 *   - congress: Excluded institutional voice (institutional/analytical) — statutory judgment rendered non-dispositive by this reading's own logic
 *   - civil_liberties_organizations: Analytical beneficiary/advocate (organized/analytical) — originates and presses the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.55).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Fourth Amendment Constitutional Floor Reading of Section 702 Queries").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, 'c73bc1ba-a780-4cb4-8ee3-ab7452c255bc').
narrative_ontology:cs_kernel_codification('c73bc1ba-a780-4cb4-8ee3-ab7452c255bc', fixed_text).
narrative_ontology:cs_authority_grounding('c73bc1ba-a780-4cb4-8ee3-ab7452c255bc', lineage).
narrative_ontology:cs_interpretation_layer_present('c73bc1ba-a780-4cb4-8ee3-ab7452c255bc').
narrative_ontology:cs_reading_relation('c73bc1ba-a780-4cb4-8ee3-ab7452c255bc', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('c73bc1ba-a780-4cb4-8ee3-ab7452c255bc', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_axiom('c73bc1ba-a780-4cb4-8ee3-ab7452c255bc', foundational, query_constitutes_independent_fourth_amendment_search).
narrative_ontology:cs_axiom_status(query_constitutes_independent_fourth_amendment_search, holdable).
narrative_ontology:cs_axiom_grounding('c73bc1ba-a780-4cb4-8ee3-ab7452c255bc', query_constitutes_independent_fourth_amendment_search, deontological).
narrative_ontology:cs_axiom('c73bc1ba-a780-4cb4-8ee3-ab7452c255bc', foundational, statutory_foreign_domestic_distinction_constitutionally_irrelevant_at_query).
narrative_ontology:cs_axiom_status(statutory_foreign_domestic_distinction_constitutionally_irrelevant_at_query, holdable).
narrative_ontology:cs_axiom_grounding('c73bc1ba-a780-4cb4-8ee3-ab7452c255bc', statutory_foreign_domestic_distinction_constitutionally_irrelevant_at_query, deontological).
narrative_ontology:cs_reference_frame('c73bc1ba-a780-4cb4-8ee3-ab7452c255bc', fourth_amendment_individualized_suspicion_baseline).
narrative_ontology:cs_drift_state('c73bc1ba-a780-4cb4-8ee3-ab7452c255bc', post_snowden_702_reauthorization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c73bc1ba-a780-4cb4-8ee3-ab7452c255bc', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_person_communicants).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, judicial_oversight_bodies).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, us_person_communicants).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, criminal_defense_bar).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_organizations).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_search_definition_applies_to_query_not_collection).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, individualized_probable_cause_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their communications are incidentally collected under Section 702 targeting foreign persons abroad, then queried by domestic agencies using U.S.-person identifiers without an individualized warrant. Under this reading, that query is itself a Fourth Amendment search; the communicant has no ability to know a query occurred, contest it in advance, or exit the collection pool because it depends on being in contact with a foreign target they did not choose. They bear the privacy cost of warrantless querying and would be the beneficiaries of a probable-cause gate if this reading prevailed.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_person_communicants, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__constitutional_floor_reading, us_person_communicants, beneficiary).

% Operate the query systems, set internal minimization and querying procedures, and resist judicial imposition of a pre-query probable cause requirement on efficiency and secrecy grounds. Under the statutory interpretation they favor, queries are not separately regulated searches; under this constitutional floor reading, their query practices become subject to individualized judicial review, materially slowing operational tempo and requiring disclosure that risks sources and methods.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, intelligence_and_law_enforcement_agencies, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Currently reviews certifications and targeting/minimization procedures programmatically rather than individualized queries. Under this reading, the court would be required to conduct case-by-case probable cause review of U.S.-person queries, expanding its docket and adjudicative role from programmatic oversight to something resembling ordinary criminal warrant review — a role it is institutionally unaccustomed to and under-resourced for.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, observer).

% Defendants whose prosecutions rest partly on evidence derived from warrantless 702 queries currently face notice and standing barriers to challenging that evidence. Under this reading, defense counsel would gain a constitutional hook to suppress query-derived evidence lacking a warrant, but until the reading is adopted they continue litigating a foreclosed argument in most circuits, absorbing the cost of a doctrine not yet recognized.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, criminal_defense_bar, payer,
    moderate, biographical, constrained, national).

% Reauthorizes Section 702 periodically and has repeatedly declined to impose a statutory warrant requirement for U.S.-person queries, instead layering procedural safeguards (query standards, some judicial approval for certain query categories). Under a constitutional floor reading, congressional statutory choices become irrelevant to the underlying constitutional question — Congress's repeated judgment that no warrant is required is treated as non-dispositive, effectively sidelining the legislature's considered institutional position on its own foreign intelligence framework.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress, excluded,
    institutional, generational, analytical, national).

% Advocate for exactly this reading in litigation and before Congress, arguing that treating queries as searches is the only way to give the Fourth Amendment operative force against a collection architecture designed to route around individualized suspicion. They bear no direct cost from adoption and stand to gain the doctrinal win they have pursued across multiple reauthorization cycles.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_organizations, beneficiary,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__constitutional_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__constitutional_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Read at its strongest, this reading solves a genuine constitutional-design problem: preventing a foreign-intelligence collection architecture from becoming a backdoor around the individualized-suspicion requirement that governs domestic searches, by locating the search event at the query rather than at collection.
% TRANSFER_FUNCTION: If adopted, the reading would shift procedural burden and delay cost from U.S.-person communicants (who currently bear the privacy cost of warrantless querying) to intelligence and law enforcement agencies and the FISA Court (who would bear compliance, disclosure, and docket costs). Currently, under the competing statutory reading, the burden runs the other way.
% ABSENT_VOICES: Foreign intelligence targets and allied foreign governments have no voice in this domestically-framed constitutional debate, yet the scope of permissible collection against them is the predicate for the U.S.-person query controversy. Congress's institutional judgment, while formally represented, is analytically excluded from the constitutional question by this reading's own logic — the reading holds that no statutory choice by Congress can satisfy the Fourth Amendment floor.
% DISAPPEARANCE_RATIONALE: If this reading vanished as a live constitutional claim, the query practices at issue would continue entirely unchanged in the near term, since it is a minority position not yet adopted by any controlling court — so operationally the world stays the same. But the parties dispute this: civil liberties advocates and affected defendants would say the disappearance of the argument forecloses a live avenue for constraining a large-scale surveillance architecture, which is a rearrangement of legal exposure and institutional incentive even without an immediate operational change.
% FOUNDING_PROBLEM: The reading was constructed to close what its proponents see as a structural loophole: collection targeted at foreigners abroad, once pooled into a searchable database, becomes a mechanism for querying U.S. persons' communications content without the individualized probable cause that would be constitutionally required if the government sought that content directly and domestically.
% FOUNDING_PROBLEM_CORROBORATION: Federal public defenders and several circuit dissents/concurrences (outside the intelligence community and outside the civil liberties advocacy groups that originated the argument) have independently flagged the query-as-search structure as an open constitutional question in suppression litigation; no federal appellate court has yet adopted the reading as controlling law, and the government's own oversight reports acknowledge the volume of U.S.-person queries without conceding the constitutional characterization — corroboration exists from adjudicative dissents but not yet from a controlling majority or from the agencies themselves.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, contested).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).
:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.25) because this reading's own metric is constitutional compliance cost to the executive, not surveillance harm to communicants — from the reading's perspective, the current absence of a warrant requirement is the extraction, and adopting the reading would impose a compliance cost on agencies that is real but bounded (individualized review of a subset of queries, not blanket collection prohibition). Suppression is moderate (0.55): the reading's proponents are not physically prevented from litigating it, but structural doctrines (standing barriers, state secrets, sealed FISC proceedings, notice deficiencies) make it very difficult for an affected communicant to ever reach a court capable of adopting the reading. Theater ratio rises across the interval (0.2 to 0.4) as agencies layer procedural query-approval requirements and public transparency reporting that create an appearance of self-imposed constraint without conceding the constitutional characterization this reading demands — a form of proxy compliance substituting for the individualized judicial review the reading actually requires. Accessibility collapse is moderate (0.45): once a party understands the structure, some avenues (habeas, criminal suppression motions, FOIA-driven oversight litigation) remain open, but the practical path to a controlling adoption is narrow. Resistance is high (0.7): agencies, much of the FISA Court's established practice, and successive Congresses have actively resisted this characterization across three reauthorization cycles.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. person communicants are declared both payer (bear the current warrantless-query exposure) and prospective beneficiary (would gain a probable-cause gate if the reading prevailed) — the derivation should place them near the target end of directionality given their trapped exit and powerless standing, since under the status quo they cannot avoid being incidentally collected or queried. Intelligence and law enforcement agencies are the agenda-setters who would bear the compliance cost this reading's ε measures; their institutional power and arbitrage-grade exit (they can adjust practice, litigate, or seek legislative cover) place them near the low-χ end structurally even though this reading casts them as bearing a cost. The FISA Court occupies an unusual dual position — administratively constrained (it did not choose its current programmatic-review role and would be required to expand it) yet institutionally powerful; it is coded agenda_setter/observer rather than payer because it administers rather than bears the substantive cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — closing the query-as-backdoor gap — remains live by this reading's own account (query volumes have risen, not fallen, since 2008), which forecloses a mandatrophy verdict for this reading: the arrangement it argues against is not treated as a dead mandate persisting on inertia, but as an active and expanding practice whose constitutional justification is precisely what is contested. The interesting mandatrophy question sits on the sibling incidental_collection_reading, not here: if that reading's founding justification (foreign intelligence necessity) has become decoupled from its current domestic-query use, that is the reading where mandatrophy_resolved would be the live question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    query_as_search_doctrinal_status,
    'Is a database query of previously and lawfully collected communications a separate ''search'' under the Fourth Amendment, or does the search occur only at the point of original collection?',
    'Circuit split resolution by the Supreme Court, or explicit adoption/rejection of the query-as-search theory in a controlling appellate decision (e.g., a future certiorari grant on a 702-derived suppression motion).',
    'If queries are searches, this reading''s structural delta (pre-query probable cause requirement, individualized FISC review) becomes binding law; if not, the incidental_collection_reading''s framework governs and this reading remains a minority constitutional argument with only persuasive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(query_as_search_doctrinal_status, conceptual, 'Whether database query of lawfully collected data is itself a Fourth Amendment search event.').

omega_variable(
    standing_and_notice_barrier_scope,
    'How much of the measured suppression (0.55) is structural (classification, sealed proceedings, no notice mechanism) versus a doctrinal choice (standing rules) that could be relaxed without touching the underlying constitutional question?',
    'Track outcomes if Congress or courts mandate query notice to affected communicants in criminal proceedings; observe whether suppression drops independent of the constitutional merits being resolved.',
    'If suppression is mostly a separable notice/standing artifact, the constitutional floor reading could gain a much larger practical test population without any doctrinal shift on the search question itself — the two barriers currently compound each other in a way that may overstate this reading''s inherent difficulty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standing_and_notice_barrier_scope, empirical, 'Whether measured suppression is a standing-rule artifact separable from the constitutional merits.').

omega_variable(
    committer_framing_alternative_query_frequency,
    'Would framing this constraint around the FREQUENCY and SCALE of U.S.-person queries (millions per year, per oversight reports) rather than around the DOCTRINAL query-as-search claim change the classification — from a contested constitutional argument (tangled_rope, moderate ε) to a bulk-surveillance snare (high ε, concentrated victim set)?',
    'A separate constraint story authored around query volume and victim concentration, linked via network.affects_constraints, would test whether the scale framing produces a materially different ε and type than the doctrinal framing authored here.',
    'If the scale-framing story computes as snare while this doctrinal-framing story computes as tangled_rope, that divergence is itself informative about which framing the corpus should treat as primary for this kernel — but per the ε-invariance principle they should be authored as two separate stories, not reconciled within this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_alternative_query_frequency, conceptual, 'Alternative framing (query scale vs. doctrinal search classification) that would plausibly shift ε and type; documented here rather than folded into this story''s classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(fisa_tr_t2011, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2011, 0.25).
narrative_ontology:measurement(fisa_tr_t2014, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2014, 0.3).
narrative_ontology:measurement(fisa_tr_t2017, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2017, 0.34).
narrative_ontology:measurement(fisa_tr_t2020, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2020, 0.37).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2008, 0.1).
narrative_ontology:measurement(fisa_be_t2011, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2011, 0.13).
narrative_ontology:measurement(fisa_be_t2014, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2014, 0.17).
narrative_ontology:measurement(fisa_be_t2017, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2017, 0.2).
narrative_ontology:measurement(fisa_be_t2020, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2020, 0.22).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2024, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fisa_702_statutory_text__constitutional_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).

% DUAL FORMULATION NOTE:
% These three constraints decompose the single natural-language label 'the 702 U.S.-person query controversy' per the ε-invariance principle: each reading of the fisa_702_statutory_text kernel produces a structurally distinct claim about where the Fourth Amendment applies, with a different ε (this reading: 0.25, measuring constitutional compliance cost to the executive under adoption; incidental_collection_reading: measuring operational/foreign-intelligence value preserved by the status quo; foreign_target_strict_reading: measuring statutory-minimization compliance cost short of reaching the constitutional question). They are linked rather than merged because adopting this reading would foreclose or substantially narrow the operative force of incidental_collection_reading, while foreign_target_strict_reading offers a narrower statutory path that could resolve some disputes without reaching this reading's constitutional claim at all.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
