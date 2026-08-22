% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Constitutional Floor Reading: Section 702 Queries as Fourth Amendment Searches Requiring Probable Cause
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   Section 702 of FISA permits warrantless collection of communications of
 *   non-U.S. persons abroad, but this collection incidentally sweeps in the
 *   communications of U.S. persons. The unresolved kernel question is what
 *   constitutional status attaches when a government analyst then runs a
 *   query using a U.S. person's identifier against that already-collected
 *   database. This story instantiates the constitutional_floor_reading: that
 *   the query itself, regardless of what statutory language says about
 *   foreign targeting, is a Fourth Amendment search of the U.S. person's
 *   communications content, and therefore requires probable cause and
 *   (functionally) a warrant-equivalent order from the FISA Court before the
 *   query may run. This is a criminal-procedure reframing of what the
 *   executive branch and much of the FISA statutory apparatus treat as a
 *   foreign-intelligence administrative question. The sibling readings —
 *   foreign_target_strict_reading (which would restrict collection itself at
 *   the front end) and incidental_collection_reading (which treats the query
 *   as a permissible use of lawfully collected foreign intelligence data) —
 *   are separate constraints with their own ε and stakeholder structures,
 *   linked here via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - nsa_fbi_query_analysts: primary agenda_setter, bears the direct operational cost of the reading if adopted
 *   - us_persons_swept_into_702_query_results: primary payer under the status quo, primary intended beneficiary if this reading is adopted — a structurally reversed position depending on which reading governs
 *   - fisa_court_judiciary: institutional beneficiary of expanded review authority, also bears administrative burden
 *   - constitutional_rights_advocacy_groups: analytical/political proponent, benefits without bearing operational cost
 *   - supreme_court: ultimate observer whose eventual ruling would resolve the kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.55).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Constitutional Floor Reading: Section 702 Queries as Fourth Amendment Searches Requiring Probable Cause").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, 'a38be0cd-5ea4-43f9-b86f-488a848e730b').
narrative_ontology:cs_kernel_codification('a38be0cd-5ea4-43f9-b86f-488a848e730b', distributed).
narrative_ontology:cs_authority_grounding('a38be0cd-5ea4-43f9-b86f-488a848e730b', distributed).
narrative_ontology:cs_reading_relation('a38be0cd-5ea4-43f9-b86f-488a848e730b', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_reading_relation('a38be0cd-5ea4-43f9-b86f-488a848e730b', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_axiom('a38be0cd-5ea4-43f9-b86f-488a848e730b', foundational, query_stage_is_the_search_event).
narrative_ontology:cs_axiom_status(query_stage_is_the_search_event, holdable).
narrative_ontology:cs_axiom_grounding('a38be0cd-5ea4-43f9-b86f-488a848e730b', query_stage_is_the_search_event, deontological).
narrative_ontology:cs_axiom('a38be0cd-5ea4-43f9-b86f-488a848e730b', secondary, foreign_domestic_distinction_irrelevant_to_warrant_trigger).
narrative_ontology:cs_axiom_status(foreign_domestic_distinction_irrelevant_to_warrant_trigger, holdable).
narrative_ontology:cs_axiom_grounding('a38be0cd-5ea4-43f9-b86f-488a848e730b', foreign_domestic_distinction_irrelevant_to_warrant_trigger, deontological).
narrative_ontology:cs_reference_frame('a38be0cd-5ea4-43f9-b86f-488a848e730b', pre_third_party_doctrine_individualized_suspicion_baseline).
narrative_ontology:cs_drift_state('a38be0cd-5ea4-43f9-b86f-488a848e730b', post_snowden_reauthorization_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('a38be0cd-5ea4-43f9-b86f-488a848e730b', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_persons_communicating_with_foreign_targets).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, constitutional_rights_advocacy_groups).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, fisa_court_judiciary).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, us_persons_swept_into_702_query_results).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, intelligence_analysts_under_query_delay).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run queries against the Section 702 collection database to search for U.S. person identifiers, often for reasons unrelated to the original foreign intelligence purpose (e.g., a criminal investigation tip). Under this reading, every such query is itself a Fourth Amendment search requiring individualized probable cause, so analysts would need to obtain a warrant or FISA Court order before running a query on a U.S. person identifier, not merely before acting on the results. Their operational speed and ability to run bulk exploratory queries is what this reading directly constrains.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, nsa_fbi_query_analysts, agenda_setter,
    institutional, immediate, constrained, national).

% Ordinary Americans whose communications with a foreign target are incidentally collected and then queried by name. Under the incidental_collection_reading they have essentially no protection once collection is lawful; under this constitutional_floor_reading they gain a warrant requirement at the query stage, which is the entire structural point of adopting this reading. They cannot know they were queried and cannot exit the database once their communications are collected — their only protection is what this reading imposes upstream on the government.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_persons_swept_into_702_query_results, payer,
    powerless, biographical, trapped, national).

% Civil liberties organizations and some members of Congress who have pushed litigation and legislative amendments (e.g., failed 2023 warrant-requirement amendments) to force this reading into binding law. They benefit reputationally and programmatically from the reading's adoption but bear none of the operational cost; their exit option is simply continuing to litigate or lobby if this reading fails to take hold.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, constitutional_rights_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% The Foreign Intelligence Surveillance Court and its review panel would gain expanded individualized review authority over queries under this reading, rather than the current after-the-fact minimization-procedure oversight. This increases the court's institutional role and workload, which is simultaneously a gain in constitutional relevance and a real administrative burden it would have to absorb.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court_judiciary, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__constitutional_floor_reading, fisa_court_judiciary, agenda_setter).

% Working analysts and their agencies who must now build query-time probable-cause packages and route them through judicial review before running identifier searches that are currently near-instantaneous. In fast-moving threat scenarios (e.g., a tip about an imminent attack) they experience this reading as a direct operational cost imposed on them personally, since their performance is measured partly on response speed.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, intelligence_analysts_under_query_delay, payer,
    moderate, immediate, constrained, national).

% ODNI, NSA, and FBI leadership who have consistently argued in reauthorization hearings that the foreign_target_strict_reading or incidental_collection_reading better reflects the statute's purpose. They are structurally present in the policy debate but their objections are treated as institutional self-interest rather than as constitutional argument within this reading's own framework — their preferred readings are the ones this reading is built to override.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_branch_intelligence_leadership, excluded,
    institutional, generational, constrained, national).

% Has not yet definitively ruled on whether a 702 query of already-lawfully-collected data constitutes an independent Fourth Amendment search (the question left partly open after cases like Carpenter on third-party records). Its eventual ruling would settle which reading of the kernel becomes binding law rather than contested doctrine.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__constitutional_floor_reading, us_persons_swept_into_702_query_results).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__constitutional_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary between the government's foreign-intelligence collection authority and the individual constitutional protection against warrantless search, by locating the trigger for Fourth Amendment protection at the query rather than at the collection or use stage — solving the problem of what specific government act must be constitutionally justified when a database already contains a U.S. person's communications.
% TRANSFER_FUNCTION: Moves procedural burden and delay from the analyst/agency (who must now justify each U.S.-person query individually) to the individual whose communications are in the database (who gains a legal shield they did not have under the incidental_collection_reading), and moves institutional authority from the executive branch's internal minimization procedures to the FISA Court's individualized review.
% ABSENT_VOICES: Individuals actually queried under Section 702 are almost never told, so the people this reading exists to protect cannot testify to whether the protection worked, failed, or was needed — the debate is conducted almost entirely by advocacy groups, agencies, and courts on their behalf, without their direct participation.
% DISAPPEARANCE_RATIONALE: If this reading vanished from legal and political discourse, the operational status quo (query without a warrant, under the incidental_collection_reading or foreign_target_strict_reading) would simply continue unchallenged, so from the executive branch's perspective the world would barely rearrange. From the perspective of privacy advocates and any U.S. persons whose queries would have been blocked, the loss is invisible but real — a protection that would have applied to them never materializes. The disagreement about whether disappearance matters is itself part of the underlying kernel contest.
% FOUNDING_PROBLEM: The founding problem is the gap opened by Section 702's original 2008 design: a foreign-targeting statute lawfully sweeps in U.S. persons' communications as a byproduct, and once that data sits in a government database, nothing in the statutory text specifies whether querying it by a U.S. person's name is itself an act requiring the same constitutional justification a domestic wiretap would need.
% FOUNDING_PROBLEM_CORROBORATION: The Privacy and Civil Liberties Oversight Board (PCLOB), an independent executive-branch oversight body outside the direct chain of intelligence-collection beneficiaries, has repeatedly found in its public reports that U.S.-person query practices raise unresolved constitutional questions and that query volume and compliance incidents have both risen — corroborating that the founding problem remains live and is not merely asserted by advocacy groups seeking to expand the reading's reach.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, contested).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.25) because under this reading's own lights, the 'cost' the constraint imposes is a compliance cost on government process (query delay, individualized review), not a rent extracted from a vulnerable population — if anything, the reading redistributes protection toward the powerless. Suppression is authored moderate (0.55) because the reading, if adopted, would require active enforcement against institutional resistance: agencies would resist and slow-walk compliance, requiring court orders and possibly contempt proceedings to bite. Theater ratio rises over the measured interval (0.15 to 0.30) reflecting increasing symbolic invocation of 'constitutional concerns' in congressional hearings and PCLOB reports without commensurate binding legal change, since as of 2024 the reading remains persuasive authority rather than settled law. Suppression_requirement is shown rising because each reauthorization cycle (2012, 2018, 2023-24) has required progressively more active advocacy and litigation pressure to keep the reading alive against a status quo that has not adopted it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (query analysts and their agencies), this reading reads as a criminal-procedure imposition on a foreign-intelligence tool — a tangled_rope where the coordination function (protecting genuine foreign intelligence gathering) is real but the added warrant requirement extracts operational speed. From the payer-turned-beneficiary seat (swept-in U.S. persons), the same structure reads as the restoration of a baseline constitutional floor that should never have been absent. The engine computes these divergent per-seat classifications from the declared power/exit/scope data; this story does not adjudicate which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, us_persons_swept_into_702_query_results moves from a high-d (target/payer) position under the sibling incidental_collection_reading to a low-d (beneficiary) position here — the entire point of the constitutional_floor_reading is to flip their directionality by imposing an upstream constitutional gate. nsa_fbi_query_analysts and intelligence_analysts_under_query_delay carry the opposite shift: they become payers (bearing delay and compliance cost) under this reading where they were largely unconstrained under the sibling readings. This is not a metrics tuning artifact; it is the structural content of choosing this reading over its siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unclear constitutional status of a query against incidentally collected data) remains live per PCLOB's independent findings, so this is not a case of an arrangement outliving its function — it is a case of a constitutional protection that arguably never fully attached rather than one that atrophied. Framing this as tangled_rope rather than snare acknowledges that mandatory individualized review is a genuine coordination mechanism (matching constitutional process to database queries) that also imposes a real, unevenly distributed cost (on analysts and agencies) — not simple extraction with no coordination content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    query_as_search_doctrinal_status,
    'Does a database query of already-lawfully-collected communications constitute an independent ''search'' for Fourth Amendment purposes, or is the search complete at the point of collection?',
    'A definitive Supreme Court ruling squarely addressing 702 queries (extending or distinguishing Carpenter v. United States and third-party doctrine cases) would resolve this; lower court and FISA Court of Review decisions to date have been mixed and non-binding nationally.',
    'If the Court adopts the query-as-search view, this reading becomes binding constitutional law rather than a contested position, collapsing the kernel to a single reading. If the Court rejects it, this reading is foreclosed as a matter of law even though it may persist as a policy/legislative advocacy position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(query_as_search_doctrinal_status, conceptual, 'Whether the query itself is the constitutionally relevant act, independent of collection legality.').

omega_variable(
    reading_selection_grounds,
    'What in the source material and legislative/litigation record justifies treating the constitutional_floor_reading as the operative frame for this story rather than the foreign_target_strict_reading or incidental_collection_reading?',
    'The reading was selected per the kernel manifest''s explicit assignment; the structural delta (query-stage warrant requirement, criminal-procedure reframing, ε≈0.25 anchored to compliance cost rather than operational efficiency) is the manifest''s own specification, not an independent editorial choice by this story.',
    'Confirms this story''s ε and stakeholder structure are reading-indexed as required by DP-001; a differently-selected reading would produce a materially different ε (the incidental_collection_reading would likely author ε against the swept-in U.S. persons rather than against executive compliance cost).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_grounds, conceptual, 'Documents the committer-axis provenance of this reading''s selection and its consequence for ε''s referent.').

omega_variable(
    false_summit_natural_procedure_framing,
    'Is the query-as-search rule best understood as restoring a pre-existing constitutional floor (a mountain-like baseline that statutory text cannot lower), or is it itself a constructed doctrinal innovation that happens to benefit specific advocacy constituencies and the FISA Court''s institutional reach?',
    'Historical Fourth Amendment doctrine review: compare pre-702 case law on database queries of lawfully held records (e.g., NCIC queries, wiretap minimization rules) to determine whether an individualized-query warrant requirement has clear doctrinal antecedents or is a novel extension.',
    'If the floor is doctrinally novel rather than restored, the reading''s beneficiaries (advocacy groups, FISA Court) gain institutional standing from a claim of constitutional inevitability it may not actually possess — a classification-relevant ambiguity even though this story is not authored as a mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_procedure_framing, conceptual, 'Whether the constitutional floor is discovered or constructed, and who benefits from each characterization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(fisa_tr_t2012, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2012, 0.18).
narrative_ontology:measurement(fisa_tr_t2016, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement(fisa_tr_t2018, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(fisa_tr_t2021, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2021, 0.28).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2008, 0.12).
narrative_ontology:measurement(fisa_be_t2012, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2012, 0.15).
narrative_ontology:measurement(fisa_be_t2016, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2016, 0.18).
narrative_ontology:measurement(fisa_be_t2018, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2018, 0.2).
narrative_ontology:measurement(fisa_be_t2021, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2021, 0.22).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2008, 0.35).
narrative_ontology:measurement(fisa_su_t2012, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2012, 0.4).
narrative_ontology:measurement(fisa_su_t2016, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2016, 0.45).
narrative_ontology:measurement(fisa_su_t2018, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2018, 0.48).
narrative_ontology:measurement(fisa_su_t2021, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2021, 0.5).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).

% DUAL FORMULATION NOTE:
% This is one of three sibling constraints decomposed from the single natural-language label 'the FISA 702 U.S. person query controversy' per the ε-invariance principle. Each reading locates the constitutionally or statutorily relevant act at a different point in the collection-to-query pipeline (query stage here; collection stage in foreign_target_strict_reading; post-collection use in incidental_collection_reading), producing different ε, different beneficiary/victim sets, and different claimed types. All three are linked via affects_constraints and should be read as a family, not averaged into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
