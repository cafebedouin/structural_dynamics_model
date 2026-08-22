% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__incidental_collection_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__incidental_collection_reading
 *   human_readable: FISA Section 702 — Incidental Collection / Backdoor Search Reading
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   Section 702 of FISA authorizes warrantless targeting of non-U.S. persons
 *   believed to be located abroad for foreign intelligence purposes,
 *   requiring the compelled assistance of U.S. communications providers. This
 *   story instantiates the incidental_collection_reading of the FISA 702
 *   kernel: the reading under which the statutory text is understood to
 *   permit retention and later warrantless querying of the U.S.-person side
 *   of collected communications, so long as the original collection was
 *   justified by a foreign intelligence purpose. Under this reading, once
 *   content is lawfully in the government's holdings, subsequent access to it
 *   via U.S.-person identifier queries is not itself a 'search' requiring new
 *   judicial authorization. This is the reading that dominates current
 *   executive branch and much FISC practice; it differs sharply from the
 *   foreign_target_strict_reading (which would require the U.S.-person
 *   content to be minimized out of reach for domestic purposes) and from the
 *   constitutional_floor_reading (which holds that any government query
 *   touching U.S. person content is a Fourth Amendment search regardless of
 *   the statutory theory). Per Rule 1, this story does not average across
 *   those readings or hedge ε between them — it authors the
 *   incidental_collection_reading's own ε, victim set, and structure as a
 *   single, ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - nsa_collection_operations: agenda-setter and beneficiary (institutional/arbitrage) — sets minimization procedures and collects
 *   - fbi_domestic_investigations_division: beneficiary (institutional/arbitrage) — queries without warrant
 *   - us_persons_incidentally_collected: primary victim (powerless/trapped) — no notice, no exit
 *   - domestic_criminal_defendants_queried_via_702: secondary victim (powerless/trapped) — evidentiary exposure without disclosure
 *   - fisc_foreign_intelligence_surveillance_court: nominal check, largely deferential (institutional/analytical)
 *   - civil_liberties_organizations: excluded voice — no standing to challenge specific instances
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.62).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA Section 702 — Incidental Collection / Backdoor Search Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, '300a1949-7653-4f44-a35f-f1ec19434fae').
narrative_ontology:cs_kernel_codification('300a1949-7653-4f44-a35f-f1ec19434fae', fixed_text).
narrative_ontology:cs_authority_grounding('300a1949-7653-4f44-a35f-f1ec19434fae', extraction).
narrative_ontology:cs_interpretation_layer_present('300a1949-7653-4f44-a35f-f1ec19434fae').
narrative_ontology:cs_reading_relation('300a1949-7653-4f44-a35f-f1ec19434fae', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('300a1949-7653-4f44-a35f-f1ec19434fae', fisa_702_statutory_text__constitutional_floor_reading, forecloses).
narrative_ontology:cs_axiom('300a1949-7653-4f44-a35f-f1ec19434fae', foundational, lawful_collection_extinguishes_subsequent_search_requirement).
narrative_ontology:cs_axiom_status(lawful_collection_extinguishes_subsequent_search_requirement, holdable).
narrative_ontology:cs_axiom_grounding('300a1949-7653-4f44-a35f-f1ec19434fae', lawful_collection_extinguishes_subsequent_search_requirement, conventional).
narrative_ontology:cs_axiom('300a1949-7653-4f44-a35f-f1ec19434fae', secondary, foreign_intelligence_purpose_justifies_domestic_query_access).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_justifies_domestic_query_access, holdable).
narrative_ontology:cs_axiom_grounding('300a1949-7653-4f44-a35f-f1ec19434fae', foreign_intelligence_purpose_justifies_domestic_query_access, instrumental).
narrative_ontology:cs_reference_frame('300a1949-7653-4f44-a35f-f1ec19434fae', collection_lawfulness_governs_subsequent_access).
narrative_ontology:cs_drift_state('300a1949-7653-4f44-a35f-f1ec19434fae', post_2023_reauthorization_debate, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('300a1949-7653-4f44-a35f-f1ec19434fae', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations_division).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_community_foreign_targeting_programs).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, nsa_collection_operations).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, domestic_criminal_defendants_queried_via_702).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, journalists_and_attorneys_in_contact_with_foreign_targets).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_purpose_doctrine).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, incidental_collection_lawfulness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collects communications of targeted non-U.S. persons abroad under Section 702 certifications approved annually by the FISC, and in the process acquires the U.S.-person side of many conversations as an unavoidable byproduct of targeting foreign accounts. Retains this incidentally collected content in a queryable database rather than purging it, citing foreign intelligence value. Sets minimization procedures that govern retention and dissemination, and can revise them without new legislation.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, nsa_collection_operations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, nsa_collection_operations, beneficiary).

% Runs queries against the 702-collected database using U.S. person identifiers for domestic criminal and national security investigations, without a warrant, on the theory that querying already-lawfully-collected data is not a new search requiring judicial authorization. Gains investigative leads it would otherwise need probable cause to obtain directly.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations_division, beneficiary,
    institutional, biographical, arbitrage, national).

% Communicate with, or are mentioned by, a foreign target and have their content swept into government databases without ever being suspected of wrongdoing or given notice. Cannot know their communications were collected, cannot contest retention, and cannot prevent later warrantless querying. Ordinary use of international communication is itself what creates the exposure — there is no behavioral change that grants exit.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected, payer,
    powerless, biographical, trapped, national).

% Become subjects of criminal investigations initiated or advanced by warrantless queries of the 702 database, often without disclosure that 702-derived information was used, foreclosing meaningful suppression challenges. Their procedural rights depend on notice they frequently do not receive.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, domestic_criminal_defendants_queried_via_702, payer,
    powerless, immediate, trapped, national).

% Maintain sourcing and client relationships that route through foreign contacts of intelligence interest, exposing privileged and confidential communications to incidental collection and later query. Professional function requires the contact that creates the exposure, so meaningful exit would mean abandoning the work itself.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, journalists_and_attorneys_in_contact_with_foreign_targets, payer,
    moderate, biographical, constrained, global).

% Reviews and approves annual Section 702 certifications and minimization procedures in a largely non-adversarial proceeding, issuing periodic compliance opinions that have documented repeated FBI querying violations. Has authority to reject certifications but has rarely done so, and its proceedings are substantially classified.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fisc_foreign_intelligence_surveillance_court, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, fisc_foreign_intelligence_surveillance_court, agenda_setter).

% Reauthorizes Section 702 on a multi-year cycle and can impose statutory query restrictions. Receives classified briefings from the agencies whose programs it oversees, creating an information asymmetry that shapes what reforms are proposed or enacted.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congress_intelligence_committees, agenda_setter,
    institutional, generational, analytical, national).

% Argue publicly and in litigation that warrantless querying of incidentally collected U.S. person communications violates the Fourth Amendment, but lack standing to challenge specific collections because the classified nature of the program prevents plaintiffs from proving they were surveilled. Their objections shape public debate but rarely reach adjudication on the merits.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_organizations, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations_division).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables continuous, unbroken foreign intelligence collection against non-U.S. persons abroad without requiring the government to filter out or discard the U.S. side of intercepted communications in real time, which would be technically difficult and would fragment intelligence value across an entire conversation.
% TRANSFER_FUNCTION: Moves investigative access and evidentiary leverage from U.S. persons who have no reason to expect government scrutiny to domestic law enforcement and intelligence agencies, without the probable-cause showing the Fourth Amendment would otherwise require for accessing the same content directly.
% ABSENT_VOICES: The individual U.S. persons whose communications are queried are never in the room — they typically never learn a query occurred, cannot contest retention before the FISC, and are represented, if at all, only by advocacy organizations who lack standing to bring their specific cases forward.
% DISAPPEARANCE_RATIONALE: If the incidental-collection querying authority vanished, the FBI would lose a substantial channel of investigative leads currently obtained without warrants, would need to seek independent probable cause or judicial authorization to access equivalent content, and the volume of warrantless domestic-facing queries (numbering in the hundreds of thousands annually per FISC compliance reports) would collapse to near zero — a materially different domestic investigative landscape.
% FOUNDING_PROBLEM: Foreign intelligence collection against targets abroad inevitably sweeps up communications involving U.S. persons who happen to correspond with, or be mentioned by, those targets; the statute was built to establish that this incidental collection is lawful and usable rather than requiring costly real-time filtering or automatic destruction.
% FOUNDING_PROBLEM_CORROBORATION: The intelligence community attests the querying function remains essential to counterterrorism and counterintelligence work. Independent corroboration outside the benefiting agencies comes from the FISC's own declassified compliance opinions documenting tens of thousands of noncompliant queries, the Privacy and Civil Liberties Oversight Board's divided 2023 report noting the querying practice exceeds what the coordination rationale requires, and federal public defenders who have identified cases where 702-derived evidence was used without required notice — these sources corroborate that the original foreign-collection rationale has been substantially decoupled from current domestic-query practice.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__incidental_collection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__incidental_collection_reading, 0.45, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__incidental_collection_reading_tests).
:- end_tests(fisa_702_statutory_text__incidental_collection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.45 reflecting the specific structural delta of this reading: a large, growing volume of warrantless domestic queries against incidentally collected content, corroborated by the FISC's own compliance opinions documenting systematic overuse. This is lower than a pure snare because a genuine foreign-intelligence coordination function is real and load-bearing (intercepting a foreign target's communications necessarily captures the other party) — hence tangled_rope rather than snare. Suppression (0.62) is high because U.S. persons cannot know they were collected, cannot contest retention, and typically cannot obtain standing to challenge specific queries; this is a raw structural fact independent of scope scaling. Theater ratio (0.40) reflects that minimization procedures and FISC oversight have real components but have documented and repeated compliance failures, suggesting a meaningful performative share.
 *
 * DIRECTIONALITY LOGIC:
 *   NSA and FBI hold institutional power with arbitrage-grade exit (they set and revise the rules governing their own access) — d is derived near the beneficiary end. U.S. persons incidentally collected are powerless and trapped by the ordinary act of international communication — d is derived near the full-target end, and no override is needed because the structural derivation already captures this correctly. Journalists and attorneys occupy a moderate-power, constrained-exit position: they retain professional standing but cannot avoid the contacts that create exposure without abandoning their work, which the derivation captures via the constrained exit atom rather than requiring an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — avoiding costly real-time filtering of incidental U.S.-person content during lawful foreign targeting — remains partially live (filtering at scale is still technically difficult), which prevents a clean 'dead mandate' classification. But the querying practice that grew on top of that founding rationale (hundreds of thousands of FBI backdoor searches annually) is a distinct, later-layered function that the original coordination problem does not require. The tangled_rope classification captures this: a real coordination core persists (foreign collection efficiency) while an asymmetric extraction layer (domestic warrantless querying) has been added and requires active enforcement (FISC certification renewal, minimization procedure maintenance) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    query_versus_search_characterization,
    'Is a warrantless query of already-collected 702 data, using a U.S. person identifier, a new ''search'' triggering Fourth Amendment protection, or merely an act of examining data the government already lawfully possesses?',
    'Circuit split resolution or Supreme Court ruling directly addressing whether querying constitutes a search independent of the original collection''s lawfulness; alternatively, definitive FISC en banc or Foreign Intelligence Surveillance Court of Review holding.',
    'If courts settle on ''query is a search,'' this reading is foreclosed and the constraint collapses toward the constitutional_floor_reading; if ''query is not a search'' is settled, this reading''s current tangled_rope status stabilizes as the controlling law rather than a contested administrative practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(query_versus_search_characterization, conceptual, 'Whether database querying of incidental collection is itself a Fourth Amendment search — the load-bearing distinction between this reading and constitutional_floor_reading.').

omega_variable(
    foreign_intelligence_purpose_genuineness,
    'In practice, how often is the ''foreign intelligence purpose'' justification for retention and querying genuinely operative versus a post-hoc label applied to what are substantively domestic criminal investigations?',
    'Statistical analysis of FISC compliance reports and DOJ Inspector General audits cross-referencing query purpose codes against downstream case outcomes (criminal prosecution versus counterintelligence action).',
    'A high rate of domestic-criminal-outcome queries labeled as foreign-intelligence-purpose would support reclassifying a larger share of the constraint''s operation as pure extraction (snare-leaning) rather than tangled coordination; a low rate would support the coordination function being more load-bearing than currently measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_intelligence_purpose_genuineness, empirical, 'Whether the stated foreign intelligence purpose gating queries is genuine or a formal label covering domestic investigative use.').

omega_variable(
    reading_selection_evidentiary_basis,
    'Given that the statutory text itself does not explicitly resolve whether post-collection querying requires new authorization, what specifically guided treating incidental_collection_reading (rather than foreign_target_strict_reading) as the operative, currently-controlling reading for this story?',
    'Documented executive branch practice (minimization procedures as approved by FISC), the volume and persistence of FBI backdoor queries under those procedures, and PCLOB/IG reports describing this as the reading actually implemented — as distinct from the reading Congress or civil liberties advocates argue the text should be given.',
    'If a future reauthorization statutorily mandates the foreign_target_strict_reading''s minimization requirements with enforceable teeth, this constraint''s ε would need to be re-measured as a new story rather than treated as a continuation of the same reading — the ε-invariance principle requires a new file, not an updated value, at that point.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, conceptual, 'Documents why this reading, among the three declared kernel readings, is authored as the currently operative one, per the CS-framing under-determination guidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(fisa_tr_t2011, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2011, 0.24).
narrative_ontology:measurement(fisa_tr_t2014, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2014, 0.28).
narrative_ontology:measurement(fisa_tr_t2017, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2017, 0.32).
narrative_ontology:measurement(fisa_tr_t2020, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2020, 0.36).
narrative_ontology:measurement(fisa_tr_t2023, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2023, 0.39).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2008, 0.22).
narrative_ontology:measurement(fisa_be_t2011, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2011, 0.28).
narrative_ontology:measurement(fisa_be_t2014, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2014, 0.33).
narrative_ontology:measurement(fisa_be_t2017, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2017, 0.38).
narrative_ontology:measurement(fisa_be_t2020, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement(fisa_be_t2023, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2023, 0.44).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2008, 0.4).
narrative_ontology:measurement(fisa_su_t2011, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2011, 0.46).
narrative_ontology:measurement(fisa_su_t2014, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2014, 0.5).
narrative_ontology:measurement(fisa_su_t2017, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2017, 0.55).
narrative_ontology:measurement(fisa_su_t2020, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2020, 0.59).
narrative_ontology:measurement(fisa_su_t2023, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2023, 0.61).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the FISA 702 kernel (fisa_702_statutory_text) per the ε-invariance principle: incidental_collection_reading (this story, ε≈0.45, tangled_rope), foreign_target_strict_reading (lower ε, closer to rope — minimization actually constrains domestic access), and constitutional_floor_reading (treats any query as a per se constitutional violation, snare-leaning from the rights-holder's structural position). Each reading has its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged into one hedged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
